(in-package :hems)

#| See if X is conditionally independent from Y given Z.
   Returns boolean. Destructively modifies net and sepsets. |#

;; net = network
;; df = data frame
;; xy = xy pair of variables to test conditional independence
;; z = list of variables to condition on
;; sepsets = hash table of sepsets
;; xy-key = xy key into sepsets
;; yx-key = yx key into sepsets
(defun n-test-conditional-independence (net df xy z sepsets xy-key yx-key &key (test #'g-squared-test) (threshold .05))
  (let (x-var y-var p-value edge-removed-p)
    (setq x-var (intern (rule-based-cpd-dependent-var (aref (car net) (car xy)))))
    (setq y-var (intern (rule-based-cpd-dependent-var (aref (car net) (cdr xy)))))
    (when (and (not (equal test #'g-squared-test))
	       (not (equal test #'chi-squared-test)))
      (error "Given statistical independence test, ~A, is not supported. Input must be eitehr #'g-squared-test or #'chi-squared-test." test))
    (setq p-value (funcall test df
			   x-var
			   y-var
			   (mapcar #'(lambda (ele)
				       (intern (rule-based-cpd-dependent-var (aref (car net) ele))))
				   z)))
    (when (> p-value threshold)
      ;; delete the edge between x and y
      (remhash (cdr xy) (gethash (car xy) (cdr net)))
      (remhash (car xy) (gethash (cdr xy) (cdr net)))
      (setf (gethash xy-key sepsets)
	    (cons z (gethash xy-key sepsets)))
      (setf (gethash yx-key sepsets)
	    (cons z (gethash yx-key sepsets)))
      (setq edge-removed-p t))
    (values edge-removed-p p-value sepsets)))

#| Perform adjacency search through a fully connected undirected graph by sequentially removing edges.
   Returns a destructively modified graph (<array of cpds> . <2d hash table of edges>). |#

;; net = graph
;; df = teddy data frame
(defun n-adjacency-search (net df)
  (labels ((get-x-y-pairs (adjacencies n)
	     (loop
	       with x-y-pairs
	       for x being the hash-keys of adjacencies
		 using (hash-value children-hash)
	       when (>= (- (hash-table-count children-hash) 1) n)
		 do
		    (loop
		      for y being the hash-keys of children-hash
		      do
			 (setq x-y-pairs (cons (list :x-y-pair (cons x y)
						     :x-children children-hash)
					       x-y-pairs)))
	       finally
		  (return x-y-pairs)))
	   (n-subsets-helper (lst n acc)
	     (cond ((= n 0)
		    (when acc
		      (list (reverse acc))))
		   ((null lst)
		    nil)                 
		   (t
		    (nconc
		     (n-subsets-helper (cdr lst) (1- n) (cons (car lst) acc))
		     (n-subsets-helper (cdr lst) n acc)))))
	   (n-subsets (lst n)
	     (n-subsets-helper lst n nil))
	   (adjx-minus-y (adjx y)
	     (loop
	       for n being the hash-keys of adjx
	       when (not (equal n y))
		 collect n into adj
	       finally
		  (return adj))))
    (when t
      (format t "~%Conducting Adjacency Search"))
    (loop
      with sepsets = (make-hash-table :test #'equal)
      with adjacencies = (cdr net)
      with x-y-pairs
      with n = 0
      do
	 (setq x-y-pairs (get-x-y-pairs adjacencies n))
	 (loop
	   with adj-list and xy and xy-key and yx-key
	   for x-y-pair in x-y-pairs
	   do
	      (setq xy (getf x-y-pair :x-y-pair))
	      (setq xy-key (format nil "~d,~d" (car xy) (cdr xy)))
	      (setq yx-key (format nil "~d,~d" (cdr xy) (car xy)))
	      (setq adj-list (adjx-minus-y (getf x-y-pair :x-children)
					   (cdr (getf x-y-pair :x-y-pair))))
	      (loop
		with p-value and edge-removed-p = nil
		with subsets = (n-subsets adj-list n) and x-var and y-var
		while (and subsets (gethash (cdr xy) (gethash (car xy) adjacencies)))
		do
		   (multiple-value-bind (status p-value sepsets-hash)
		       (n-test-conditional-independence net df xy (car subsets) sepsets xy-key yx-key :test #'g-squared-test)
		     (setq edge-removed-p status)
		     (when nil
		       (format t "~%~%xy-pair: ~A~%n: ~d~%subset, T, of adjacencies(X)\{Y}:~%~S~%net:~%~S~%edge removed?: ~S~%p-value: ~d" xy n (car subsets) (cdr net) edge-removed-p p-value))
		     (setq sepsets sepsets-hash))
		   (setq subsets (rest subsets))))
	 (setq n (+ n 1))
      until (null x-y-pairs)
      finally
	 (return (values net sepsets)))))

#| Determine if the triple of variables, x, y, and z form a triangle |#

;; net = graph
;; x, y, z = variables in the graph
(defun triangle-p (net x y z)
  (let ((xy-edge (gethash y (gethash x (cdr net))))
	(yx-edge (gethash x (gethash y (cdr net))))
	(xz-edge (gethash z (gethash x (cdr net))))
	(zx-edge (gethash x (gethash z (cdr net))))
	(zy-edge (gethash y (gethash z (cdr net))))
	(yz-edge (gethash z (gethash y (cdr net)))))
    (when (and xy-edge yx-edge
	       xz-edge zx-edge
	       zy-edge yz-edge)
      t)))

#| Determine if y is a collider in the triple of variables, x, y, and z |#

;; net = graph
;; x, y, z = variables in the graph
(defun collider-p (net x y z)
  (let ((xy-edge (gethash y (gethash x (cdr net))))
	(yx-edge (gethash x (gethash y (cdr net))))
	(xz-edge (gethash z (gethash x (cdr net))))
	(zx-edge (gethash x (gethash z (cdr net))))
	(zy-edge (gethash y (gethash z (cdr net))))
	(yz-edge (gethash z (gethash y (cdr net)))))
    (when (and (not (null xy-edge))
	       (not (null yx-edge))
	       (not (null zy-edge))
	       (not (null yz-edge))
	       (equal #\> (aref xy-edge 2))
	       (equal #\< (aref yx-edge 0))
	       (equal #\> (aref zy-edge 2))
	       (equal #\< (aref yz-edge 0))
	       (null xz-edge)
	       (null zx-edge))
      t)))

#| Determine if y is a collider in the triple of variables, x, y, and z |#

;; net = graph
;; x, y, z = variables in the graph
(defun potential-collider-p (net x y z)
  (let ((xy-edge (gethash y (gethash x (cdr net))))
	(xz-edge (gethash z (gethash x (cdr net))))
	(zx-edge (gethash x (gethash z (cdr net))))
	(zy-edge (gethash y (gethash z (cdr net)))))
    (when (and xy-edge
	       zy-edge
	       (null xz-edge)
	       (null zx-edge))
      t)))

#| Get all undirected paths from a to b.
   Returns a list of paths. Each path is a list of vertices. |#

;; net = graph
;; a = source node
;; b = destination node
(defun get-all-paths-from-a-to-b (net a b &key (min-path-length 3))
  (labels ((path-worker (x visited path acc)
	     (setf (gethash x visited) t)
	     (setq path (cons x path))
	     (if (equal x b)
		 (when (>= (length path) min-path-length)
		   (setq acc (cons (reverse path) acc)))
		 (loop
		    for i being the hash-keys of (gethash x (cdr net))
		    when (not (gethash i visited))
		    do
		      (setq acc (path-worker i visited path acc))))
	     (setq path (cdr path))
	     (remhash x visited)
	     acc))
    (path-worker a (make-hash-table) nil nil)))

(defun fci (df)
  (labels ((generate-network (variables) 
	     (when t
	       (format t "~%Generating fully connected network"))
	     (let (vars program)
	       (if variables
		   (setq vars variables)
		   (setq vars (ls-user:keys df)))
	       (loop
		 for v being the elements of vars
		 for i from 0
		 do
		    (setq program (concatenate 'list
					       program `(,(intern (format nil "C~d" i)) = (percept-node ,v :value "T")))))
	       (loop
		 for v1 being the elements of vars
		 for i from 0
		 do
		    (loop
		      for v2 being the elements of vars
		      for j from 0
		      when (not (= i j))
			do
			   (setq program
				 (concatenate 'list
					      program
					      `(,(intern (format nil "C~d" i))
						---
						,(intern (format nil "C~d" j)))))))
	       (eval `(compile-program (:sort-p nil :causal-discovery t) ,@program))))
	   (get-x-y-pairs (net)
	     (loop
	       with adjacencies = (cdr net)
	       with x-y-pairs
	       for x being the hash-keys of adjacencies
		 using (hash-value children-hash)
	       do
		  (loop
		    for y being the hash-keys of children-hash
		    do
		       (setq x-y-pairs (cons (cons x y) x-y-pairs)))
	       finally
		  (return x-y-pairs)))
	   (get-x-y-z-triples (net)
	     (loop
	       with adjacencies = (cdr net)
	       with x-y-z-pairs
	       for x being the hash-keys of adjacencies
		 using (hash-value x-children-hash)
	       do
		  (loop
		    for y being the hash-keys of x-children-hash
		    do
		       (loop
			 for z being the hash-keys of (gethash y adjacencies)
			 when (and (not (equal x y))
				   (not (equal x z))
				   (not (equal y z)))
			   do
			      (setq x-y-z-pairs (cons (list x y z) x-y-z-pairs))))
	       finally
		  (return x-y-z-pairs)))
	   (n-orient-edges-ambiguously (net)
	     (loop
	       for x being the hash-keys of (cdr net)
		 using (hash-value x-children)
	       do
		  (loop
		    for y being the hash-keys of x-children
		    do
		       (setf (gethash y x-children) "o-o"))))
	   (n-orient-colliders (net sepsets)
	     (loop
	       with adjacencies = (cdr net)
	       with xy-edge and zy-edge and yx-edge and yz-edge
	       with sepset-key
	       with triples = (get-x-y-z-triples net)
	       for (x y z) in triples
	       when (potential-collider-p net x y z)
		 do
		    (setq sepset-key (format nil "~d,~d" x z))
		    (when (not (in-sepset-p y  (gethash sepset-key sepsets)))
		      (setq xy-edge (gethash y (gethash x adjacencies)))
		      (setq yx-edge (gethash x (gethash y adjacencies)))
		      (setq zy-edge (gethash y (gethash z adjacencies)))
		      (setq yz-edge (gethash z (gethash y adjacencies)))
		      (setf xy-edge (format nil "~a~a" (aref xy-edge 0) "->"))
		      (setf yx-edge (format nil "~a~a" "<-" (aref yx-edge 2)))
		      (setf zy-edge (format nil "~a~a" (aref zy-edge 0) "->"))
		      (setf yz-edge (format nil "~a~a" "<-" (aref yz-edge 2))))))
	   (get-network-paths (net)
	     (loop
	       with paths-hash = (make-hash-table :test #'equal)
	       for i being the hash-keys of (cdr net)
		 using (hash-value neighbors)
	       do
		  (loop
		    for j being the hash-keys of (cdr net)
		    when (not (equal i j))
		      do
			 (setf (gethash (format nil "~a,~a" i j) paths-hash)
			       (get-all-paths-from-a-to-b net i j)))
	       finally
		  (return paths-hash)))
	   (get-possible-d-sep (net a b possible-d-seps)
	     (let ((d-sep-key (format nil "~d,~d" a b))
		   (a-b-paths nil))
	       (when (null (gethash d-sep-key possible-d-seps))
		 (setq a-b-paths (get-all-paths-from-a-to-b net a b))
		 (loop
		   for path in a-b-paths
		   do
		      (loop
			named subpath-check
			for x on path
			for y on (rest x)
			for z on (rest y)
			when (or (collider-p net (car x) (car y) (car z))
				 (triangle-p net (car x) (car y) (car z)))
			  do
			     (setf (gethash d-sep-key possible-d-seps)
				   (cons (car y) (gethash d-sep-key possible-d-seps))))))
	       (gethash d-sep-key possible-d-seps)))
	   (n-remove-d-separated-edges (net possible-d-seps sepsets)
	     (loop
	       with xy-key and yx-key and xy-seps
	       for xy-pair in (get-x-y-pairs net)
	       do
		  (setq xy-key (format nil "~d,~d" (car xy-pair) (cdr xy-pair)))
		  (setq yx-key (format nil "~d,~d" (cdr xy-pair) (car xy-pair)))
		  (setq xy-seps (get-possible-d-sep net (car xy-pair) (cdr xy-pair) possible-d-seps))
		  (when nil
		    (format t "~%~%xy-pair: ~S~%xy-seps:~%~S" xy-pair xy-seps))
		  (loop
		    for z on (set-difference xy-seps (list (car xy-pair) (cdr xy-pair)))
		    do
		       (when nil
			 (format t "~%z:~%~S" z))
		       (n-test-conditional-independence net df xy-pair z sepsets xy-key yx-key))
	       finally
		  (return (values net sepsets))))
	   (in-sepset-p (var sepsets)
	     (loop
	       for sep in sepsets
	       when (member var sep)
		 do
		    (return-from in-sepset-p t))
	     nil)
	   (n-orient-unshielded-triples (net sepsets-hash)
	     (loop
		for a being the hash-keys of (cdr net)
		using (hash-value a-children)
		do
		  (loop
		     for b being the hash-keys of (cdr net)
		     using (hash-value b-children)
		     when (gethash b a-children)
		     do
		       (loop
			  for c being the hash-keys of (cdr net)
			  using (hash-value c-children)
			  when (gethash c b-children)
			  do
			    (when (and (not (equal a b))
				       (not (equal a c))
				       (not (equal b c))
				       (equal "---" (gethash b a-children))
				       (equal "---" (gethash c b-children))
				       (not (gethash c a-children))
				       (not (in-sepset-p b (gethash (format nil "~d,~d" a c) sepsets-hash))))
			      (setf (gethash b a-children) "-->")
			      (setf (gethash a b-children) "<--")
			      (setf (gethash b c-children) "-->")
			      (setf (gethash c b-children) "<--"))))))
	   (directed-path-p (net path)
	     (loop
		with adjacencies = (cdr net)
		with edge
		for x in path
		for y in (rest path)
		do
		  (setq edge (gethash y (gethash x adjacencies)))
		  (when (not (equal #\> (aref edge 2)))
		    (return-from directed-path-p nil)))
	     t)
	   (colliding-path-p (net path c)
	     (loop
	       named check
	       with adjacencies = (cdr net)
	       for x on path
	       for y on (rest x)
	       for z on (rest y)
	       when (or (not (collider-p net (car x) (car y) (car z)))
			(not (gethash y (gethash c adjacencies))))
		 do
		    (return-from check nil))
	     t)
	   (definite-discriminating-path-p (net path)
	     (let* ((triple (last path 3))
		    (d (first triple))
		    (b (second triple))
		    (c (third triple))
		    (a (first path))
		    (adjacencies (cdr net)))
	       (if (and (triangle-p net d c b)
			(collider-p net d c b)
			(not (gethash a (gethash c adjacencies)))
			(colliding-path-p net (butlast path) c))
		 t)))
	   (n-orient-interpath-edges-p (net path underlined-vars-hash sepsets-hash)
	     (loop
	       with oriented-p = nil
	       with adjacencies = (cdr net)
	       with a = (first path) and b = (car (last path))
	       for x on path
	       for y on (rest x)
	       for z on (rest y)
	       do
		  (cond ((and (equal #\> (aref (gethash (car y) (gethash (car x) adjacencies)) 2))
			      (member (list (car x) (car y) (car z)) (gethash (car y) underlined-vars-hash)
				      :test #'equal))
			 (setf (gethash (car z)
					(gethash (car y) adjacencies))
			       "-->")
			 (setf (gethash (car y)
					(gethash (car z) adjacencies))
			       "<--")
			 (setq oriented-p t))
			(t
			 (loop
			   with d = (car (last path 2))
			   for c being the hash-keys of (gethash b adjacencies)
			   when (and (not (equal c a))
				     (not (equal c d)))
			     do
				(cond ((and (collider-p net a b c)
					    (not (gethash c (gethash a adjacencies)))
					    (not (collider-p net a d c)))
				       (setq oriented-p t)
				       (setf (gethash b (gethash d adjacencies))
					     (format nil "~a->" (aref (gethash b (gethash d adjacencies)) 0)))
				       (setf (gethash d (gethash b adjacencies))
					     (format nil "<-~a" (aref (gethash d (gethash b adjacencies)) 2))))
				      ((and (equal #\> (aref (gethash b (gethash c adjacencies)) 2))
					    (equal "-->" (gethash d (gethash b adjacencies)))
					    (equal #\o (aref (gethash c (gethash d adjacencies)) 0)))
				       (setq oriented-p t)
				       (setf (gethash d (gethash c adjacencies))
					     (format nil "~a->" (aref (gethash d (gethash c adjacencies)) 0)))
				       (setf (gethash c (gethash d adjacencies))
					     (format nil "<-~a" (aref (gethash c (gethash d adjacencies )) 2))))
				      ((definite-discriminating-path-p net (append path (list c)))
				       (cond ((member b (gethash (format nil "~a,~a" a c) sepsets-hash))
					      (setf (gethash b underlined-vars-hash)
						    (cons (list d b c) (gethash b underlined-vars-hash))))
					     (t
					      (setq oriented-p t)
					      (let ((db-edge (gethash b (gethash d adjacencies)))
						    (bd-edge (gethash d (gethash b adjacencies)))
						    (bc-edge (gethash c (gethash b adjacencies)))
						    (cb-edge (gethash b (gethash c adjacencies))))
						(setf db-edge (format nil "~a->" (aref db-edge 0)))
						(setf bd-edge (format nil "<-~a" (aref bd-edge 2)))
						(setf bc-edge (format nil "<-~a" (aref bc-edge 2)))
						(setf cb-edge (format nil "~a->" (aref cb-edge 0)))))))))))
	       finally
		  (return oriented-p)))
	   (n-final-orientation (net network-paths underlined-vars-hash sepsets-hash)
	     (loop
	       with oriented-p = nil
	       do
		  (setq oriented-p nil)
		  (loop
		    with adjacencies = (cdr net)
		    with ab-edge and ba-edge
		    with ba-key
		    with a and b
	            for ab-key being the hash-keys of network-paths
		      using (hash-value ab-paths)
		    do
		       (setq ba-key (split-sequence:split-sequence #\, ab-key))
		       (setq b (parse-integer (second ba-key)))
		       (setq a (parse-integer (first ba-key)))
		       (setq ba-key (format nil "~d,~d" (second ba-key) (first ba-key)))
		       (setq ab-edge (gethash b (gethash a adjacencies)))
		       (setq ba-edge (gethash a (gethash b adjacencies)))
		       (loop
			 for path in ab-paths
			 do
			    (cond ((and ab-edge (directed-path-p net path))
				   (setf ab-edge (format nil "~a->" (aref ab-edge 0)))
				   (setf ba-edge (format nil "<-~a" (aref ba-edge 2)))
				   (setq oriented-p t))
				  (t
				   (setq oriented-p (n-orient-interpath-edges-p net path underlined-vars-hash sepsets-hash))))))
	       until (not oriented-p))))
    (let ((possible-d-seps (make-hash-table :test #'equal))
	  (underlined-vars-hash (make-hash-table :test #'equal))
	  (net (generate-network (ls-user:keys df))))
      (when nil
	(format t "~%network: ~A" (cdr net))
	(break))
      (multiple-value-bind (net1 sepsets-hash)
	  (n-adjacency-search net df)
	(declare (ignore net1))
	(when nil
	  (format t "~%network after adjacency search: ~A" (cdr net))
	  (break))
	(n-orient-unshielded-triples net sepsets-hash)
	(when nil
	  (format t "~%network after orienting unshielded triples: ~A" (cdr net))
	  (break))
	(multiple-value-bind (net2 sepsets-h)
	    (n-remove-d-separated-edges net possible-d-seps sepsets-hash)
	  (declare (ignore net2))
	  (when nil
	    (format t "~%network after removing d-separated-edges: ~A" (cdr net))
	    (break))
	  (setq sepsets-hash sepsets-h))
	(n-orient-edges-ambiguously net)
	(when nil
	  (format t "~%network after resetting network edges to o-o: ~A" (cdr net))
	  (break))
	(n-orient-colliders net sepsets-hash)
	(when nil
	  (format t "~%network after orienting collider edges: ~A" (cdr net))
	  (break))
	(n-final-orientation net (get-network-paths net) underlined-vars-hash sepsets-hash)
	(when nil
	  (format t "~%network after the final orientation rule: ~A" (cdr net))
	  (break)))
      net)))

(defun make-edge-bindings (net)
  (loop with bindings = nil
	for node being the hash-keys of (cdr net)
	using (hash-value nbrs)
	do
	(loop for nbr being the hash-keys of nbrs
	      do
	      (setq bindings (cons (list node nbr) bindings))
	finally
	(return bindings))))

#| TESTS
(ql:quickload :hems)
(ls-user:defdf df (ls-user:read-csv #P"/home/david/Code/HARLEM/ep_data_1000/ppo_FrozenLake-v1_data.csv"))
(hems:make-df mnist "/home/david/Code/HARLEM/mnist_data.csv")
(hems:n-format-df-column-names df)
(setq df (ls-user:remove-columns df '(episode_number timestep)))
(hems:fci df)

(defdf df* (ls-user:read-csv #P"/home/david/Code/HARLEM/ep_data_1000/ppo_FrozenLake-v1_data.csv"))
(setf df* (ls-user:remove-columns df* '("Episode_Number" "Timestep")))
|#
