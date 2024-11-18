#| See if X is conditionally independent from Y given Z.
   Returns boolean. Destructively modifies net and sepsets. |#

;; net = network
;; df = data frame
;; xy = xy pair of variables to test conditional independence
;; z = list of variables to condition on
;; sepsets = hash table of sepsets
;; xy-key = xy key into sepsets
;; yx-key = yx key into sepsets
(defun n-test-conditional-independence (net df xy z sepsets xy-key yx-key)
  (let (x-var y-var p-value edge-removed-p)
    (setq x-var (rule-based-cpd-dependent-var (aref (car net) (car xy))))
    (setq y-var (rule-based-cpd-dependent-var (aref (car net) (cdr xy))))
    (setq p-value (g-squared-test df
				  x-var
				  y-var
				  (mapcar #'(lambda (ele)
					      (rule-based-cpd-dependent-var (aref (car net) ele)))
					  z)))
    (when (< p-value .05)
      ;; delete the edge between x and y
      (remhash (cdr xy) (gethash (car xy) (cdr net)))
      (remhash (car xy) (gethash (cdr xy) (cdr net)))
      (setf (gethash xy-key sepsets)
	    (cons (car subsets) (gethash xy-key sepsets)))
      (setf (gethash yx-key sepsets)
	    (cons (car subsets) (gethash yx-key sepsets)))
      (setq edge-removed-p t))
    (values edge-removed sepsets)))

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
		    (list (reverse acc)))
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
		 do
		   (multiple-value-bind (status sepsets-hash)
		       (n-test-conditional-independence net df xy (car subsets) sepsets xy-key yx-key)
		     (setq edge-removed status)
		     (setq sepsets sepsets-hash))
		   (setq subsets (rest subsets))
		until (or (null subsets)
			  edge-removed-p)))
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
	(zy-edge (gethash z (gethash y (cdr net)))))
    (when (and (equal #\> (aref xy-edge 2))
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
	       (null xz-edge))
      t)))
#| Get all discriminating paths from a to b for c, ignoring edge orientations.
   Returns a list of paths, each path is a list of verticex |#

;; net = graph
;; x = source node
;; y = destination node
;; b = the variable the descriminating path is for
(defun get-definite-discriminating-paths-for-b (net x y b)
  (labels ((worker (a visited path acc)
	     (setf (gethash a visited) t)
	     (setq path (cons a path))
	     (if (equal a y)
		 (when (and (>= path 3)
			    (equal (b (second path))))
		   (setq acc (cons (reverse path) acc)))
		 (loop
		   wth adjacencies = (cdr net)
		   for i being the hash-keys of (gethash a adjacencies)
		   when (and (not (gethash i visited))
			     (or (and (equal a x)
				      (gethash y (gethash i adjacnecies))
				      (equal ">" (aref (gethash i (gethash a adjacencies))) 2))
				 (and (not (equal a b))
				      (not (equal a x))
				      (gethash y (gethash i adjacnecies))
				      (equal ">" (aref (gethash i (gethash a adjacencies))) 2)
				      (collider-p net (second path 2) a i))
				 (equal a b)))
		     do
			(setq acc (worker i visited path acc))))
	     (setq path (cdr path))
	     (remhash a visited)
	     acc)))
  (when (and (not (gethash y (gethash x (cdr net))))
	     (not (gethash x (gethash y (cdr net)))))
    (worker x (make-hash-table) nil nil)))

#| Get all paths from a to b.
   Returns a list of paths. Each path is a list of vertices. |#

;; net = graph
;; a = source node
;; b = destination node
(defun get-all-paths-from-a-to-b (net a b)
  (labels ((path-worker (x visited path acc)
	     (setf (gethash x visited) t)
	     (setq path (cons x path))
	     (if (equal x b)
		 (when (>= path 3)
		   (setq acc (cons (reverse path) acc)))
		 (loop
		    for i being the hash-keys of (gethash x (cdr net))
		    when (not (gethash i visited))
		    do
		      (setq acc (path-worker i visited path acc))))
	     (setq path (cdr path))
	     (remhash x visited)
	     acc)))
  (path-worker a (make-hash-table) nil))

(defun n-fci (net df)
  (labels ((get-x-y-pairs ()
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
	   (get-x-y-z-triples ()
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
			 do
			    (setq x-y-z-pairs (cons (list x y z) x-y-z-pairs)))
		    finally
		       (return x-y-z-pairs))))
	   (n-orient-edges-ambiguously ()
	     (loop
	       for x being the hash-keys of (cdr net)
		 using (hash-value x-children)
	       do
		  (loop
		    for y being the hash-keys of x-children
		    do
		       (setf (gethash y x-children "o-o")))))
	   (n-orient-colliders ()
	     (loop
		with adjacencies = (cdr net)
		with xy-edge and zy-edge and yx-edge and yz-edge
		with sepset-key
		with triples = (get-x-y-z-triples)
		for (x y z) in triples
		when (potential-collider-p net x y z)
		do
		  (setq sepset-key (format nil "~d,~d" x z))
		  (when (not (member y (gethash sepset-key sepsets)))
		    (setq xy-edge (gethash y (gethash x adjacencies)))
		    (setq yx-edge (gethash x (gethash y adjacencies)))
		    (setq zy-edge (gethash y (gethash z adjacencies)))
		    (setq yz-edge (gethash z (gethash y adjacencies)))
		    (setf xy-edge (format nil "~a~a" (aref xy-edge 0) "->"))
		    (setf yx-edge (format nil "~a~a" "<-" (aref yx-edge 2)))
		    (setf zy-edge (format nil "~a~a" (aref zy-edge 0) "->"))
		    (setf yz-edge (format nil "~a~a" "<-" (aref yz-edge 2))))))
	   (get-network-paths ()
	     (loop
	       with paths-hash = (make-hash-table #'equal)
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
	   (get-possible-d-sep (a b possible-d-seps)
	     (let ((d-sep-key (format nil "~d,~d" a b))
		   (a-b-paths nil))
	       (when (null (gethash d-sep-key possible-d-seps))
		 (setq a-b-paths (get-all-paths-from-a-to-b net a b))
		 (loop
		   for path in a-b-paths
		   do
		      (loop
			for x on path
			for y on (rest x)
			for z on (rest y)
			when (or (collider-p net (car x) (car y) (car z))
				 (triangle-p net (car x) (car y) (car z)))
			  do
			     (setf (gethash d-sep-key possible-d-seps)
				   (cons y (gethash d-sep-key possible-d-seps))))))
	       (values (gethash d-sep-key possible-d-seps) a-b-paths possible-d-seps)))
	   (n-remove-d-separated-edges (possible-d-seps sepsets)
	     (loop
	       with xy-key and yx-key
	       with all-paths = (make-hash-table :test #'equal)
	       for xy-pair in (get-x-y-pairs)
		do
		  (setq xy-key (format nil "~d,~d" (car xy-pair) (cdr xy-pair)))
		  (setq yx-key (format nil "~d,~d" (cdr xy-pair) (car xy-pair)))
		  (multiple-value-bind (xy-seps xy-paths)
		      (get-possible-d-sep net (car xy-par) (cdr xy-pair) possible-d-seps)
		    (setf (gethash xy-key all-paths) xy-paths)
		    (loop
		       for z on (remove (cdr xy-pair)
					(remove (car xy-pair) xy-seps))
		       do
			 (n-test-conditional-independence net df xy-pair z sepsets xy-key yx-key)))
		finally
		  (return (values net sepsets))))
	   (in-sepset-p (var sepsets)
	     (loop
		for key being the hash-keys of sepsets
		using (hash-value s)
	       when (member var s)
		 do
		    (return-from in-sepset-p t))
	     nil)
	   (n-orient-unshielded-triples (sepsets-hash)
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
	   (directed-path-p (path)
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
	   (n-add-edges-into-collider (triples)
	     (loop
	       with adjacencies = (cdr net) and d-oriented
	       for (a b c) in triples
	       when (and (not (triangle-p net a b c))
			 (collider-p net a b c))
		 do
		    (loop
		      with db-edge
		      for d in being the hash-keys of (gethash b adjacencies)
		      when (not (collider-p net a d c))
			do
			   (setq db-edge (gethash b (gethash d adjacencies)))
			   (setf db-edge (format nil "~a~a" (aref db-edge 0) "->"))
			   (setq d-oriented t))
	       finally
		  (return d-oriented)))
	   (n-orient-potential-triangle (triples)
	     (loop
	       with adjacencies = (cdr net) and oriented-p
	       with yx-edge and xz-edge and yz-edge
	       with xy-edge and zx-edge and zy-edge
	       for (x y z) in triples
	       when (potential-triangle-p net y x z)
		 do
		    (yx-edge (gethash x (gethash y adjacencies)))
		    (xz-edge (gethash z (gethash x adjacencies)))
		    (yz-edge (gethash y (gethash z adjacencies)))
		    (xy-edge (gethash y (gethash x adjacencies)))
		    (zx-edge (gethash x (gethash z adjacencies)))
		    (zy-edge (gethash z (gethash y adjacencies)))
		    (when (and (or (equal yx-edge "<->")
				   (equal yx-edge "-->")
				   (equal yx-edge "o->"))
			       (equal xz-edge "-->")
			       (or (equal yz-edge "<-o")
				   (equal yz-edge "--o")
				   (equal yz-edge "o-o")))
		      (setf yz-edge (format nil "~a~a" (aref yz-edge 0) "->"))
		      (setf zy-edge (format nil "~a~a" "<-" (aref zy-edge 2))))))
	   (colliding-path-p (path C)
	     (loop
	       named check
	       with adjacencies = (cdr net)
	       for x on path
	       for y on (rest x)
	       for z on (rest y)
	       when (or (not (collider-p net (car x) (car y) (car z)))
			(not (gethash y (gethash c adjacencies)))
		 do
		    (return-from check nil))
	     t))
	   (definite-discriminating-path-p (path)
	     (let* ((triple (last path 3))
		    (d (first triple))
		    (b (second triple))
		    (c (third triple))
		    (a (first path))
		    (adjacencies (cdr net)))
	       (if (and (triangle-p d c b)
			(collider-p d c b)
			(not (gethash a (gethash c adjacencies)))
			(colliding-path-p (butlast path) c))
		 t)))
	   (orient-interpath-edges-p (path underlined-vars-hash sepsets-hash)
	     (loop
	       with oriented-p = nil
	       with adjacencies = (cdr net)
	       with a = (first path) and b = (car (last path))
	       for x on path
	       for y on (rest x)
	       for z on (rest y)
	       do
		  (cond ((and (equal #\> (aref (gethash (car y) (gethash (car x) adjacencies))))
			      (member (list (car x) (car y) (car z)) (gethash (car y) underlined-vars)
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
				      ((and (equal #\> (aref (gethash b (gethash c adjacencies))))
					    (equal "-->" (gethash d (gethash b adjacencies)))
					    (equal #\o (aref (gethash c (gethash d adjacencies)) 0)))
				       (setq oriented-p t)
				       (setf (gethash d (gethash c adjacencies))
					     (format nil "~a->" (aref (gethash d (gethash c adjacencies)) 0)))
				       (setf (gethash c (gethash d adjacencies))
					     (format nil "<-~a" (aref (gethash c (gethash d adjacencies )) 2))))
				      ((definite-discriminating-path-p (append path (list c)))
				       (cond ((member b (gethash (format nil "~a,~a" a c) sepsets-hash))
					      (setf (gethash b underlined-vars-hash)
						    (cons (list d b c) (gethash b underlined-vars-hash))))
					     (t
					      (setq oriented-p t)
					      (let ((db-edge (gethash b (gethash d adjacencies)))
						    (bd-edge (gethash d (getaash b adjacencies)))
						    (bc-edge (gethach c (gethash b adjacencies)))
						    (cb-edge (gethach b (gethash c adjacencies))))
						(setf db-edge (format nil "~a->" (aref db-edge 0)))
						(setf bd-edge (format nil "<-~a" (aref bd-edge 2)))
						(setf bc-edge (format nil "<-~a" (aref bc-edge 2)))
						(setf cb-edge (format nil "~a->" (aref cb-edge 0)))))))))))
	       finally
		  (return oriented-p)))
	   (n-final-orientation (network-paths underlined-vars-hash sepsets-hash)
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
		       (setq b (second ba-key))
		       (setq a (first ba-key)
			     (setq ba-key (format nil "~d,~d" (second ba-key) (first ba-key)))
			     (setq ab-edge (gethash b (gethash a adjacencies)))
			     (setq ba-edge (gethash a (gethash b adjacencies)))
			     (loop
			       for path in ab-paths
			       do
				  (cond ((and ab-edge (directed-path-p path))
					 (setf ab-edge (format nil "~a->" (aref ab-edge 0)))
					 (setf ba-edge (format nil "<-~a" (aref ba-edge 2)))
					 (setq oriented-p t))
					(t
					 (setq oriented-p (orient-interpath-edges-p path underlined-vars-hash sepsets-hash)))))))
	       until (not oriented-p)))
    (let ((possible-d-seps (make-hash-table :test #'equal))
	  network-paths
	  triples
	  underlined-vars-hash (mkae-hash-table :test #'equal))
      (multiple-value-bind (net1 sepsets-hash)
	  (n-adjacency-search net df)
	(declare (ignore net1))
	(n-orient-unshielded-triples sepsets-hash)
	(multiple-value-bind (net2 sepsets-h)
	    (n-remove-d-separated-edges possible-d-seps sepsets-hash)
	  (declare (ignore net2))
	  (setq sepsets-hash sepsets-h))
	(n-orient-edges-ambiguously)
	(setq network-paths (get-network-paths))
	(n-orient-colliders)
	(n-final-orientation (get-network-paths) underlined-vars-hash sepsets-hash))
      net))))
