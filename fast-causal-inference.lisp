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
    edge-removed))

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
		   (setq edge-removed (n-test-conditional-independence net df xy (car subsets) xy-key yx-key)) 
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
	(xz-edge (gethash z (gethash x (cdr net))))
	(zx-edge (gethash x (gethash z (cdr net))))
	(zy-edge (gethash y (gethash z (cdr net)))))
    (when (and (or (equal xy-edge "-->")
		   (equal xy-edge "o->")
		   (equal xy-edge "<->")
		   (equal xy-edge "---")
		   (equal xy-edge "o-o"))
	       (or (equal zy-edge "-->")
		   (equal zy-edge "o->")
		   (equal zy-edge "<->")
		   (equal zy-edge "---")
		   (equal zy-edge "o-o"))
	       (or (equal xz-edge "-->")
		   (equal xz-edge "o->")
		   (equal xz-edge "<->")
		   (equal xz-edge "---")
		   (equal xy-edge "o-o")))
      t)))

#| Determine if y is a collider in the triple of variables, x, y, and z |#

;; net = graph
;; x, y, z = variables in the graph
(defun collider-p (net x y z)
  (let ((xy-edge (gethash y (gethash x (cdr net))))
	(xz-edge (gethash z (gethash x (cdr net))))
	(zx-edge (gethash x (gethash z (cdr net))))
	(zy-edge (gethash y (gethash z (cdr net)))))
    (when (and (or (equal xy-edge "->")
		   (equal xy-edge "o->")
		   (equal xy-edge "<->"))
	       (or (equal zy-edge "->")
		   (equal zy-edge "o->")
		   (equal zy-edge "<->"))
	       (null xz-edge))
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

(defun get-possible-d-sep (net a b possible-d-seps)
  (labels ((get-paths-from-a-to-b (x visited path acc)
	     (setf (gethash x visited) t)
	     (setq path (cons x path))
	     (if (equal x b)
		 (when (>= path 3)
		   (setq acc (cons path acc)))
		 (loop
		    for i being the hash-keys of (gethash x (cdr net))
		    when (not (gethash i visited))
		    do
		      (setq acc (get-paths-from-a-to-b i b visited path acc))))
	     (setq path (cdr path))
	     (remhash x visited)
	     acc)
	   )
    (let ((d-sep-key (format nil "~d,~d" a b))
	  (a-b-paths nil))
      (when (null (gethash d-sep-key possible-d-seps))
	(setq a-b-paths (get-paths-from-a-to-b a (make-hash-table) nil nil))
	(loop
	  for path in a-b-paths
	  do
	     (loop
	       for x on path
	       for y on (rest x)
	       for z on (rest y)
	       when (or (collider-p net x y z)
			(triangle-p net x y z))
		 do
		    (setf (gethash d-sep-key possible-d-seps)
			  (cons y (gethash d-sep-key possible-d-seps))))))
      (values (gethash d-sep-key possible-d-seps) a-b-paths possible-d-seps)))
  
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
	       with xy-edge and zy-edge and sepset-key
	       with triples = (get-x-y-z-triples)
	       for (x y z) in triples
	       when (potential-collider-p net x y z)
		do
		  (setq sepset-key (format nil "~d,~d" x z))
		  (when (not (member y (gethash sepset-key sepsets)))
		    (setq xy-edge (gethash y (gethash x adjacencies)))
		    (setf zy-edge (gethaxh y (gethash z adjacencies)))
		    (setf (gethash y (gethash x adjacencies))
			  (format nil "~a~a" (aref xy-edge 0)"->"))
		    (setf (gethash y (gethash z adjacencies))
			  (format nil "~a~a" (aref zy-edge 0) "->")))
	       finally
		  (return triples)))
	   (remove-d-separated-edges (possible-d-seps sepsets)
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
		  (return all-paths)))
	   (in-sepset-p (var sepsets)
	     (loop
	       for sepset in sepsets
	       when (member var sepset)
		 do
		    (return-from in-sepset-p t))
	     nil)
	   (get-definite-discriminating-path (x y b)

	     (loop
	       for x on path
	       for y on (rest x)
	       for z on (rest y)
	       when (or (collider-p net x y z)
			(triangle-p net x y z))
		 do
		    (setf (gethash d-sep-key possible-d-seps)
			  (cons y (gethash d-sep-key possible-d-seps)))))
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
					   (gethash b a-children)
					   (gethash c b-children)
					   (not (gethash c a-children))
					   (not (in-sepset-p b (gethash (format nil "~d,~d" a c) sepsets-hash))))
				  (setf (gethash b a-children) "-->")
				  (setf (gethash b c-children) "-->"))))))
	   (directed-path-p (path)
	     (loop
		with adjacencies = (cdr net)
		with edge
		for x in path
		for y in (rest path)
		do
		  (setq edge (gethash y (gethash x adjacencies)))
		  (when (not (equal ">" (aref edge 2)))
		    (return-from directed-path-p nil)))
	     t)
	   (n-orient-r-paths (net-paths triples)
	     (loop
		with adjacencies = (cdr net)
		with xy-edge and oriented-p
	        for xy-key being the hash-keys of network-paths
		  using (hash-value xy-paths)
		do
		     (destructuring-bind (x y)
			 (split-sequence:split-sequence #\, xy-key)
		       (setq x (parse-integer x))
		       (setq y (parse-integer y))
		       (setq xy-edge (gethash y (gethash x adjacencies)))
		       (when (and xy-edge 
				  (not (equal xy-edge "-->"))
				  (some #'directed-path-p xy-paths))
			 (setf xy-edge (format nil "~a~a" (aref xy-edge 0) "->"))
			 (setq oriented-p t)
			 (return-from n-orient-r-paths oriented-p))
		       (loop
			 named tripler
			 for triple in triples
			 when (and (equal x (car (last triple)))
				   (not (and (equal ">" (aref (gethash (second triple)
								       (gethash (first triple) adjacencies))
							      2))
					     (not (equal ">" (aref (gethash (second triple)
									    (gethash x adjacencies))
								   2))))))
			   do
			      (return-from tripler nil)
			 finally
			    (when (not (equal "-->"
					      (gethash x (gethash (second triple) adjacencies))))
			      (setf (gethash x (gethash (second triple) adjacencies)) "-->")
			      (setq oriented-p t)
			      (return-from n-orient-r-paths oriented-p))))
		finally
		   (return oriented-p)))
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
	   (ddp-helper (a y b visited path acc)
	     (setf (gethash a visited) t)
	     (setq path (cons a path))
	     (if (equal a y)
		 (when (and (>= path 3)
			    (null (gethash y (gethash a (cdr net))))
			    (null (gethash a (gethash y (cdr net)))))
		   (setq acc (cons path acc)))
		 (loop
		   with adjacencies = (cdr net)
		   with ia-edge
		   for i being the hash-keys of adjacencies
		   using (hash-value edges-hash);;(gethash a adjacencies)
		   when (gethash a edges-hash)
		     do
			(setq ia-edge (gethash a edges-hash))
			(and (not (gethash i visited))
			     (or (and (equal a x)
				      (gethash y (gethash i adjacnecies))
				      (equal ">" (aref ia-edge 2)))
				 (and (not (equal a b))
				      (not (equal a x))
				      (gethash y (gethash i adjacnecies))
				      (equal ">" (aref ia-edge 2))
				      (collider-p net (second path 2) a i))
				 (equal a b)))
		     do
			(setq acc (worker i visited path acc))))
	     (setq path (cdr path))
	     (remhash a visited)
	     acc)
	   (get-definite-discriminating-path-for-b (x y b acc)
	     (ddp-helper worker x (make-hash-table) nil nil))
	   (definite-discriminating-paths (triples)
	     (loop
	       with adjacencies = (cdr net)
	       with paths
	       for triple in triples
	       when (triangle-p triple)
		 do
		    (loop
		      with c and bs and ds and pair
		      for v in triple
		      do
			 (setq pair (remove v triple))
			 (setq bs nil)
			 (setq ds nil)
			 (when (collider-p (first pair) v (second pair))
			   (setq c v)
			   (when (equal ">"
					(aref (gethash (first pair)
						       (gethash (second pair) adjacencies))
					      2))
			     (setq ds (cons (first pair) ds))
			     (setq bs (cons (second pair) bs)))
			   (when (equal ">"
					(aref (gethash (second pair)
						       (gethash (first pair) adjacencies))
					      2))
			     (setq ds (cons (second pair) ds))
			     (setq bs (cons (first pair) bs)))
			   (when bs
			     (loop
			       for b in bs
			       for d in ds
			       do
				  (setq paths (get-definite-discriminating-path-for-b d c b paths))))))
	       finally
		  (return paths)))
	   (n-final-orientation (network-paths triples sepsets-hash)
	     (let (oriented-r-edge oriented-triple-edge))
	     (loop
		with adjacencies = (cdr net)
		with xy-edge
		with yx-key
		with x and y
	        for xy-key being the hash-keys of network-paths
		  using (hash-value xy-paths)
		do
		   (setq yx-key (split-sequence:split-sequence #\, xy-key))
		   (setq yx-key (format nil "~d,~d" (second yx-key) (first yx-key)))
		   (setq xy-edge (gethash y (gethash x adjacencies)))
		   (cond ((and xy-edge (some #'directed-path-p xy-paths))
			  (setf xy-edge (format nil "~a~a" (aref xy-edge 0) "->"))))))))
    (let ((possible-d-seps (make-hash-table :test #'equal))
	  network-paths
	  triples)
      (multiple-value-bind (net1 sepsets-hash)
	  (adjacency-search net df)
	(declare (ignore net1))
	(n-orient-unshielded-triples sepsets-hash)
	(setq network-paths (remove-d-separated-edges possible-d-seps sepsets-hash))
	(n-orient-edges-ambiguously)
	(setq triples (n-orient-colliders))))))
