#| Algorithm for finding nodes reachable from X given Z via active trains.
   Returns a list of cpds
   Taken from Koller 2009 |#

;; bn = Bayesian network graph
;; x = source variable
;; big-z = list of cpd observations
;; id-cpd-hash = hash table mapping cpd id to cpd structure. Can obtain by calling (get-cpd-from-id-aux)
(defun reachable (bn x big-z id-cpd-hash)
  (labels ((add-children-to-l (y l)
	     (loop
	       with i = (- (array-dimension (car bn) 0) 1)
	       with z = (aref (car bn) i) 
	       while (not (equal y z))
	       do
		  (when (cpd-child-p z y)
		    (setq l (cons (cons z 'down) l)))
		  (setq i (- i 1)))
	     l)
	   (add-parents-to-l (y l)
	     (loop
	       for z being the hash-keys of y
		 using (hash-value idx)
	       when (> idx 0)
		 do
		    (setq l (cons (cons
				   (gehash z id-cpd-hash)
				   'up)
				  l)))
	     l))
    (let ((l big-z)
	  (a nil)
	  (v nil)
	  (r nil))
      (loop
	with y
	while (not (null l))
	do
	   (setq y (car l))
	   (setq l (cdr l))
	   (if (not (member y l :test #'equal))
	       (loop
		 for parent being the hash-keys of y
		   using (hash-value idx)
		 when (not (= idx 0))
		   do
		      (setq l (cons (gethash parent id-cpd-hash) l))))
	   (setq a (cons y a)))
      (setq l (cons (cons x 'up) nil))
      (loop
	with y and d
	while (not (null l))
	do
	   (setq y (caar l))
	   (setq d (cdar l))
	   (setq l (cdr l))
	   (when (not (member (cons y d) v :test #'equal))
	     (if (not (member y big-z :test #'equal))
		 (setq r (cons y r)))
	     (setq v (cons (cons y d) v))
	     (cond ((and (eq d 'up) (not (member y big-z :test #'equal)))
		    (setq l (add-parents-to-l y l))
		    (setq l (add-children-to-l y l)))
		   ((eq d 'down)
		    (if (not (member y big-z :test #'equal))
			(setq l (add-children-to-l y l)))
		    (if (member y a :test #'equal)
			(setq l (add-parents-to-l y l)))))))
      r)))

#| Algorithm for determining d-separation between two nodes in a Bayesian network
  Returns boolean |#

;; bn = Bayesian network
;; id-cpd-hash = hash table mapping id to cpd structure. Can obtain by calling (get-cpd-from-id-aux)
;; x = source variable
;; y = sink variable
;; z = list of cpd observations
(defun d-separation (bn id-cpd-hash x y z)
  (if (member y (reachable bn x z id-cpd-hash) :test #'equal)
      nil
      t)
