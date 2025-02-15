(in-package :hems)

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
      t))

#| Performs g-squared conditional independence test. 
   Null hypothesis: The variables X and Y are conditionally dependent given the variable Z
   Returns scalar p-value  |#

;; df = lisp-stat dataframe
;; x = column name
;; y = column name
;; zs = column names
(defun g-squared-test (df x y zs)
  (let ((smoothing 0)
	(x-vals (remove-duplicates (coerce (ls-user:column df x) 'list) :test #'equal))
	(y-vals (remove-duplicates (coerce (ls-user:column df y) 'list) :test #'equal))
	(z-vals (loop
		   for z in zs
		   nconcing (remove-duplicates (coerce (ls-user:column df z) 'list) :test #'equal) into vals
		   finally
		     (return vals)))
	(x-idx (col-index df x))
	(y-idx (col-index df y))
	(z-idxs (mapcar #'(lambda (z)
			    (col-index df z))
			zs) )
	(nxyz-hash (make-hash-table :test #'equal))
	(nxz-hash (make-hash-table :test #'equal))
	(nz-hash (make-hash-table :test #'equal))
	(nyz-hash (make-hash-table :test #'equal))
	(dof -1)
	(sum 0))
    (loop
      with cur-x and cur-y and cur-z
      with nxyz-key and nxz-key and nz-key and nyz-key
      for row being the elements of (ls-user:rows df)
      do
	 (setq cur-x (aref row x-idx))
	 (setq cur-y (aref row y-idx))
	 (setq cur-z (format nil "(~{~a~^  ~})"
			     (mapcar #'(lambda (z-idx)
					 (aref row z-idx))
				     z-idxs)))
	 (setq nxyz-key (format nil "~d,~d,~d" cur-x cur-y cur-z))
	 (setq nxz-key (format nil "~d,~d" cur-x cur-z))
	 (setq nz-key (format nil "~d" cur-z))
	 (setq nyz-key (format nil "~d,~d" cur-y cur-z))
	 (when (null (gethash nxyz-key nxyz-hash))
	   (setf (gethash nxyz-key nxyz-hash) 0))
	 (when (null (gethash nxz-key nxz-hash))
	   (setf (gethash nxz-key nxz-hash) 0))
	 (when (null (gethash nz-key nz-hash))
	   (setf (gethash nz-key nz-hash) 0))
	 (when (null (gethash nyz-key nyz-hash))
	   (setf (gethash nyz-key nyz-hash) 0))	     
	 (setf (gethash nxyz-key nxyz-hash)
	       (+ (gethash nxyz-key nxyz-hash) 1))
	 (setf (gethash nxz-key nxz-hash)
	       (+ (gethash nxz-key nxz-hash) 1))
	 (setf (gethash nyz-key nyz-hash)
	       (+ (gethash nyz-key nyz-hash) 1))
	 (setf (gethash nz-key nz-hash)
	       (+ (gethash nz-key nz-hash) 1)))
    (loop
      with nxz-key and nz-key and nyz-key
      with nxz and nz and nyz and all-keys
      for nxyz-key being the hash-keys of nxyz-hash
	using (hash-value nxyz)
      do
	 (setq all-keys (split-sequence:split-sequence #\, nxyz-key))
	 (setq nxz-key (format nil "~a,~a" (nth 0 all-keys) (nth 2 all-keys)))
	 (setq nyz-key (format nil "~a,~a" (nth 1 all-keys) (nth 2 all-keys)))
	 (setq nz-key (format nil "~a" (nth 2 all-keys)))
	 (setq nxz (gethash nxz-key nxz-hash))
	 (setq nz (gethash nz-key nz-hash))
	 (setq nyz (gethash nyz-key nyz-hash))
	 (setq sum (+ sum (* nxyz
			     (if (> nxyz 0)
				 (log (/ (* nxyz nz)
					 (* (+ nxz smoothing)
					    (+ nyz smoothing))))
				 0)))))    
    (setq dof (* (- (length x-vals) 1) (- (length y-vals) 1) (length z-vals)))
    (if (= dof 0)
	1
	(- 1 (statistics:chi-square-cdf (* 2d0 (+ sum (if (= sum 0) .0001d0 0d0))) dof)))))

#| Performs chi-squared conditional independence test. 
   Null hypothesis: The variables X and Y are conditionally dependent given the variable Z
   Returns scalar p-value  |#

;; df = lisp-stat dataframe
;; x = column name
;; y = column name
;; zs = column names
(defun chi-squared-test (df x y zs)
  (let ((smoothing 0)
	(x-vals (remove-duplicates (coerce (ls-user:column df x) 'list) :test #'equal))
	(y-vals (remove-duplicates (coerce (ls-user:column df y) 'list) :test #'equal))
	(z-vals (loop
		   for z in zs
		   nconcing (remove-duplicates (coerce (ls-user:column df z) 'list) :test #'equal) into vals
		   finally
		     (return vals)))
	(x-idx (col-index df x))
	(y-idx (col-index df y))
	(z-idxs (mapcar #'(lambda (z)
			    (col-index df z))
			zs) )
	(nxyz-hash (make-hash-table :test #'equal))
	(nxz-hash (make-hash-table :test #'equal))
	(nz-hash (make-hash-table :test #'equal))
	(nyz-hash (make-hash-table :test #'equal))
	(dof -1)
	(sum 0))
    (loop
      with cur-x and cur-y and cur-z
      with nxyz-key and nxz-key and nz-key and nyz-key
      for row being the elements of (ls-user:rows df)
      do
	 (setq cur-x (aref row x-idx))
	 (setq cur-y (aref row y-idx))
	 (setq cur-z (format nil "(~{~a~^  ~})"
			     (mapcar #'(lambda (z-idx)
					 (aref row z-idx))
				     z-idxs)))
	 (setq nxyz-key (format nil "~d,~d,~d" cur-x cur-y cur-z))
	 (setq nxz-key (format nil "~d,~d" cur-x cur-z))
	 (setq nz-key (format nil "~d" cur-z))
	 (setq nyz-key (format nil "~d,~d" cur-y cur-z))
	 (when (null (gethash nxyz-key nxyz-hash))
	   (setf (gethash nxyz-key nxyz-hash) 0))
	 (when (null (gethash nxz-key nxz-hash))
	   (setf (gethash nxz-key nxz-hash) 0))
	 (when (null (gethash nz-key nz-hash))
	   (setf (gethash nz-key nz-hash) 0))
	 (when (null (gethash nyz-key nyz-hash))
	   (setf (gethash nyz-key nyz-hash) 0))	     
	 (setf (gethash nxyz-key nxyz-hash)
	       (+ (gethash nxyz-key nxyz-hash) 1))
	 (setf (gethash nxz-key nxz-hash)
	       (+ (gethash nxz-key nxz-hash) 1))
	 (setf (gethash nyz-key nyz-hash)
	       (+ (gethash nyz-key nyz-hash) 1))
	 (setf (gethash nz-key nz-hash)
	       (+ (gethash nz-key nz-hash) 1)))
    (loop
       with nxz-key and nz-key and nyz-key
       with nxz and nz and nyz and all-keys
       with o and e
       for nxyz-key being the hash-keys of nxyz-hash
       using (hash-value nxyz)
       do
	 (setq all-keys (split-sequence:split-sequence #\, nxyz-key))
	 (setq nxz-key (format nil "~a,~a" (nth 0 all-keys) (nth 2 all-keys)))
	 (setq nyz-key (format nil "~a,~a" (nth 1 all-keys) (nth 2 all-keys)))
	 (setq nz-key (format nil "~a" (nth 2 all-keys)))
	 (setq nxz (gethash nxz-key nxz-hash))
	 (setq nz (gethash nz-key nz-hash))
	 (setq nyz (gethash nyz-key nyz-hash))
	 (setq o nxyz)
	 (setq e (* (+ nxz smoothing)
		    (+ nyz smoothing)))
	 (setq sum (+ sum (/ (expt (- o e) 2) e))))    
    (setq dof (* (- (length x-vals) 1) (- (length y-vals) 1) (length z-vals)))
    (if (= dof 0)
	1
	(- 1 (statistics:chi-square-cdf (* 2d0 (+ sum (if (= sum 0) .0001d0 0d0))) dof)))))

#| TESTS
(hems:g-squared-test
 (hems:read-csv "/home/david/Code/HARLEM/ep_data_1/ppo_FrozenLake-v1_data.csv")
 "Action" "Observation" '("Hidden State"))
|#
