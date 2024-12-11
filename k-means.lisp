(in-package :hems)

#| Euclidean distance formula |#

;; p1 = point 1
;; p2 = point 2
(defun distance (p1 p2)
  (sqrt (reduce #'+ (mapcar #'(lambda (e1 e2)
				(expt (- e1 e2) 2))
			    p1 p2))))

;; df = teddy dataframe
;; num-clusters = number of clusters
(defun k-means (df num-clusters)
  (labels ((argmax (lst)
	     (loop
	       with max = most-negative-fixnum
	       with am
	       for i in lst
	       for j from 0
	       when (> i max)
		 do
		    (setq max i)
		    (setq am j)
	       finally
		  (return am)))
	   (get-teddy-df-row (idx)
	     (loop
	       named finder
	       with it = (teddy/data-frame:make-iterator df)
	       with i = 0
	       for row = (funcall it)
	       do
		  (when (= i idx)
		    (return-from finder row))
		  (incf i)))
	   (initialize ()
	     (let ((centroids))
	       (setq centroids (cons (get-teddy-df-row
				      (random (teddy/data-frame::num-rows df)))
				     centroids))
	       (loop
		 with next-centroid
		 with dist
		 for cid from 1 to (- num-clusters 1)
		 do
		    (setq dist nil)
		    (format t "~%~%cid: ~d~%dist: ~S" cid dist)
		    (loop
		      with d = most-positive-fixnum
		      with iter = (teddy/data-frame:make-iterator df)
		      for point = (funcall iter)
		      when (numberp point)
			do
			   (format t "~%point: ~S" point)
			   (loop
			     for c in centroids
			     do
				(format t "~%centroid: ~S" c)
				(setq d (min d (distance point c))))
			   (setq dist (cons d dist)))
		    (setq next-centroid (get-teddy-df-row (argmax dist)))
		    (setq centroids (cons next-centroid centroids))
		    (setq dist nil))
	       centroids)))
    (initialize)))

#| TESTS 
(let (df)
  (setq df (hems:read-csv "/home/david/Code/HARLEM/ep_data_1000/ppo_FrozenLake-v1_data.csv")) 
  (setq df (teddy/data-frame::slice df :columns '("ACTION" "REWARDS")))
  (hems::k-means df 2)
  )
|#

#|
(ql:quickload :lisp-stat)
(ql:quickload :cl-ppcre)
(ql:quickload :sqldf)
(ql:quickload :hems)
(ls-user:defdf df (ls-user:read-csv #P"/home/david/Code/HARLEM/ep_data_1000/ppo_FrozenLake-v1_data.csv"))
(setf df (ls-user:remove-columns df '(EPISODE-NUMBER TIMESTEP)))
(hems:n-format-df-column-names df)
(ls-user:column-names df)
(hems:get-row "df" 10)
(hems:get-rows "df" 10 20)
|#
