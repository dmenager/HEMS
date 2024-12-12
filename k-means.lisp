(in-package :hems)

#| Euclidean distance formula |#

;; p1 = point 1
;; p2 = point 2
(defun distance (p1 p2)
  (sqrt (reduce #'+ (map 'list #'(lambda (e1 e2)
				(expt (- e1 e2) 2))
			    p1 p2))))

;; df = lisp-stat dataframe
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
	   (initialize (df-name)
	     (let ((centroids))
	       (setq centroids (cons (aref (ls-user:rows (get-row df-name (random (ls-user:nrow df)))) 0)
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
		      for point being the elements of (ls-user:rows df)
		      do
			   (format t "~%point: ~S" point)
			   (loop
			     for c being the elements of centroids
			     do
				(format t "~%centroid: ~S" c)
				(setq d (min d (distance point c))))
			   (setq dist (cons d dist)))
		    (setq next-centroid (aref (ls-user:rows
					       (get-row df-name (argmax dist)))
					      0))
		    (setq centroids (cons next-centroid centroids))
		    (setq dist nil))
	       centroids)))
    (initialize "df")))

#| TESTS 
(let (df)
  (setq df (hems:read-csv "/home/david/Code/HARLEM/ep_data_1000/ppo_FrozenLake-v1_data.csv")) 
  (setq df (teddy/data-frame::slice df :columns '("ACTION" "REWARDS")))
  (hems::k-means df 2)
  )
|#

#|
(ql:quickload :hems)
(ls-user:defdf df (ls-user:read-csv #P"/home/david/Code/HARLEM/ep_data_1000/ppo_FrozenLake-v1_data.csv"))
(setf df (ls-user:remove-columns df '(EPISODE-NUMBER TIMESTEP hidden-state observation)))
(hems:n-format-df-column-names df)
(setq df (ls-user:filter-rows df '(numberp action)))

(k-means df 2)
(hems:get-row "df" 10)
(hems:get-rows "df" 10 20)
|#
