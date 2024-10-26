(defun fci (net df)
  (labels ((select-var-pair (adjacencies n)
	     (loop
	       for x being the hash-keys of adjacencies
		 using (hash-value children-hash)
	       when (>= (- (hash-table-count children-hash) 1) n)
		 do
		    (return-from select-var-par (list x children-hash))))
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
	     (n-subsets-helper lst n nil)))
    (loop
      with adjacencies = (cdr net)
      with n = 0
      while
      do
	 (loop
	   while
	   do
	      (loop
		while
		do
		until )
	   until )
      until )))
