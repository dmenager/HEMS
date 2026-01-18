(ql:quickload :hems)
(in-package :hems)

(defun run (observation state)
  (format t "~%observation:")
  (hems:print-bn observation)
  (format t "~%state:")
  (hems:print-bn state)
  (break)
  (new-push-to-ep-buffer :observation observation :state state :action-name "NIL" :hidden-state-p t :insertp t :temporal-p t))

(defun example2 ()
  (let (observation state)
    (setq observation
	  (compile-program nil
	    v1 = (functional-node eposition_prev
				  :arguments NIL
				  :generator
				  (cons (cons "NA" 0)
					(mapcar #'(lambda (value-list)
						    (cons (getf value-list :value)
							  (getf value-list :probability)))
						(discrete-uniform :values '("50" "150" "250" "350" "450" "550" "650" "750" "850" "950" "1050" "1150" "1250" "1350" "1450" "1550" "1650" "1750" "1850" "1950" "2050"))))
				  :type "percept")
	    v2 = (functional-node nposition_prev
				  :arguments NIL
				  :generator
				  (cons (cons "NA" 0)
					(mapcar #'(lambda (value-list)
						    (cons (getf value-list :value)
							  (getf value-list :probability)))
						(discrete-uniform :values '("50" "150" "250" "350" "450" "550" "650" "750" "850" "950" "1050"))))
				  :type "percept")
	    v3 = (functional-node evelocity_prev
				  :arguments NIL
				  :generator
				  (cons (cons "NA" 0)
					(mapcar #'(lambda (value-list)
						    (cons (getf value-list :value)
							  (getf value-list :probability)))
						(discrete-uniform :values '("-15" "-12" "-9" "-6" "-3" "0" "3" "6" "9" "12" "15"))))
				  :type "percept")
	    v4 = (functional-node nvelocity_prev
				  :arguments NIL
				  :generator
				  (cons (cons "NA" 0)
					(mapcar #'(lambda (value-list)
						    (cons (getf value-list :value)
							  (getf value-list :probability)))
						(discrete-uniform :values '("-15" "-12" "-9" "-6" "-3" "0" "3" "6" "9" "12" "15"))))
				  :type "percept")
	    v5 = (functional-node time_delta
				  :arguments NIL
				  :generator
				  (cons (cons "NA" 0)
					(mapcar #'(lambda (value-list)
						    (cons (getf value-list :value)
							  (getf value-list :probability)))
						(discrete-uniform :values '("1" "27"))))
				  :type "percept")
	    v6 = (functional-node eposition
				  :arguments (v1 v3 v5)
				  :generator
				  (let (new-mean raw-mean domain domain-ints max-d min-d)
				    (setq domain '("50" "150" "250" "350" "450" "550" "650" "750" "850" "950" "1050" "1150" "1250" "1350" "1450" "1550" "1650" "1750" "1850" "1950" "2050"))
				    (setq domain-ints (mapcar #'parse-integer domain))
				    (setq max-d (apply #'max domain-ints))
				    (setq min-d (apply #'min domain-ints))
				    (cond ((some #'(lambda (arg) (string-equal "na" arg)) (list v1 v3 v5))
					   (loop
					     for d in domain
					     collect (cons d 0) into result
					     finally
						(return result)))
					  (t
					   (setq raw-mean (+ (* (parse-integer v3)
								(parse-integer v5))
							     (parse-integer v1)))
					   (setq new-mean (reduce (lambda (a b)
								    (if (< (abs (- a raw-mean)) (abs (- b raw-mean))) a b))
								  (mapcar #'parse-integer domain)))
					   (when nil
					     (format t "eposition_prev: ~d evelocity_prev: ~d new eposition mean: ~d" v1 v3 new-mean))
					   (cond ((> new-mean max-d)
						  (setq new-mean max-d))
						 ((< new-mean min-d)
						  (setq new-mean min-d)))
					   (when nil
					     (format t "bounded mean: ~d" new-mean))
					   (setq new-mean (write-to-string new-mean))
					   (mapcar #'(lambda (value-list)
						       (cons (getf value-list :value)
							     (getf value-list :probability)))
						   (discrete-normal-approximation :values domain :modes (list new-mean) :stdev 100)))))
				  :type "percept")
	    v7 = (functional-node nposition
				  :arguments (v2 v4 v5)
				  :generator
				  (let (new-mean raw-mean domain domain-ints max-d min-d)
				    (setq domain '("50" "150" "250" "350" "450" "550" "650" "750" "850" "950" "1050"))
				    (setq domain-ints (mapcar #'parse-integer domain))
				    (setq max-d (apply #'max domain-ints))
				    (setq min-d (apply #'min domain-ints))
				    (cond ((some #'(lambda (arg) (string-equal "na" arg)) (list v2 v4 v5))
					   (loop
					     for d in domain
					     collect (cons d 0) into result
					     finally
						(return result)))
					  (t
					   (setq raw-mean (+ (* (parse-integer v4)
								(parse-integer v5))
							     (parse-integer v2)))
					   (setq new-mean (reduce (lambda (a b)
								    (if (< (abs (- a raw-mean)) (abs (- b raw-mean))) a b))
								  (mapcar #'parse-integer domain)))
					   (when nil
					     (format t "nposition_prev: ~d nvelocity_prev: ~d new nposition mean: ~d" v2 v4 new-mean))
					   (cond ((> new-mean max-d)
						  (setq new-mean max-d))
						 ((< new-mean min-d)
						  (setq new-mean min-d)))
					   (when nil
					     (format t "bounded mean: ~d" new-mean))
					   (setq new-mean (write-to-string new-mean))
					   (mapcar #'(lambda (value-list)
						       (cons (getf value-list :value)
							     (getf value-list :probability)))
						   (discrete-normal-approximation :values domain :modes (list new-mean) :stdev 100)))))
				  :type "percept")
	    v8 = (functional-node evelocity
				  :arguments (v1 v6 v5)
				  :generator
				  (let (new-mean raw-mean domain domain-ints max-d min-d)
				    (setq domain '("-15" "-12" "-9" "-6" "-3" "0" "3" "6" "9" "12" "15"))
				    (setq domain-ints (mapcar #'parse-integer domain))
				    (setq max-d (apply #'max domain-ints))
				    (setq min-d (apply #'min domain-ints))
				    (cond ((some #'(lambda (arg) (string-equal "na" arg)) (list v1 v6 v5))
					   (loop
					     for d in domain
					     collect (cons d 0) into result
					     finally
						(return result)))
					  (t
					   (setq raw-mean (/ (- (parse-integer v6)
								(parse-integer v1))
							     (parse-integer v5)))
					   (setq new-mean (reduce (lambda (a b)
								    (if (< (abs (- a raw-mean))
									   (abs (- b raw-mean)))
									a b))
								  (mapcar #'parse-integer domain)))
					   (when nil
					     (format t "eposition_prev: ~d eposition: ~d new eposition mean: ~d" v1 v6 new-mean))
					   (cond ((> new-mean max-d)
						  (setq new-mean max-d))
						 ((< new-mean min-d)
						  (setq new-mean min-d)))
					   (when nil
					     (format t "bounded mean: ~d" new-mean))
					   (setq new-mean (write-to-string new-mean))
					   (mapcar #'(lambda (value-list)
						       (cons (getf value-list :value)
							     (getf value-list :probability)))
						   (discrete-normal-approximation :values domain :modes (list new-mean) :stdev 100)))))
				  :type "percept")
	    v9 = (functional-node nvelocity
				  :arguments (v2 v7 v5)
				  :generator
				  (let (new-mean raw-mean domain domain-ints max-d min-d)
				    (setq domain '("-15" "-12" "-9" "-6" "-3" "0" "3" "6" "9" "12" "15"))
				    (setq domain-ints (mapcar #'parse-integer domain))
				    (setq max-d (apply #'max domain-ints))
				    (setq min-d (apply #'min domain-ints))
				    (cond ((some #'(lambda (arg) (string-equal "na" arg)) (list v2 v7 v5))
					   (loop
					     for d in domain
					     collect (cons d 0) into result
					     finally
						(return result)))
					  (t
					   (setq raw-mean (/ (- (parse-integer v7)
								(parse-integer v2))
							     (parse-integer v5)))
					   (setq new-mean (reduce (lambda (a b)
								    (if (< (abs (- a raw-mean))
									   (abs (- b raw-mean)))
									a b))
								  (mapcar #'parse-integer domain)))
					   (when nil
					     (format t "npositionₚrev: ~dnposition: ~dnew nposition mean: ~d" v2 v7 new-mean))
					   (cond ((> new-mean max-d)
						  (setq new-mean max-d))
						 ((< new-mean min-d)
						  (setq new-mean min-d)))
					   (when nil
					     (format t "bounded mean: ~d" new-mean))
					   (setq new-mean (write-to-string new-mean))
					   (mapcar #'(lambda (value-list)
						       (cons (getf value-list :value)
							     (getf value-list :probability)))
						   (discrete-normal-approximation :values domain :modes (list new-mean) :stdev 100)))))
				  :type "percept")))

(setq state
      (compile-program nil
	v10 = (functional-node time :arguments NIL
				    :generator
				    (cons (cons "NA" 0)
					  (mapcar #'(lambda (value-list)
						      (cons (getf value-list :value)
							    (getf value-list :probability)))
						  (discrete-uniform :values '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" "35"))))
				    :type "percept")
))
    (run observation state)))

(defun example1 ()
  (let (observation state)
    (setq observation
	  (compile-program nil
	    v1 = (functional-node eposition_prev :arguments NIL
						 :generator
						 (cons (cons "NA" 0)
						       (mapcar #'(lambda (value-list)
								   (cons (getf value-list :value)
									 (getf value-list :probability)))
							       (hems:discrete-uniform :values '("50" "150" "250" "350" "450" "550" "650" "750" "850" "950" "1050" "1150" "1250" "1350" "1450" "1550" "1650" "1750" "1850" "1950" "2050"))))
						 :type "percept")
	    v2 = (functional-node nposition_prev :arguments NIL
						 :generator
						 (cons (cons "NA" 0)
						       (mapcar #'(lambda (value-list)
								   (cons (getf value-list :value)
									 (getf value-list :probability)))
							       (hems:discrete-uniform :values '("50" "150" "250" "350" "450" "550" "650" "750" "850" "950" "1050"))))
						 :type "percept")
	    v3 = (functional-node evelocity_prev :arguments NIL
						 :generator (cons (cons "NA" 0)
								  (mapcar #'(lambda (value-list)
									      (cons (getf value-list :value)
										    (getf value-list :probability)))
									  (hems:discrete-uniform :values '("-15" "-13" "-11" "-9" "-7" "-5" "-3" "-1" "1" "3" "5" "7" "9" "11" "13" "15"))))
						 :type "percept")
	    v4 = (functional-node nvelocity_prev
				  :arguments NIL
				  :generator
				  (cons (cons "NA" 0)
					(mapcar #'(lambda (value-list)
						    (cons (getf value-list :value)
							  (getf value-list :probability)))
						(hems:discrete-uniform :values '("-15" "-13" "-11" "-9" "-7" "-5" "-3" "-1" "1" "3" "5" "7" "9" "11" "13" "15"))))
				  :type "percept")
	    v5 = (functional-node time_delta
				  :arguments NIL
				  :generator
				  (cons (cons "NA" 0)
					(mapcar #'(lambda (value-list)
						    (cons (getf value-list :value)
							  (getf value-list :probability)))
						(hems:discrete-uniform :values '("1"))))
				  :type "percept")
	    v6 = (functional-node eposition
				  :arguments (v1 v3 v5)
				  :generator
				  (let (new-mean raw-mean domain domain-ints)
				    (setq domain '("50" "150" "250" "350" "450" "550" "650" "750" "850" "950" "1050" "1150" "1250" "1350" "1450" "1550" "1650" "1750" "1850" "1950" "2050"))
				    (setq domain-ints (mapcar #'parse-integer domain))
				    (cond ((some #'(lambda (arg)
						     (string-equal "na" arg))
						 (list v1 v3 v5))
					   (loop for d in domain
						 collect (cons d (float 0)) into result
						 finally
						    (return result)))
					  (t
					   (setq raw-mean (+ (* (parse-integer v3)
								(parse-integer v5))
							     (parse-integer v1)))
					   (setq new-mean (reduce (lambda (a b)
								    (if (< (abs (- a raw-mean))
									   (abs (- b raw-mean)))
									a
									b))
								  domain-ints))
					   (setq new-mean (write-to-string new-mean))
					   (when nil
					     (format t "~%~%eposition_prev: ~d evelocity_prev: ~d time delta: ~d new eposition mean: ~a~%distribution for row:~%~S" v1 v3 v5 new-mean
						     (mapcar #'(lambda (value-list)
								 (cons (getf value-list :value)
								       (getf value-list :probability)))
							     (hems::discrete-normal-approximation :values domain :modes (list new-mean)))))
					   (mapcar #'(lambda (value-list)
						       (cons (getf value-list :value)
							     (getf value-list :probability)))
						   (hems::discrete-normal-approximation :values domain :modes (list new-mean))))))
				  :type "percept")
	    v7 = (functional-node nposition
				  :arguments (v2 v4 v5)
				  :generator
				  (let (new-mean raw-mean domain domain-ints)
				    (setq domain '("50" "150" "250" "350" "450" "550" "650" "750" "850" "950" "1050"))
				    (setq domain-ints (mapcar #'parse-integer domain))
				    (cond ((some #'(lambda (arg)
						     (string-equal "na" arg))
						 (list v2 v4 v5))
					   (loop for d in domain
						 collect (cons d (float 0)) into result
						 finally
						    (return result)))
					  (t
					   (setq raw-mean (+ (* (parse-integer v4) (parse-integer v5)) (parse-integer v2)))
					   (setq new-mean (reduce (lambda (a b)
								    (if (< (abs (- a raw-mean)) (abs (- b raw-mean))) a b))
								  domain-ints))
					   (when nil
					     (format t "nposition_prev: ~dnvelocity_prev: ~dnew nposition mean: ~d" v2 v4 new-mean))
					   (when nil
					     (format t "bounded mean: ~d" new-mean))
					   (setq new-mean (write-to-string new-mean))
					   (mapcar #'(lambda (value-list)
						       (cons (getf value-list :value) (getf value-list :probability)))
						   (hems::discrete-normal-approximation :values domain :modes (list new-mean))))))
				  :type "percept")
	    v8 = (functional-node evelocity
				  :arguments (v1 v6 v5)
				  :generator (let (new-mean raw-mean domain domain-ints)
					       (setq domain '("-15" "-13" "-11" "-9" "-7" "-5" "-3" "-1" "1" "3" "5" "7" "9" "11" "13" "15"))
					       (setq domain-ints (mapcar #'parse-integer domain))
					       (cond ((some #'(lambda (arg) (string-equal "na" arg)) (list v1 v6 v5))
						      (loop for d in domain
							    collect (cons d (float 0)) into result
							    finally
							       (return result)))
						     (t
						      (setq raw-mean (/ (- (parse-integer v6)
									   (parse-integer v1))
									(parse-integer v5)))
						      (setq new-mean (reduce (lambda (a b)
									       (if (< (abs (- a raw-mean)) (abs (- b raw-mean))) a b))
									     domain-ints))
						      (when nil
							(format t "eposition_prev: ~deposition: ~dnew eposition mean: ~d" v1 v6 new-mean))
						      (when nil
							(format t "bounded mean: ~d" new-mean))
						      (setq new-mean (write-to-string new-mean))
						      (mapcar #'(lambda (value-list)
								  (cons (getf value-list :value)
									(getf value-list :probability)))
							      (hems::discrete-normal-approximation :values domain :modes (list new-mean))))))
				  :type "percept")
	    v9 = (functional-node nvelocity
				  :arguments (v2 v7 v5)
				  :generator
				  (let (new-mean raw-mean domain domain-ints)
				    (setq domain '("-15" "-13" "-11" "-9" "-7" "-5" "-3" "-1" "1" "3" "5" "7" "9" "11" "13" "15"))
				    (setq domain-ints (mapcar #'parse-integer domain))
				    (cond ((some #'(lambda (arg) (string-equal "na" arg)) (list v2 v7 v5))
					   (loop for d in domain
						 collect (cons d (float 0)) into result
						 finally
						    (return result)))
					  (t
					   (setq raw-mean (/ (- (parse-integer v7)
								(parse-integer v2))
							     (parse-integer v5)))
					   (setq new-mean (reduce (lambda (a b)
								    (if (< (abs (- a raw-mean)) (abs (- b raw-mean))) a b))
								  domain-ints))
					   (when nil
					     (format t "nposition_prev: ~dnposition: ~dnew nposition mean: ~d" v2 v7 new-mean))
					   (when nil
					     (format t "bounded mean: ~d" new-mean))
					   (setq new-mean (write-to-string new-mean))
					   (mapcar #'(lambda (value-list)
						       (cons (getf value-list :value)
							     (getf value-list :probability)))
						   (hems::discrete-normal-approximation :values domain :modes (list new-mean))))))
				  :type "percept")))
    
    (setq state
	  (compile-program nil
	    v10 = (functional-node time
				   :arguments NIL
				   :generator
				   (cons (cons "NA" 0)
					 (mapcar #'(lambda (value-list)
						     (cons (getf value-list :value)
							   (getf value-list :probability)))
						 (hems:discrete-uniform :values '("0" "1" "2" "3" "4" "5"))))
				   :type "percept")))
    (run observation state)))
