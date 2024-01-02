(ql:quickload :hems)
(in-package :hems)

(defun ex2()
  (let ((bn (compile-program
	      c0 = (percept-node node1 :value "5")
	      c1 = (percept-node node2 :value "10")
	      c2 = (percept-node node3 :value "6")
	      c3 = (percept-node node4 :value "10")
	      c1 -> c0
	      c2 -> c0))
	last-node keep remove)
    ;;(format t "~%bn:~%~S" (aref (car bn) 3))
    (setq last-node (aref (car bn) 3))
    (setq keep (rule-based-cpd-dependent-id last-node))
    (setq remove (remove keep
			 (hems::hash-keys-to-list (rule-based-cpd-identifiers last-node))
			 :test #'equal))
    (format t "~%keep:~%~S~%remove:~%~S" keep remove)
    (hems::factor-operation last-node (list keep) remove #'+)
    ))

(defun ex3()
  (let ((bn (compile-program
	     c1 = (percept-node Facial_Expression_ID :value "32")
	     c2 = (percept-node Happiness :value "6")
	     c3 = (percept-node Sadness :value "0")
	     c4 = (percept-node Excitement :value "3")
	     c5 = (percept-node Boredom :value "0")
	     c6 = (percept-node Anger :value "0")
	     c7 = (percept-node Surprise :value "1")
	     c8 = (percept-node Gender :value "Female")
	     c9 = (percept-node Education :value "Bachelor")
	     c10 = (percept-node Age :value "0")
	     c11 = (percept-node PQ1 :value "3")
	     c12 = (percept-node PQ2 :value "3")
	     c13 = (percept-node PQ3 :value "3")
	     c14 = (percept-node PQ4 :value "3")
	     c15 = (percept-node PQ5 :value "3")
	     c16 = (percept-node PQ6 :value "3")
	     c17 = (percept-node PQ7 :value "3")
	     c18 = (percept-node PQ8 :value "3")
	     c19 = (percept-node PQ9 :value "3")
	     c20 = (percept-node PQ10 :value "3")
	     c21 = (percept-node AQ1 :value "3")
	     c22 = (percept-node AQ2 :value "3")
	     c23 = (percept-node AQ3 :value "3")
	     c24 = (percept-node AQ4 :value "3")
	     c25 = (percept-node AQ5 :value "3")
	     c26 = (percept-node AQ6 :value "3")
	     c27 = (percept-node AQ7 :value "3")
	     c28 = (percept-node AQ8 :value "3")
	     c29 = (percept-node AQ9 :value "3")
	     c30 = (percept-node AQ10 :value "3")
	     c31 = (percept-node AQ11 :value "3")
	     c32 = (percept-node AQ12 :value "3")
	     c33 = (percept-node AQ13 :value "3")
	     c34 = (percept-node AQ14 :value "3")
	     c35 = (percept-node AQ15 :value "3")
	     c36 = (percept-node AQ16 :value "3")
	     c37 = (percept-node AQ17 :value "3")
	     c38 = (percept-node AQ18 :value "3")
	     c39 = (percept-node AQ19 :value "3")
	     c40 = (percept-node AQ20 :value "3"))))
    (push-to-ep-buffer :state bn :insertp t)))
  
(defun ex1 ()
(let (bn1 bn2 bn3 bn4 bn5 q1 q2)
    (setq q1 (compile-program
	       c1 = (percept-node rank :value "MILITARY" :kb-concept-id "CNPT-4")
	       c2 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")))
    
    (setq q2 (compile-program
	       c1 = (percept-node rank :value "MILITARY" :kb-concept-id "CNPT-4")
	       c2 = (percept-node hrpmin :value "120" :kb-concept-id "CNPT-5")
	       c3 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")))
    
    (setq bn1 (compile-program
		c1 = (percept-node casualty :value "CASUALTY-A" :kb-concept-id "CNPT-1")
		c2 = (percept-node age :value "22" :kb-concept-id "CNPT-2")
		c3 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")
		c4 = (percept-node rank :value "MILITARY" :kb-concept-id "CNPT-4")
		c5 = (percept-node hrpmin :value "145" :kb-concept-id "CNPT-5")
		c6 = (percept-node mmHG :value "60" :kb-concept-id "CNPT-6")
		c7 = (percept-node Spo2 :value "85" :kb-concept-id "CNPT-7")
		c8 = (percept-node RR :value "40" :kb-concept-id "CNPT-8")
		c9 = (percept-node pain :value "0" :kb-concept-id "CNPT-9")
		c10 = (relation-node IED_injury :value "T" :kb-concept-id "CNPT-10")
		c11 = (relation-node 2nd_degree_burn :value "T" :kb-concept-id "CNPT-11")
		c12 = (relation-node 3rd_degree_burn :value "T" :kb-concept-id "CNPT-12")
		c13 = (relation-node unconscious :value "T" :kb-concept-id "CNPT-13")
		c1 -> c2
		c1 -> c3
		c1 -> c4
		c1 -> c5
		c1 -> c6
		c1 -> c7
		c1 -> c8
		c1 -> c9
		c10 -> c1
		c11 -> c1
		c12 -> c1
		c13 -> c1))

    (setq bn2 (compile-program
		c1 = (percept-node casualty :value "CASUALTY-B" :kb-concept-id "CNPT-1")
		c2 = (percept-node age :value "25" :kb-concept-id "CNPT-2")
		c3 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")
		c4 = (percept-node rank :value "MILITARY" :kb-concept-id "CNPT-4")
		c5 = (percept-node hrpmin :value "120" :kb-concept-id "CNPT-5")
		c6 = (percept-node mmHG :value "80" :kb-concept-id "CNPT-6")
		c7 = (percept-node Spo2 :value "98" :kb-concept-id "CNPT-7")
		c8 = (percept-node RR :value "18" :kb-concept-id "CNPT-8")
		c9 = (percept-node pain :value "6" :kb-concept-id "CNPT-9")
		c10 = (relation-node IED_injury :value "T" :kb-concept-id "CNPT-10")
		c11 = (relation-node 2nd_degree_burn :value "T" :kb-concept-id "CNPT-11")
		c12 = (relation-node 3rd_degree_burn :value "T" :kb-concept-id "CNPT-12")
		c1 -> c2
		c1 -> c3
		c1 -> c4
		c1 -> c5
		c1 -> c6
		c1 -> c7
		c1 -> c8
		c1 -> c9
		c10 -> c1
		c11 -> c1
		c12 -> c1))
    
    (setq bn3 (compile-program
		c1 = (percept-node casualty :value "CASUALTY-D" :kb-concept-id "CNPT-1")
		c2 = (percept-node age :value "40" :kb-concept-id "CNPT-2")
		c3 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")
		c4 = (percept-node rank :value "VIP" :kb-concept-id "CNPT-4")
		c5 = (percept-node hrpmin :value "105" :kb-concept-id "CNPT-5")
		c6 = (percept-node mmHG :value "120" :kb-concept-id "CNPT-6")
		c7 = (percept-node Spo2 :value "99" :kb-concept-id "CNPT-7")
		c8 = (percept-node RR :value "15" :kb-concept-id "CNPT-8")
		c9 = (percept-node pain :value "2" :kb-concept-id "CNPT-9")
		c10 = (relation-node IED_injury :value "T" :kb-concept-id "CNPT-10")
		c11 = (relation-node suborbital_ecchymosis :value "T" :kb-concept-id "CNPT-13")
		c12 = (relation-node traumatic_hyphema :value "T" :kb-concept-id "CNPT-14")
		c1 -> c2
		c1 -> c3
		c1 -> c4
		c1 -> c5
		c1 -> c6
		c1 -> c7
		c1 -> c8
		c1 -> c9
		c10 -> c1
		c11 -> c1
		c12 -> c1))
    
    (setq bn4 (compile-program
		c1 = (percept-node casualty :value "CASUALTY-E" :kb-concept-id "CNPT-1")
		c2 = (percept-node age :value "26" :kb-concept-id "CNPT-2")
		c3 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")
		c4 = (percept-node rank :value "MILITARY" :kb-concept-id "CNPT-4")
		c5 = (percept-node hrpmin :value "120" :kb-concept-id "CNPT-5")
		c6 = (percept-node mmHG :value "100" :kb-concept-id "CNPT-6")
		c7 = (percept-node Spo2 :value "95" :kb-concept-id "CNPT-7")
		c8 = (percept-node RR :value "15" :kb-concept-id "CNPT-8")
		c9 = (percept-node pain :value "10" :kb-concept-id "CNPT-9")
		c10 = (relation-node IED_injury :value "T" :kb-concept-id "CNPT-10")
		c1 -> c2
		c1 -> c3
		c1 -> c4
		c1 -> c5
		c1 -> c6
		c1 -> c7
		c1 -> c8
		c1 -> c9
		c10 -> c1))
    
    (setq bn5 (compile-program
		c1 = (percept-node casualty :value "CASUALTY-F" :kb-concept-id "CNPT-1")
		c2 = (percept-node age :value "12" :kb-concept-id "CNPT-2")
		c3 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")
		c4 = (percept-node rank :value "CIVILIAN" :kb-concept-id "CNPT-4")
		c5 = (percept-node hrpmin :value "120" :kb-concept-id "CNPT-5")
		c6 = (percept-node mmHG :value "30" :kb-concept-id "CNPT-6")
		c7 = (percept-node Spo2 :value "99" :kb-concept-id "CNPT-7")
		c8 = (percept-node RR :value "25" :kb-concept-id "CNPT-8")
		c9 = (percept-node pain :value "3" :kb-concept-id "CNPT-9")
		c10 = (relation-node shrapnel_injury :value "T" :kb-concept-id "CNPT-15")
		c11 = (relation-node difficult_breathing :value "T" :kb-concept-id "CNPT-16")
		c1 -> c2
		c1 -> c3
		c1 -> c4
		c1 -> c5
		c1 -> c6
		c1 -> c7
		c1 -> c8
		c1 -> c9
		c10 -> c1
		c11 -> c1))

    ;; insert into event memory
    (map nil #'(lambda (bn)
		 (push-to-ep-buffer :state bn :insertp t))
	 (list bn1 bn2 bn3 bn4 bn5))

    ;; remember from retrieval cue
    (multiple-value-bind (recollection eme)
	(remember (list (car eltm*)) (list q2) '+  1 t)
      (declare (ignore eme))
      (let (singletons)
	(setq singletons (mapcan #'(lambda (cpd)
				     (when (rule-based-cpd-singleton-p cpd)
				       (list cpd)))
				 recollection))
	(loop
	  with spread
	  for cpd in singletons
	  do
	     (setq spread (- 1 (compute-cpd-concentration cpd)))
	     (format t "~%~%singleton:~%~S~%spread: ~d" cpd spread)
	  collect spread into spreads
	   finally
	     (format t "~%mean spreads: ~d~%stdev: ~d" (mean spreads) (stdev spreads))
	     ;;(return (values singletons (mean spreads) (stdev spreads)))
	  )))
    ;;(eltm-to-pdf)
    )
)
