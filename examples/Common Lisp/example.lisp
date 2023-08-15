(ql:quickload :hems)
(in-package :hems)

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
