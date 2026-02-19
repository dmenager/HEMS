(ql:quickload :hems)
(in-package :hems)

(defun insert-two-time-slice (obss sts)
  (loop
    for (obs1 obs2) on obss
    for (st1 st2) on sts
    while (and (not (null obs1))
	       (not (null obs2)))
    do
       (new-push-to-ep-buffer :observation obs1 :state st1 :action-name "NIL" :bic-p t :insertp nil :temporal-p t :hidden-state-p t)
       (new-push-to-ep-buffer :observation obs2 :state st2 :action-name "NIL" :bic-p t :insertp t :temporal-p t :hidden-state-p t))
  (eltm-to-pdf))

(defun example2 ()
  (let (obs1 obs2 obs3 obs4 st1 st2 st3 st4)
    (setq obs1 (compile-program nil
		 n0_ps = (percept-node population_segment :value "renter" :kb-concept-id "POPULATION_SEGMENT")
		 n0_z = (percept-node zip :value "11217" :kb-concept-id "ZIP")
		 n0_m = (percept-node mood :value "surprise" :kb-concept-id "MOOD")
		 n0_b = (percept-node belief :value "unsafe_area" :kb-concept-id "BELIEF")
		 n0_s = (percept-node sales :value "10" :kb-concept-id "SALES")
		 n0_a = (percept-node action :value "sell" :kb-concept-id "ACTION")
		 n0_z --> n0_s
		 n0_a --> n0_s
		 n0_b --> n0_a
		 n0_m --> n0_a
		 n0_ps --> n0_a
		 n0_ps --> n0_b
		 n0_ps --> n0_m
		 n0_b --> n0_m))
    
    (setq obs2 (compile-program nil
		 n6_ps = (percept-node population_segment :value "builder" :kb-concept-id "POPULATION_SEGMENT")
		 n6_z = (percept-node zip :value "11217" :kb-concept-id "ZIP")
		 n6_m = (percept-node mood :value "joy" :kb-concept-id "MOOD")
		 n6_b = (percept-node belief :value "market_correction" :kb-concept-id "BELIEF")
		 n6_s = (percept-node sales :value "10" :kb-concept-id "SALES")
		 n6_a = (percept-node action :value "sell" :kb-concept-id "ACTION")
		 n6_z --> n6_s
		 n6_a --> n6_s
		 n6_b --> n6_a
		 n6_m --> n6_a
		 n6_ps --> n6_a
		 n6_ps --> n6_b
		 n6_ps --> n6_m
		 n6_b --> n6_m))
    
    (setq obs3 (compile-program nil
		 n7_ps = (percept-node population_segment :value "builder" :kb-concept-id "POPULATION_SEGMENT")
		 n7_z = (percept-node zip :value "11219" :kb-concept-id "ZIP")
		 n7_m = (percept-node mood :value "fear" :kb-concept-id "MOOD")
		 n7_b = (percept-node belief :value "unsafe_area" :kb-concept-id "BELIEF")
		 n7_s = (percept-node sales :value "10" :kb-concept-id "SALES")
		 n7_a = (percept-node action :value "sell" :kb-concept-id "ACTION")
		 n7_z --> n7_s
		 n7_a --> n7_s
		 n7_b --> n7_a
		 n7_m --> n7_a
		 n7_ps --> n7_a
		 n7_ps --> n7_b
		 n7_ps --> n7_m
		 n7_b --> n7_m))
    
    (setq obs4 (compile-program nil
		 n14_ps = (percept-node population_segment :value "buyer" :kb-concept-id "POPULATION_SEGMENT")
		 n14_z = (percept-node zip :value "11217" :kb-concept-id "ZIP")
		 n14_m = (percept-node mood :value "disgust" :kb-concept-id "MOOD")
		 n14_b = (percept-node belief :value "buyers_market" :kb-concept-id "BELIEF")
		 n14_s = (percept-node sales :value "10" :kb-concept-id "SALES")
		 n14_a = (percept-node action :value "sell" :kb-concept-id "ACTION")
		 n14_z --> n14_s
		 n14_a --> n14_s
		 n14_b --> n14_a
		 n14_m --> n14_a
		 n14_ps --> n14_a
		 n14_ps --> n14_b
		 n14_ps --> n14_m
		 n14_b --> n14_m))
    
    (setq st1 (compile-program nil
		n0_t = (percept-node time :value "2026_03" :kb-concept-id "TIME")))
    
    (setq st2 (compile-program nil
		n6_t = (percept-node time :value "2026_06" :kb-concept-id "TIME")))
    
    (setq st3 (compile-program nil
		n7_t = (percept-node time :value "2026_06" :kb-concept-id "TIME")))
    
    (setq st4 (compile-program nil
		n14_t = (percept-node time :value "2026_12" :kb-concept-id "TIME")))
    
    (insert-two-time-slice (list obs1 obs2 obs3 obs4) (list st1 st2 st3 st4))))

(defun example1 ()
  (let (bn1 bn2 bn3 bn4)
    (setq bn1 (compile-program nil
		n0_ps = (percept-node population_segment :value "renter" :kb-concept-id "POPULATION_SEGMENT")
		n0_z = (percept-node zip :value "11217" :kb-concept-id "ZIP")
		n0_m = (percept-node mood :value "surprise" :kb-concept-id "MOOD")
		n0_b = (percept-node belief :value "unsafe_area" :kb-concept-id "BELIEF")
		n0_s = (percept-node sales :value "10" :kb-concept-id "SALES")
		n0_a = (percept-node action :value "sell" :kb-concept-id "ACTION")
		n0_z --> n0_s
		n0_a --> n0_s
		n0_b --> n0_a
		n0_m --> n0_a
		n0_ps --> n0_a
		n0_ps --> n0_b
		n0_ps --> n0_m
		n0_b --> n0_m))
    (setq bn2 (compile-program nil
		t1 = (percept-node time :value "03172016" :kb-concept-id "TIME")))

    (setq bn3 (compile-program nil
		n6_ps = (percept-node population_segment :value "builder" :kb-concept-id "POPULATION_SEGMENT")
		n6_z = (percept-node zip :value "11217" :kb-concept-id "ZIP")
		n6_m = (percept-node mood :value "joy" :kb-concept-id "MOOD")
		n6_b = (percept-node belief :value "market_correction" :kb-concept-id "BELIEF")
		n6_s = (percept-node sales :value "10" :kb-concept-id "SALES")
		n6_a = (percept-node action :value "sell" :kb-concept-id "ACTION")
		n6_z --> n6_s
		n6_a --> n6_s
		n6_b --> n6_a
		n6_m --> n6_a
		n6_ps --> n6_a
		n6_ps --> n6_b
		n6_ps --> n6_m
		n6_b --> n6_m))
    (setq bn4 (compile-program nil
		t1 = (percept-node time :value "03202454" :kb-concept-id "TIME")))

    (insert-two-time-slice (list bn1 bn3) (list bn2 bn4))
    ))
