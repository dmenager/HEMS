(ql:quickload :hems)
(in-package :hems)

(defun infer ()
  (let (cue)
    (setq cue (compile-program nil
		vital_0 = (percept-node temperature :value "unknown")
		vital_1 = (percept-node heartrate :value "unknown")
		vital_2 = (percept-node resprate :value "unknown")
		vital_3 = (percept-node o2sat :value "unknown")
		vital_4 = (percept-node pain :value "unknown")
		injury_0 = (relation-node open_wounds :value "T" :kb-concept-id "INJURY")
		injury_1 = (relation-node laceration :value "T" :kb-concept-id "INJURY")
		injury_0 --> vital_0
		injury_0 --> vital_1
		injury_0 --> vital_2
		injury_0 --> vital_3
		injury_0 --> vital_4
		injury_1 --> vital_0
		injury_1 --> vital_1
		injury_1 --> vital_2
		injury_1 --> vital_3
		injury_1 --> vital_4))
    (multiple-value-bind (posterior marginals)
	(remember eltm* cue '+ 1 t :type "observation")
      (declare (ignore posterior))
      marginals)))

(defun load-data ()
  (let (bn1 bn2 bn3 bn4 bn5 bn6 bn7 bn8 bn9 bn10)
    (setq bn1 (compile-program nil 
			       vital_0 = (percept-node temperature :value "unknown")
		vital_1 = (percept-node heartrate :value "unknown")
		vital_2 = (percept-node resprate :value "unknown")
			       vital_3 = (percept-node o2sat :value "unknown")
			       vital_4 = (percept-node pain :value "unknown")
			       acuity = (percept-node acuity :value "unknown")
			       injury_0 = (relation-node traumatic_brain_injury :value "T" :kb-concept-id "INJURY")
			       injury_1 = (relation-node laceration :value "T" :kb-concept-id "INJURY")
			       death = (percept-node death :value "T")
			       injury_0 --> death
			       injury_0 --> acuity
			       injury_0 --> vital_0
			       injury_0 --> vital_1
			       injury_0 --> vital_2
			       injury_0 --> vital_3
			       injury_0 --> vital_4
			       injury_1 --> death
			       injury_1 --> acuity
			       injury_1 --> vital_0
			       injury_1 --> vital_1
			       injury_1 --> vital_2
			       injury_1 --> vital_3
			       injury_1 --> vital_4))

    (setq bn2 (compile-program nil
			       vital_0 = (percept-node temperature :value "unknown")
			       vital_1 = (percept-node heartrate :value "unknown")
			       vital_2 = (percept-node resprate :value "unknown")
			       vital_3 = (percept-node o2sat :value "unknown")
			       vital_4 = (percept-node pain :value "unknown")
			       acuity = (percept-node acuity :value "unknown")
			       injury_0 = (relation-node internal :value "T" :kb-concept-id "INJURY")
			       injury_1 = (relation-node laceration :value "T" :kb-concept-id "INJURY")
			       death = (percept-node death :value "Nil")
			       injury_0 --> death
			       injury_0 --> acuity
			       injury_0 --> vital_0
			       injury_0 --> vital_1
			       injury_0 --> vital_2
			       injury_0 --> vital_3
			       injury_0 --> vital_4
			       injury_1 --> death
			       injury_1 --> acuity
			       injury_1 --> vital_0
			       injury_1 --> vital_1
			       injury_1 --> vital_2
			       injury_1 --> vital_3
			       injury_1 --> vital_4))


    (setq bn3 (compile-program nil
			       vital_0 = (percept-node temperature :value "unknown")			      
			       vital_1 = (percept-node heartrate :value "unknown")			       
			       vital_2 = (percept-node resprate :value "unknown")			       
			       vital_3 = (percept-node o2sat :value "unknown")
			       vital_4 = (percept-node pain :value "unknown")
			       acuity = (percept-node acuity :value "unknown")
			       injury_0 = (relation-node open_wounds :value "T" :kb-concept-id "INJURY")
			       injury_1 = (relation-node amputation :value "T" :kb-concept-id "INJURY")
			       injury_2 = (relation-node chest_collapse :value "T" :kb-concept-id "INJURY")
			       injury_3 = (relation-node laceration :value "T" :kb-concept-id "INJURY")
			       death = (percept-node death :value "Nil")
			       injury_0 --> death
			       injury_0 --> acuity
			       injury_0 --> vital_0
			       injury_0 --> vital_1
			       injury_0 --> vital_2
			       injury_0 --> vital_3
			       injury_0 --> vital_4
			       injury_1 --> death
			       injury_1 --> acuity
			       injury_1 --> vital_0
			       injury_1 --> vital_1
			       injury_1 --> vital_2
			       injury_1 --> vital_3
			       injury_1 --> vital_4
			       injury_2 --> death
			       injury_2 --> acuity
			       injury_2 --> vital_0
			       injury_2 --> vital_1
			       injury_2 --> vital_2
			       injury_2 --> vital_3
			       injury_2 --> vital_4
			       injury_3 --> death
			       injury_3 --> acuity
			       injury_3 --> vital_0
			       injury_3 --> vital_1
			       injury_3 --> vital_2
			       injury_3 --> vital_3
			       injury_3 --> vital_4))
    
    (setq bn4 (compile-program nil
			       vital_0 = (percept-node temperature :value "high")			      
			       vital_1 = (percept-node heartrate :value "high")
			       vital_2 = (percept-node resprate :value "normal")
			       vital_3 = (percept-node o2sat :value "normal")
			       vital_4 = (percept-node pain :value "2")
			       acuity = (percept-node acuity :value "delayed")
			       injury_0 = (relation-node open_wounds :value "T" :kb-concept-id "INJURY")
			       injury_1 = (relation-node laceration :value "T" :kb-concept-id "INJURY")
			       death = (percept-node death :value "Nil")
			       injury_0 --> death
			       injury_0 --> acuity
			       injury_0 --> vital_0
			       injury_0 --> vital_1
			       injury_0 --> vital_2
			       injury_0 --> vital_3
			       injury_0 --> vital_4
			       injury_1 --> death
			       injury_1 --> acuity
			       injury_1 --> vital_0
			       injury_1 --> vital_1
			       injury_1 --> vital_2
			       injury_1 --> vital_3
			       injury_1 --> vital_4))

    (setq bn5 (compile-program nil 
			       vital_0 = (percept-node temperature :value "unknown")      
			       vital_1 = (percept-node heartrate :value "normal")
			       vital_2 = (percept-node resprate :value "unknown")
			       vital_3 = (percept-node o2sat :value "normal")
			       vital_4 = (percept-node pain :value "unknown")
			       acuity = (percept-node acuity :value "delayed")
			       injury_0 = (relation-node amputation :value "T" :kb-concept-id "INJURY")
			       injury_1 = (relation-node laceration :value "T" :kb-concept-id "INJURY")
			       death = (percept-node death :value "Nil")
			       injury_0 --> death
			       injury_0 --> acuity
			       injury_0 --> vital_0
			       injury_0 --> vital_1
			       injury_0 --> vital_2
			       injury_0 --> vital_3
			       injury_0 --> vital_4
			       injury_1 --> death
			       injury_1 --> acuity
			       injury_1 --> vital_0
			       injury_1 --> vital_1
			       injury_1 --> vital_2
			       injury_1 --> vital_3
			       injury_1 --> vital_4))
    
    (setq bn6 (compile-program nil
			       vital_0 = (percept-node temperature :value "normal")
			       vital_1 = (percept-node heartrate :value "high")
			       vital_2 = (percept-node resprate :value "normal")
			       vital_3 = (percept-node o2sat :value "normal")
			       vital_4 = (percept-node pain :value "0")
			       acuity = (percept-node acuity :value "immediate")
			       injury_0 = (relation-node amputation :value "T" :kb-concept-id "INJURY")
			       injury_1 = (relation-node open_wounds :value "T" :kb-concept-id "INJURY")
			       injury_2 = (relation-node laceration :value "T" :kb-concept-id "INJURY")
			       death = (percept-node death :value "Nil")
			       injury_0 --> death
			       injury_0 --> acuity
			       injury_0 --> vital_0
			       injury_0 --> vital_1
			       injury_0 --> vital_2
			       injury_0 --> vital_3
			       injury_0 --> vital_4
			       injury_1 --> death
			       injury_1 --> acuity
			       injury_1 --> vital_0
			       injury_1 --> vital_1
			       injury_1 --> vital_2
			       injury_1 --> vital_3
			       injury_1 --> vital_4
			       injury_2 --> death
			       injury_2 --> acuity
			       injury_2 --> vital_0
			       injury_2 --> vital_1
			       injury_2 --> vital_2
			       injury_2 --> vital_3
			       injury_2 --> vital_4))
    (setq bn7 (compile-program nil
			       vital_0 = (percept-node temperature :value "unknown")
			       vital_1 = (percept-node heartrate :value "unknown")
			       vital_2 = (percept-node resprate :value "unknown")
			       vital_3 = (percept-node o2sat :value "unknown")
			       vital_4 = (percept-node pain :value "unknown")
			       acuity = (percept-node acuity :value "unknown")
			       injury_0 = (relation-node internal :value "T" :kb-concept-id "INJURY")
			       injury_1 = (relation-node chest_collapse :value "T" :kb-concept-id "INJURY")
			       injury_2 = (relation-node laceration :value "T" :kb-concept-id "INJURY")
			       death = (percept-node death :value "Nil")
			       injury_0 --> death
			       injury_0 --> acuity
			       injury_0 --> vital_0
			       injury_0 --> vital_1
			       injury_0 --> vital_2
			       injury_0 --> vital_3
			       injury_0 --> vital_4
			       injury_1 --> death
			       injury_1 --> acuity
			       injury_1 --> vital_0
			       injury_1 --> vital_1
			       injury_1 --> vital_2
			       injury_1 --> vital_3
			       injury_1 --> vital_4
			       injury_2 --> death
			       injury_2 --> acuity
			       injury_2 --> vital_0
			       injury_2 --> vital_1
			       injury_2 --> vital_2
			       injury_2 --> vital_3
			       injury_2 --> vital_4))

    (setq bn8 (compile-program nil
	       vital_0 = (percept-node temperature :value "normal")
	       vital_1 = (percept-node heartrate :value "normal")
	       vital_2 = (percept-node resprate :value "normal")
	       vital_3 = (percept-node o2sat :value "normal")
	       vital_4 = (percept-node pain :value "3")
	       acuity = (percept-node acuity :value "delayed")
	       injury_0 = (relation-node open_wounds :value "T" :kb-concept-id "INJURY")
	       injury_1 = (relation-node laceration :value "T" :kb-concept-id "INJURY")
	       death = (percept-node death :value "Nil")
	       injury_0 --> death
	       injury_0 --> acuity
	       injury_0 --> vital_0
	       injury_0 --> vital_1
	       injury_0 --> vital_2
	       injury_0 --> vital_3
	       injury_0 --> vital_4
	       injury_1 --> death
	       injury_1 --> acuity
	       injury_1 --> vital_0
	       injury_1 --> vital_1
	       injury_1 --> vital_2
	       injury_1 --> vital_3
	       injury_1 --> vital_4))


    (setq bn9 (compile-program nil
			       vital_0 = (percept-node temperature :value "unknown")
			       vital_1 = (percept-node heartrate :value "unknown")
			       vital_2 = (percept-node resprate :value "unknown")
			       vital_3 = (percept-node o2sat :value "unknown")
			       vital_4 = (percept-node pain :value "unknown")
			       acuity = (percept-node acuity :value "unknown")
			       injury_0 = (relation-node open_wounds :value "T" :kb-concept-id "INJURY")
			       injury_1 = (relation-node amputation :value "T" :kb-concept-id "INJURY")
			       injury_2 = (relation-node laceration :value "T" :kb-concept-id "INJURY")
			       death = (percept-node death :value "Nil")
			       injury_0 --> death
			       injury_0 --> acuity
			       injury_0 --> vital_0
			       injury_0 --> vital_1
			       injury_0 --> vital_2
			       injury_0 --> vital_3
			       injury_0 --> vital_4
			       injury_1 --> death
			       injury_1 --> acuity
			       injury_1 --> vital_0
			       injury_1 --> vital_1
			       injury_1 --> vital_2
			       injury_1 --> vital_3
			       injury_1 --> vital_4
			       injury_2 --> death
			       injury_2 --> acuity
			       injury_2 --> vital_0
			       injury_2 --> vital_1
			       injury_2 --> vital_2
			       injury_2 --> vital_3
			       injury_2 --> vital_4))
    
    (setq bn10 (compile-program nil
				vital_0 = (percept-node temperature :value "unknown")
				vital_1 = (percept-node heartrate :value "unknown")
				vital_2 = (percept-node resprate :value "unknown")
				vital_3 = (percept-node o2sat :value "unknown")
				vital_4 = (percept-node pain :value "unknown")
				acuity = (percept-node acuity :value "unknown")
				injury_0 = (relation-node internal :value "T" :kb-concept-id "INJURY")
				injury_1 = (relation-node laceration :value "T" :kb-concept-id "INJURY")
				death = (percept-node death :value "Nil")
				injury_0 --> death
				injury_0 --> acuity
				injury_0 --> vital_0
				injury_0 --> vital_1
				injury_0 --> vital_2
				injury_0 --> vital_3
				injury_0 --> vital_4
				injury_1 --> death
				injury_1 --> acuity
				injury_1 --> vital_0
				injury_1 --> vital_1
				injury_1 --> vital_2
				injury_1 --> vital_3
				injury_1 --> vital_4))
    
    (loop
      for bn in (list bn1 bn2 bn3 bn4 bn5 bn6 bn7 bn8 bn9 bn10)
      do
	 (new-push-to-ep-buffer :observation bn :bic-p t :insertp t :temporal-p nil :hidden-state-p nil)
      finally
	 (print-episode (car eltm*))
	 (eltm-to-pdf))))
