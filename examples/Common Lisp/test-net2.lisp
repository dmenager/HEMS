(ql:quickload :hems)
(in-package :hems)

(defun ex1 ()
  (let (bn1 bn2 bn3)
    (setq bn1 (compile-program nil
			       temperature = (percept-node temperature :value "normal")
			       heartrate = (percept-node heartrate :value "normal")
			       resprate = (percept-node resprate :value "normal")
			       o2sat = (percept-node o2sat :value "normal")
			       sbp = (percept-node sbp :value "normal")
			       dbp = (percept-node dbp :value "low")
			       pain = (percept-node pain :value "5")
			       chiefcomplaint = (percept-node chiefcomplaint :value "None")
			       acuity = (percept-node acuity :value "immediate")
			       death = (percept-node death :value "Nil")
			       resp_compromise = (relation-node resp_compromise :value "T")
			       shock = (relation-node shock :value "T")
			       temperature --> acuity
			       shock --> heartrate
			       heartrate --> acuity
			       resprate --> acuity
			       o2sat --> acuity
			       shock --> sbp
			       sbp --> acuity
			       shock --> dbp
			       dbp --> acuity
			       pain --> acuity
			       chiefcomplaint --> acuity
			       shock --> death))
    
    (setq bn2 (compile-program nil
			       temperature = (percept-node temperature :value "normal")
			       heartrate = (percept-node heartrate :value "high")
			       resprate = (percept-node resprate :value "normal")
			       o2sat = (percept-node o2sat :value "low")
			       sbp = (percept-node sbp :value "high")
			       dbp = (percept-node dbp :value "high")
			       pain = (percept-node pain :value "0")
			       chiefcomplaint = (percept-node chiefcomplaint :value "None")
			       acuity = (percept-node acuity :value "immediate")
			       semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
			       death = (percept-node death :value "Nil")
			       resp_compromise = (relation-node resp_compromise :value "T")
			       shock = (relation-node shock :value "T")
			       shock --> semicoma_stupor
			       resp_compromise --> semicoma_stupor
			       temperature --> acuity
			       shock --> heartrate
			       heartrate --> acuity
			       resp_compromise --> resprate
			       resprate --> acuity
			       resp_compromise --> o2sat
			       o2sat --> acuity
			       shock --> sbp
			       sbp --> acuity
			       shock --> dbp
			       dbp --> acuity
			       pain --> acuity
			       chiefcomplaint --> acuity
			       shock --> death
			       resp_compromise --> death))
    
    (setq bn3 (compile-program nil
			       temperature = (percept-node temperature :value "normal")
			       heartrate = (percept-node heartrate :value "high")
			       resprate = (percept-node resprate :value "normal")
			       o2sat = (percept-node o2sat :value "low")
			       sbp = (percept-node sbp :value "high")
			       dbp = (percept-node dbp :value "high")
			       pain = (percept-node pain :value "0")
			       chiefcomplaint = (percept-node chiefcomplaint :value "None")
			       acuity = (percept-node acuity :value "delayed")
			       septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
			       sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
			       death = (percept-node death :value "Nil")
			       resp_compromise = (relation-node resp_compromise :value "T")
			       shock = (relation-node shock :value "T")
			       septicemia --> sepsis
			       sepsis --> shock
			       temperature --> acuity
			       shock --> heartrate
			       heartrate --> acuity
			       resp_compromise --> resprate
			       resprate --> acuity
			       resp_compromise --> o2sat
			       o2sat --> acuity
			       shock --> sbp
			       sbp --> acuity
			       shock --> dbp
			       dbp --> acuity
			       pain --> acuity
			       chiefcomplaint --> acuity
			       shock --> death
			       resp_compromise --> death
			       ))
    
    (loop
      for bn in (list bn1 bn2 bn3)
      do
	 ;; (format t "~%~%inserting:")
	 ;; (print-bn bn)
	 (new-push-to-ep-buffer :observation bn :hidden-state-p nil :insertp t :temporal-p nil))))
