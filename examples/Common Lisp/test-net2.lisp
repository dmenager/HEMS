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
    
    (list bn1 bn2 bn3)))

(defun ex2 ()
  (let (bns)
    (setq bns
	  (cons (compile-program nil
				 temperature = (percept-node temperature :value "normal")
				 heartrate = (percept-node heartrate :value "normal")
				 resprate = (percept-node resprate :value "normal")
				 o2sat = (percept-node o2sat :value "normal")
				 sbp = (percept-node sbp :value "normal")
				 dbp = (percept-node dbp :value "normal")
				 pain = (percept-node pain :value "7")
				 chiefcomplaint = (percept-node chiefcomplaint :value "None")
				 acuity = (percept-node acuity :value "delayed")
				 chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
				 death = (percept-node death :value "Nil")
				 resp_compromise = (relation-node resp_compromise :value "T")
				 shock = (relation-node shock :value "T")
				 cardiac_arrest = (relation-node cardiac_arrest :value "Nil" :kb-concept-id "INJURY")
				 semicoma_stupor = (relation-node semicoma_stupor :value "Nil" :kb-concept-id "INJURY")
				 myocardial_infarction = (relation-node myocardial_infarction :value "Nil" :kb-concept-id "INJURY")
				 intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "Nil" :kb-concept-id "INJURY")
				 acute_respiratory_failure = (relation-node acute_respiratory_failure :value "Nil" :kb-concept-id "INJURY")
				 malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "Nil" :kb-concept-id "INJURY")
				 septicemia = (relation-node septicemia :value "Nil" :kb-concept-id "INJURY")
				 sepsis = (relation-node sepsis :value "Nil" :kb-concept-id "INJURY")
				 cardiac_arrest --> shock
				 shock --> semicoma_stupor
				 resp_compromise --> semicoma_stupor
				 myocardial_infarction --> cardiac_arrest
				 intracerebral_hemorrhage --> shock
				 chronic_airway_obstruction --> acute_respiratory_failure
				 acute_respiratory_failure --> resp_compromise
				 malignant_neoplasm_of_bronchus --> acute_respiratory_failure
				 septicemia --> sepsis
				 sepsis --> cardiac_arrest
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
				 )
		bns))
    (setq bns
	  (cons (compile-program nil
				 temperature = (percept-node temperature :value "low")
				 heartrate = (percept-node heartrate :value "normal")
				 resprate = (percept-node resprate :value "high")
				 o2sat = (percept-node o2sat :value "normal")
				 sbp = (percept-node sbp :value "high")
				 dbp = (percept-node dbp :value "high")
				 pain = (percept-node pain :value "0")
				 chiefcomplaint = (percept-node chiefcomplaint :value "None")
				 acuity = (percept-node acuity :value "immediate")
				 chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
				 death = (percept-node death :value "Nil")
				 resp_compromise = (relation-node resp_compromise :value "T")
				 shock = (relation-node shock :value "T")
				 cardiac_arrest = (relation-node cardiac_arrest :value "Nil" :kb-concept-id "INJURY")
				 semicoma_stupor = (relation-node semicoma_stupor :value "Nil" :kb-concept-id "INJURY")
				 myocardial_infarction = (relation-node myocardial_infarction :value "Nil" :kb-concept-id "INJURY")
				 intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "Nil" :kb-concept-id "INJURY")
				 acute_respiratory_failure = (relation-node acute_respiratory_failure :value "Nil" :kb-concept-id "INJURY")
				 malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "Nil" :kb-concept-id "INJURY")
				 septicemia = (relation-node septicemia :value "Nil" :kb-concept-id "INJURY")
				 sepsis = (relation-node sepsis :value "Nil" :kb-concept-id "INJURY")
				 cardiac_arrest --> shock
				 shock --> semicoma_stupor
				 resp_compromise --> semicoma_stupor
				 myocardial_infarction --> cardiac_arrest
				 intracerebral_hemorrhage --> shock
				 chronic_airway_obstruction --> acute_respiratory_failure
				 acute_respiratory_failure --> resp_compromise
				 malignant_neoplasm_of_bronchus --> acute_respiratory_failure
				 septicemia --> sepsis
				 sepsis --> cardiac_arrest
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
				 )
		bns))
    (setq bns
	  (cons (compile-program nil
				 temperature = (percept-node temperature :value "normal")
				 heartrate = (percept-node heartrate :value "low")
				 resprate = (percept-node resprate :value "normal")
				 o2sat = (percept-node o2sat :value "normal")
				 sbp = (percept-node sbp :value "high")
				 dbp = (percept-node dbp :value "normal")
				 pain = (percept-node pain :value "10")
				 chiefcomplaint = (percept-node chiefcomplaint :value "None")
				 acuity = (percept-node acuity :value "immediate")
				 chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
				 death = (percept-node death :value "Nil")
				 resp_compromise = (relation-node resp_compromise :value "T")
				 shock = (relation-node shock :value "T")
				 cardiac_arrest = (relation-node cardiac_arrest :value "Nil" :kb-concept-id "INJURY")
				 semicoma_stupor = (relation-node semicoma_stupor :value "Nil" :kb-concept-id "INJURY")
				 myocardial_infarction = (relation-node myocardial_infarction :value "Nil" :kb-concept-id "INJURY")
				 intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "Nil" :kb-concept-id "INJURY")
				 acute_respiratory_failure = (relation-node acute_respiratory_failure :value "Nil" :kb-concept-id "INJURY")
				 malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "Nil" :kb-concept-id "INJURY")
				 septicemia = (relation-node septicemia :value "Nil" :kb-concept-id "INJURY")
				 sepsis = (relation-node sepsis :value "Nil" :kb-concept-id "INJURY")
				 cardiac_arrest --> shock
				 shock --> semicoma_stupor
				 resp_compromise --> semicoma_stupor
				 myocardial_infarction --> cardiac_arrest
				 intracerebral_hemorrhage --> shock
				 chronic_airway_obstruction --> acute_respiratory_failure
				 acute_respiratory_failure --> resp_compromise
				 malignant_neoplasm_of_bronchus --> acute_respiratory_failure
				 septicemia --> sepsis
				 sepsis --> cardiac_arrest
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
				 )
		bns))
    (setq bns
	  (cons (compile-program nil
				 temperature = (percept-node temperature :value "low")
				 heartrate = (percept-node heartrate :value "normal")
				 resprate = (percept-node resprate :value "normal")
				 o2sat = (percept-node o2sat :value "normal")
				 sbp = (percept-node sbp :value "normal")
				 dbp = (percept-node dbp :value "normal")
				 pain = (percept-node pain :value "1")
				 chiefcomplaint = (percept-node chiefcomplaint :value "None")
				 acuity = (percept-node acuity :value "immediate")
				 myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
				 death = (percept-node death :value "Nil")
				 resp_compromise = (relation-node resp_compromise :value "T")
				 shock = (relation-node shock :value "T")
				 cardiac_arrest = (relation-node cardiac_arrest :value "Nil" :kb-concept-id "INJURY")
				 semicoma_stupor = (relation-node semicoma_stupor :value "Nil" :kb-concept-id "INJURY")
				 intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "Nil" :kb-concept-id "INJURY")
				 chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "Nil" :kb-concept-id "INJURY")
				 acute_respiratory_failure = (relation-node acute_respiratory_failure :value "Nil" :kb-concept-id "INJURY")
				 malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "Nil" :kb-concept-id "INJURY")
				 septicemia = (relation-node septicemia :value "Nil" :kb-concept-id "INJURY")
				 sepsis = (relation-node sepsis :value "Nil" :kb-concept-id "INJURY")
				 cardiac_arrest --> shock
				 shock --> semicoma_stupor
				 resp_compromise --> semicoma_stupor
				 myocardial_infarction --> cardiac_arrest
				 intracerebral_hemorrhage --> shock
				 chronic_airway_obstruction --> acute_respiratory_failure
				 acute_respiratory_failure --> resp_compromise
				 malignant_neoplasm_of_bronchus --> acute_respiratory_failure
				 septicemia --> sepsis
				 sepsis --> cardiac_arrest
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
				 resp_compromise --> death)
		bns))
    (setq bns
	  (cons (compile-program nil
				 temperature = (percept-node temperature :value "high")
				 heartrate = (percept-node heartrate :value "normal")
				 resprate = (percept-node resprate :value "normal")
				 o2sat = (percept-node o2sat :value "normal")
				 sbp = (percept-node sbp :value "normal")
				 dbp = (percept-node dbp :value "normal")
				 pain = (percept-node pain :value "10")
				 chiefcomplaint = (percept-node chiefcomplaint :value "None")
				 acuity = (percept-node acuity :value "immediate")
				 septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
				 sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
				 death = (percept-node death :value "Nil")
				 resp_compromise = (relation-node resp_compromise :value "T")
				 shock = (relation-node shock :value "T")
				 cardiac_arrest = (relation-node cardiac_arrest :value "Nil" :kb-concept-id "INJURY")
				 semicoma_stupor = (relation-node semicoma_stupor :value "Nil" :kb-concept-id "INJURY")
				 myocardial_infarction = (relation-node myocardial_infarction :value "Nil" :kb-concept-id "INJURY")
				 intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "Nil" :kb-concept-id "INJURY")
				 chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "Nil" :kb-concept-id "INJURY")
				 acute_respiratory_failure = (relation-node acute_respiratory_failure :value "Nil" :kb-concept-id "INJURY")
				 malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "Nil" :kb-concept-id "INJURY")
				 cardiac_arrest --> shock
				 shock --> semicoma_stupor
				 resp_compromise --> semicoma_stupor
				 myocardial_infarction --> cardiac_arrest
				 intracerebral_hemorrhage --> shock
				 chronic_airway_obstruction --> acute_respiratory_failure
				 acute_respiratory_failure --> resp_compromise
				 malignant_neoplasm_of_bronchus --> acute_respiratory_failure
				 septicemia --> sepsis
				 sepsis --> cardiac_arrest
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
				 )
		bns))
    (setq bns
	  (cons (compile-program nil
				 temperature = (percept-node temperature :value "normal")
				 heartrate = (percept-node heartrate :value "low")
				 resprate = (percept-node resprate :value "normal")
				 o2sat = (percept-node o2sat :value "low")
				 sbp = (percept-node sbp :value "normal")
				 dbp = (percept-node dbp :value "low")
				 pain = (percept-node pain :value "0")
				 chiefcomplaint = (percept-node chiefcomplaint :value "None")
				 acuity = (percept-node acuity :value "immediate")
				 semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
				 death = (percept-node death :value "Nil")
				 resp_compromise = (relation-node resp_compromise :value "T")
				 shock = (relation-node shock :value "T")
				 cardiac_arrest = (relation-node cardiac_arrest :value "Nil" :kb-concept-id "INJURY")
				 myocardial_infarction = (relation-node myocardial_infarction :value "Nil" :kb-concept-id "INJURY")
				 intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "Nil" :kb-concept-id "INJURY")
				 chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "Nil" :kb-concept-id "INJURY")
				 acute_respiratory_failure = (relation-node acute_respiratory_failure :value "Nil" :kb-concept-id "INJURY")
				 malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "Nil" :kb-concept-id "INJURY")
				 septicemia = (relation-node septicemia :value "Nil" :kb-concept-id "INJURY")
				 sepsis = (relation-node sepsis :value "Nil" :kb-concept-id "INJURY")
				 cardiac_arrest --> shock
				 shock --> semicoma_stupor
				 resp_compromise --> semicoma_stupor
				 myocardial_infarction --> cardiac_arrest
				 intracerebral_hemorrhage --> shock
				 chronic_airway_obstruction --> acute_respiratory_failure
				 acute_respiratory_failure --> resp_compromise
				 malignant_neoplasm_of_bronchus --> acute_respiratory_failure
				 septicemia --> sepsis
				 sepsis --> cardiac_arrest
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
				 )
		bns))
    (reverse bns)))

(defun run ()
  (loop
	for bn in (ex2)
	do
	;; (format t "~%~%inserting:")
	;; (print-bn bn)
	(new-push-to-ep-buffer :observation bn :hidden-state-p nil :insertp t :temporal-p nil)))
