(ql:quickload :hems)
(in-package :hems)

(defun ex2 ()
  (load-eltm-from-file "eltm.txt")
  (print-bn (episode-observation (car eltm*))))

(defun ex1 ()
  (load-eltm-from-file "eltm.txt")
  (let (cue)
    (setq cue (compile-program nil
			      temperature = (percept-node temperature :value "normal")
			      heartrate = (percept-node heartrate :value "normal")
			      resprate = (percept-node resprate :value "normal")
			      o2sat = (percept-node o2sat :value "normal")
			      sbp = (percept-node sbp :value "normal")
			      dbp = (percept-node dbp :value "normal")
			      pain = (percept-node pain :value "6")
			      chiefcomplaint = (percept-node chiefcomplaint :value "None")
			      chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
			      resp_compromise = (percept-node resp_compromise :value "T")
			      shock = (percept-node shock :value "T")
			      shock --> heartrate
			      resp_compromise --> resprate
			      resp_compromise --> o2sat
			      shock --> sbp
			      shock --> dbp))
    (remember eltm* cue '+ 1 t :type "observation")))
