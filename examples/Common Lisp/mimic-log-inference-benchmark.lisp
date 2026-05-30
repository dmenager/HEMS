(ql:quickload :hems)
(in-package :hems)

;; Static fixture extracted from examples/Common Lisp/log.txt.
;; Do not parse log.txt at runtime; this file intentionally materializes
;; the generated HEMS programs as compile-program calls.

(defmacro with-mimic-log-timing ((label) &body body)
  `(progn
     (format t "~&~A~%" ,label)
     (finish-output)
     (time (progn ,@body))))

(defun reset-mimic-log-benchmark ()
  (init-eltm)
  (setq episode-buffer* (list ':obs (make-hash-table) ':h-model nil))
  eltm*)

(defun mimic-log-bn-0 ()
  ;; log.txt Tempfile Contents block 0
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "2")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-1 ()
  ;; log.txt Tempfile Contents block 1
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-2 ()
  ;; log.txt Tempfile Contents block 2
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-3 ()
  ;; log.txt Tempfile Contents block 3
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "4")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-4 ()
  ;; log.txt Tempfile Contents block 4
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-5 ()
  ;; log.txt Tempfile Contents block 5
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-6 ()
  ;; log.txt Tempfile Contents block 6
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "6")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-7 ()
  ;; log.txt Tempfile Contents block 7
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-8 ()
  ;; log.txt Tempfile Contents block 8
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-9 ()
  ;; log.txt Tempfile Contents block 9
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "Nil")
    septicemia --> sepsis
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

(defun mimic-log-bn-10 ()
  ;; log.txt Tempfile Contents block 10
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "low")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-11 ()
  ;; log.txt Tempfile Contents block 11
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "2")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-12 ()
  ;; log.txt Tempfile Contents block 12
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "unknown")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "Nil")
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

(defun mimic-log-bn-13 ()
  ;; log.txt Tempfile Contents block 13
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-14 ()
  ;; log.txt Tempfile Contents block 14
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "unknown")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "Nil")
    septicemia --> sepsis
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

(defun mimic-log-bn-15 ()
  ;; log.txt Tempfile Contents block 15
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-16 ()
  ;; log.txt Tempfile Contents block 16
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-17 ()
  ;; log.txt Tempfile Contents block 17
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "6")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-18 ()
  ;; log.txt Tempfile Contents block 18
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-19 ()
  ;; log.txt Tempfile Contents block 19
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "2")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defun mimic-log-bn-20 ()
  ;; log.txt Tempfile Contents block 20
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    acute_respiratory_failure = (relation-node acute_respiratory_failure :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    acute_respiratory_failure --> resp_compromise
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

(defun mimic-log-bn-21 ()
  ;; log.txt Tempfile Contents block 21
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-22 ()
  ;; log.txt Tempfile Contents block 22
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-23 ()
  ;; log.txt Tempfile Contents block 23
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-24 ()
  ;; log.txt Tempfile Contents block 24
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "low")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-25 ()
  ;; log.txt Tempfile Contents block 25
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "5")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defun mimic-log-bn-26 ()
  ;; log.txt Tempfile Contents block 26
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-27 ()
  ;; log.txt Tempfile Contents block 27
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-28 ()
  ;; log.txt Tempfile Contents block 28
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defun mimic-log-bn-29 ()
  ;; log.txt Tempfile Contents block 29
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defun mimic-log-bn-30 ()
  ;; log.txt Tempfile Contents block 30
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "3")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-31 ()
  ;; log.txt Tempfile Contents block 31
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-32 ()
  ;; log.txt Tempfile Contents block 32
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-33 ()
  ;; log.txt Tempfile Contents block 33
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "Nil")
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

(defun mimic-log-bn-34 ()
  ;; log.txt Tempfile Contents block 34
  (compile-program nil
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
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-35 ()
  ;; log.txt Tempfile Contents block 35
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "1")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-36 ()
  ;; log.txt Tempfile Contents block 36
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-37 ()
  ;; log.txt Tempfile Contents block 37
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "4")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-38 ()
  ;; log.txt Tempfile Contents block 38
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-39 ()
  ;; log.txt Tempfile Contents block 39
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "8")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-40 ()
  ;; log.txt Tempfile Contents block 40
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "6")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-41 ()
  ;; log.txt Tempfile Contents block 41
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    acute_respiratory_failure = (relation-node acute_respiratory_failure :value "T" :kb-concept-id "INJURY")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    acute_respiratory_failure --> resp_compromise
    chronic_airway_obstruction --> acute_respiratory_failure
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

(defun mimic-log-bn-42 ()
  ;; log.txt Tempfile Contents block 42
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-43 ()
  ;; log.txt Tempfile Contents block 43
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-44 ()
  ;; log.txt Tempfile Contents block 44
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-45 ()
  ;; log.txt Tempfile Contents block 45
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-46 ()
  ;; log.txt Tempfile Contents block 46
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-47 ()
  ;; log.txt Tempfile Contents block 47
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-48 ()
  ;; log.txt Tempfile Contents block 48
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defun mimic-log-bn-49 ()
  ;; log.txt Tempfile Contents block 49
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-50 ()
  ;; log.txt Tempfile Contents block 50
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-51 ()
  ;; log.txt Tempfile Contents block 51
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "8")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-52 ()
  ;; log.txt Tempfile Contents block 52
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "5")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-53 ()
  ;; log.txt Tempfile Contents block 53
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "unknown")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    cardiac_arrest = (relation-node cardiac_arrest :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "Nil")
    intracerebral_hemorrhage --> shock
    cardiac_arrest --> shock
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

(defun mimic-log-bn-54 ()
  ;; log.txt Tempfile Contents block 54
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-55 ()
  ;; log.txt Tempfile Contents block 55
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-56 ()
  ;; log.txt Tempfile Contents block 56
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-57 ()
  ;; log.txt Tempfile Contents block 57
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "low")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-58 ()
  ;; log.txt Tempfile Contents block 58
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-59 ()
  ;; log.txt Tempfile Contents block 59
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "3")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-60 ()
  ;; log.txt Tempfile Contents block 60
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "6")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-61 ()
  ;; log.txt Tempfile Contents block 61
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    acute_respiratory_failure = (relation-node acute_respiratory_failure :value "T" :kb-concept-id "INJURY")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    acute_respiratory_failure --> resp_compromise
    chronic_airway_obstruction --> acute_respiratory_failure
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

(defun mimic-log-bn-62 ()
  ;; log.txt Tempfile Contents block 62
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-63 ()
  ;; log.txt Tempfile Contents block 63
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-64 ()
  ;; log.txt Tempfile Contents block 64
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-65 ()
  ;; log.txt Tempfile Contents block 65
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-66 ()
  ;; log.txt Tempfile Contents block 66
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-67 ()
  ;; log.txt Tempfile Contents block 67
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-68 ()
  ;; log.txt Tempfile Contents block 68
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defun mimic-log-bn-69 ()
  ;; log.txt Tempfile Contents block 69
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-70 ()
  ;; log.txt Tempfile Contents block 70
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-71 ()
  ;; log.txt Tempfile Contents block 71
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "8")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-72 ()
  ;; log.txt Tempfile Contents block 72
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "5")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-73 ()
  ;; log.txt Tempfile Contents block 73
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "unknown")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    cardiac_arrest = (relation-node cardiac_arrest :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "Nil")
    intracerebral_hemorrhage --> shock
    cardiac_arrest --> shock
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

(defun mimic-log-bn-74 ()
  ;; log.txt Tempfile Contents block 74
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-75 ()
  ;; log.txt Tempfile Contents block 75
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-76 ()
  ;; log.txt Tempfile Contents block 76
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-77 ()
  ;; log.txt Tempfile Contents block 77
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "low")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-78 ()
  ;; log.txt Tempfile Contents block 78
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-79 ()
  ;; log.txt Tempfile Contents block 79
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "3")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-80 ()
  ;; log.txt Tempfile Contents block 80
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-81 ()
  ;; log.txt Tempfile Contents block 81
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-82 ()
  ;; log.txt Tempfile Contents block 82
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "unknown")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    cardiac_arrest = (relation-node cardiac_arrest :value "T" :kb-concept-id "INJURY")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "Nil")
    shock --> semicoma_stupor
    resp_compromise --> semicoma_stupor
    cardiac_arrest --> shock
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

(defun mimic-log-bn-83 ()
  ;; log.txt Tempfile Contents block 83
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-84 ()
  ;; log.txt Tempfile Contents block 84
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-85 ()
  ;; log.txt Tempfile Contents block 85
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-86 ()
  ;; log.txt Tempfile Contents block 86
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-87 ()
  ;; log.txt Tempfile Contents block 87
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "low")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-88 ()
  ;; log.txt Tempfile Contents block 88
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-89 ()
  ;; log.txt Tempfile Contents block 89
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "1")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-90 ()
  ;; log.txt Tempfile Contents block 90
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defun mimic-log-bn-91 ()
  ;; log.txt Tempfile Contents block 91
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "2")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-92 ()
  ;; log.txt Tempfile Contents block 92
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "5")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-93 ()
  ;; log.txt Tempfile Contents block 93
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-94 ()
  ;; log.txt Tempfile Contents block 94
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-95 ()
  ;; log.txt Tempfile Contents block 95
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    acute_respiratory_failure = (relation-node acute_respiratory_failure :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "Nil")
    acute_respiratory_failure --> resp_compromise
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

(defun mimic-log-bn-96 ()
  ;; log.txt Tempfile Contents block 96
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-97 ()
  ;; log.txt Tempfile Contents block 97
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-98 ()
  ;; log.txt Tempfile Contents block 98
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "low")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "low")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-99 ()
  ;; log.txt Tempfile Contents block 99
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "9")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-100 ()
  ;; log.txt Tempfile Contents block 100
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-101 ()
  ;; log.txt Tempfile Contents block 101
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-102 ()
  ;; log.txt Tempfile Contents block 102
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-103 ()
  ;; log.txt Tempfile Contents block 103
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-104 ()
  ;; log.txt Tempfile Contents block 104
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-105 ()
  ;; log.txt Tempfile Contents block 105
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-106 ()
  ;; log.txt Tempfile Contents block 106
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "low")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "3")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-107 ()
  ;; log.txt Tempfile Contents block 107
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "low")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defun mimic-log-bn-108 ()
  ;; log.txt Tempfile Contents block 108
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-109 ()
  ;; log.txt Tempfile Contents block 109
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defun mimic-log-bn-110 ()
  ;; log.txt Tempfile Contents block 110
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "low")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "3")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-111 ()
  ;; log.txt Tempfile Contents block 111
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-112 ()
  ;; log.txt Tempfile Contents block 112
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "Nil")
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

(defun mimic-log-bn-113 ()
  ;; log.txt Tempfile Contents block 113
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-114 ()
  ;; log.txt Tempfile Contents block 114
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "8")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defun mimic-log-bn-115 ()
  ;; log.txt Tempfile Contents block 115
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-116 ()
  ;; log.txt Tempfile Contents block 116
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-117 ()
  ;; log.txt Tempfile Contents block 117
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-118 ()
  ;; log.txt Tempfile Contents block 118
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "8")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-119 ()
  ;; log.txt Tempfile Contents block 119
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-120 ()
  ;; log.txt Tempfile Contents block 120
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    acute_respiratory_failure = (relation-node acute_respiratory_failure :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    acute_respiratory_failure --> resp_compromise
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

(defun mimic-log-bn-121 ()
  ;; log.txt Tempfile Contents block 121
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-122 ()
  ;; log.txt Tempfile Contents block 122
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-123 ()
  ;; log.txt Tempfile Contents block 123
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-124 ()
  ;; log.txt Tempfile Contents block 124
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "low")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-125 ()
  ;; log.txt Tempfile Contents block 125
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "5")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defun mimic-log-bn-126 ()
  ;; log.txt Tempfile Contents block 126
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-127 ()
  ;; log.txt Tempfile Contents block 127
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-128 ()
  ;; log.txt Tempfile Contents block 128
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defun mimic-log-bn-129 ()
  ;; log.txt Tempfile Contents block 129
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defun mimic-log-bn-130 ()
  ;; log.txt Tempfile Contents block 130
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "3")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-131 ()
  ;; log.txt Tempfile Contents block 131
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-132 ()
  ;; log.txt Tempfile Contents block 132
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-133 ()
  ;; log.txt Tempfile Contents block 133
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "Nil")
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

(defun mimic-log-bn-134 ()
  ;; log.txt Tempfile Contents block 134
  (compile-program nil
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
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-135 ()
  ;; log.txt Tempfile Contents block 135
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "1")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-136 ()
  ;; log.txt Tempfile Contents block 136
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-137 ()
  ;; log.txt Tempfile Contents block 137
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "4")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-138 ()
  ;; log.txt Tempfile Contents block 138
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-139 ()
  ;; log.txt Tempfile Contents block 139
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "8")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-140 ()
  ;; log.txt Tempfile Contents block 140
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "2")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-141 ()
  ;; log.txt Tempfile Contents block 141
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-142 ()
  ;; log.txt Tempfile Contents block 142
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-143 ()
  ;; log.txt Tempfile Contents block 143
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "4")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-144 ()
  ;; log.txt Tempfile Contents block 144
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-145 ()
  ;; log.txt Tempfile Contents block 145
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-146 ()
  ;; log.txt Tempfile Contents block 146
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "6")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-147 ()
  ;; log.txt Tempfile Contents block 147
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-148 ()
  ;; log.txt Tempfile Contents block 148
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-149 ()
  ;; log.txt Tempfile Contents block 149
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "Nil")
    septicemia --> sepsis
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

(defun mimic-log-bn-150 ()
  ;; log.txt Tempfile Contents block 150
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "low")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-151 ()
  ;; log.txt Tempfile Contents block 151
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "2")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-152 ()
  ;; log.txt Tempfile Contents block 152
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "unknown")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "Nil")
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

(defun mimic-log-bn-153 ()
  ;; log.txt Tempfile Contents block 153
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-154 ()
  ;; log.txt Tempfile Contents block 154
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "unknown")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "Nil")
    septicemia --> sepsis
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

(defun mimic-log-bn-155 ()
  ;; log.txt Tempfile Contents block 155
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
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
    resp_compromise --> death
    ))

(defun mimic-log-bn-156 ()
  ;; log.txt Tempfile Contents block 156
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-157 ()
  ;; log.txt Tempfile Contents block 157
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "6")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
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

(defun mimic-log-bn-158 ()
  ;; log.txt Tempfile Contents block 158
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    septicemia = (relation-node septicemia :value "T" :kb-concept-id "INJURY")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    septicemia --> sepsis
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

(defun mimic-log-bn-159 ()
  ;; log.txt Tempfile Contents block 159
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "2")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
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

(defparameter *mimic-log-training-builders*
  (list
   #'mimic-log-bn-0
   #'mimic-log-bn-1
   #'mimic-log-bn-2
   #'mimic-log-bn-3
   #'mimic-log-bn-4
   #'mimic-log-bn-5
   #'mimic-log-bn-6
   #'mimic-log-bn-7
   #'mimic-log-bn-8
   #'mimic-log-bn-9
   #'mimic-log-bn-10
   #'mimic-log-bn-11
   #'mimic-log-bn-12
   #'mimic-log-bn-13
   #'mimic-log-bn-14
   #'mimic-log-bn-15
   #'mimic-log-bn-16
   #'mimic-log-bn-17
   #'mimic-log-bn-18
   #'mimic-log-bn-19
   #'mimic-log-bn-20
   #'mimic-log-bn-21
   #'mimic-log-bn-22
   #'mimic-log-bn-23
   #'mimic-log-bn-24
   #'mimic-log-bn-25
   #'mimic-log-bn-26
   #'mimic-log-bn-27
   #'mimic-log-bn-28
   #'mimic-log-bn-29
   #'mimic-log-bn-30
   #'mimic-log-bn-31
   #'mimic-log-bn-32
   #'mimic-log-bn-33
   #'mimic-log-bn-34
   #'mimic-log-bn-35
   #'mimic-log-bn-36
   #'mimic-log-bn-37
   #'mimic-log-bn-38
   #'mimic-log-bn-39
   #'mimic-log-bn-40
   #'mimic-log-bn-41
   #'mimic-log-bn-42
   #'mimic-log-bn-43
   #'mimic-log-bn-44
   #'mimic-log-bn-45
   #'mimic-log-bn-46
   #'mimic-log-bn-47
   #'mimic-log-bn-48
   #'mimic-log-bn-49
   #'mimic-log-bn-50
   #'mimic-log-bn-51
   #'mimic-log-bn-52
   #'mimic-log-bn-53
   #'mimic-log-bn-54
   #'mimic-log-bn-55
   #'mimic-log-bn-56
   #'mimic-log-bn-57
   #'mimic-log-bn-58
   #'mimic-log-bn-59
   #'mimic-log-bn-60
   #'mimic-log-bn-61
   #'mimic-log-bn-62
   #'mimic-log-bn-63
   #'mimic-log-bn-64
   #'mimic-log-bn-65
   #'mimic-log-bn-66
   #'mimic-log-bn-67
   #'mimic-log-bn-68
   #'mimic-log-bn-69
   #'mimic-log-bn-70
   #'mimic-log-bn-71
   #'mimic-log-bn-72
   #'mimic-log-bn-73
   #'mimic-log-bn-74
   #'mimic-log-bn-75
   #'mimic-log-bn-76
   #'mimic-log-bn-77
   #'mimic-log-bn-78
   #'mimic-log-bn-79
   #'mimic-log-bn-80
   #'mimic-log-bn-81
   #'mimic-log-bn-82
   #'mimic-log-bn-83
   #'mimic-log-bn-84
   #'mimic-log-bn-85
   #'mimic-log-bn-86
   #'mimic-log-bn-87
   #'mimic-log-bn-88
   #'mimic-log-bn-89
   #'mimic-log-bn-90
   #'mimic-log-bn-91
   #'mimic-log-bn-92
   #'mimic-log-bn-93
   #'mimic-log-bn-94
   #'mimic-log-bn-95
   #'mimic-log-bn-96
   #'mimic-log-bn-97
   #'mimic-log-bn-98
   #'mimic-log-bn-99
   #'mimic-log-bn-100
   #'mimic-log-bn-101
   #'mimic-log-bn-102
   #'mimic-log-bn-103
   #'mimic-log-bn-104
   #'mimic-log-bn-105
   #'mimic-log-bn-106
   #'mimic-log-bn-107
   #'mimic-log-bn-108
   #'mimic-log-bn-109
   #'mimic-log-bn-110
   #'mimic-log-bn-111
   #'mimic-log-bn-112
   #'mimic-log-bn-113
   #'mimic-log-bn-114
   #'mimic-log-bn-115
   #'mimic-log-bn-116
   #'mimic-log-bn-117
   #'mimic-log-bn-118
   #'mimic-log-bn-119
   #'mimic-log-bn-120
   #'mimic-log-bn-121
   #'mimic-log-bn-122
   #'mimic-log-bn-123
   #'mimic-log-bn-124
   #'mimic-log-bn-125
   #'mimic-log-bn-126
   #'mimic-log-bn-127
   #'mimic-log-bn-128
   #'mimic-log-bn-129
   #'mimic-log-bn-130
   #'mimic-log-bn-131
   #'mimic-log-bn-132
   #'mimic-log-bn-133
   #'mimic-log-bn-134
   #'mimic-log-bn-135
   #'mimic-log-bn-136
   #'mimic-log-bn-137
   #'mimic-log-bn-138
   #'mimic-log-bn-139
   #'mimic-log-bn-140
   #'mimic-log-bn-141
   #'mimic-log-bn-142
   #'mimic-log-bn-143
   #'mimic-log-bn-144
   #'mimic-log-bn-145
   #'mimic-log-bn-146
   #'mimic-log-bn-147
   #'mimic-log-bn-148
   #'mimic-log-bn-149
   #'mimic-log-bn-150
   #'mimic-log-bn-151
   #'mimic-log-bn-152
   #'mimic-log-bn-153
   #'mimic-log-bn-154
   #'mimic-log-bn-155
   #'mimic-log-bn-156
   #'mimic-log-bn-157
   #'mimic-log-bn-158
   #'mimic-log-bn-159))

(defun mimic-log-training-bns (&key limit)
  (loop
    for builder in *mimic-log-training-builders*
    for i from 0
    while (or (null limit) (< i limit))
    collect (funcall builder)))

(defun insert-mimic-log-training-bns (bns &key (bic-p t))
  (loop
    for bn in bns
    count bn into inserted
    do (new-push-to-ep-buffer :observation bn
                              :bic-p bic-p
                              :insertp t
                              :temporal-p nil
                              :hidden-state-p nil)
    finally (return inserted)))

(defun mimic-log-test-cue ()
  ;; Held-out-style cue from the same MIMIC-derived variable/value distribution.
  ;; Outcome variables are omitted so remember must infer them from the retrieved model.
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    sepsis --> shock
    resp_compromise --> sepsis
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp))

(defun mimic-log-test-case-0-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    acute_respiratory_failure = (relation-node acute_respiratory_failure :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    acute_respiratory_failure --> resp_compromise
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-0-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "10")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acute_respiratory_failure = (relation-node acute_respiratory_failure :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    acute_respiratory_failure --> resp_compromise
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-1-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    shock --> semicoma_stupor
    resp_compromise --> semicoma_stupor
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-1-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    shock --> semicoma_stupor
    resp_compromise --> semicoma_stupor
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-2-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    shock --> semicoma_stupor
    resp_compromise --> semicoma_stupor
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-2-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    shock --> semicoma_stupor
    resp_compromise --> semicoma_stupor
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-3-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "4")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-3-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "4")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-4-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-4-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-5-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-5-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-6-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "6")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "expectant")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-6-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "6")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-7-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "low")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    sepsis --> shock
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-7-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "low")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    sepsis --> shock
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-8-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-8-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "7")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-9-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "13")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "expectant")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    sepsis --> shock
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-9-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "13")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    sepsis --> shock
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-10-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "low")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "expectant")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    sepsis --> shock
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-10-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "low")
    dbp = (percept-node dbp :value "low")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    sepsis --> shock
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-11-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "low")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "2")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-11-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "low")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "2")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-12-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "unknown")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-12-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "unknown")
    resprate = (percept-node resprate :value "unknown")
    o2sat = (percept-node o2sat :value "unknown")
    sbp = (percept-node sbp :value "unknown")
    dbp = (percept-node dbp :value "unknown")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-13-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-13-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    chronic_airway_obstruction = (relation-node chronic_airway_obstruction :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-14-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "expectant")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    shock --> semicoma_stupor
    resp_compromise --> semicoma_stupor
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-14-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "unknown")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    semicoma_stupor = (relation-node semicoma_stupor :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    shock --> semicoma_stupor
    resp_compromise --> semicoma_stupor
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-15-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "13")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "expectant")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    sepsis --> shock
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-15-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "13")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    sepsis --> shock
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-16-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "6")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-16-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "6")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    myocardial_infarction = (relation-node myocardial_infarction :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-17-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    sepsis --> shock
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-17-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "high")
    heartrate = (percept-node heartrate :value "high")
    resprate = (percept-node resprate :value "high")
    o2sat = (percept-node o2sat :value "low")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "unknown")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    sepsis = (relation-node sepsis :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "T")
    shock = (percept-node shock :value "T")
    sepsis --> shock
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-18-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "2")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "immediate")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-18-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "low")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "high")
    dbp = (percept-node dbp :value "high")
    pain = (percept-node pain :value "2")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    intracerebral_hemorrhage = (relation-node intracerebral_hemorrhage :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    intracerebral_hemorrhage --> shock
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-19-ground-truth ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    acuity = (percept-node acuity :value "delayed")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    death = (percept-node death :value "Nil")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    temperature --> acuity
    heartrate --> acuity
    resprate --> acuity
    o2sat --> acuity
    sbp --> acuity
    dbp --> acuity
    pain --> acuity
    chiefcomplaint --> acuity
    shock --> death
    resp_compromise --> death
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defun mimic-log-test-case-19-cue ()
  (compile-program nil
    temperature = (percept-node temperature :value "normal")
    heartrate = (percept-node heartrate :value "normal")
    resprate = (percept-node resprate :value "normal")
    o2sat = (percept-node o2sat :value "normal")
    sbp = (percept-node sbp :value "normal")
    dbp = (percept-node dbp :value "normal")
    pain = (percept-node pain :value "0")
    chiefcomplaint = (percept-node chiefcomplaint :value "None")
    malignant_neoplasm_of_bronchus = (relation-node malignant_neoplasm_of_bronchus :value "T" :kb-concept-id "INJURY")
    resp_compromise = (percept-node resp_compromise :value "Nil")
    shock = (percept-node shock :value "T")
    shock --> heartrate
    resp_compromise --> resprate
    resp_compromise --> o2sat
    shock --> sbp
    shock --> dbp
    ))

(defparameter *mimic-log-test-cases*
  (list
   (list :id 0 :cue #'mimic-log-test-case-0-cue :ground-truth #'mimic-log-test-case-0-ground-truth :death "Nil" :acuity "immediate")
   (list :id 1 :cue #'mimic-log-test-case-1-cue :ground-truth #'mimic-log-test-case-1-ground-truth :death "Nil" :acuity "immediate")
   (list :id 2 :cue #'mimic-log-test-case-2-cue :ground-truth #'mimic-log-test-case-2-ground-truth :death "Nil" :acuity "delayed")
   (list :id 3 :cue #'mimic-log-test-case-3-cue :ground-truth #'mimic-log-test-case-3-ground-truth :death "Nil" :acuity "immediate")
   (list :id 4 :cue #'mimic-log-test-case-4-cue :ground-truth #'mimic-log-test-case-4-ground-truth :death "Nil" :acuity "immediate")
   (list :id 5 :cue #'mimic-log-test-case-5-cue :ground-truth #'mimic-log-test-case-5-ground-truth :death "Nil" :acuity "immediate")
   (list :id 6 :cue #'mimic-log-test-case-6-cue :ground-truth #'mimic-log-test-case-6-ground-truth :death "Nil" :acuity "expectant")
   (list :id 7 :cue #'mimic-log-test-case-7-cue :ground-truth #'mimic-log-test-case-7-ground-truth :death "Nil" :acuity "immediate")
   (list :id 8 :cue #'mimic-log-test-case-8-cue :ground-truth #'mimic-log-test-case-8-ground-truth :death "Nil" :acuity "delayed")
   (list :id 9 :cue #'mimic-log-test-case-9-cue :ground-truth #'mimic-log-test-case-9-ground-truth :death "Nil" :acuity "expectant")
   (list :id 10 :cue #'mimic-log-test-case-10-cue :ground-truth #'mimic-log-test-case-10-ground-truth :death "Nil" :acuity "expectant")
   (list :id 11 :cue #'mimic-log-test-case-11-cue :ground-truth #'mimic-log-test-case-11-ground-truth :death "Nil" :acuity "immediate")
   (list :id 12 :cue #'mimic-log-test-case-12-cue :ground-truth #'mimic-log-test-case-12-ground-truth :death "Nil" :acuity "unknown")
   (list :id 13 :cue #'mimic-log-test-case-13-cue :ground-truth #'mimic-log-test-case-13-ground-truth :death "Nil" :acuity "delayed")
   (list :id 14 :cue #'mimic-log-test-case-14-cue :ground-truth #'mimic-log-test-case-14-ground-truth :death "Nil" :acuity "expectant")
   (list :id 15 :cue #'mimic-log-test-case-15-cue :ground-truth #'mimic-log-test-case-15-ground-truth :death "Nil" :acuity "expectant")
   (list :id 16 :cue #'mimic-log-test-case-16-cue :ground-truth #'mimic-log-test-case-16-ground-truth :death "Nil" :acuity "immediate")
   (list :id 17 :cue #'mimic-log-test-case-17-cue :ground-truth #'mimic-log-test-case-17-ground-truth :death "Nil" :acuity "immediate")
   (list :id 18 :cue #'mimic-log-test-case-18-cue :ground-truth #'mimic-log-test-case-18-ground-truth :death "Nil" :acuity "immediate")
   (list :id 19 :cue #'mimic-log-test-case-19-cue :ground-truth #'mimic-log-test-case-19-ground-truth :death "Nil" :acuity "delayed")
   ))

(defun mimic-log-remember (&key (soft-likelihoods t) score-only singleton-only)
  (let ((cue (mimic-log-test-cue)))
    (remember eltm* cue '+ 1 t
              :type "observation"
              :soft-likelihoods soft-likelihoods
              :score-only score-only
              :singleton-only singleton-only)))

(defun print-mimic-log-singleton-posteriors (posterior-marginals &key (stream t))
  (format stream "~&~%Posterior singleton CPDs (~D):~%" (length posterior-marginals))
  (loop
    for cpd in posterior-marginals
    for i from 0
    when (rule-based-cpd-singleton-p cpd)
      do (progn
           (format stream "~&~%[~D] ~A~%" i (rule-based-cpd-dependent-id cpd))
           (print-cpd cpd :stream stream))))

(defparameter *mimic-log-trained-eltm-file*
  "examples/Common Lisp/mimic-log-trained-eltm.txt")

(defun mimic-log-save-and-reload-eltm (file)
  (with-mimic-log-timing ("save trained ELTM")
    (save-eltm-to-file eltm* :path "" :filename file))
  (with-mimic-log-timing ("reload trained ELTM")
    (load-eltm-from-file file))
  eltm*)

(defun run-mimic-log-inference-benchmark (&key limit (bic-p t) trace-filter-ops-p (print-posterior-singletons-p t))
  (reset-mimic-log-benchmark)
  (let (bns inserted result)
    (setq bns (with-mimic-log-timing ("compile static log programs")
                (mimic-log-training-bns :limit limit)))
    (setq inserted (with-mimic-log-timing ("insert training observations")
                     (insert-mimic-log-training-bns bns :bic-p bic-p)))
    (format t "~&Inserted ~D observations.~%" inserted)
    (with-mimic-log-timing ("write ELTM PDF")
      (eltm-to-pdf))
    (when trace-filter-ops-p
      (trace operate-filter-rules normalize-rule-probabilities factor-filter factor-operation update-cpd-rules bn-score new-retrieve-episode))
    (unwind-protect
         (setq result (multiple-value-list
                       (with-mimic-log-timing ("remember held-out-style cue")
                         (mimic-log-remember))))
      (when trace-filter-ops-p
        (untrace operate-filter-rules normalize-rule-probabilities factor-filter factor-operation update-cpd-rules bn-score new-retrieve-episode)))
    (format t "~&Inference episode id: ~A~%"
            (if (and (fifth result) (car (fifth result)))
                (episode-id (car (fifth result)))
                nil))
    (when print-posterior-singletons-p
      (print-mimic-log-singleton-posteriors (second result)))
    result))

(defun mimic-log-value-label (value)
  (cond ((null value) "Nil")
        ((symbolp value) (symbol-name value))
        (t (write-to-string value))))

(defun mimic-log-cpd-value-labels (cpd dep-values)
  (let* ((dep-id (rule-based-cpd-dependent-id cpd))
         (idx (gethash dep-id (rule-based-cpd-identifiers cpd)))
         (vvbm (and idx (gethash idx (rule-based-cpd-var-value-block-map cpd)))))
    (loop
      for (value . block) in vvbm
      when (member (cdr value) dep-values)
        collect (car value))))

(defun mimic-log-prediction-from-cpd (cpd)
  (loop
    with dep-id = (rule-based-cpd-dependent-id cpd)
    with best-rule = nil
    with best-prob = nil
    for rule being the elements of (rule-based-cpd-rules cpd)
    for prob = (rule-probability rule)
    when (or (null best-prob) (> prob best-prob))
      do (setq best-rule rule
               best-prob prob)
    finally
       (let* ((dep-values (and best-rule
                               (or (gethash dep-id (rule-conditions best-rule))
                                   (let ((dep-idx (gethash dep-id (rule-based-cpd-identifiers cpd))))
                                     (and dep-idx
                                          (copy-list (gethash dep-idx (rule-based-cpd-var-values cpd))))))))
              (labels (and dep-values (mimic-log-cpd-value-labels cpd dep-values))))
         (return (list :dependent-id dep-id
                       :probability best-prob
                       :values labels
                       :assigned-p (not (null labels)))))))

(defun mimic-log-find-posterior-cpd (posterior-marginals var-name)
  (find var-name posterior-marginals
        :key #'rule-based-cpd-dependent-var
        :test #'string-equal))

(defun mimic-log-positive-death-p (value)
  (and value (string-equal value "T")))

(defun mimic-log-prediction-correct-p (prediction truth)
  (member truth (getf prediction :values) :test #'string-equal))

(defun mimic-log-safe-ratio (num den)
  (if (zerop den) nil (/ (float num) den)))

(defun mimic-log-print-binary-metrics (tp fp tn fn &key (stream t))
  (let ((total (+ tp fp tn fn)))
    (format stream "~&Death confusion matrix: TP=~D FP=~D TN=~D FN=~D~%" tp fp tn fn)
    (format stream "Death accuracy: ~A~%" (mimic-log-safe-ratio (+ tp tn) total))
    (format stream "Death precision: ~A~%" (mimic-log-safe-ratio tp (+ tp fp)))
    (format stream "Death recall: ~A~%" (mimic-log-safe-ratio tp (+ tp fn)))))

(defun mimic-log-run-one-test-case (case &key (print-case-p t))
  (let* ((cue (funcall (getf case :cue)))
         (result (multiple-value-list
                  (remember eltm* cue '+ 1 t :type "observation")))
         (posterior-marginals (second result))
         (episode-ref (fifth result))
	 (prior-death-cpd (mimic-log-find-posterior-cpd (car (episode-observation (car episode-ref))) "death"))
         (prior-acuity-cpd (mimic-log-find-posterior-cpd (car (episode-observation (car episode-ref))) "acuity"))
         (death-cpd (mimic-log-find-posterior-cpd posterior-marginals "death"))
         (acuity-cpd (mimic-log-find-posterior-cpd posterior-marginals "acuity"))
         (death-pred (and death-cpd (mimic-log-prediction-from-cpd death-cpd)))
         (acuity-pred (and acuity-cpd (mimic-log-prediction-from-cpd acuity-cpd)))
         (death-truth (getf case :death))
         (acuity-truth (getf case :acuity)))
    (when print-case-p
      (format t "~&~%Test case ~D~%" (getf case :id))
      (format t "Episode id: ~A~%" (if (and episode-ref (car episode-ref)) (episode-id (car episode-ref)) nil))
      (format t "~%Prior death marginal belief:")
      (print-cpd (factor-operation prior-death-cpd (list (rule-based-cpd-dependent-id prior-death-cpd))
				   (loop
				     for ident being the hash-keys of (rule-based-cpd-identifiers prior-death-cpd)
				     when (not (equal ident (rule-based-cpd-dependent-id prior-death-cpd)))
				       collect ident)
				   '+))
      (format t "~%Inferred death belief:")
      (print-cpd death-cpd)
      (format t "~%Prior acuity marginal belief:")
      (print-cpd (factor-operation prior-acuity-cpd (list (rule-based-cpd-dependent-id prior-acuity-cpd))
				   (loop
				     for ident being the hash-keys of (rule-based-cpd-identifiers prior-acuity-cpd)
				     when (not (equal ident (rule-based-cpd-dependent-id prior-acuity-cpd)))
				       collect ident)
				   '+))
      (format t "~%Inferred acuity belief:")
      (print-cpd acuity-cpd)
      (format t "Death truth=~A prediction=~S correct=~A~%"
              death-truth death-pred (and death-pred (mimic-log-prediction-correct-p death-pred death-truth)))
      (format t "Acuity truth=~A prediction=~S correct=~A~%"
              acuity-truth acuity-pred (and acuity-pred (mimic-log-prediction-correct-p acuity-pred acuity-truth))))
    (list :id (getf case :id)
          :episode-id (if (and episode-ref (car episode-ref)) (episode-id (car episode-ref)) nil)
          :death-truth death-truth
          :death-prediction death-pred
          :death-correct-p (and death-pred (mimic-log-prediction-correct-p death-pred death-truth))
          :acuity-truth acuity-truth
          :acuity-prediction acuity-pred
          :acuity-correct-p (and acuity-pred (mimic-log-prediction-correct-p acuity-pred acuity-truth)))))

(defun run-mimic-log-predictive-performance-benchmark (&key training-limit (bic-p t)
                                                       (insert-training-bns-p t)
                                                       (eltm-cache-file *mimic-log-trained-eltm-file*))
  (reset-mimic-log-benchmark)
  (let (bns inserted results (tp 0) (fp 0) (tn 0) (fn 0) (death-correct 0) (acuity-correct 0) (total 0))
    (cond (insert-training-bns-p
           (setq bns (with-mimic-log-timing ("compile static log training programs")
                       (mimic-log-training-bns :limit training-limit)))
           (setq inserted (with-mimic-log-timing ("insert training observations")
                            (insert-mimic-log-training-bns bns :bic-p bic-p)))
           (format t "~&Inserted ~D training observations.~%" inserted)
           (mimic-log-save-and-reload-eltm eltm-cache-file))
          (t
           (with-mimic-log-timing ("load trained ELTM")
             (load-eltm-from-file eltm-cache-file))))
    (setq results
          (with-mimic-log-timing ("run 20 ground-truth retrieval cues")
            (loop for case in *mimic-log-test-cases*
                  collect (mimic-log-run-one-test-case case))))
    (loop for result in results
          for truth-pos = (mimic-log-positive-death-p (getf result :death-truth))
          for pred-values = (getf (getf result :death-prediction) :values)
          for pred-pos = (member "T" pred-values :test #'string-equal)
          do (cond ((and truth-pos pred-pos) (incf tp))
                   ((and (not truth-pos) pred-pos) (incf fp))
                   ((and (not truth-pos) (not pred-pos)) (incf tn))
                   (t (incf fn)))
             (when (getf result :death-correct-p) (incf death-correct))
             (when (getf result :acuity-correct-p) (incf acuity-correct))
             (incf total))
    (format t "~&~%Predictive performance over ~D test cases~%" total)
    (format t "Death max-probability accuracy: ~A~%" (mimic-log-safe-ratio death-correct total))
    (mimic-log-print-binary-metrics tp fp tn fn)
    (format t "Acuity max-probability accuracy: ~A~%" (mimic-log-safe-ratio acuity-correct total))
    results))
