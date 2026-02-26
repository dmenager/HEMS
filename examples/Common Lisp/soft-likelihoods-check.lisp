(ql:quickload :hems)
(in-package :hems)

(defun cue-1 ()
  (let (q)
    (setq q (hems:compile-program nil
	      c = (percept-node c :value "3")))
    (hems:remember eltm* q '+ 1 t :type "observation")))

(defun cue-2 ()
  (let (q)
    (setq q (hems:compile-program nil
	      a = (percept-node a :value "NA")))
    (hems:remember eltm* q '+ 1 t :type "observation")))

(defun cue-3 ()
  (let (q)
    (setq q (hems:compile-program nil
	      a = (percept-node a :value "1")))
    (hems:remember eltm* q '+ 1 t :type "observation")))

(defun load-eltm()
  (let (bn)
    (setq bn (hems:compile-program nil
				   a = (percept-node a :value "a")
				   b = (percept-node b :value "b")
				   c = (percept-node c :value "c")
				   a --> c
				   b --> c))
    (new-push-to-ep-buffer :observation bn :bic-p t :insertp t :temporal-p nil :hidden-state-p nil)
    ()))
