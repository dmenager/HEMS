(ql:quickload :hems)
(in-package :hems)

(defun example ()
(let (observations)
    (setq observations
(list (compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "T" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "T" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "T" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
	c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
	nil
  c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
(compile-program
    nil
  c1 = (percept-node zeus_angry :value "NIL" :kb-concept-id "CNPT-1"))
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)))


;;----------------------------

;;# CPDs
;;P(zeus_angry=1) = 0.1000
