(in-package :hems)

#| Measure of how concentrated the probability mass is across categorical values |#

;; cpd = conditional probability distribution
(defun compute-cpd-concentration (cpd)
  (loop
    with weights
    for rule being the elements of (rule-based-cpd-rules cpd)
    when (> (rule-probability rule) 0)
      minimize (rule-probability rule) into min-p
      and collect (rule-probability rule) into probs
    finally
       (loop
	 for p in probs
	 collect (/ p min-p) into w
	 finally (setq weights w))
       
       (return (/ (apply #'+ (mapcar #'(lambda (x p)
					 (* x p))
				     weights probs))
		  (apply #'+ weights)))))
