(in-package :hems)

#| Get the mean of a list. Returns float. |#

;; x = list
(defun mean (x)
  (/ (reduce #'+ x)
     (length x)))

#| Get standard deviation of a list. Returns float |#

;; x = list
(defun stdev (x)
  (let ((avg (mean x)))
  (sqrt (/ (apply '+ (mapcar #'(lambda (xi) (expt (- xi avg) 2) ) x))
           (length x)))))

#| Measure of how concentrated the probability mass is across categorical values. Returns float |#

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

#| Compute spread of the probability mass across the network nodes. Returns multiple values: (float, float) |#

;; episode = episode from ELTM
(defun compute-network-concentration (episode)
  (loop
    with bn = (if (episode-temporal-p episode)
		  (episode-state-transitions episode)
		  (episode-observation episode))
    with hash = (make-hash-table :test #'equal)
    for cpd being the elements of (car bn)
    collect (compute-cpd-concentration cpd) into concentrations
    finally
       (return (values (mean concentrations) (stdev concentration)))))
