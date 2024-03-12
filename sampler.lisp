(in-package :hems)

#| Draw a random sample from an episode in the event memory. Returns an association list of variables and their values |# 

;; episode = episode in event memory
;; output-percepts-p = optional flag determining whether or not only percept nodes will be output or all node types
(defun sample-observation (episode &key output-percepts-p)
  (let (bn)
    (cond ((episode-temporal-p episode)
	   (setq bn (episode-state-transitions episode)))
	  (t
	   (setq bn (episode-observation episode))))
    (loop
      with dice
      with sample-rule = (make-rule :conditions (make-hash-table :test #'equal))
      with sample-list 
      with compatible-rules
      for cpd being the elements of (car bn)
      do
	 (setq dice (random 100))
	 (setq compatible-rules (sort (get-compatible-rules cpd cpd sample-rule :find-all t) #'< :key #'rule-probability))
	 (loop
	   named looper
	   with low-end = 0 and high-end and value
	   for rule in compatible-rules
	   do
	      (setq high-end (+ low-end (* (rule-probability rule) 100)))
	      (when (and (<= low-end dice)
			 (> high-end dice))
		(setq value (gethash (rule-based-cpd-dependent-id cpd)
				     (rule-conditions rule)))
		(setf (gethash (rule-based-cpd-dependent-id cpd)
			       (rule-conditions sample-rule))
		      value)
		(when (or (not output-percepts-p)
			  (and output-percepts-p
			       (equal "PERCEPT" (gethash 0 (rule-based-cpd-types cpd)))))
		  (setq sample-list (cons (cons (rule-based-cpd-dependent-id cpd)
						(caar (nth value (gethash 0 (rule-based-cpd-var-value-block-map cpd)))))
					  sample-list)))
		(return-from looper nil))
	      (setq low-end high-end))
      finally
	 (return (reverse sample-list)))))

#| Draw a random sample from an episode in the event memory. Returns an association list of variables and their values |# 

;; episode = episode in event memory
;; output-percepts-p = optional flag determining whether or not only percept nodes will be output or all node types
(defun sample-state (episode &key output-percepts-p)
  (sample-observation episode :output-percepts-p output-percepts-p))

#| Draw a random sample from an episode in the event memory. Returns an association list of variables and their values |# 

;; episode = episode in event memory
;; output-percepts-p = optional flag determining whether or not only percept nodes will be output or all node types
(defun sample-action (episode &key output-percepts-p)
  (sample-observation episode :output-percepts-p output-percepts-p))

#| Draw a random sample from an episode in the event memory. Returns a list of association lists of variables and their values |# 

;; episode = episode in event memory
;; output-percepts-p = optional flag determining whether or not only percept nodes will be output or all node types
(defun sample-state-transitions (episode hidden-state-p &key output-percepts-p)
  (let (bn)
    (cond ((episode-temporal-p episode)
	   (setq bn (episode-state-transitions episode)))
	  (t
	   (error "~%Given episode is not temporal. Check temporal-p flag")))
    (loop
      with marker
      with dice
      with ref
      with sample-rule = (make-rule :conditions (make-hash-table :test #'equal)) 
      with compatible-rules
      with data and trajectory
      for cpd being the elements of (car bn)
      for i from 0
      do
	 (setq dice (random 100))
	 (setq compatible-rules (sort (get-compatible-rules cpd cpd sample-rule :find-all t) #'< :key #'rule-probability))
	 (if hidden-state-p
	     (setq marker (mod i 3))
	     (setq marker (mod i 2)))
	 (loop
	   named looper
	   with low-end = 0 and high-end and value
	   for rule in compatible-rules
	   do
	      (setq high-end (+ low-end (* (rule-probability rule) 100)))
	      (when (and (<= low-end dice)
			 (> high-end dice))
		(setq value (gethash (rule-based-cpd-dependent-id cpd)
				     (rule-conditions rule)))
		(setf (gethash (rule-based-cpd-dependent-id cpd)
			       (rule-conditions sample-rule))
		      value)
		(setq ref (caar (nth value (gethash 0 (rule-based-cpd-var-value-block-map cpd)))))
		(cond ((equal "NA" ref)
		       (setq data (cons nil data)))
		      (t
		       (cond (hidden-state-p
			      (cond ((= marker 0)
				     (setq data (cons (sample-state (car (gethash ref (episode-backlinks episode))) :output-percepts-p output-percepts-p) data)))
				    ((= marker 1)
				     (setq data (cons (sample-observation (car (gethash ref (episode-backlinks episode))) :output-percepts-p output-percepts-p) data)))
				    ((= marker 2)
				     (setq data (cons ref data))
				     (setq trajectory (cons (reverse data) trajectory))
				     (setq data nil))))
			     (t
			      (cond ((= marker 0)
				     (setq data (cons (sample-observation (car (gethash ref (episode-backlinks episode))) :output-percepts-p output-percepts-p) data)))
				    ((= marker 1)
				     (setq data (cons ref data))
				     (setq trajectory (cons (reverse data) trajectory))
				     (setq data nil)))))))
		(return-from looper nil))
	      (setq low-end high-end))
      finally
	 (return (reverse trajectory)))))

#| Draw a random sample from an episode in the event memory. For observations, returns an association list of variables and their values. For temporal models, it returns a trajectory where each element of the trajectory is a tuple containing state (if present), observation, and action. |# 

;; episode = episode from event memory
;; hidden-state-p = optional flag if the temporal model has hidden states
;; output-percepts-p = optional flag to output only the samples from the sensors rather than inferred variables not directly observed
(defun sample (episode &key hidden-state-p output-percepts-p)
  (if (episode-temporal-p episode)
      (sample-state-transitions episode hidden-state-p :output-percepts-p output-percepts-p)
      (sample-observation episode :output-percepts-p output-percepts-p)))

(defun py-sample (episode &key hiddenstatep outputperceptsp)
  (sample episode :hidden-state-p hiddenstatep :output-percepts-p output-percepts-p))
#| TESTS 
(ql:quickload :hems)

(hems::new-push-to-ep-buffer 
	  :observation (hems:compile-program nil 
			 c1 = (percept-node a :value "10")
			 c2 = (percept-node b :value "3"))
	  :temporal-p nil
	  :insertp t)

(hems::new-push-to-ep-buffer 
	  :observation (hems:compile-program nil 
			 c1 = (percept-node a :value "10")
			 c2 = (percept-node b :value "3"))
	  :state (hems:compile-program nil
		   c1 = (percept-node s1 :value "a"))
	  :action-name "act1"
	  :temporal-p t
	  :hidden-state-p t
	  :insertp t)
(hems::sample (car (hems:get-eltm)) :hidden-state-p t)

|#
