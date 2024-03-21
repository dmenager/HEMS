(in-package :hems)

#| Draw a random sample from an episode in the event memory. Returns an association list of variables and their values |# 

;; episode = episode in event memory
;; output-percepts-p = optional flag determining whether or not only percept nodes will be output or all node types
(defun sample-observation (episode &key output-percepts-p (key "OBSERVATION"))
  (let (bn)
    (cond ((equal key "OBSERVATION") ;;(> (array-dimension (car (episode-observation episode)) 0) 0)
	   (setq bn (episode-observation episode)))
	  ((equal key "STATE") ;;(> (array-dimension (car (episode-state episode)) 0) 0)
	   (setq bn (episode-state episode)))
	  ;; enable this branch when we can do hierarchical segmentation/sampling
	  (nil (> (array-dimension (car (episode-state-transitions episode)) 0) 0)
	   (setq bn (episode-state-transitions episode)))
	  (t
	   (error "uh oh")))
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
  (sample-observation episode :output-percepts-p output-percepts-p :key "STATE"))

#| Draw a random sample from an episode in the event memory. Returns an association list of variables and their values |# 

;; episode = episode in event memory
;; output-percepts-p = optional flag determining whether or not only percept nodes will be output or all node types
(defun sample-action (episode &key output-percepts-p)
  (sample-observation episode :output-percepts-p output-percepts-p :key "ACTION"))

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
	 (when nil
	   (format t "~%~%distribution")
	   (print-hash-entry i cpd)
	   (format t "~%sample rule:")
	   (print-cpd-rule sample-rule)
	   (format t "~%roll: ~d~%compatible-rules:" dice)
	   (map nil #'print-cpd-rule compatible-rules)
	   (break))

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
  (sample episode :hidden-state-p hiddenstatep :output-percepts-p outputperceptsp))

#| Condition the sampling function on observations made in the environment. |#

;; eltm = episodic-long-term-memory
;; evidence-bn = the bayesian network that represents observations made in the environment
;; episode-type = episode field in which observation was made, be it, "observation", "state", or "state-transitions"
(defun conditional-sample (eltm evidence-bn episode-type &key hidden-state-p output-percepts-p)
  (let (new-episode new-bn)
  (multiple-value-bind (recollection eme)
      (remember eltm evidence-bn '+ 1 t)
    (loop
      for cpd in recollection
      when (not (rule-based-cpd-singleton-p cpd)) collect cpd into bn
      and count cpd into len
      finally 
	 (setq new-bn (cons (make-array len :initial-contents bn) (make-hash-table))))
    (setq new-episode (copy-ep (car eme)))
    (cond ((string-equal episode-type "observation")
	   (setf (episode-observation new-episode) new-bn))
	  ((string-equal episode-type "state")
	   (setf (episode-state new-episode) new-bn))
	  ((string-equal episode-type "state-transitions")
	   (setf (episode-state-transitions new-episode) new-bn))
	  (t
	   (error "Unsupported episode type: ~A. Expected \"OBSERVATION\", \"STATE\", or \"STATE-TRANSITIONS\"" episode-type))))
  (sample new-episode :hidden-state-p hidden-state-p :output-percepts-p output-percepts-p)))

(defun py-conditional-sample (eltm evidence-bn episode-type &key hiddenstatep outputperceptsp)
  (conditional-sample eltm evidence-bn episode-type :hidden-state-p hiddenstatep :output-percepts-p outputperceptsp))

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

------------
(ql:quickload :hems)
(hems::run-execution-trace "/home/david/Code/HARLEM/ep_data_10/ppo_CliffWalking-v0_data.csv")
(hems::sample (car (hems:get-eltm)) :hidden-state-p t :output-percepts-p t)
(conditional-sample (get-eltm) (compile-program nil
c1 = (percept-node action :value "2")) "state-transitions" :hidden-state-p t :output-percepts-p t)
|#
