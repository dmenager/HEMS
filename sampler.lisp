(in-package :hems)

#| Condition the retrieved model on observations made in the environment.
   Returns episode. |#

;; eltm = episodic-long-term-memory
;; evidence-bn = the bayesian network that represents observations made in the environment
;; episode-type = episode field in which observation was made, be it, "observation", "state", or "state-transitions"
(defun condition-model (eltm evidence-bn episode-type &key (backlinks (make-hash-table :test #'equal)) (keep-singletons nil) (soft-likelihoods nil))
  (let (new-episode new-bn)
    (when t ;;nil
      (format t "~%evidence-bn:")
      (print-bn evidence-bn))
    (multiple-value-bind (recollection eme sol)
	(remember eltm evidence-bn '+ 1 t :backlinks backlinks :type episode-type :soft-likelihoods soft-likelihoods)
      ;; verify in (remember) if the single observation node matches to the state transition models.
      (when nil (string-equal episode-type "state-transitions")
	;;(format t "~%Posterior network:~%~S" recollection)
	(format t "~%eme network:")
	(cond ((string-equal episode-type "state-transitions")
	       (print-bn (episode-state-transitions (car eme))))
	      ((string-equal episode-type "observation")
	       (print-bn (episode-observation (car eme))))
	      ((string-equal episode-type "state")
	       (print-bn (episode-state (car eme)))))
	    (format t "~%sol:~%~S" sol)
	    ;;(break)
	    )
      (if keep-singletons
	  (setq new-bn (cons (make-array (length recollection) :initial-contents recollection) (make-hash-table :test #'equal)))
	  (loop
	    for cpd in recollection
	    when (not (rule-based-cpd-singleton-p cpd)) collect cpd into bn
	    and count cpd into len
	    finally 
	       (setq new-bn (cons (make-array len :initial-contents bn) (make-hash-table :test #'equal)))))
      (setq new-episode (copy-ep (car eme)))
      (cond ((string-equal episode-type "observation")
	     (setf (episode-observation new-episode) new-bn))
	    ((string-equal episode-type "state")
	     (setf (episode-state new-episode) new-bn))
	    ((string-equal episode-type "state-transitions")
	     (setf (episode-state-transitions new-episode) new-bn))
	    (t
	     (error "Unsupported episode type: ~A. Expected \"OBSERVATION\", \"STATE\", or \"STATE-TRANSITIONS\"" episode-type)))
      (values new-episode sol))))

#| Draw a random sample from an episode in the event memory. Returns an association list of variables and their values |# 

;; episode = episode in event memory
;; eltm = episodic long-term memory
;; output-percepts-p = optional flag determining whether or not only percept nodes will be output or all node types
(defun sample-observation (eltm &key output-percepts-p (key "OBSERVATION") evidence-bn)
  (let (bn)
    (when nil t
      (format t "~%episode id: ~S~%key: ~S~% (equal ~S \"OBSERVATION\"): ~s~%evidence-bn-p? ~S" (episode-id (car eltm)) key key (equal key "OBSERVATION") (not (null evidence-bn))))
    (cond ((equal key "OBSERVATION") ;;(> (array-dimension (car (episode-observation episode)) 0) 0)
	   (cond (evidence-bn
		  (when nil t
		    (format t "~%evidence:~%~S" evidence-bn))
		  (setq bn (episode-observation
			    (condition-model
			     eltm
			     evidence-bn
			     key)))
		  (when nil t
		    (format t "~%conditioned observation:~%~S" bn)))
		 (t
		  (setq bn (episode-observation (car eltm))))))
	  ((equal key "STATE") ;;(> (array-dimension (car (episode-state episode)) 0) 0)
	   (setq bn (episode-state (car eltm))))
	  ;; enable this branch when we can do hierarchical segmentation/sampling
	  (nil (> (array-dimension (car (episode-state-transitions (car eltm))) 0) 0)
	       (setq bn (episode-state-transitions (car eltm))))
	  (t
	   (error "uh oh")))
    (when nil nil
      (format t "~%~%  episode id: ~S" (episode-id (car eltm))))
    (loop
      with dice
      with sample-rule = (make-rule :conditions (make-hash-table :test #'equal))
      with sample-list 
      with compatible-rules
      for cpd being the elements of (car bn)
      for i from 0
      do
	 (setq dice (random 100))
	 (setq compatible-rules (sort (get-compatible-rules cpd cpd sample-rule :find-all t) #'< :key #'rule-probability))
	 (when nil t
	   (format t "~%~%   state/observation distribution")
	   (print-hash-entry i cpd)
	   (format t "~%   sample rule:")
	   (print-cpd-rule sample-rule)
	   (format t "~%  roll: ~d~%   compatible-rules:" dice)
	   (map nil #'print-cpd-rule compatible-rules)
	   ;;(break)
	   )
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
		(when nil t
		  (format t "~%  sample: ~S" (cons (rule-based-cpd-dependent-id cpd) value)))
		(when (or (not output-percepts-p)
			  (and output-percepts-p
			       (equal "PERCEPT" (gethash 0 (rule-based-cpd-types cpd)))))
		  (setq sample-list (cons (cons (rule-based-cpd-dependent-id cpd)
						(caar (nth value (gethash 0 (rule-based-cpd-var-value-block-map cpd)))))
					  sample-list)))
		(return-from looper nil))
	      (setq low-end high-end))
      finally
	 (when nil t
	   (format t "~%conditional samples:~%~S" (reverse sample-list))
	   (break))
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

#| Draw a random sample from an episode in the event memory.
   Returns a list of bindings of variables and their values |# 

;; episode = episode in event memory
;; output-percepts-p = optional flag determining whether or not only percept nodes will be output or all node types
(defun sample-state-transitions (episode hidden-state-p &key output-percepts-p evidence-bn)
  (let (bn)
    (cond ((episode-temporal-p episode)
	   #|
	   (when nil t
	     (format t "~%sampling temporal episode: ~A" (episode-id episode))
	     
	     (loop
		for backlink being the hash-keys of (episode-backlinks episode)
		using (hash-value branch)
		collect `(:label ,backlink :episode-id ,(episode-id (car branch))) into backlinks
		finally
		  (format t "~%backlinks:~%~S" backlinks))
	     (break))
	   |#
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
	 (when nil t
	   (format t "~%~%distribution")
	   (print-hash-entry i cpd)
	   (format t "~%sample rule:")
	   (print-cpd-rule sample-rule)
	   (format t "~%roll: ~d~%compatible-rules:" dice)
	   (map nil #'print-cpd-rule compatible-rules)
	   (format t "~%i: ~d~%marker: ~d" i marker)
	   ;;(break)
	   )
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
		(when nil
		  (format t "~%ref: ~S" (episode-id (car (gethash ref (episode-backlinks episode))))))
		(cond ((equal "NA" ref)
		       (setq data (cons nil data)))
		      (t
		       (cond (hidden-state-p
			      (cond ((= marker 0)
				     (setq data (cons (sample-state (gethash ref (episode-backlinks episode)) :output-percepts-p output-percepts-p) data)))
				    ((= marker 1)
				     (when nil t
				       (format t "~%here"))
				     (setq data (cons (sample-observation (gethash ref (episode-backlinks episode)) :output-percepts-p output-percepts-p :evidence-bn evidence-bn) data)))
				    ((= marker 2)
				     (setq data (cons ref data))
				     (setq trajectory (cons (reverse data) trajectory))
				     (setq data nil))))
			     (t
			      (cond ((= marker 0)
				     (setq data (cons (sample-observation (gethash ref (episode-backlinks episode)) :output-percepts-p output-percepts-p :evidence-bn evidence-bn) data)))
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
;; evidence-bn = evidence for the observation model
(defun sample (episode &key hidden-state-p output-percepts-p evidence-bn)
  (let (res)
    (setq res
	  (if (episode-temporal-p episode)
	      (sample-state-transitions episode hidden-state-p :output-percepts-p output-percepts-p :evidence-bn evidence-bn)
	      (sample-observation episode :output-percepts-p output-percepts-p :evidence-bn evidence-bn)))
    (loop
      for slice in res
      for i from 0
      do
      (format t "~%~%time step: ~d" i)
      (format t "~%    state:~%    ~A~%    observation:~%    ~A~%    action:~%    ~A" (first slice) (second slice) (third slice)))))

(defun py-sample (episode &key hiddenstatep outputperceptsp)
  (sample episode :hidden-state-p hiddenstatep :output-percepts-p outputperceptsp))

#| Condition the sampling function on observations made in the environment. |#

;; eltm = episodic-long-term-memory
;; evidence-bn = the bayesian network that represents observations made in the environment
;; episode-type = episode field in which observation was made, be it, "observation", "state", or "state-transitions"
(defun conditional-sample (eltm evidence-bn episode-type &key hidden-state-p output-percepts-p (backlinks (make-hash-table :test #'equal)) obs-evidence-bn)
  (when nil t
    (format t "~%temporal evidence:~%~S" evidence-bn)
    (break))
  (sample
   (condition-model eltm
		    evidence-bn
		    episode-type
		    :backlinks backlinks)
   :hidden-state-p hidden-state-p
   :output-percepts-p output-percepts-p
   :evidence-bn obs-evidence-bn))

(defun py-conditional-sample (eltm evidence-bn episode-type &key hiddenstatep outputperceptsp (backlinks (make-hash-table :test #'equal)))
  (conditional-sample eltm evidence-bn episode-type :hidden-state-p hiddenstatep :output-percepts-p outputperceptsp :backlinks backlinks))

(defun filter-sample (sample &key (test #'equal))
  (labels ((find-in-tree (item)
	     (labels ((find-in-tree-aux (tree rest)
		        (cond ((funcall test item tree)                                  
			       (return-from find-in-tree (if rest t tree)))
			      ((consp tree)
			       (find-in-tree-aux  (car tree) (cdr tree))
			       (find-in-tree-aux  (cdr tree) (cdr tree))))))
	       (find-in-tree-aux sample (rest sample)))))
    (cond ((null sample)
	   nil)
	  ((find-in-tree "NA")
	   nil)
	  ((find-in-tree nil)
	   nil)
	  (t
	   sample))))

(defun sample-to-file (n-samples fname episode hidden-state-p output-percepts-p)
  (let ((*print-pretty* nil)
	(*print-circle* nil))
    (with-open-file (stream fname :direction :output
				  :if-does-not-exist :create
				  :if-exists :supersede)
      (format stream "observation, action~%")
      (loop
	with action-counts and action and act-count
	with i = 0
	with s
	while (< i n-samples)
	do
	   (setq s (sample episode :hidden-state-p hidden-state-p :output-percepts-p output-percepts-p))
	when (filter-sample s)
	  do
	     (setq action (car (last (car s))))
	     (setq act-count (assoc action action-counts :test #'string-equal))
	     (if act-count
		 (setf (cdr act-count) (+ (cdr act-count) 1))
		 (setq action-counts (cons (cons action 1) action-counts)))
	     ;;(format stream "~S,~%" s)
	     (loop
	       with len = (length (second (car s)))
	       with arr = (make-array len)
	       with lst
	       with obs-var
	       for obs in (second (car s))
	       do
		  (setq obs-var (second (split-sequence:split-sequence #\_ (car obs))))
		  (cond ((search "1" obs-var)
			 (setf (aref arr (- len 1)) (parse-integer (cdr obs))))
			((search "2" obs-var)
			 (setf (aref arr (- len 2)) (parse-integer (cdr obs))))
			((search "3" obs-var)
			 (setf (aref arr (- len 3)) (parse-integer (cdr obs))))
			((search "4" obs-var)
			 (setf (aref arr (- len 4)) (parse-integer (cdr obs))))
			((search "5" obs-var)
			 (setf (aref arr (- len 5)) (parse-integer (cdr obs)))))
	       finally
		  (format stream "~{~d~}, ~s~%" (coerce arr 'list) (if (string-equal "terminal" action) action (parse-integer action))))
	     (setq i (+ i 1))
	finally
	   (return action-counts)))))

(defun balance-action-samples (action-counts training-file hidden-state-p output-percepts-p)
  (let ((max-act -1))
    (loop
      for (act . count) in action-counts
      when (> count max-act)
	do
	   (setq max-act count))
    (with-open-file (stream (concatenate 'string "HEMS-samples-" training-file)
			    :direction :output
			    :if-does-not-exist :error
			    :if-exists :append)
      (loop
	with diff and action
	for (act . count) in action-counts
	do
	   (setq diff (- max-act count))
	   (setf count (+ count diff))
	   (loop
	     with s
	     while (> diff 0)
	     do
		(format t "~%generating ~d more samples for action ~S" diff act)
		(setq s (conditional-sample eltm* (eval `(compile-program nil
							   c1 = (action-node action :value ,act)))
					    "state-transitions"
					    :hidden-state-p hidden-state-p
					    :output-percepts-p output-percepts-p))
	     when (filter-sample s)
	       do
		  (setq action (car (last (car s))))
		  (let ((*print-pretty* nil)
			(*print-circle* nil))
		    ;;(format stream "~S,~%" s)
		    (loop
		      with len = (length (second (car s)))
		      with arr = (make-array len)
		      with lst
		      with obs-var
		      for obs in (second (car s))
		      do
			 (setq obs-var (second (split-sequence:split-sequence #\_ (car obs))))
			 (cond ((search "1" obs-var)
				(setf (aref arr (- len 1)) (parse-integer (cdr obs))))
			       ((search "2" obs-var)
				(setf (aref arr (- len 2)) (parse-integer (cdr obs))))
			       ((search "3" obs-var)
				(setf (aref arr (- len 3)) (parse-integer (cdr obs))))
			       ((search "4" obs-var)
				(setf (aref arr (- len 4)) (parse-integer (cdr obs))))
			       ((search "5" obs-var) 
				(setf (aref arr (- len 5)) (parse-integer (cdr obs)))))
		      finally
			 (format stream "~{~d~}, ~S~%" (coerce arr 'list) (if (string-equal "terminal" action) action (parse-integer action)))
		      ))
		  (setq diff (- diff 1)))))))

(defun generate-hems-data (n-samples hidden-state-p output-percepts-p)
  (let ((file-path "~/Code/HARLEM/ep_data_10/")
	(training-files (list ;;"a2c_CliffWalking-v0_data.csv"
			      ;;"dqn_Taxi-v3_data.csv"
			      ;;"a2c_FrozenLake-v1_data.csv"
			      ;;"ppo_CliffWalking-v0_data.csv"
			      ;;"a2c_Taxi-v3_data.csv"
			      "ppo_FrozenLake-v1_data.csv"
			      ;;"ars_FrozenLake-v1_data.csv"
			      ;;"ppo_Taxi-v3_data.csv"
			      ;;"ars_Taxi-v3_data.csv"
			      ;;"qrdqn_CliffWalking-v0_data.csv"
			      ;;"dqn_CliffWalking-v0_data.csv"
			      ;;"qrdqn_FrozenLake-v1_data.csv"
			      ;;"dqn_FrozenLake-v1_data.csv"
			      ;;"qrdqn_Taxi-v3_data.csv"
			      )))
    (loop
      with action-counts
      for training-file in training-files
      do
	 (setq eltm* nil)
	 (format t "~%~%Loading ~A" training-file)
	 (run-execution-trace (merge-pathnames file-path training-file) :hidden-state-p hidden-state-p)
	 (eltm-to-pdf)
	 (format t "~%Generating random samples from the model.")
	 (setq action-counts (sample-to-file n-samples (concatenate 'string "HEMS-samples-" training-file) (car eltm*) hidden-state-p output-percepts-p))
	 (format t "~%Balancing action samples.")
	 (balance-action-samples action-counts training-file hidden-state-p output-percepts-p)
	 ;;(break)
      )))

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
(setf *print-circle* nil)
(hems::run-execution-trace "/home/david/Code/HARLEM/ep_data_1/ppo_CliffWalking-v0_data.csv")
(hems::run-execution-trace "/home/david/Code/HARLEM/ep_data_10/ppo_FrozenLake-v1_data.csv")

;; Get random samples of the learned policy according to the data distribution
(hems:sample (car (hems:get-eltm)) :hidden-state-p t :output-percepts-p t)


;; Condition our random samples because interesting behavior we want to learn might be rare
(hems:conditional-sample (hems:get-eltm) (hems:compile-program nil
c1 = (action-node action :value "2")) "state-transitions" :hidden-state-p t :output-percepts-p t)
--------------------------------------
(hems::generate-hems-data 2000 t t)

(let (obs-evidence evidence-slices slice)
  (setq obs-evidence (hems:compile-program nil
					   c1 = (relation-node number_1 :value "314")
					   c2 = (percept-node three_hundred_fourteen_1 :value "314")
					   c1 -> c2))
  (setq slice (make-hash-table :test #'equal))
  (setf (gethash "OBSERVATION" slice) obs-evidence)
  (setq evidence-slices (cons slice evidence-slices))
  (multiple-value-bind (evidence-bn backlinks evidence-bns)
      (hems:make-temporal-episode-retrieval-cue
       (hems:get-eltm)
       evidence-slices)
    (hems:conditional-sample (hems:get-eltm) evidence-bn "state-transitions" :hidden-state-p t :output-percepts-p t :backlinks backlinks :obs-evidence-bn obs-evidence)))
|#

