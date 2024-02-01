(in-package :hems)

#| Get the likelihood that model predicts state |#

;; episode = element in episodic long-term memory
;; state = state as a graph
(defun model-predict (episode state &key (bic-p t) &aux forbidden-types)
  (setq forbidden-types '("GOAL" "INTENTION"))
  (multiple-value-bind (matches no-matches cost bindings)
      (new-maximum-common-subgraph state (episode-observation episode) nil nil :bic-p bic-p :cost-of-nil (episode-count episode) :forbidden-types forbidden-types)
    ;;(subgraph-greedy-monomorphism state (car (episode-states episode)) :bic-p bic-p :cost-of-nil (episode-count episode))
    (declare (ignore cost no-matches))
    (loop
      with x-copy and y
      with l and total = 1 and r and threshold = 1
      for (pattern . base) being the elements of matches
      do
         (setq x-copy (subst-cpd (aref (car state) pattern) (when base (aref (caar (episode-states episode)) base)) bindings))
         (setq y (if base (aref (episode-observation episode) base)))
         (multiple-value-bind (dif forbidden-likelihood)
             (hash-difference (if y (rule-based-cpd-identifiers y)) (rule-based-cpd-identifiers x-copy) y forbidden-types)
           (declare (ignore dif))
           (setq l (local-likelihood x-copy y forbidden-likelihood))
           (when nil (< l 1)
                 (format t "~%state cpd:~%~S~%model cpd:~%~S~%likelihood: ~d~%forbidden-likelihood: ~A" x-copy y l forbidden-likelihood)))
         (setq total (* total l))
         (setq r (if y (/ 1 (aref (cpd-cardinalities y) 0)) 0))
         (setq threshold (* threshold r))
      finally
         (return (values total threshold)))))

#| Return true if likelihood of observation under the model is greater than random chance |#

;; likelihood = likelihood that model generates observation
;; threshold = probability that the model generates state at random
;; auto-pass = flag for passing prediction no matter what
(defun pass-prediction (likelihood threshold auto-pass)
  (cond (auto-pass t)
        (t
         (> likelihood threshold))))

#| Generate a ground network for the current time step from which to do inference |#

;; cpds = array of conditional probability distributions
;; time-step is the index of the current state
;; type = string enumeration on the type of node in the temporal network, be it "STATE", "OBSERVATION", or "PERCEPT"
(defun get-ground-network (cpds time-step type)
  (loop
    with modifier = (cond ((equal "STATE" type) 0)
			  ((equal "OBSERVATION" type) 1)
			  ((equal "PERCEPT" type) 2))
    with current-cpd = (aref cpds (+ time-step modifier))
    with new-cpds = (list current-cpd)
    for cpd-id being the hash-keys of (rule-based-cpd-identifiers current-cpd)
      using (hash-value pos)
    when (> pos 0)
      do
	 (loop
	   with simple-cpd
	   for cpd being the elements of cpds
	   with dep-var and vars and types-hash and id and dep-id and cid and qvars and vvbm and nvvbm and sva and svna and lower-vvbm and lower-nvvbm and var-values and cards and steps and rules and lvl
	   when (equal (rule-based-cpd-dependent-id cpd) cpd-id)
	     do
		(setq dep-id (rule-based-cpd-dependent-id cpd))
		(setq id (make-hash-table :test #'equal))
		(setf (gethash dep-id id) 0)
		(setq dep-var (rule-based-cpd-dependent-var cpd))
		(setq vars (make-hash-table))
		(setf (gethash 0 vars) dep-var)
		(setq types-hash (make-hash-table))
		(setf (gethash 0 types-hash) (gethash 0 (rule-based-cpd-types cpd)))
		(setq cid (make-hash-table))
		(setf (gethash 0 cid) (gethash 0 (rule-based-cpd-concept-ids cpd)))
		(setq qvars (make-hash-table))
		(setf (gethash 0 qvars) (gethash 0 (rule-based-cpd-qualified-vars cpd)))
		(setq vvbm (make-hash-table))
		(setf (gethash 0 vvbm) (gethash 0 (rule-based-cpd-var-value-block-map cpd)))
		(setq nvvbm (make-hash-table))
		(setf (gethash 0 nvvbm) (gethash 0 (rule-based-cpd-negated-vvbms cpd)))
		(setq sva (make-hash-table))
		(setf (gethash 0 sva) (gethash 0 (rule-based-cpd-set-valued-attributes cpd)))
		(setq svna (make-hash-table))
		(setf (gethash 0 svna) (gethash 0 (rule-based-cpd-set-valued-negated-attributes cpd)))
		(setq lower-vvbm (make-hash-table))
		(setf (gethash 0 lower-vvbm) (gethash 0 (rule-based-cpd-lower-approx-negated-vvbms cpd)))
		(setq lower-nvvbm (make-hash-table))
		(setf (gethash 0 lower-nvvbm) (gethash 0 (rule-based-cpd-lower-approx-negated-vvbms cpd)))
		(setq var-values (make-hash-table))
		(setf (gethash 0 var-values) (gethash 0 (rule-based-cpd-var-values cpd)))
		(setq cards (make-array 1 :initial-contents (list (aref (rule-based-cpd-cardinalities cpd) 0)) :fill-pointer t))
		(setq steps (make-array 1 :initial-element 1 :fill-pointer t))
		(setq rules (initialize-rule-potentials cpd (/1 (aref cards 0))))
		(setq lvl (rule-based-cpd-lvl cpd))
		(setq simple-cpd (make-rule-based-cpd :dependent-id dep-id
						     :identifiers id
						     :dependent-var dep-var
						     :vars vars
						     :types types-hash
						     :concept-ids cid
						     :qualified-vars qvars
						     :var-value-block-map vvbm
						     :negated-vvbms nvvbm
						     :set-valued-attributes sva
						     :set-valued-negated-attributes svna
						     :lower-approx-var-value-block-map lower-vvbm
						     :lower-approx-negated-vvbms lower-nvvbm
						     :characteristic-sets (make-hash-table)
						     :characteristic-sets-values (make-hash-table)
						     :var-values var-values
						     :cardinalities cards
						     :step-sizes steps
						     :rules rules
						     :singleton-p nil
						     :lvl lvl))
		(setq new-cpds (cons simple-cpd new-cpds))
	   finally
	   (return (make-array (length new-cpds) :initial-contents new-cpds)))))

#| Determine if episode is a good predictor for state sequence. Returns current ground-level model. |#

;; model = model for predicting state
;; models = list of models for predicting state
;; obs-window = list of states as a graph
;; reject-lists = list of lists of episode ids to reject and not expand/return
(defun good-fit-to-observations? (model models obs-window &key (bic-p t) (auto-pass nil))
  (labels ((predict-observation (model observation)
	     (multiple-value-bind (likelihood threshold)
                 (model-predict (getf model :episode) state :bic-p bic-p)
               (cond ((pass-prediction likelihood threshold auto-pass)
                      (when t
                        (format t "~%Good fit for state with likelihood ~d and threshold ~d. Incrementing scope." likelihood threshold))
		      model)
                     (t
		      (when t
                        (format t "~%failure with likelihood ~d and threshold ~d." likelihood threshold))
		      nil))))
	   (track-observation (models observation)
	     ;; infer distribution over the state given previous action, and previous state
             ;; for each possible observation that has non-zero posterior probability, check to see if it matches obs
             ;;   if yes, then set evidence for that branch to 1
             ;;   if no, then set evidence for that branch to 0
             ;; if model fails to predict observation, fail
             ;; infer distribution over the action given observation
             ;; for each possible action that has non-zeor posterior probability, check to see if it matches act
             ;;   if yes, then set evidence for that action to 1
             ;;   if no, then set evidence for that action to 0
             ;; if model fails to predict action, fail
             ;; if next state
             ;;   infer distribution over the state given observation and action
             ;;   repeat
             ;; else
             ;;   return success
	     (let (obs act ground-network recollection)
	       (setq obs (car observation))
	       (setq act (cdr observation))
	       (setq ground-network (get-ground-network (car (episode-state-transitions (getf (car models) :episode))) (getf (car models) :cur-step) "STATE"))
               (setq recollection (loopy-belief-propagation ground-network evidence #'+ 1))
               (loop
		 for cpd in recollection
		 when (and (rule-based-cpd-singleton-p cpd)
			   (equal "STATE" (gethash 0 (rule-based-cpd-types cpd))))
		   do
                      (setf (gethash (rule-based-dependent-id cpd) evidence) nil)
                      (loop
			with cond = (rule-based-dependent-id cpd)
			with val
			for rule in (rule-based-cpd-rules cpd)
			do
			   (setq val (gethash cond (rule-conditions rule)))
			   (setf (gethash cond evidence)
				 (cons (cons val (rule-probability rule))
                                       (gethash (rule-based-cpd-dependent-id cpd) (getf (car models) :inferred-decompositions))))))
               (setq ground-network (get-ground-network (car (episode-state-transitions episode)) time-step "OBSERVATION"))
               (when ground-network
		 ;; infer distribution over the observation given state
		 (setq recollection (loopy-belief-propagation ground-network evidence #'+ 1))
		 (loop
		   for cpd in recollection
		   when (and (rule-based-cpd-singleton-p cpd)
                             (equal "OBSERVATION" (gethash 0 (rule-based-cpd-types cpd))))
                     do
			(setf (gethash (rule-based-dependent-id cpd) evidence) nil)
			(loop
			  with cond = (rule-based-dependent-id cpd)
			  with val and model-ref and vvbm = (gethash 0 (rule-based-cpd-var-value-block-map cpd)) and failp = t
			  for rule in (rule-based-cpd-rules cpd)
			  do
                             (cond ((> (rule-probability rule) 0)
                                    (setq val (caar (rassoc (gethash cond (rule-conditions rule)) vvbm :key #'cdar)))
                                    (setq model-ref (gethash val (episode-backlinks episode)))
                                    (cond ((good-fit-to-observations? (cons (make-model :ep (car model-ref)
											:model-parent model)
									    (rest models))
                                                                      (list observation))
					   (setq failp nil)
					   (setf (gethash cond evidence)
						 (cons (cons val 1)
                                                       (gethash (rule-based-cpd-dependent-id cpd) (getf (car models) :inferred-decompositions)))))
					  (t
					   (setf (gethash cond evidence)
						 (cons (cons val 0)
                                                       (gethash (rule-based-cpd-dependent-id cpd) (getf (car models) :inferred-decompositions)))))))
				   (t
                                    (setf (gethash cond evidence)
					  (cons (cons val 0)
						(gethash (rule-based-cpd-dependent-id cpd) (getf (car models) :inferred-decompositions))))))
                             (if failp
				 (return-from good-fit-to-observations? (values (rest models) observation))))))
               (setq ground-network (get-ground-network (car (episode-state-transitions episode)) time-step "PERCEPT"))
               (when ground-network
		 ;; infer distribution over the action given observation
		 (setq recollection (loopy-belief-propagation ground-network evidence #'+ 1))
		 (loop
		   for cpd in recollection
		   when (and (rule-based-cpd-singleton-p cpd)
                             (equal "PERCEPT" (gethash 0 (rule-based-cpd-types cpd))))
                     do
			(setf (gethash (rule-based-dependent-id cpd) evidence) nil)
			(loop
			  with new-models = (rest models)
			  with cond = (rule-based-dependent-id cpd)
			  with val and vvbm = (gethash 0 (rule-based-cpd-var-value-block-map cpd)) and failp = t
			  for rule in (rule-based-cpd-rules cpd)
			  do
                             (cond ((> (rule-probability rule) 0)
                                    (setq val (caar (rassoc (gethash cond (rule-conditions rule)) vvbm :key #'cdar)))
                                    (cond ((equal act val)
					   (setq failp nil)
					   (setf (gethash cond evidence)
						 (cons (cons val 1)
                                                       (gethash (rule-based-cpd-dependent-id cpd) (getf (car models) :inferred-decompositions)))))
					  (t
					   (setf (gethash cond evidence)
						 (cons (cons val 0)
                                                       (gethash (rule-based-cpd-dependent-id cpd) (getf model :inferred-decompositions)))))))
				   (t
                                    (setf (gethash cond evidence)
					  (cons (cons val 0)
						(gethash (rule-based-cpd-dependent-id cpd) (getf model :inferred-decompositions))))))
			  finally
			     (setq models new-models)))
		 (if failp
                     (return-from good-fit-to-observations? (values (rest models) observation)))
		 (loop
		   with new-models = (rest models)
		   with act-idx = (+ (getf (car models) :cur-step) 2)
		   for idx being the hash-keys of (gethash act-idx (cdr (episode-state-transitions (getf (car models) :model))))
		   do
		      (setq new-models (cons (make-model :ep (getf (car models) :model)
							 :cur-step idx
							 :model-parent (getf (car models) :model-parent)
							 :evidence (copy-hash-table (getf (car models) :evidence)))
					     new-models))
		   finally
		      (setq models new-models)
		      (values models observation))))))
    (let* ((model (car models))
	   (episode (getf model :model)))
      (when t
	(format t "~%Assessing model fit on state sequence of length ~d." (length obs-window)))
      (cond ((null obs-window)
	     (values models obs-window))
	    ((null models)
	     (values nil obs-window))
	    ((null episode)
             (values nil obs-window))
            ((not (episode-temporal-p (getf (car models) :episode)))
             (when t
               (format t "~%model is ground-level model"))
	     (cond ((predict-observation (getf (car models) :episode) (caar obs-window))
		    (values models obs-window))
		   (t
		    (values nil obs-window))))
            (t
	     (good-fit-to-observations (track-observation models (car obs-window)) (rest obs-window)))))))

(defun get-model (obs-window eltm reject-list)
  (loop
    with cue and eme and ref and st-ref-hash
    with obs-st and cur-act and st-bn and id-ref-hash = (make-hash-table :test #'equal)
    for (obs . act-name) in (gethash 0 (getf episode-buffer* :obs))
    do
       (setq cue (make-episode :observation (copy-observation obs)
			       :count 1
			       :lvl 1))
       (setq ref (new-retrieve-episode eltm cue reject-list
				   :bic-p bic-p
				   :lvl-func lvl-func
				   :forbidden-types forbidden-types
				   :check-decomps check-decomps
				   :check-abstraction-ptrs check-abstraction-ptrs
				   :check-index-case check-index-case))
       (setf (gethash (episode-id (car ref)) id-ref-hash) ref)
       (setq obs-st (gensym "OBS-"))
       (setq cur-act (gensym "ACT-"))
    nconcing `(,obs-st = (observation-node observation :value ,(episode-id (car ref)))) into state-transitions
    nconcing `(,cur-act = (percept-node action :value ,act-name)) into state-transitions
    nconcing `(,obs-st -> ,cur-act) into state-transitions
    do
       (setq prev-st cur-st)
       (setq prev-act cur-act)
    finally
       (setq st-bn (eval `(compile-program ,@state-transitions)))
       ;; make temporal episode from state transitions
       (setq cue (make-episode :state-transitions st-bn
			       :backlinks id-ref-hash
			       :temporal-p t
			       :count 1
			       :lvl 2))
       (setq eme (new-retrieve-episode eltm cue reject-list
				   :bic-p bic-p
				   :lvl-func lvl-func
				   :forbidden-types forbidden-types
				   :check-decomps check-decomps
				   :check-abstraction-ptrs check-abstraction-ptrs
				   :check-index-case check-index-case))
       (return (make-model :ep (car eme) :model-parent nil))))

(defun event-boundary-p  (model obs-window eltm reject-list)
  (loop
    do
       (when (null (getf model :model))
	 (setq model (get-model obs-window eltm reject-list)))
       (multiple-value-bind (new-model remaining-states new-rejects calls-to-retrieve)
	   (good-fit-to-observations? model obs-window reject-list)
	 (declare (ignore calls-to-retrieve))
	 (setq reject-list new-rejects)
	 (setq model new-model))
    while (and (null (getf model :model))
	       (not (member (episode-id (car eltm)) reject-list :test #'equal))))
  model)

(defun test ()
  (labels ((integer-string-p (string)
	     (ignore-errors (parse-integer string))))
    (ql:quickload :split-sequence)
    (let (features data)
      (setq data (uiop:read-file-lines "~/Code/Data/HARLEM/test_data.csv"))
      (setq features (split-sequence:split-sequence #\, (car data)))
      (setq data (rest data))
      (loop
	with processed and variables
	for line in data
	do
	   (setq processed (split-sequence:split-sequence #\, line))
	   (setq variables (mapcan #'(lambda (string)
				       (when (integer-string-p string)
					 (list string)))
				   (split-sequence:split-sequence #\Space (third processed))))
	   (loop
	     with st
	     for var in variables
	     for i from 0
	     nconcing `(,(gensym "C") = (percept-node ,(intern (format nil "VAR~d" i) :value ,var))) into program
	     finally
		(setq st (eval `(compile-program ,@program)))
		(new-push-to-ep-bufffer (:state st :action-name action)))))))
