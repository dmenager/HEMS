(in-package :hems)

#| Get the likelihood that model predicts state |#

;; episode = element in episodic long-term memory
;; state = state as a graph
(defun model-predict (episode state &key (bic-p t) &aux forbidden-types)
  (setq forbidden-types '("GOAL" "INTENTION"))
  (multiple-value-bind (matches no-matches weighted-cost bindings cost)
      (maximum-common-subgraph state (car (episode-states episode)) :bic-p bic-p :cost-of-nil (episode-count episode) :forbidden-types forbidden-types)
    ;;(subgraph-greedy-monomorphism state (car (episode-states episode)) :bic-p bic-p :cost-of-nil (episode-count episode))
    (declare (ignore weighted-cost cost no-matches))
    (loop
      with x-copy and y
      with l and total = 1 and r and threshold = 1
      for (pattern . base) being the elements of matches
      do
         (setq x-copy (subst-cpd (aref (car state) pattern) (when base (aref (caar (episode-states episode)) base)) bindings))
         (setq y (if base (aref (caar (episode-states episode)) base)))
         (multiple-value-bind (dif forbidden-likelihood)
             (hash-difference (if y (cpd-identifiers y)) (cpd-identifiers x-copy) y forbidden-types)
           (declare (ignore dif))
           (setq l (local-likelihood x-copy y forbidden-likelihood))
           (when nil (< l 1)
                 (format t "~%state cpd:~%~S~%model cpd:~%~S~%likelihood: ~d~%forbidden-likelihood: ~A" x-copy y l forbidden-likelihood)))
         (setq total (* total l))
         (setq r (if y (/ 1 (aref (cpd-cardinalities y) 0)) 0))
         (setq threshold (* threshold r))
      finally
         (return (values total threshold)))))

#| Determine if episode is a good predictor for state sequence. Returns current ground-level model. |#

;; model = model for predicting state
;; obs-window = list of states as a graph
;; reject-lists = list of lists of episode ids to reject and not expand/return
(defun good-fit-to-observations? (model obs-window reject-lists &key (bic-p t) (check-disjunction nil) (failure-p nil) (auto-pass nil) (decompose-last-p nil))
  (when t
    (format t "~%Assessing model fit on state sequence of length ~d." (length obs-window)))
  (let (episode decompositions observed-decomps)
    (setq episode (getf model :model))
    (when episode
      (setq decompositions (episode-decompositions episode))
      (loop
        with dcmps-p = (not (equalp (make-array 0) (getf model :inferred-decompositions)))
        for i from 0 to (if check-disjunction (+ (getf model :cur-step) 1) (getf model :cur-step))
        when dcmps-p
          collect (aref (getf model :inferred-decompositions) i) into dcmps
        finally
           (setq observed-decomps (make-array (length dcmps) :initial-contents dcmps))))
    (when t
      (format t "~%check-disjunction: ~A~%cur-step: ~d~%inferred decompositions:~%~S~%observed decompositions: ~S" check-disjunction (if check-disjunction (+ (getf model :cur-step) 1) (getf model :cur-step)) (getf model :inferred-decompositions) observed-decomps))
    (cond ((null episode)
           (values nil obs-window reject-lists 0))
          (failure-p
           (when t
             (format t "~%Model doesn't satisfy evaluation requirements. Fail"))
           (values nil obs-window reject-lists 0))
          ((equalp (make-empty-graph) decompositions)
           (when t
             (format t "~%model is ground-level model"))
           (loop
             for (state . states) on obs-window
             for i from 0
             do
                (multiple-value-bind (likelihood threshold)
                    (model-predict episode state :bic-p bic-p)
                  (cond ((pass-prediction likelihood threshold auto-pass)
			 (when t
                           (format t "~%Good fit for state with likelihood ~d and threshold ~d. Incrementing scope." likelihood threshold))
			 (setq obs-window states)
			 (loop
                            with mod = model
                            while mod
                            do
                              (setf (getf mod :scope) (+ (getf mod :scope) 1))
                              (setq mod (getf mod :model-parent))))
			(t
			 (cond ((= i 0)
				(when t
                                  (format t "~%failure with likelihood ~d and threshold ~d." likelihood threshold))
				(return-from good-fit-to-observations? (values nil obs-window reject-lists 0)))
                               (t
				(cond ((getf model :model-parent)
                                       (when t
					 (format t "~%Detected event boundary, moving to next step."))
                                       (return-from good-fit-to-observations? (values model obs-window reject-lists 0)))
                                      (t
                                       (when t
					 (format t "~%Failure. Model has no parent and cannot explain remaining states."))
                                       (return-from good-fit-to-observations? (values nil obs-window reject-lists 0)))))))))
              finally
                (return (values model obs-window reject-lists 0))))
          (t
           (when t
             (format t "~%decomposing higher-level model ~A." (episode-id episode)))
           (loop
             with calls-to-retrieve = 0
             with decomp-refs = (infer-decompositions episode (car obs-window) observed-decomps (+ (getf model :cur-step) 1))
             for i from (if decompose-last-p (- (array-dimension decomp-refs 0) 1) (+ (getf model :cur-step) 1)) to (- (array-dimension decomp-refs 0) 1)
             with branch-ref and cue and decomp-model
             while obs-window do
	       (setf (getf model :cur-step) (+ (getf model :cur-step) 1))
	       (setf (getf model :inferred-decompositions) decomp-refs)
	       (setq cue (make-episode
                          :states (list (copy-observation (car obs-window)))
                          :decompositions (make-empty-graph)
                          :id-ref-map (make-hash-table :test #'equal)
                          :num-decompositions 0
                          :lvl (episode-lvl (getf model :model))
                          :abstraction-ptrs (list (list (getf model :model)))))
	       (setq branch-ref (gethash (aref decomp-refs i) (episode-id-ref-map episode)))
	       (multiple-value-bind (new-model new-reject-lists remaining-states new-calls)
                   (define-decomposition obs-window branch-ref cue reject-lists bic-p
		     :lvl-func #'<
		     :check-abstraction-ptrs t
		     :parent model
		     :auto-pass auto-pass
		     :decompose-last-p decompose-last-p)
                 (setq decomp-model new-model)
                 (setq obs-window remaining-states)
                 (setq reject-lists new-reject-lists)
                 (setq calls-to-retrieve (+ calls-to-retrieve new-calls))
                 (cond (decomp-model
                        (when t
                          (format t "~%Success. Advanced model step to ~d. Advanced model scope to ~d." (getf model :cur-step) (getf model :scope))))
		       (t
                        (when t
                          (format t "~%Decomposition failed. Skipping to next step.")))))
             finally
                (cond (decomp-model
                       (when t
                         (format t "~%model succeeded for all decompositions."))
                       (return (values decomp-model obs-window reject-lists calls-to-retrieve)))
                      (t
                       (when t
                         (format t "~%Failure because state transition model failed."))
                       (return (values nil obs-window reject-lists calls-to-retrieve)))))))))

(defun get-model (obs-window eltm reject-list)
  (loop
    with cue and eme and ref and st-ref-hash
    with cur-st and prev-st and st-bn and st-ref-hash
    for obs in (gethash 0 (getf episode-buffer* :obs))
    do
       (setq cue (make-episode :observation (mapcar #'copy-observation obs)
			       :id-ref-map (make-hash-table :test #'equal)
			       :num-decompositions 0
			       :count 1
			       :lvl 1))
       (setq ref (new-retrieve-episode eltmi cue reject-list
				   :bic-p bic-p
				   :lvl-func lvl-func
				   :forbidden-types forbidden-types
				   :check-decomps check-decomps
				   :check-abstraction-ptrs check-abstraction-ptrs
				   :check-index-case check-index-case))
       (setq cur-st (gensym "STATE-"))
    collect ref into refs
    nconcing `(,cur-st = (state-node state :value "T")) into state-transitions
    when prev-st
      nconcing `(,prev-st -> ,cur-st) into state-transitions
    do
       (setq prev-st cur-st)
    finally
       (setq st-bn (eval `(compile-program ,@state-transitions)))
       (setq st-ref-hash (make-hash-table :test #'equal))
       (loop
	 for s being the elements of (car st-bn)
	 for r in refs
	 do
	    (setf (gethash (rule-based-cpd-dependent-id s) st-ref-hash) r))
       ;; make temporal episode from state transitions
       (setq cue (make-episode :state-transitions st-bn
			       :decompositions st-ref-hash
			       :temporal-p t
			       :id-ref-map (make-hash-table :test #'equal)
			       :num-decompositions 0
			       :count 1
			       :lvl 2))
       (setq eme (retrieve-episode eltm cue reject-list
				   :bic-p bic-p
				   :lvl-func lvl-func
				   :forbidden-types forbidden-types
				   :check-decomps check-decomps
				   :check-abstraction-ptrs check-abstraction-ptrs
				   :check-index-case check-index-case))
       (return (make-model :ep eme :model-parent nil))))

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
