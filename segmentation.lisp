(in-package :hems)

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
                (cond(decomp-model
                      (when t
                        (format t "~%model succeeded for all decompositions."))
                      (return (values decomp-model obs-window reject-lists calls-to-retrieve)))
                     (t
                      (when t
                        (format t "~%Failure because state transition model failed."))
                      (return (values nil obs-window reject-lists calls-to-retrieve)))))))))

(defun get-model ()
  ;; make a retrieval cue from the latest observation in state window
  ;; return the retrieved model
  (let (cue eme bn st-ref-hash)
    (setq bn (compile-program c0 = (state-node state :value "T")))
    (setf (gethash (rule-based-cpd-dependent-id (aref 0 (car bn)))
		   st-ref-hash)
	  (list (copy-observation (car (last obs-window)))))
    (setq cue (make-episode :state-transitions bn
                            :decompositions st-ref-hash
			    :temporal-p t
                            :id-ref-map (make-hash-table :test #'equal)
                            :num-decompositions 0
                            :lvl 1))
    (setq eme
	  (retrieve-episode eltm cue (car (last reject-lists)) :bic-p bic-p :lvl-func lvl-func :forbidden-types forbidden-types :check-decomps check-decomps :check-abstraction-ptrs check-abstraction-ptrs :check-index-case check-index-case))
    (make-model :ep eme :model-parent nil)))

(defun event-boundary-p  (model obs-window)
  (when (null model)
    (setq model (get-model)))
  (multiple-value-bind (new-model remaining-states rejects calls-to-retrieve)
      (good-fit-to-observations? model obs-window nil)
    (declare (ignore rejects calls-to-retrieve))
    new-model))
