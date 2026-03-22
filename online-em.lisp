(in-package :hems)

;;;; Online EM for Bayesian networks represented as rule-based CPDs.
;;;;
;;;; Expected input:
;;;;   BN          = cons whose CAR is an array of RULE-BASED-CPD objects.
;;;;                 The CDR is preserved but otherwise ignored.
;;;;   LATENT-VARS = list of RULE-BASED-CPD-DEPENDENT-ID strings to be returned
;;;;                 as posterior singleton factors in the output network.
;;;;   DATASET     = sequence processed incrementally. Each element can be:
;;;;                   1) an evidence hash table suitable for LOOPY-BELIEF-PROPAGATION,
;;;;                   2) a list/vector/array of singleton RULE-BASED-CPD evidence factors,
;;;;                   3) a BN-like cons whose CAR is such a list/array of evidence factors.
;;;;
;;;; Return value:
;;;;   A cons whose CAR is an array of RULE-BASED-CPD objects. All CPDs are updated
;;;;   online via EM. In addition, the CPDs whose dependent-id appears in LATENT-VARS
;;;;   are replaced by their posterior singleton distributions from the final datum.
;;;;   The CDR of the input BN is preserved unchanged.
;;;;
;;;; Notes:
;;;;   * This implementation reuses COMPILE-BN-PRIORS, LOOPY-BELIEF-PROPAGATION,
;;;;     MAKE-OBSERVATIONS, COPY-BN, COPY-RULE-BASED-CPD, UPDATE-CPD-RULES, and
;;;;     NORMALIZE-RULE-PROBABILITIES from the existing code base.
;;;;   * The online sufficient-statistic update is:
;;;;        S_t = (1 - eta_t) S_{t-1} + eta_t * ESS(x,u | o_t, theta_{t-1})
;;;;     followed by a local M-step for each CPD.

(defparameter *online-em-default-step-size*
  #'(lambda (n)
      (/ 1.0d0 (sqrt (max 1 n)))))

(defun online-em--hash-keys-sorted (hash)
  (sort (loop for k being the hash-keys of hash collect k)
        #'string< :key #'prin1-to-string))

(defun online-em--normalize-idx-list (idxs)
  (sort (copy-list (if (listp idxs) idxs (list idxs))) #'<))

(defun online-em--rule-key (rule)
  "Canonical key for a rule: sorted ((identifier . sorted-idx-list) ...)."
  (loop for ident in (online-em--hash-keys-sorted (rule-conditions rule))
        collect (cons ident
                      (online-em--normalize-idx-list
                       (copy-list (gethash ident (rule-conditions rule)))))))

(defun online-em--parent-key (cpd rule)
  "Canonical parent-context key for RULE in CPD."
  (let ((dep-id (rule-based-cpd-dependent-id cpd)))
    (remove dep-id
            (online-em--rule-key rule)
            :test #'equal
            :key #'car)))

(defun online-em--rule-map (cpd)
  "Map canonical rule key -> rule object for CPD."
  (let ((map (make-hash-table :test #'equal)))
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          do (setf (gethash (online-em--rule-key rule) map) rule))
    map))

(defun online-em--cpd-by-id (bn dep-id)
  (car (member dep-id (car bn)
               :key #'rule-based-cpd-dependent-id
               :test #'equal)))

(defun online-em--posterior-map (posterior-factors)
  (let ((map (make-hash-table :test #'equal)))
    (dolist (factor posterior-factors map)
      (setf (gethash (rule-based-cpd-dependent-id factor) map) factor))))

(defun online-em--singleton-map (posterior-singletons)
  (let ((map (make-hash-table :test #'equal)))
    (dolist (factor posterior-singletons map)
      (setf (gethash (rule-based-cpd-dependent-id factor) map) factor))))

(defun online-em--copy-bn-factors-only (bn)
  (cons (map 'vector #'copy-rule-based-cpd (car (copy-bn bn)))
        (cdr bn)))

(defun online-em--initialize-statistics (bn equivalent-sample-size)
  "Create a mirrored BN of sufficient statistics, storing counts in RULE-COUNT."
  (let* ((bn-copy (online-em--copy-bn-factors-only bn))
         (alpha (float equivalent-sample-size 1.0d0)))
    (loop for cpd being the elements of (car bn-copy) do
      (loop for rule being the elements of (rule-based-cpd-rules cpd) do
        (setf (rule-count rule)
              (* alpha (float (rule-probability rule) 1.0d0)))))
    bn-copy))

(defun online-em--apply-decay (stats-cpd eta)
  (loop for rule being the elements of (rule-based-cpd-rules stats-cpd) do
    (setf (rule-count rule)
          (* (- 1.0d0 eta)
             (float (rule-count rule) 1.0d0)))))

(defun online-em--accumulate-posterior (stats-cpd posterior-cpd eta)
  "Blend the posterior ESS for POSTERIOR-CPD into STATS-CPD."
  (let ((posterior-map (online-em--rule-map posterior-cpd)))
    (loop for rule being the elements of (rule-based-cpd-rules stats-cpd)
          for key = (online-em--rule-key rule)
          for post-rule = (gethash key posterior-map)
          when post-rule do
            (incf (rule-count rule)
                  (* eta (float (rule-probability post-rule) 1.0d0))))))

(defun online-em--m-step-cpd (cpd stats-cpd &key (min-prob 1.0d-12))
  "Closed-form local M-step for a rule-based CPD.
Groups rules by parent-context and normalizes counts within each group."
  (let ((totals (make-hash-table :test #'equal))
        (stats-map (online-em--rule-map stats-cpd)))
    ;; First pass: accumulate totals by parent context.
    (loop for rule being the elements of (rule-based-cpd-rules stats-cpd)
          for parent-key = (online-em--parent-key stats-cpd rule) do
            (incf (gethash parent-key totals 0.0d0)
                  (float (rule-count rule) 1.0d0)))
    ;; Second pass: write updated probabilities into CPD.
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          for key = (online-em--rule-key rule)
          for stats-rule = (gethash key stats-map)
          for parent-key = (online-em--parent-key cpd rule)
          for denom = (gethash parent-key totals 0.0d0) do
            (setf (rule-probability rule)
                  (cond ((and stats-rule (> denom 0.0d0))
                         (max min-prob (/ (float (rule-count stats-rule) 1.0d0) denom)))
                        (t min-prob))))
    ;; Rebuild any cached internal rule structure and normalize defensively.
    (normalize-rule-probabilities
     (update-cpd-rules cpd (rule-based-cpd-rules cpd)
                       :check-uniqueness nil
                       :check-prob-sum nil)
     (rule-based-cpd-dependent-id cpd))))

(defun online-em--coerce-evidence (datum)
  "Convert DATUM into the evidence structure expected by LOOPY-BELIEF-PROPAGATION."
  (cond
    ((hash-table-p datum)
     datum)
    ((and (consp datum)
          (or (arrayp (car datum))
              (listp (car datum))))
     (online-em--coerce-evidence (car datum)))
    ((arrayp datum)
     (make-observations datum))
    ((listp datum)
     (make-observations (coerce datum 'vector)))
    (t
     (error "Unsupported online-EM datum format: ~S" datum))))

(defun online-em--infer (bn evidence &key (lr 1.0d0))
  (multiple-value-bind (bn-with-priors priors)
      (compile-bn-priors bn)
    (loopy-belief-propagation bn-with-priors evidence priors '+ lr :singleton-only nil)))

(defun online-em--replace-latents-with-posteriors (bn latent-vars posterior-singletons)
  (let ((singleton-map (online-em--singleton-map posterior-singletons))
        (new-factors (map 'vector #'copy-rule-based-cpd (car bn))))
    (loop for i from 0 below (length new-factors)
          for cpd = (aref new-factors i)
          for dep-id = (rule-based-cpd-dependent-id cpd)
          when (member dep-id latent-vars :test #'equal) do
            (let ((posterior (gethash dep-id singleton-map)))
              (when posterior
                (setf (aref new-factors i) (copy-rule-based-cpd posterior)))))
    (cons new-factors (cdr bn))))

(defun online-em-bn (bn latent-vars dataset
                      &key
                        (step-size *online-em-default-step-size*)
                        (lr 1.0d0)
                        (equivalent-sample-size 1.0d0)
                        (return-learned-cpds t))
  "Online EM for a Bayesian network represented as rule-based CPDs.

STEP-SIZE may be either a function of the 1-based datum index or a constant.
If RETURN-LEARNED-CPDS is NIL, the learned BN is still used for inference, but the
returned network will contain only latent posterior replacements over the current BN
copy."
  (let* ((theta (online-em--copy-bn-factors-only bn))
         (stats (online-em--initialize-statistics theta equivalent-sample-size))
         (last-posterior-singletons nil))
    (loop for datum in dataset
          for n from 1 do
            (let* ((eta (float (if (functionp step-size)
                                   (funcall step-size n)
                                   step-size)
                               1.0d0))
                   (evidence (online-em--coerce-evidence datum)))
              (multiple-value-bind (posterior-factors posterior-singletons)
                  (online-em--infer theta evidence :lr lr)
                (setq last-posterior-singletons posterior-singletons)
                (let ((posterior-map (online-em--posterior-map posterior-factors)))
                  (loop for i from 0 below (length (car theta)) do
                    (let* ((cpd (aref (car theta) i))
                           (stats-cpd (aref (car stats) i))
                           (posterior-cpd (gethash (rule-based-cpd-dependent-id cpd)
                                                   posterior-map)))
                      (online-em--apply-decay stats-cpd eta)
                      (when posterior-cpd
                        (online-em--accumulate-posterior stats-cpd posterior-cpd eta))
                      (setf (aref (car theta) i)
                            (online-em--m-step-cpd cpd stats-cpd))))))))
    (let ((result-bn (if return-learned-cpds
                         theta
                         (online-em--copy-bn-factors-only bn))))
      (if last-posterior-singletons
          (online-em--replace-latents-with-posteriors
           result-bn latent-vars last-posterior-singletons)
          result-bn))))

;;; Convenience alias matching the task phrasing.
(defun online-em (bn latent-vars dataset &rest keys &key &allow-other-keys)
  (apply #'online-em-bn bn latent-vars dataset keys))

