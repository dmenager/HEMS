(in-package :hems)

;;;; Online EM for Bayesian networks represented as rule-based CPDs.
;;;;
;;;; Input:
;;;;   BN          = cons whose CAR is an array of RULE-BASED-CPD objects.
;;;;                 The CDR is ignored by ONLINE-EM but preserved in output.
;;;;   LATENT-VARS = list of RULE-BASED-CPD-DEPENDENT-ID strings whose CPDs
;;;;                 should be replaced by posterior singleton factors.
;;;;   DATUM       = one new example/datapoint processed incrementally. DATUM can be:
;;;;                   1) an evidence hash table suitable for LOOPY-BELIEF-PROPAGATION,
;;;;                   2) a list/vector/array of singleton RULE-BASED-CPD evidence factors,
;;;;                   3) a BN-like cons whose CAR is such a list/array of evidence factors.
;;;;
;;;; Output:
;;;;   A cons with:
;;;;     CAR = array of RULE-BASED-CPD objects (updated by one online EM step), and
;;;;           CPDs in LATENT-VARS replaced by posterior singleton distributions.
;;;;     CDR = original CDR from BN.
;;;;
;;;; Notes:
;;;;   * This implements one incremental E/M update per datum.
;;;;   * The sufficient-statistic update is:
;;;;        S_t = (1 - eta_t) S_{t-1} + eta_t * ESS(x,u | o_t, theta_{t-1})
;;;;   * Running ONLINE-EM repeatedly with the returned network performs online learning.

(defparameter *online-em-default-step-size*
  #'(lambda (n)
      (/ 1.0d0 (sqrt (max 1 n)))))

(defun online-em-hash-keys-sorted (hash)
  (sort (loop for k being the hash-keys of hash collect k)
        #'string< :key #'prin1-to-string))

(defun online-em-normalize-idx-list (idxs)
  (sort (copy-list (if (listp idxs) idxs (list idxs))) #'<))

(defun online-em-rule-key (rule)
  "Canonical key for a rule: sorted ((identifier . sorted-idx-list) ...)."
  (loop for ident in (online-em-hash-keys-sorted (rule-conditions rule))
        collect (cons ident
                      (online-em-normalize-idx-list
                       (copy-list (gethash ident (rule-conditions rule)))))))

(defun online-em-parent-key (cpd rule)
  "Canonical parent-context key for RULE in CPD."
  (let ((dep-id (rule-based-cpd-dependent-id cpd)))
    (remove dep-id
            (online-em-rule-key rule)
            :test #'equal
            :key #'car)))

(defun online-em-rule-map (cpd)
  "Map canonical rule key -> rule object for CPD."
  (let ((map (make-hash-table :test #'equal)))
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          do (setf (gethash (online-em-rule-key rule) map) rule))
    map))

(defun online-em-posterior-map (posterior-factors)
  (let ((map (make-hash-table :test #'equal)))
    (dolist (factor posterior-factors map)
      (setf (gethash (rule-based-cpd-dependent-id factor) map) factor))))

(defun online-em-initialize-statistics (bn equivalent-sample-size)
  "Create a mirrored BN of sufficient statistics, storing counts in RULE-COUNT.
If a rule already has a count, preserve it; otherwise initialize from alpha*P(rule)."
  (let* ((bn-copy (copy-bn bn))
         (alpha (float equivalent-sample-size 1.0d0)))
    (loop for cpd being the elements of (car bn-copy) do
      (loop for rule being the elements of (rule-based-cpd-rules cpd) do
        (setf (rule-count rule)
              (if (numberp (rule-count rule))
                  (float (rule-count rule) 1.0d0)
                  (* alpha (float (rule-probability rule) 1.0d0))))))
    bn-copy))

(defun online-em-apply-decay (stats-cpd eta)
  (loop for rule being the elements of (rule-based-cpd-rules stats-cpd) do
    (setf (rule-count rule)
          (* (- 1.0d0 eta)
             (float (or (rule-count rule) 0.0d0) 1.0d0)))))

(defun online-em-accumulate-posterior (stats-cpd posterior-cpd eta)
  "Blend posterior ESS for POSTERIOR-CPD into STATS-CPD."
  (let ((posterior-map (online-em-rule-map posterior-cpd)))
    (loop for rule being the elements of (rule-based-cpd-rules stats-cpd)
          for key = (online-em-rule-key rule)
          for post-rule = (gethash key posterior-map)
          when post-rule do
            (incf (rule-count rule)
                  (* eta (float (rule-probability post-rule) 1.0d0))))))

(defun online-em-m-step-cpd (cpd stats-cpd &key (min-prob 1.0d-12))
  "Closed-form local M-step for a rule-based CPD.
Groups rules by parent-context and normalizes counts within each group."
  (let ((totals (make-hash-table :test #'equal))
        (stats-map (online-em-rule-map stats-cpd)))
    (loop for rule being the elements of (rule-based-cpd-rules stats-cpd)
          for parent-key = (online-em-parent-key stats-cpd rule) do
            (incf (gethash parent-key totals 0.0d0)
                  (float (rule-count rule) 1.0d0)))
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          for key = (online-em-rule-key rule)
          for stats-rule = (gethash key stats-map)
          for parent-key = (online-em-parent-key cpd rule)
          for denom = (gethash parent-key totals 0.0d0) do
            (setf (rule-probability rule)
                  (cond ((and stats-rule (> denom 0.0d0))
                         (max min-prob (/ (float (rule-count stats-rule) 1.0d0) denom)))
                        (t min-prob))))
    (normalize-rule-probabilities
     (update-cpd-rules cpd (rule-based-cpd-rules cpd)
                       :check-uniqueness nil
                       :check-prob-sum nil)
     (rule-based-cpd-dependent-id cpd))))

(defun online-em-coerce-evidence (datum)
  "Convert DATUM into evidence expected by LOOPY-BELIEF-PROPAGATION."
  (cond
    ((hash-table-p datum)
     datum)
    ((and (consp datum)
          (or (arrayp (car datum))
              (listp (car datum))))
     (online-em-coerce-evidence (car datum)))
    ((arrayp datum)
     (make-observations datum))
    ((listp datum)
     (make-observations (coerce datum 'vector)))
    (t
     (error "Unsupported online-EM datum format: ~S" datum))))

(defun online-em-infer (bn evidence &key (lr 1.0d0))
  (multiple-value-bind (bn-with-priors priors)
      (compile-bn-priors bn)
    (setq bn-with-priors (copy-bn bn-with-priors))
    (loopy-belief-propagation bn-with-priors evidence priors '+ lr :singleton-only nil)))

(defun online-em-replace-latents-with-posteriors (bn latent-vars posterior-factors)
  (let ((posterior-map (online-em-posterior-map posterior-factors))
        (new-factors (map 'vector #'copy-rule-based-cpd (car bn))))
    (when t
      (format t "~%latent vars:~%~S" latent-vars)
      (print-bn bn))
    (loop
      for i from 0 below (length new-factors)
      for cpd = (aref new-factors i)
      for dep-id = (rule-based-cpd-dependent-id cpd)
      when (member dep-id latent-vars :test #'equal)
	do
           (let ((posterior (gethash dep-id posterior-map)))
	     (when posterior
	       (setf (rule-based-cpd-count posterior)
		     (rule-based-cpd-count (aref new-factors i)))
               (setf (aref new-factors i) (copy-rule-based-cpd posterior)))))
    (cons new-factors (cdr bn))))

(defun online-em-step (bn latent-vars datum
                        &key
                          (step-size 1.0d0)
                          (lr 1.0d0)
                          (equivalent-sample-size 1.0d0)
                          (iteration 1)
                          (return-learned-cpds t))
  "Run a single online EM update using one DATUM.

STEP-SIZE may be a constant or a function of ITERATION (1-based)."
  (let* ((theta (copy-bn bn))
         (stats (online-em-initialize-statistics theta equivalent-sample-size))
         (eta (float (if (functionp step-size)
                         (funcall step-size iteration)
                         step-size)
                     1.0d0))
         (evidence (online-em-coerce-evidence datum))
         (posterior-factors nil))
    (multiple-value-bind (inferred-factors ignored-singleton-factors)
        (online-em-infer theta evidence :lr lr)
      (declare (ignore ignored-singleton-factors))
      (setq posterior-factors inferred-factors)
      (let ((posterior-map (online-em-posterior-map inferred-factors)))
        (loop for i from 0 below (array-dimension (car theta) 0) do
          (let* ((cpd (aref (car theta) i))
                 (stats-cpd (aref (car stats) i))
                 (posterior-cpd (gethash (rule-based-cpd-dependent-id cpd)
                                         posterior-map)))
            (online-em-apply-decay stats-cpd eta)
            (when posterior-cpd
              (online-em-accumulate-posterior stats-cpd posterior-cpd eta))
            (setf (aref (car theta) i)
                  (online-em-m-step-cpd cpd stats-cpd))))))
    (let ((result-bn (if return-learned-cpds
                         theta
                         (copy-bn bn))))
      (if posterior-factors
          (online-em-replace-latents-with-posteriors
           result-bn latent-vars posterior-factors)
          result-bn))))

(defun online-em (bn latent-vars datum &rest keys &key &allow-other-keys)
  "Main entry point.

If DATUM is a sequence, each element is processed incrementally and the final BN is
returned. If DATUM is a single example (including cons form), one update is run."
  (cond((or (null datum)
            (hash-table-p datum)
            (arrayp datum)
            (and (consp datum) (arrayp (car datum))))
	(apply #'online-em-step bn latent-vars datum keys))
       ((and (listp datum)
             (or (null datum)
		 (and (consp (car datum)) (or (arrayp (caar datum)) (listp (caar datum))))
		 (hash-table-p (car datum))))
	(loop
	      with current = bn
              for example in datum
              for n from 1
              do
	      (setf current (apply #'online-em-step current latent-vars example
                                   :iteration n
                                   keys))
              finally
	      (return current)))
       (t
	(apply #'online-em-step bn latent-vars datum keys))))

#| TESTS

I have added a :latent-p flag to the rule-based cpd so now I can set it whenever a cpd is latent which will trigger the online-em during insertion, rather than the standard method that currently in place. I have identified some things in the code that may be bugs and want to bring it by you. 

1. After doing EM, we want to replace the latent variables with their inferred posterior distributions. These variables may not be singleton factors, but it appears that they are assumed to be singletons in the code.

2. when initializing the stats  variable in (online-em-one-step), it appears that (online-em-initialize-statistics) sets the rule counts of each rule is to 1.0. Why not use the existing rule counts that are already in the cpd?

3. After doing EM, do the rule counts of the inferred latent variables mean anything? Check (loopy-belief-propagation) and see that we convert all the rule counts to 1 before calibrationg the model. Will this even be a problem if the posterior counts for latent variables are always 1? I'm not sure that this will cause an issue.


I have also pushed the control logic for EM inside (new-combine-bns) and I'd like your opinion on whether I have placed the code in the proper place and if there are any outstanding bugs now.

|#
