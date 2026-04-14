(in-package :hems)

;;;; Online EM for Bayesian networks represented as rule-based CPDs.
;;;;
;;;; Input:
;;;;   BN          = cons whose CAR is an array of RULE-BASED-CPD objects.
;;;;                 The CDR is ignored by ONLINE-EM but preserved in output.
;;;;   LATENT-VARS = list of RULE-BASED-CPD-DEPENDENT-ID strings whose CPDs
;;;;                 should be treated as latent during inference.
;;;;   DATUM       = one new example/datapoint processed incrementally. DATUM can be:
;;;;                   1) an evidence hash table suitable for LOOPY-BELIEF-PROPAGATION,
;;;;                   2) a list/vector/array of singleton RULE-BASED-CPD evidence factors,
;;;;                   3) a BN-like cons whose CAR is such a list/array of evidence factors.
;;;;
;;;; Output:
;;;;   A cons with:
;;;;     CAR = array of RULE-BASED-CPD objects updated by one online EM step.
;;;;     CDR = original CDR from BN.
;;;;
;;;; Notes:
;;;;   * This implements one incremental E/M update per datum.
;;;;   * Existing row-support counts are converted to rule numerator counts,
;;;;     adjusted for the incoming merge support, incremented by the current
;;;;     posterior ESS, then normalized back into CPD parameters.
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

(defun online-em-cpd-domain (cpd ident)
  (let ((idx (gethash ident (rule-based-cpd-identifiers cpd))))
    (copy-list (gethash idx (rule-based-cpd-var-values cpd)))))

(defun online-em-latent-child-cpd-p (cpd latent-set)
  (loop for ident being the hash-keys of (rule-based-cpd-identifiers cpd)
        thereis (and (not (equal ident (rule-based-cpd-dependent-id cpd)))
                     (gethash ident latent-set))))

(defun online-em-latent-identifiers (cpd latent-set)
  (loop for ident being the hash-keys of (rule-based-cpd-identifiers cpd)
        when (gethash ident latent-set)
          collect ident))

(defun online-em-na-value-p (value)
  (and value (string-equal (princ-to-string value) "NA")))

(defun online-em-na-domain-values (cpd ident)
  (let* ((idx (gethash ident (rule-based-cpd-identifiers cpd)))
         (vvbm (and idx (gethash idx (rule-based-cpd-var-value-block-map cpd)))))
    (loop for entry in vvbm
          for label-value = (if (and (consp entry)
                                     (consp (car entry)))
                                (car entry)
                                entry)
          when (and (consp label-value)
                    (online-em-na-value-p (car label-value)))
            collect (cdr label-value))))

(defun online-em-rule-contains-na-p (rule cpd ident)
  (let ((na-values (online-em-na-domain-values cpd ident)))
    (and na-values
         (intersection na-values
                       (or (gethash ident (rule-conditions rule))
                           (online-em-cpd-domain cpd ident))
                       :test #'equal))))

(defun online-em-rule-contains-latent-na-p (rule cpd latent-set)
  (loop for ident being the hash-keys of (rule-based-cpd-identifiers cpd)
        thereis (and (gethash ident latent-set)
                     (online-em-rule-contains-na-p rule cpd ident))))

(defun online-em-zero-latent-na-rules (cpd latent-set
                                       &key
                                         zero-counts
                                         normalize-probabilities)
  (let ((row-sums (make-hash-table :test #'equal))
        row-key)
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          do
             (when (online-em-rule-contains-latent-na-p rule cpd latent-set)
               (setf (rule-probability rule) 0.0d0)
               (when zero-counts
                 (setf (rule-count rule) 0.0d0)))
             (when normalize-probabilities
               (setq row-key (online-em-parent-key cpd rule))
               (incf (gethash row-key row-sums 0.0d0)
                     (float (rule-probability rule) 1.0d0))))
    (when normalize-probabilities
      (loop for rule being the elements of (rule-based-cpd-rules cpd)
            do
               (setq row-key (online-em-parent-key cpd rule))
               (when (> (gethash row-key row-sums) 0.0d0)
                 (setf (rule-probability rule)
                       (/ (rule-probability rule)
                          (gethash row-key row-sums)))))))
  cpd)

(defun online-em-label-index (cpd ident label)
  (let* ((idx (gethash ident (rule-based-cpd-identifiers cpd)))
         (vvbm (and idx (gethash idx (rule-based-cpd-var-value-block-map cpd)))))
    (loop for entry in vvbm
          for label-value = (if (and (consp entry)
                                     (consp (car entry)))
                                (car entry)
                                entry)
          when (and (consp label-value)
                    (equal (car label-value) label))
            return (cdr label-value))))

(defun online-em-evidence-indexes (cpd ident evidence)
  (loop for (label . probability) in (gethash ident evidence)
        for idx = (online-em-label-index cpd ident label)
        when (and idx (> (float probability 1.0d0) 0.0d0))
          collect idx))

(defun online-em-rule-compatible-with-evidence-p (rule cpd evidence)
  (let ((dep-id (rule-based-cpd-dependent-id cpd)))
    (loop for ident being the hash-keys of (rule-based-cpd-identifiers cpd)
          always
             (or (equal ident dep-id)
                 (let ((evidence-values
                         (and evidence
                              (online-em-evidence-indexes cpd ident evidence))))
                   (or (null evidence-values)
                       (intersection
                        (or (gethash ident (rule-conditions rule))
                            (online-em-cpd-domain cpd ident))
                        evidence-values
                        :test #'equal)))))))

(defun online-em-rule-explicit-over-identifiers-p (rule identifiers)
  (every #'(lambda (ident)
             (let ((values (gethash ident (rule-conditions rule))))
               (and values (listp values) (= (length values) 1))))
         identifiers))

(defun online-em-cpd-explicit-over-identifiers-p (cpd identifiers)
  (loop for rule being the elements of (rule-based-cpd-rules cpd)
        always (online-em-rule-explicit-over-identifiers-p rule identifiers)))

(defun online-em-expand-rule-over-identifiers (rule cpd identifiers)
  (cond ((null identifiers)
         (list rule))
        (t
         (let* ((ident (first identifiers))
                (domain (online-em-cpd-domain cpd ident))
                (values (gethash ident (rule-conditions rule)))
                (split-values (cond ((null values) domain)
                                    ((listp values) values)
                                    (t (list values)))))
           (loop for value in split-values
                 append
                 (loop for expanded in (online-em-expand-rule-over-identifiers
                                         (copy-cpd-rule rule :fresh-id t)
                                         cpd
                                         (rest identifiers))
                       do
                          (setf (gethash ident (rule-conditions expanded))
                                (list value))
                       collect expanded))))))

(defun online-em-expand-cpd-over-identifiers (cpd identifiers)
  (cond ((or (null identifiers)
             (online-em-cpd-explicit-over-identifiers-p cpd identifiers))
         (values cpd nil))
        (t
         (let ((rules (loop for rule being the elements of (rule-based-cpd-rules cpd)
                            append (online-em-expand-rule-over-identifiers
                                    rule cpd identifiers))))
           (values
            (update-cpd-rules
             cpd
             (make-array (length rules) :initial-contents rules)
             :check-prob-sum nil)
            t)))))

(defun online-em-key-without-identifiers (key identifiers)
  (remove-if #'(lambda (entry)
                 (member (car entry) identifiers :test #'equal))
             key))

(defun online-em-key-with-identifiers (key identifiers)
  (remove-if-not #'(lambda (entry)
                     (member (car entry) identifiers :test #'equal))
                 key))

(defun online-em-distribution-signature (distribution)
  (sort (copy-list distribution)
        #'string<
        :key #'prin1-to-string))

(defun online-em-cpd-tied-over-identifiers-p (cpd identifiers)
  (let ((dep-id (rule-based-cpd-dependent-id cpd))
        (rows (make-hash-table :test #'equal)))
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          for key = (online-em-rule-key rule)
          for base-key = (online-em-key-without-identifiers
                          key
                          (cons dep-id identifiers))
          for latent-key = (online-em-key-with-identifiers key identifiers)
          for dep-key = (online-em-key-with-identifiers key (list dep-id))
          do
             (unless (gethash base-key rows)
               (setf (gethash base-key rows) (make-hash-table :test #'equal)))
             (push (cons dep-key (rule-probability rule))
                   (gethash latent-key (gethash base-key rows))))
    (loop for row being the hash-values of rows
          thereis
          (let ((signatures nil))
            (loop for distribution being the hash-values of row
                  do
                     (push (online-em-distribution-signature distribution)
                           signatures))
            (and (> (length signatures) 1)
                 (every #'(lambda (signature)
                            (equal signature (first signatures)))
                        (rest signatures)))))))

(defun online-em-hash-string (string)
  (loop with hash = 1469598103934665603
        for char across string
        do
           (setq hash (logxor hash (char-code char)))
           (setq hash (mod (* hash 1099511628211)
                           2305843009213693951))
        finally
           (return (mod hash 1000000007))))

(defun online-em-rule-perturbation (cpd rule)
  (/ (online-em-hash-string
      (format nil "~S:~S"
              (rule-based-cpd-dependent-id cpd)
              (online-em-rule-key rule)))
     1000000007.0d0))

(defun online-em-perturb-cpd (cpd &key (epsilon 1.0d-3))
  (let ((row-sums (make-hash-table :test #'equal))
        row-key new-prob)
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          do
             (setq row-key (online-em-parent-key cpd rule))
             (setq new-prob (+ (float (rule-probability rule) 1.0d0)
                               (* epsilon
                                  (online-em-rule-perturbation cpd rule))))
             (setf (rule-probability rule) new-prob)
             (incf (gethash row-key row-sums 0.0d0) new-prob))
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          do
             (setq row-key (online-em-parent-key cpd rule))
             (when (> (gethash row-key row-sums) 0.0d0)
               (setf (rule-probability rule)
                     (/ (rule-probability rule)
                        (gethash row-key row-sums)))))
    (update-cpd-rules cpd (rule-based-cpd-rules cpd) :check-prob-sum nil)))

(defun online-em-random-weight (cpd rule)
  (+ 0.05d0
     (/ (online-em-hash-string
         (format nil "DIRICHLET:~S:~S"
                 (rule-based-cpd-dependent-id cpd)
                 (online-em-rule-key rule)))
        1000000007.0d0)))

(defun online-em-randomize-latent-child-cpd (cpd)
  (let ((row-sums (make-hash-table :test #'equal))
        row-key new-prob)
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          do
             (setq row-key (online-em-parent-key cpd rule))
             (setq new-prob (online-em-random-weight cpd rule))
             (setf (rule-probability rule) new-prob)
             (incf (gethash row-key row-sums 0.0d0) new-prob))
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          do
             (setq row-key (online-em-parent-key cpd rule))
             (when (> (gethash row-key row-sums) 0.0d0)
               (setf (rule-probability rule)
                     (/ (rule-probability rule)
                        (gethash row-key row-sums)))))
    (update-cpd-rules cpd (rule-based-cpd-rules cpd) :check-prob-sum nil)))

(defun online-em-posterior-map (posterior-factors)
  (let ((map (make-hash-table :test #'equal)))
    (dolist (factor posterior-factors map)
      (setf (gethash (rule-based-cpd-dependent-id factor) map) factor))))

(defun online-em-initialize-statistics (bn
                                        equivalent-sample-size
                                        current-sample-weight
                                        evidence)
  "Create a mirrored BN of sufficient statistics, storing numerator counts in RULE-COUNT.
Existing rule counts are row support, so convert them to rule mass by multiplying
by the current probability. If a rule has no count, initialize from alpha*P(rule)."
  (let* ((bn-copy (copy-bn bn))
         (alpha (float equivalent-sample-size 1.0d0)))
    (loop for cpd being the elements of (car bn-copy) do
      (loop for rule being the elements of (rule-based-cpd-rules cpd) do
        (setf (rule-count rule)
              (if (numberp (rule-count rule))
                  (* (max 0.0d0
                          (- (float (rule-count rule) 1.0d0)
                             (if (online-em-rule-compatible-with-evidence-p
                                  rule cpd evidence)
                                 current-sample-weight
                                 0.0d0)))
                     (float (rule-probability rule) 1.0d0))
                  (* alpha (float (rule-probability rule) 1.0d0))))))
    bn-copy))

(defun online-em-latent-posterior-weight (target-rule posterior-rule posterior-map latent-set)
  (loop
    with weight = 1.0d0
    for ident being the hash-keys of (rule-conditions target-rule)
    for target-values = (gethash ident (rule-conditions target-rule))
    when (and latent-set
              posterior-map
              (gethash ident latent-set)
              target-values
              (null (gethash ident (rule-conditions posterior-rule))))
      do
         (let ((latent-cpd (gethash ident posterior-map))
               (query-rule (make-rule
                            :conditions (make-hash-table :test #'equal)))
               (latent-weight 0.0d0))
           (when latent-cpd
             (setf (gethash ident (rule-conditions query-rule))
                   target-values)
             (loop for latent-rule being the elements of (rule-based-cpd-rules latent-cpd)
                   when (compatible-rule-p latent-rule query-rule nil nil)
                     do
                        (incf latent-weight
                              (float (rule-probability latent-rule) 1.0d0)))
             (setf weight (* weight latent-weight))))
    finally
       (return weight)))

(defun online-em-current-ess (target-rule posterior-cpd posterior-map latent-set)
  (if (online-em-rule-contains-latent-na-p target-rule posterior-cpd latent-set)
      0.0d0
      (loop for posterior-rule being the elements of (rule-based-cpd-rules posterior-cpd)
            when (compatible-rule-p posterior-rule target-rule nil nil)
              sum (* (float (rule-probability posterior-rule) 1.0d0)
                     (online-em-latent-posterior-weight
                      target-rule posterior-rule posterior-map latent-set)))))

(defun online-em-accumulate-posterior (stats-cpd posterior-cpd eta posterior-map latent-set)
  "Add current-datum posterior ESS for POSTERIOR-CPD into STATS-CPD."
  (when posterior-cpd
    (loop
      for rule being the elements of (rule-based-cpd-rules stats-cpd)
      do
         (incf (rule-count rule)
               (* eta
                  (online-em-current-ess
                   rule posterior-cpd posterior-map latent-set))))))

(defun online-em-normalize-statistics-cpd (stats-cpd
                                           &key
                                             latent-set
                                             (min-prob 1.0d-12))
  (let ((row-sums (make-hash-table :test #'equal))
        row-key)
    (when latent-set
      (online-em-zero-latent-na-rules stats-cpd latent-set :zero-counts t))
    (loop
      for rule being the elements of (rule-based-cpd-rules stats-cpd)
      do
         (setq row-key (online-em-parent-key stats-cpd rule))
         (incf (gethash row-key row-sums 0.0d0)
               (float (or (rule-count rule) 0.0d0) 1.0d0)))
    (loop
      for rule being the elements of (rule-based-cpd-rules stats-cpd)
      for numerator = (float (or (rule-count rule) 0.0d0) 1.0d0)
      for denom = (gethash (online-em-parent-key stats-cpd rule) row-sums)
      do
         (setf (rule-probability rule)
               (cond ((and latent-set
                           (online-em-rule-contains-latent-na-p
                            rule stats-cpd latent-set))
                      0.0d0)
                     ((> denom 0.0d0)
                      (max min-prob (/ numerator denom)))
                     (t min-prob)))
         (setf (rule-count rule) denom))
    (update-cpd-rules stats-cpd (rule-based-cpd-rules stats-cpd)
                      :check-prob-sum nil)))

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
    (loopy-belief-propagation bn-with-priors evidence priors '+ lr
                              :singleton-only nil
                              :preserve-rule-counts t)))

(defun online-em-latent-set (latent-vars)
  "Create a set for fast latent-variable membership checks."
  (let ((latent-set (make-hash-table :test #'equal)))
    (dolist (latent-var latent-vars latent-set)
      (setf (gethash latent-var latent-set) t))))

(defun online-em-initialize-latent-cpd (cpd latent-set &key (epsilon 1.0d-3))
  (let ((latent-identifiers (online-em-latent-identifiers cpd latent-set)))
    (cond ((or (null latent-identifiers)
               (not (or (rule-based-cpd-latent-p cpd)
                        (online-em-latent-child-cpd-p cpd latent-set))))
           cpd)
          (t
           (let ((needs-perturb-p
                   (or (not (online-em-cpd-explicit-over-identifiers-p
                             cpd latent-identifiers))
                       (online-em-cpd-tied-over-identifiers-p
                        cpd latent-identifiers)))
                 (explicit-identifiers
                   (remove-duplicates
                    (cons (rule-based-cpd-dependent-id cpd)
                          latent-identifiers)
                    :test #'equal)))
             (multiple-value-bind (expanded-cpd expanded-p)
                 (online-em-expand-cpd-over-identifiers cpd explicit-identifiers)
               (declare (ignore expanded-p))
               (if needs-perturb-p
                   (if (online-em-latent-child-cpd-p expanded-cpd latent-set)
                       (online-em-randomize-latent-child-cpd expanded-cpd)
                       (online-em-perturb-cpd expanded-cpd
                                              :epsilon (* 0.02d0 epsilon)))
                   expanded-cpd)
               (online-em-zero-latent-na-rules
                expanded-cpd latent-set
                :zero-counts t
                :normalize-probabilities t)))))))

(defun online-em-initialize-latent-parameters (bn latent-vars &key (epsilon 1.0d-3))
  (let ((latent-set (online-em-latent-set latent-vars)))
    (loop for i from 0 below (array-dimension (car bn) 0)
          do
             (setf (aref (car bn) i)
                   (online-em-initialize-latent-cpd
                    (aref (car bn) i)
                    latent-set
                    :epsilon epsilon))))
  bn)

(defun online-em-step (bn latent-vars datum
                        &key
                          (step-size 1.0d0)
                          (lr 1.0d0)
                          (equivalent-sample-size 1.0d0)
                          (latent-perturbation 5.0d-2)
                          (iteration 1))
  "Run a single online EM update using one DATUM.

STEP-SIZE may be a constant or a function of ITERATION (1-based)."
  ;; Don't need stats bn since sufficient statistics are computed during inference and stored as posterior-factors
  (let* ((theta (online-em-initialize-latent-parameters
                 (copy-bn bn)
                 latent-vars
                 :epsilon latent-perturbation))
         (eta (float (if (functionp step-size)
                         (funcall step-size iteration)
                         step-size)
                     1.0d0))
         (evidence (online-em-coerce-evidence datum))
         (stats (online-em-initialize-statistics
                 theta equivalent-sample-size eta evidence))
         (posterior-factors nil))

    (multiple-value-bind (inferred-factors ignored-singleton-factors)
        (online-em-infer theta evidence :lr lr)
      (declare (ignore ignored-singleton-factors))
      (setq posterior-factors inferred-factors)
      (when t
	(format t "~%E step:")
	(loop
	  for posterior-factor in posterior-factors
	  do
	     (print-cpd posterior-factor)))
      (let ((posterior-map (online-em-posterior-map inferred-factors))
            (latent-set (online-em-latent-set latent-vars)))
        (loop
	      for i from 0 below (array-dimension (car theta) 0)
	      do
              (let* ((cpd (aref (car theta) i))
                     (stats-cpd (aref (car stats) i))
                     (latent-identifiers
                       (online-em-latent-identifiers cpd latent-set))
                     (posterior-cpd (gethash (rule-based-cpd-dependent-id cpd)
                                             posterior-map)))
                (when latent-identifiers
                  (online-em-accumulate-posterior
                   stats-cpd posterior-cpd eta posterior-map latent-set)
                  (setf (aref (car theta) i)
                        (online-em-normalize-statistics-cpd
                         stats-cpd :latent-set latent-set)))))))
    (when t
      (format t "~%M step:")
      (print-bn theta))
    theta))

(defun online-em (bn latent-vars datum &rest keys &key &allow-other-keys)
  "Main entry point.

If DATUM is a sequence, each element is processed incrementally and the final BN is
returned. If DATUM is a single example (including cons form), one update is run."
  (when t
    (format t "~%updating bn:")
    (print-bn bn))
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
