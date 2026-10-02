(ql:quickload :hems)

(in-package :hems)

;;; Focused regression tests for the online-EM posterior/statistics contract.
;;; Run with:
;;;   sbcl --disable-debugger --load "examples/Common Lisp/online-em-regression-tests.lisp" --quit

(defparameter *online-em-test-tolerance* 1.0d-9)

(defun online-em-test-assert-close (expected actual description
                                    &optional
                                      (tolerance *online-em-test-tolerance*))
  (unless (<= (abs (- (float expected 1.0d0)
                      (float actual 1.0d0)))
              tolerance)
    (error "~A: expected ~,12F, got ~,12F"
           description expected actual)))

(defun online-em-test-assert (condition description)
  (unless condition
    (error "~A" description)))

(defun online-em-test-set-rule (probability x-values h1-values h2-values
                                &optional (count 1.0d0))
  (let ((conditions (make-hash-table :test #'equal)))
    (setf (gethash "X" conditions) (copy-list x-values))
    (setf (gethash "H1" conditions) (copy-list h1-values))
    (setf (gethash "H2" conditions) (copy-list h2-values))
    (make-rule :id (symbol-name (gensym "EM-TEST-RULE-"))
               :conditions conditions
               :probability probability
               :block (make-hash-table)
               :certain-block (make-hash-table)
               :avoid-list (make-hash-table)
               :redundancies (make-hash-table)
               :count count)))

(defun online-em-test-rule (probability x h1 h2 &optional (count 1.0d0))
  (online-em-test-set-rule probability (list x) (list h1) (list h2) count))

(defun online-em-test-value-block-map ()
  (loop for value from 0 to 1
        collect (list (cons (write-to-string value) value)
                      (make-hash-table))))

(defun online-em-test-family-cpd (probabilities &key (row-count 1.0d0))
  (let ((identifiers (make-hash-table :test #'equal))
        (var-values (make-hash-table))
        (vvbm (make-hash-table))
        (lower-vvbm (make-hash-table))
        (set-valued-attributes (make-hash-table)))
    (setf (gethash "X" identifiers) 0
          (gethash "H1" identifiers) 1
          (gethash "H2" identifiers) 2)
    (loop for i from 0 below 3
          do (setf (gethash i var-values) '(0 1)
                   (gethash i vvbm) (online-em-test-value-block-map)
                   (gethash i lower-vvbm) (online-em-test-value-block-map)
                   (gethash i set-valued-attributes) '(0 1)))
    (make-rule-based-cpd
     :dependent-id "X"
     :identifiers identifiers
     :vars (make-hash-table)
     :types (make-hash-table)
     :concept-ids (make-hash-table)
     :qualified-vars (make-hash-table)
     :var-values var-values
     :var-value-block-map vvbm
     :set-valued-attributes set-valued-attributes
     :lower-approx-var-value-block-map lower-vvbm
     :characteristic-sets (make-hash-table)
     :characteristic-sets-values (make-hash-table)
     :concept-blocks (make-hash-table)
     :cardinalities #(2 2 2)
     :step-sizes #(1 2 4)
     :rules
     (make-array
      8
      :initial-contents
      (loop for h2 from 0 to 1 append
        (loop for h1 from 0 to 1 append
          (loop for x from 0 to 1
                for probability in probabilities
                collect (online-em-test-rule probability x h1 h2 row-count)
                finally (setq probabilities (nthcdr 2 probabilities)))))))))

(defun online-em-test-compressed-family-cpd ()
  (let ((cpd (online-em-test-family-cpd
              '(0.4d0 0.6d0 0.4d0 0.6d0
                0.4d0 0.6d0 0.4d0 0.6d0)
              :row-count 5.0d0)))
    (update-cpd-rules
     cpd
     (vector
      (online-em-test-set-rule 0.4d0 '(0) '(0 1) '(0 1) 5.0d0)
      (online-em-test-set-rule 0.6d0 '(1) '(0 1) '(0 1) 5.0d0))
     :check-prob-sum nil)))

(defun online-em-test-find-rule (cpd x-values h1-values h2-values)
  (find-if
   #'(lambda (rule)
       (and (same-value-set-p
             (cpd-rule-value-set cpd rule "X") x-values)
            (same-value-set-p
             (cpd-rule-value-set cpd rule "H1") h1-values)
            (same-value-set-p
             (cpd-rule-value-set cpd rule "H2") h2-values)))
   (rule-based-cpd-rules cpd)))

(defun online-em-test-covering-rule (cpd x h1 h2)
  (find-if
   #'(lambda (rule)
       (and (member x (cpd-rule-value-set cpd rule "X"))
            (member h1 (cpd-rule-value-set cpd rule "H1"))
            (member h2 (cpd-rule-value-set cpd rule "H2"))))
   (rule-based-cpd-rules cpd)))

(defun online-em-test-atomic-mass (cpd x h1 h2)
  (let ((query (online-em-test-rule 0.0d0 x h1 h2)))
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          when (compatible-rule-p rule query nil nil)
            sum (rule-probability rule))))

(defun online-em-test-global-family-normalization ()
  ;; These are unnormalized weights ordered by H2, H1, then X.
  (let* ((weights '(0.075d0 0.025d0
                    0.050d0 0.200d0
                    0.050d0 0.200d0
                    0.175d0 0.225d0))
         (cpd (online-em-test-family-cpd weights)))
    (normalize-rule-probabilities-globally cpd)
    (online-em-test-assert-close
     1.0d0
     (loop for rule being the elements of (rule-based-cpd-rules cpd)
           sum (rule-probability rule))
     "Globally normalized family mass")
    (online-em-test-assert-close
     0.225d0 (online-em-test-atomic-mass cpd 1 1 1)
     "Joint family assignment mass")
    cpd))

(defun online-em-test-correlated-parent-ess ()
  ;; For X=1, the joint parent masses are .025, .2, .2, .225. The H1 and
  ;; H2 marginals are both .65, so multiplying marginals would incorrectly
  ;; assign .4225 to H1=1,H2=1 rather than the correct .225.
  (let* ((posterior (online-em-test-global-family-normalization))
         (target-cpd (copy-rule-based-cpd posterior))
         (target (online-em-test-rule 0.0d0 1 1 1))
         (latent-set (online-em-latent-set '("H1" "H2")))
         (evidence (make-hash-table :test #'equal)))
    (online-em-test-assert-close
     0.225d0
     (online-em-current-ess target target-cpd posterior latent-set evidence)
     "Correlated latent-parent ESS")))

(defun online-em-test-statistic-recurrence ()
  (let* ((cpd (online-em-test-family-cpd
               '(0.8d0 0.2d0 0.8d0 0.2d0
                 0.8d0 0.2d0 0.8d0 0.2d0)
               :row-count 10.0d0))
         (bn (cons (vector cpd) (make-hash-table)))
         (stats (online-em-initialize-statistics
                 bn 1.0d0 0.25d0 (make-hash-table :test #'equal)
                 :decay-statistics-p t))
         (first-rule (aref (rule-based-cpd-rules (aref (car stats) 0)) 0)))
    ;; Old numerator is 10 * .8 = 8; decay by (1 - .25).
    (online-em-test-assert-close
     6.0d0 (rule-count first-rule) "Online statistic decay")))

(defun online-em-test-selective-evidence-partitioning ()
  (let* ((cpd (online-em-test-compressed-family-cpd))
         (evidence (make-hash-table :test #'equal)))
    (setf (gethash "X" evidence) '(("0" . 1.0d0))
          (gethash "H1" evidence) '(("0" . 1.0d0))
          (gethash "H2" evidence) '(("0" . 1.0d0)))
    (setq cpd
          (online-em-partition-statistics-cpd-for-evidence cpd evidence))
    (online-em-test-assert
     (= 4 (length (rule-based-cpd-rules cpd)))
     "Multi-identifier partition should produce three X=0 pieces and preserve X=1")
    (let ((compatible (online-em-test-find-rule cpd '(0) '(0) '(0)))
          (h2-residual (online-em-test-find-rule cpd '(0) '(0) '(1)))
          (h1-residual (online-em-test-find-rule cpd '(0) '(1) '(0 1)))
          (untouched (online-em-test-find-rule cpd '(1) '(0 1) '(0 1))))
      (online-em-test-assert compatible
                             "Compatible atomic branch was not created")
      (online-em-test-assert h2-residual
                             "Second-dimension residual branch was not preserved")
      (online-em-test-assert h1-residual
                             "First-dimension residual branch was not preserved")
      (online-em-test-assert untouched
                             "Evidence-incompatible compressed rule was not preserved")
      (dolist (rule (list compatible h2-residual h1-residual untouched))
        (online-em-test-assert-close
         5.0d0 (rule-count rule) "Partitioned rule inherited count")
        (online-em-test-assert-close
         (if (eq rule untouched) 0.6d0 0.4d0)
         (rule-probability rule)
         "Partitioned rule inherited per-assignment probability"))
      (let ((posterior
              (online-em-test-family-cpd
               '(0.6d0 0.0d0 0.0d0 0.0d0
                 0.0d0 0.0d0 0.0d0 0.0d0))))
        (online-em-accumulate-posterior
         cpd posterior 1.0d0
         (online-em-latent-set '("X")) evidence)
        (online-em-test-assert-close
         5.6d0 (rule-count compatible)
         "ESS was added to the compatible atomic branch")
        (dolist (rule (list h2-residual h1-residual untouched))
          (online-em-test-assert-close
           5.0d0 (rule-count rule)
           "ESS spilled into an evidence-incompatible branch"))))))

(defun online-em-test-soft-evidence-partitioning ()
  (let* ((cpd (online-em-test-compressed-family-cpd))
         (evidence (make-hash-table :test #'equal)))
    (setf (gethash "X" evidence) '(("0" . 1.0d0))
          (gethash "H1" evidence) '(("0" . 0.7d0) ("1" . 0.3d0)))
    (setq cpd
          (online-em-partition-statistics-cpd-for-evidence cpd evidence))
    (online-em-test-assert
     (= 3 (length (rule-based-cpd-rules cpd)))
     "Soft evidence should atomize both supported H1 values only")
    (online-em-test-assert
     (online-em-test-find-rule cpd '(0) '(0) '(0 1))
     "First soft-evidence value was not atomized")
    (online-em-test-assert
     (online-em-test-find-rule cpd '(0) '(1) '(0 1))
     "Second soft-evidence value was not atomized")
    (online-em-test-assert
     (online-em-test-find-rule cpd '(1) '(0 1) '(0 1))
     "Identifier absent from a compatible rule's evidence path was changed")))

(defun online-em-test-count-normalization-after-partitioning ()
  (let* ((cpd (online-em-test-compressed-family-cpd))
         (evidence (make-hash-table :test #'equal))
         (latent-set (online-em-latent-set '("X"))))
    ;; Treat these as numerator statistics: 2 for X=0 and 3 for X=1.
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          do (setf (rule-count rule)
                   (if (member 0 (cpd-rule-value-set cpd rule "X"))
                       2.0d0
                       3.0d0)))
    (setf (gethash "X" evidence) '(("0" . 1.0d0))
          (gethash "H1" evidence) '(("0" . 1.0d0))
          (gethash "H2" evidence) '(("0" . 1.0d0)))
    (setq cpd
          (online-em-partition-statistics-cpd-for-evidence cpd evidence))
    (online-em-accumulate-posterior
     cpd
     (online-em-test-family-cpd
      '(0.6d0 0.0d0 0.0d0 0.0d0
        0.0d0 0.0d0 0.0d0 0.0d0))
     1.0d0 latent-set evidence)
    (setq cpd (normalize-rule-count-statistics cpd "X"))
    (online-em-test-assert-close
     (/ 2.6d0 5.6d0)
     (online-em-test-atomic-mass cpd 0 0 0)
     "Count-normalized updated child probability")
    (online-em-test-assert-close
     (/ 3.0d0 5.6d0)
     (online-em-test-atomic-mass cpd 1 0 0)
     "Count-normalized broad sibling probability")
    (online-em-test-assert-close
     5.6d0
     (rule-count (online-em-test-covering-rule cpd 0 0 0))
     "Updated actual-row denominator")
    (loop for h1 from 0 to 1
          do
             (loop for h2 from 0 to 1
                   for expected-count = (if (and (= h1 0) (= h2 0))
                                            5.6d0
                                            5.0d0)
                   do
                      (online-em-test-assert-close
                       1.0d0
                       (+ (online-em-test-atomic-mass cpd 0 h1 h2)
                          (online-em-test-atomic-mass cpd 1 h1 h2))
                       "Actual parent row probability sum")
                      (online-em-test-assert-close
                       expected-count
                       (rule-count
                        (online-em-test-covering-rule cpd 0 h1 h2))
                       "Actual parent row count")))))

(defun online-em-test-count-normalization-preserves-zero ()
  (let ((cpd (online-em-test-compressed-family-cpd)))
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          for x-zero-p = (member 0 (cpd-rule-value-set cpd rule "X"))
          do
             (setf (rule-count rule) (if x-zero-p 0.0d0 5.0d0))
             (setf (rule-probability rule) (if x-zero-p 0.0d0 1.0d0)))
    (setq cpd (normalize-rule-count-statistics cpd "X"))
    (loop for h1 from 0 to 1
          do
             (loop for h2 from 0 to 1
                   do
                      (online-em-test-assert-close
                       0.0d0 (online-em-test-atomic-mass cpd 0 h1 h2)
                       "Zero numerator probability")
                      (online-em-test-assert-close
                       1.0d0 (online-em-test-atomic-mass cpd 1 h1 h2)
                       "Positive sibling probability")
                      (online-em-test-assert-close
                       5.0d0
                       (rule-count
                        (online-em-test-covering-rule cpd 0 h1 h2))
                       "Zero-probability rule retains row denominator")))))

(defun online-em-test-initialization-probability-normalization ()
  (let ((cpd (online-em-test-compressed-family-cpd))
        (latent-set (make-hash-table :test #'equal)))
    (update-cpd-rules
     cpd
     (vector
      (online-em-test-set-rule 0.8d0 '(0) '(0) '(0 1) 5.0d0)
      (online-em-test-set-rule 0.8d0 '(0) '(1) '(0 1) 5.0d0)
      (online-em-test-set-rule 0.2d0 '(1) '(0 1) '(0 1) 5.0d0))
     :check-prob-sum nil)
    (online-em-perturb-cpd
     cpd :epsilon 0.0d0 :preserve-zero-probabilities-p t)
    (setq cpd
          (online-em-zero-latent-na-rules
           cpd latent-set :normalize-probabilities t))
    (loop for h1 from 0 to 1
          do
             (loop for h2 from 0 to 1
                   do
                      (online-em-test-assert-close
                       0.8d0 (online-em-test-atomic-mass cpd 0 h1 h2)
                       "Probability-mode overlapping child probability")
                      (online-em-test-assert-close
                       0.2d0 (online-em-test-atomic-mass cpd 1 h1 h2)
                       "Probability-mode broad sibling probability")))))

(defun online-em-test-perturbation-preserves-zero ()
  (let ((cpd (online-em-test-compressed-family-cpd))
        (latent-set (make-hash-table :test #'equal)))
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          for x-zero-p = (member 0 (cpd-rule-value-set cpd rule "X"))
          do
             (setf (rule-probability rule) (if x-zero-p 0.0d0 1.0d0)))
    (online-em-perturb-cpd
     cpd :epsilon 0.2d0 :preserve-zero-probabilities-p t)
    (setq cpd
          (online-em-zero-latent-na-rules
           cpd latent-set :normalize-probabilities t))
    (online-em-test-assert-close
     0.0d0 (online-em-test-atomic-mass cpd 0 0 0)
     "Perturbation changed a preserved zero")
    (online-em-test-assert-close
     1.0d0 (online-em-test-atomic-mass cpd 1 0 0)
     "Perturbation zero-preserving row normalization")))

(defun online-em-test-step-size-validation ()
  (online-em-test-assert-close
   (/ 1.0d0 (sqrt 4.0d0))
   (funcall *online-em-default-step-size* 4)
   "Default online-EM step-size schedule")
  (handler-case
      (progn
        (online-em-step
         (cons (make-array 0) (make-hash-table)) nil
         (make-hash-table :test #'equal) :step-size 1.01d0)
        (error "Step sizes greater than one must be rejected"))
    (error (condition)
      (unless (search "step size must be between 0 and 1"
                      (princ-to-string condition) :test #'char-equal)
        (error condition)))))

(defun run-online-em-regression-tests ()
  (online-em-test-global-family-normalization)
  (online-em-test-correlated-parent-ess)
  (online-em-test-statistic-recurrence)
  (online-em-test-selective-evidence-partitioning)
  (online-em-test-soft-evidence-partitioning)
  (online-em-test-count-normalization-after-partitioning)
  (online-em-test-count-normalization-preserves-zero)
  (online-em-test-initialization-probability-normalization)
  (online-em-test-perturbation-preserves-zero)
  (online-em-test-step-size-validation)
  (format t "~&Online EM regression tests passed.~%")
  t)

(run-online-em-regression-tests)
