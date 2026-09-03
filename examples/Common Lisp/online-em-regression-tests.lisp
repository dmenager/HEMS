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

(defun online-em-test-rule (probability x h1 h2 &optional (count 1.0d0))
  (let ((conditions (make-hash-table :test #'equal)))
    (setf (gethash "X" conditions) (list x))
    (setf (gethash "H1" conditions) (list h1))
    (setf (gethash "H2" conditions) (list h2))
    (make-rule :id (symbol-name (gensym "EM-TEST-RULE-"))
               :conditions conditions
               :probability probability
               :block (make-hash-table)
               :certain-block (make-hash-table)
               :avoid-list (make-hash-table)
               :redundancies (make-hash-table)
               :count count)))

(defun online-em-test-family-cpd (probabilities &key (row-count 1.0d0))
  (let ((identifiers (make-hash-table :test #'equal))
        (var-values (make-hash-table))
        (vvbm (make-hash-table)))
    (setf (gethash "X" identifiers) 0
          (gethash "H1" identifiers) 1
          (gethash "H2" identifiers) 2)
    (loop for i from 0 below 3
          do (setf (gethash i var-values) '(0 1)
                   (gethash i vvbm) nil))
    (make-rule-based-cpd
     :dependent-id "X"
     :identifiers identifiers
     :vars (make-hash-table)
     :types (make-hash-table)
     :concept-ids (make-hash-table)
     :qualified-vars (make-hash-table)
     :var-values var-values
     :var-value-block-map vvbm
     :set-valued-attributes (make-hash-table)
     :lower-approx-var-value-block-map (make-hash-table)
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
  (online-em-test-step-size-validation)
  (format t "~&Online EM regression tests passed.~%")
  t)

(run-online-em-regression-tests)
