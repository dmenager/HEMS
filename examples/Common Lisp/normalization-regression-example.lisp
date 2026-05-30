(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;; Keep this file runnable from a fresh checkout even when unrelated system
;; files still have non-fatal compile warnings.
(setf asdf:*compile-file-warnings-behaviour* :warn)
(setf asdf:*compile-file-failure-behaviour* :warn)

(ql:quickload :hems)

(in-package :hems)

(defparameter *epsilon* 1d-6)

(defun approx= (x y)
  (< (abs (- (coerce x 'double-float)
             (coerce y 'double-float)))
     *epsilon*))

(defun hash (&rest pairs)
  (loop
    with table = (make-hash-table :test #'equal)
    for (key value) on pairs by #'cddr
    do (setf (gethash key table) value)
    finally (return table)))

(defun value-block-map ()
  (loop
    with map = (make-hash-table)
    for var-index from 0 below 3
    do (setf (gethash var-index map)
             (loop
               for value from 0 below 3
               collect (list (cons value value) (make-hash-table))))
    finally (return map)))

(defun cpd-shell (rules &key (name "normalization-example"))
  (let* ((identifiers (hash "A" 0 "B" 1 "C" 2))
         (var-values (hash 0 '(0 1 2) 1 '(0 1 2) 2 '(0 1 2)))
         (cardinalities (make-array 3 :initial-contents '(3 3 3) :fill-pointer t))
         (cpd (make-rule-based-cpd
               :dependent-id "A"
               :dependent-var "A"
               :identifiers identifiers
               :vars (hash 0 "A" 1 "B" 2 "C")
               :types (hash 0 nil 1 nil 2 nil)
               :concept-ids (hash 0 nil 1 nil 2 nil)
               :qualified-vars (hash 0 "A" 1 "B" 2 "C")
               :var-value-block-map (value-block-map)
               :set-valued-attributes (hash 0 '((0) (1) (2))
                                             1 '((0) (1) (2))
                                             2 '((0) (1) (2)))
               :lower-approx-var-value-block-map (value-block-map)
               :characteristic-sets (make-hash-table)
               :characteristic-sets-values (make-hash-table)
               :var-values var-values
               :cardinalities cardinalities
               :step-sizes (generate-cpd-step-sizes cardinalities)
               :rules rules
               :concept-blocks (make-hash-table)
               :count nil
               :singleton-p nil
               :prior name)))
    (update-cpd-rules cpd rules :check-prob-sum nil)))

(defun dense-rule (id a b c probability count)
  (make-rule :id id
             :conditions (hash "A" (list a) "B" (list b) "C" (list c))
             :probability probability
             :block (make-hash-table)
             :certain-block (make-hash-table)
             :avoid-list (make-hash-table)
             :redundancies (make-hash-table)
             :count count))

(defun row-probs (b c table)
  (cdr (assoc (list b c) table :test #'equal)))

(defun dense-rules-from-rows (prefix row-table count-table)
  (loop
    with rules = nil
    with i = 0
    for b from 0 below 3
    do (loop
         for c from 0 below 3
         for probs = (row-probs b c row-table)
         for count = (cdr (assoc (list b c) count-table :test #'equal))
         do (loop
              for a from 0 below 3
              for probability in probs
              do (push (dense-rule (format nil "~A-~D" prefix i)
                                   a b c probability count)
                       rules)
                 (incf i)))
    finally (return (make-array (length rules)
                                :initial-contents (nreverse rules)))))

(defparameter *cpd1-rows*
  '(((0 0) . (0.7d0 0.2d0 0.1d0))
    ((0 1) . (0.7d0 0.2d0 0.1d0))
    ((0 2) . (0.7d0 0.2d0 0.1d0))
    ((1 0) . (0.1d0 0.6d0 0.3d0))
    ((1 1) . (0.1d0 0.6d0 0.3d0))
    ((1 2) . (0.2d0 0.2d0 0.6d0))
    ((2 0) . (0.4d0 0.4d0 0.2d0))
    ((2 1) . (0.4d0 0.4d0 0.2d0))
    ((2 2) . (0.4d0 0.4d0 0.2d0))))

(defparameter *cpd2-rows*
  '(((0 0) . (0.25d0 0.5d0 0.25d0))
    ((0 1) . (0.25d0 0.5d0 0.25d0))
    ((0 2) . (0.25d0 0.5d0 0.25d0))
    ((1 0) . (0.5d0 0.25d0 0.25d0))
    ((1 1) . (0.5d0 0.25d0 0.25d0))
    ((1 2) . (0.25d0 0.25d0 0.5d0))
    ((2 0) . (0.2d0 0.2d0 0.6d0))
    ((2 1) . (0.2d0 0.2d0 0.6d0))
    ((2 2) . (0.2d0 0.2d0 0.6d0))))

(defparameter *product-counts*
  '(((0 0) . 80) ((0 1) . 80) ((0 2) . 80)
    ((1 0) . 80) ((1 1) . 80) ((1 2) . 80)
    ((2 0) . 100) ((2 1) . 100) ((2 2) . 100)))

(defparameter *product-rows*
  '(((0 0) . (0.175d0 0.1d0 0.025d0))
    ((0 1) . (0.175d0 0.1d0 0.025d0))
    ((0 2) . (0.175d0 0.1d0 0.025d0))
    ((1 0) . (0.05d0 0.15d0 0.075d0))
    ((1 1) . (0.05d0 0.15d0 0.075d0))
    ((1 2) . (0.05d0 0.05d0 0.3d0))
    ((2 0) . (0.08d0 0.08d0 0.12d0))
    ((2 1) . (0.08d0 0.08d0 0.12d0))
    ((2 2) . (0.08d0 0.08d0 0.12d0))))

(defparameter *normalized-reference-rows*
  '(((0 0) . (0.5833333333333334d0 0.33333333333333337d0 0.08333333333333334d0))
    ((0 1) . (0.5833333333333334d0 0.33333333333333337d0 0.08333333333333334d0))
    ((0 2) . (0.5833333333333334d0 0.33333333333333337d0 0.08333333333333334d0))
    ((1 0) . (0.18181818181818182d0 0.5454545454545454d0 0.2727272727272727d0))
    ((1 1) . (0.18181818181818182d0 0.5454545454545454d0 0.2727272727272727d0))
    ((1 2) . (0.125d0 0.125d0 0.75d0))
    ((2 0) . (0.28571428571428575d0 0.28571428571428575d0 0.4285714285714286d0))
    ((2 1) . (0.28571428571428575d0 0.28571428571428575d0 0.4285714285714286d0))
    ((2 2) . (0.28571428571428575d0 0.28571428571428575d0 0.4285714285714286d0))))

(defun make-cpd1 ()
  (cpd-shell (dense-rules-from-rows "CPD1" *cpd1-rows*
                                    (loop for row in *cpd1-rows*
                                          collect (cons (car row) 10)))))

(defun make-cpd2 ()
  (cpd-shell (dense-rules-from-rows "CPD2" *cpd2-rows*
                                    (loop for row in *cpd2-rows*
                                          collect (cons (car row)
                                                        (if (= (first (car row)) 2) 10 8))))))

(defun make-unnormalized-product-cpd ()
  (cpd-shell (dense-rules-from-rows "PRODUCT" *product-rows* *product-counts*)
             :name "unnormalized-product"))

(defun make-reference-normalized-cpd ()
  (cpd-shell (dense-rules-from-rows "REFERENCE" *normalized-reference-rows*
                                    *product-counts*)
             :name "normalized-reference"))

(defun normalize-copy (cpd)
  (normalize-rule-probabilities (copy-rule-based-cpd cpd) "A"))

(defun compressed-normalize-copy (cpd)
  (normalize-rule-probabilities
   (get-local-coverings (copy-rule-based-cpd cpd))
   "A"))

(defun compatible-p (actual-rule actual-cpd reference-rule reference-cpd)
  (multiple-value-bind (forward-p)
      (compatible-rule-p actual-rule reference-rule actual-cpd reference-cpd)
    (multiple-value-bind (backward-p)
        (compatible-rule-p reference-rule actual-rule reference-cpd actual-cpd)
      (and forward-p backward-p))))

(defun assert-compatible-rule-agreement (actual-cpd reference-cpd)
  (loop
    for reference-rule being the elements of (rule-based-cpd-rules reference-cpd)
    for matched = nil
    do (loop
         for actual-rule being the elements of (rule-based-cpd-rules actual-cpd)
         when (compatible-p actual-rule actual-cpd reference-rule reference-cpd)
           do (setf matched t)
              (unless (approx= (rule-probability actual-rule)
                               (rule-probability reference-rule))
                (error "Probability mismatch.~%Actual: ~S~%Reference: ~S"
                       actual-rule reference-rule))
              (unless (equal (rule-count actual-rule) (rule-count reference-rule))
                (error "Count mismatch.~%Actual: ~S~%Reference: ~S"
                       actual-rule reference-rule)))
       (unless matched
         (error "No actual rule compatible with reference rule: ~S" reference-rule)))
  (loop
    for actual-rule being the elements of (rule-based-cpd-rules actual-cpd)
    for matched = nil
    do (loop
         for reference-rule being the elements of (rule-based-cpd-rules reference-cpd)
         when (compatible-p actual-rule actual-cpd reference-rule reference-cpd)
           do (setf matched t))
       (unless matched
         (error "No reference rule compatible with actual rule: ~S" actual-rule)))
  t)

(defun run-uncompressed-normalization-test ()
  (let ((actual (normalize-copy (make-unnormalized-product-cpd)))
        (reference (make-reference-normalized-cpd)))
    (assert-compatible-rule-agreement actual reference)))

(defun run-compressed-normalization-test ()
  (let ((actual (compressed-normalize-copy (make-unnormalized-product-cpd)))
        (reference (make-reference-normalized-cpd)))
    (assert-compatible-rule-agreement actual reference)))

(defun run-regression-case (name thunk)
  (format t "~&~A ... " name)
  (finish-output)
  (handler-case
      (progn
        (funcall thunk)
        (format t "PASS~%"))
    (error (condition)
      (format t "FAIL~%")
      (format *error-output* "~&~A failed:~%~A~%" name condition)
      (uiop:quit 1))))

(defun run-normalization-regression-example ()
  ;; CPD1 and CPD2 are constructed here so the example keeps the training
  ;; factors next to the product factor that is actually normalized.
  (make-cpd1)
  (make-cpd2)
  (run-regression-case "Uncompressed normalization"
                       #'run-uncompressed-normalization-test)
  (run-regression-case "Compressed normalization"
                       #'run-compressed-normalization-test))

(run-normalization-regression-example)
(uiop:quit 0)
