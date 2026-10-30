(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(setf asdf:*compile-file-warnings-behaviour* :warn)
(setf asdf:*compile-file-failure-behaviour* :warn)
(ql:quickload :hems)

(in-package :hems)

;;; Golden mathematical system test for HEMS.
;;;
;;; Run from the HEMS repository with:
;;;   sbcl --disable-debugger --load "examples/Common Lisp/hems-golden-system-tests.lisp"
;;;
;;; Results are written beside this script to:
;;;   hems-golden-system-test-results.txt

(defparameter *golden-tolerance* 1.0d-8)
(defparameter *golden-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))
(defparameter *golden-report-path*
  (merge-pathnames "hems-golden-system-test-results.txt" *golden-directory*))
(defparameter *golden-results* nil)

(defstruct golden-result
  stage name expected actual tolerance passed-p detail)

(defmacro golden-quietly (&body body)
  `(let ((*standard-output* (make-broadcast-stream))
         (*trace-output* (make-broadcast-stream)))
     ,@body))

(defun golden-record (stage name expected actual passed-p
                      &key tolerance detail)
  (push (make-golden-result :stage stage :name name
                            :expected expected :actual actual
                            :tolerance tolerance :passed-p passed-p
                            :detail detail)
        *golden-results*)
  passed-p)

(defun golden-close (stage name expected actual
                     &optional (tolerance *golden-tolerance*))
  (golden-record
   stage name expected actual
   (and (numberp actual)
        (<= (abs (- (float expected 1.0d0) (float actual 1.0d0)))
            tolerance))
   :tolerance tolerance))

(defun golden-true (stage name value &optional detail)
  (golden-record stage name t value (not (null value)) :detail detail))

(defun golden-run-stage (stage thunk)
  (handler-case (funcall thunk)
    (error (condition)
      (golden-record stage "stage completed without an unhandled condition"
                     "no condition" (princ-to-string condition) nil
                     :detail "The stage stopped here; earlier checks remain valid."))))

(defun golden-write-report ()
  (let* ((results (reverse *golden-results*))
         (passed (count-if #'golden-result-passed-p results))
         (failed (- (length results) passed)))
    (with-open-file (out *golden-report-path* :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
      (format out "HEMS GOLDEN SYSTEM TEST REPORT~%")
      (format out "==============================~%~%")
      (format out "SUMMARY~%-------~%")
      (format out "Total checks: ~D~%Passed:       ~D~%Failed:       ~D~%Overall:      ~A~%~%"
              (length results) passed failed (if (zerop failed) "PASS" "FAIL"))
      (format out "Structural-zero persistence after insertion/EM is intentionally~%")
      (format out "excluded from pass/fail criteria. Initial generator outputs, including~%")
      (format out "zeros, are checked because they define the requested distribution.~%~%")
      (format out "Fixture: 100 coherent seed cases over STATUS, CONTEXT, and CHOICE.~%~%")
      (when (plusp failed)
        (format out "FAILED CHECKS~%-------------~%")
        (dolist (r results)
          (unless (golden-result-passed-p r)
            (format out "[~A] ~A~%  expected: ~S~%  actual:   ~S~%"
                    (golden-result-stage r) (golden-result-name r)
                    (golden-result-expected r) (golden-result-actual r))
            (when (golden-result-tolerance r)
              (format out "  tolerance: ~S~%" (golden-result-tolerance r)))
            (when (golden-result-detail r)
              (format out "  detail:    ~A~%" (golden-result-detail r)))
            (terpri out)))
        (terpri out))
      (format out "ALL CHECKS~%----------~%")
      (dolist (r results)
        (format out "~A | ~A | ~A~%  expected: ~S~%  actual:   ~S~%"
                (if (golden-result-passed-p r) "PASS" "FAIL")
                (golden-result-stage r) (golden-result-name r)
                (golden-result-expected r) (golden-result-actual r))
        (when (golden-result-tolerance r)
          (format out "  tolerance: ~S~%" (golden-result-tolerance r)))
        (when (golden-result-detail r)
          (format out "  detail:    ~A~%" (golden-result-detail r)))
        (terpri out)))
    (values passed failed)))

;;; Independent oracle. These functions intentionally use no HEMS operations.

(defun golden-oracle-posterior (prior-a likelihood-a prior-b likelihood-b)
  (let* ((mass-a (* prior-a likelihood-a))
         (mass-b (* prior-b likelihood-b))
         (z (+ mass-a mass-b)))
    (values (/ mass-a z) (/ mass-b z))))

(defun golden-hidden-bypass-responsibilities ()
  ;; After one labeled BENIGN/DECISION/CHECKPOINT event:
  ;; P(B)=71/101, P(T)=30/101, P(BYPASS|B,D)=10/51,
  ;; P(BYPASS|T,D)=9/10.
  (golden-oracle-posterior (/ 71.0d0 101.0d0) (/ 10.0d0 51.0d0)
                           (/ 30.0d0 101.0d0) (/ 9.0d0 10.0d0)))

;;; HEMS model inspection helpers.

(defun golden-cpd (bn var)
  (loop for cpd being the elements of (car bn)
        when (string-equal (rule-based-cpd-dependent-var cpd) var)
          return cpd
        finally (error "No CPD for ~S" var)))

(defun golden-factor (factors var)
  (loop for cpd being the elements of factors
        when (string-equal (rule-based-cpd-dependent-var cpd) var)
          return cpd
        finally (error "No posterior factor for ~S" var)))

(defun golden-id (cpd var)
  (loop for id being the hash-keys of (rule-based-cpd-identifiers cpd)
          using (hash-value pos)
        when (string-equal (gethash pos (rule-based-cpd-vars cpd)) var)
          return id
        finally (error "No identifier for ~S in ~S"
                       var (rule-based-cpd-dependent-var cpd))))

(defun golden-index (cpd id label)
  (let* ((pos (gethash id (rule-based-cpd-identifiers cpd)))
         (vvbm (gethash pos (rule-based-cpd-var-value-block-map cpd))))
    (loop for entry in vvbm
          when (string-equal (caar entry) label) return (cdar entry)
          finally (error "No value ~S for ~S" label id))))

(defun golden-rule-matches-p (cpd rule assignments)
  (loop for (var . label) in assignments
        for id = (golden-id cpd var)
        for idx = (golden-index cpd id label)
        always (member idx (cpd-rule-value-set cpd rule id))))

(defun golden-rule (cpd assignments)
  (find-if (lambda (rule) (golden-rule-matches-p cpd rule assignments))
           (rule-based-cpd-rules cpd)))

(defun golden-prob (cpd child &rest parents)
  (let ((rule (golden-rule
               cpd (cons (cons (rule-based-cpd-dependent-var cpd) child)
                         parents))))
    (unless rule (error "No rule for ~S ~S" child parents))
    (float (rule-probability rule) 1.0d0)))

(defun golden-count (cpd child &rest parents)
  (let ((rule (golden-rule
               cpd (cons (cons (rule-based-cpd-dependent-var cpd) child)
                         parents))))
    (unless rule (error "No rule count for ~S ~S" child parents))
    (float (or (rule-count rule) 0.0d0) 1.0d0)))

(defun golden-set-row-count (cpd count &rest parents)
  (loop for rule being the elements of (rule-based-cpd-rules cpd)
        when (golden-rule-matches-p cpd rule parents)
          do (setf (rule-count rule) (float count 1.0d0))))

(defun golden-set-row (cpd probabilities count &rest parents)
  (dolist (entry probabilities)
    (let ((rule (golden-rule
                 cpd (cons (cons (rule-based-cpd-dependent-var cpd) (car entry))
                           parents))))
      (unless rule (error "Cannot set ~S in ~S" entry parents))
      (setf (rule-probability rule) (float (cdr entry) 1.0d0))))
  (apply #'golden-set-row-count cpd count parents))

(defun golden-seed-counts (bn)
  ;; Coherent 100-case seed:
  ;; B=70, T=30; D=60, O=40;
  ;; B,D: checkpoint=40, bypass=10; T,D: checkpoint=1, bypass=9;
  ;; B,O: left=10, right=10; T,O: left=10, right=10.
  (let ((status (golden-cpd bn "STATUS"))
        (context (golden-cpd bn "CONTEXT"))
        (choice (golden-cpd bn "CHOICE")))
    (setf (rule-based-cpd-count status) 100
          (rule-based-cpd-count context) 100
          (rule-based-cpd-count choice) 100)
    (golden-set-row-count status 100.0d0)
    (golden-set-row-count context 100.0d0)
    (golden-set-row-count choice 50.0d0
                          '("STATUS" . "BENIGN") '("CONTEXT" . "DECISION"))
    (golden-set-row-count choice 10.0d0
                          '("STATUS" . "THREAT") '("CONTEXT" . "DECISION"))
    (golden-set-row-count choice 20.0d0
                          '("STATUS" . "BENIGN") '("CONTEXT" . "ORDINARY"))
    (golden-set-row-count choice 20.0d0
                          '("STATUS" . "THREAT") '("CONTEXT" . "ORDINARY"))
    bn))

(defun golden-model (&key (latent-status-p nil))
  (let ((bn
          (compile-program nil
            s = (functional-node status :arguments nil
                  :generator (list (cons "NA" 0.0d0)
                                   (cons "BENIGN" 0.7d0)
                                   (cons "THREAT" 0.3d0))
                  :type "percept")
            c = (functional-node context :arguments nil
                  :generator (list (cons "NA" 0.0d0)
                                   (cons "DECISION" 0.6d0)
                                   (cons "ORDINARY" 0.4d0))
                  :type "percept")
            y = (functional-node choice :arguments (s c)
                  :generator
                  (cond
                    ((and (string-equal s "BENIGN")
                          (string-equal c "DECISION"))
                     '(("NA" . 0.0d0) ("CHECKPOINT" . 0.8d0)
                       ("BYPASS" . 0.2d0) ("LEFT" . 0.0d0)
                       ("RIGHT" . 0.0d0)))
                    ((and (string-equal s "THREAT")
                          (string-equal c "DECISION"))
                     '(("NA" . 0.0d0) ("CHECKPOINT" . 0.1d0)
                       ("BYPASS" . 0.9d0) ("LEFT" . 0.0d0)
                       ("RIGHT" . 0.0d0)))
                    ((and (member s '("BENIGN" "THREAT") :test #'string-equal)
                          (string-equal c "ORDINARY"))
                     '(("NA" . 0.0d0) ("CHECKPOINT" . 0.0d0)
                       ("BYPASS" . 0.0d0) ("LEFT" . 0.5d0)
                       ("RIGHT" . 0.5d0)))
                    (t
                     '(("NA" . 0.0d0) ("CHECKPOINT" . 0.0d0)
                       ("BYPASS" . 0.0d0) ("LEFT" . 0.0d0)
                       ("RIGHT" . 0.0d0))))
                  :type "percept"))))
    (setf (rule-based-cpd-latent-p (golden-cpd bn "STATUS")) latent-status-p)
    (golden-seed-counts bn)))

(defun golden-after-hard-model (&key (latent-status-p nil))
  (let* ((bn (golden-model :latent-status-p latent-status-p))
         (status (golden-cpd bn "STATUS"))
         (context (golden-cpd bn "CONTEXT"))
         (choice (golden-cpd bn "CHOICE")))
    (golden-set-row status
                    `(("NA" . 0.0d0) ("BENIGN" . ,(/ 71.0d0 101.0d0))
                      ("THREAT" . ,(/ 30.0d0 101.0d0)))
                    101.0d0)
    (golden-set-row context
                    `(("NA" . 0.0d0) ("DECISION" . ,(/ 61.0d0 101.0d0))
                      ("ORDINARY" . ,(/ 40.0d0 101.0d0)))
                    101.0d0)
    (golden-set-row choice
                    `(("NA" . 0.0d0) ("CHECKPOINT" . ,(/ 41.0d0 51.0d0))
                      ("BYPASS" . ,(/ 10.0d0 51.0d0))
                      ("LEFT" . 0.0d0) ("RIGHT" . 0.0d0))
                    51.0d0 '("STATUS" . "BENIGN") '("CONTEXT" . "DECISION"))
    (setf (rule-based-cpd-count status) 101
          (rule-based-cpd-count context) 101
          (rule-based-cpd-count choice) 101)
    bn))

(defun golden-hard-observation ()
  (compile-program nil
    s = (percept-node status :value "BENIGN")
    c = (percept-node context :value "DECISION")
    y = (percept-node choice :value "CHECKPOINT")
    s --> y
    c --> y))

(defun golden-latent-bypass-observation ()
  (compile-program nil
    s = (percept-node status :latent-p t
          :values ((:value "BENIGN") (:value "THREAT")))
    c = (percept-node context :value "DECISION")
    y = (percept-node choice :value "BYPASS")
    s --> y
    c --> y))

(defun golden-cue (context-label choice-label)
  (eval `(compile-program nil
           c = (percept-node context :value ,context-label)
           y = (percept-node choice :value ,choice-label))))

(defun golden-evidence (bn specs)
  (let ((result (make-hash-table :test #'equal)))
    (dolist (spec specs result)
      (setf (gethash (rule-based-cpd-dependent-id (golden-cpd bn (car spec)))
                     result)
            (cdr spec)))))

(defun golden-hard-evidence (bn &rest pairs)
  (golden-evidence
   bn (loop for (var label) on pairs by #'cddr
            collect (cons var (list (cons label 1.0d0))))))

(defun golden-inferred-prob (bn evidence var label)
  (multiple-value-bind (families singletons)
      (golden-quietly (online-em-infer bn evidence :lr 1.0d0))
    (declare (ignore families))
    (golden-prob (golden-factor singletons var) label)))

(defun golden-reset-memory ()
  (golden-quietly (init-eltm))
  (setf episode-buffer* (list ':obs (make-hash-table) ':h-model nil)))

(defun golden-insert (bn)
  (golden-quietly
    (new-push-to-ep-buffer :observation bn :bic-p nil :insertp t
                           :temporal-p nil :hidden-state-p nil)))

(defun golden-memory-episodes ()
  (labels ((walk (branch acc)
             (cond
               ((and (consp branch) (episode-p (car branch)))
                (let ((next (cons (car branch) acc)))
                  (dolist (child (cdr branch) next)
                    (setq next (walk child next)))))
               ((consp branch)
                (let ((next acc))
                  (dolist (child branch next)
                    (setq next (walk child next)))))
               (t acc))))
    (walk eltm* nil)))

(defun golden-fixture-bn-p (bn)
  (handler-case
      (progn (golden-cpd bn "STATUS") (golden-cpd bn "CONTEXT")
             (golden-cpd bn "CHOICE") t)
    (error () nil)))

(defun golden-memory-model ()
  (let ((candidates
          (remove-if-not
           (lambda (ep) (golden-fixture-bn-p (episode-observation ep)))
           (golden-memory-episodes))))
    (unless candidates (error "No fixture observation episode in ELTM"))
    (episode-observation (car (sort candidates #'> :key #'episode-count)))))

(defun golden-compressed-copy (bn)
  (let* ((copy (copy-bn bn))
         (factors (car copy)))
    (loop for i below (array-dimension factors 0)
          for cpd = (aref factors i)
          do (setf (aref factors i)
                   (get-local-coverings
                    (update-cpd-rules cpd (rule-based-cpd-rules cpd)))))
    (cons factors (make-graph-edges factors))))

;;; Assertions over complete CPD rows.

(defun golden-check-row (stage cpd expected-count probabilities &rest parents)
  (let ((sum 0.0d0))
    (dolist (entry probabilities)
      (let ((actual (apply #'golden-prob cpd (car entry) parents)))
        (incf sum actual)
        (golden-close stage
                      (format nil "~A ~S P(~A)"
                              (rule-based-cpd-dependent-var cpd) parents
                              (car entry))
                      (cdr entry) actual)))
    (golden-close stage
                  (format nil "~A ~S row sums to one"
                          (rule-based-cpd-dependent-var cpd) parents)
                  1.0d0 sum)
    (when expected-count
      (golden-close stage
                    (format nil "~A ~S row count"
                            (rule-based-cpd-dependent-var cpd) parents)
                    expected-count
                    (apply #'golden-count cpd (caar probabilities) parents)))))

(defun golden-test-functional-generation ()
  (let* ((stage "functional-node generation")
         (bn (golden-model))
         (status (golden-cpd bn "STATUS"))
         (context (golden-cpd bn "CONTEXT"))
         (choice (golden-cpd bn "CHOICE")))
    (golden-check-row stage status 100.0d0
                      '(("NA" . 0.0d0) ("BENIGN" . 0.7d0)
                        ("THREAT" . 0.3d0)))
    (golden-check-row stage context 100.0d0
                      '(("NA" . 0.0d0) ("DECISION" . 0.6d0)
                        ("ORDINARY" . 0.4d0)))
    (golden-check-row stage choice 50.0d0
                      '(("NA" . 0.0d0) ("CHECKPOINT" . 0.8d0)
                        ("BYPASS" . 0.2d0) ("LEFT" . 0.0d0)
                        ("RIGHT" . 0.0d0))
                      '("STATUS" . "BENIGN") '("CONTEXT" . "DECISION"))
    (golden-check-row stage choice 10.0d0
                      '(("NA" . 0.0d0) ("CHECKPOINT" . 0.1d0)
                        ("BYPASS" . 0.9d0) ("LEFT" . 0.0d0)
                        ("RIGHT" . 0.0d0))
                      '("STATUS" . "THREAT") '("CONTEXT" . "DECISION"))
    (dolist (status-label '("BENIGN" "THREAT"))
      (golden-check-row stage choice 20.0d0
                        '(("NA" . 0.0d0) ("CHECKPOINT" . 0.0d0)
                          ("BYPASS" . 0.0d0) ("LEFT" . 0.5d0)
                          ("RIGHT" . 0.5d0))
                        (cons "STATUS" status-label)
                        '("CONTEXT" . "ORDINARY")))))

(defun golden-test-inference ()
  (let* ((stage "pre-insertion inference")
         (bn (golden-model)))
    (multiple-value-bind (threat benign)
        (golden-oracle-posterior 0.3d0 0.1d0 0.7d0 0.8d0)
      (let ((e (golden-hard-evidence bn "CONTEXT" "DECISION"
                                    "CHOICE" "CHECKPOINT")))
        (golden-close stage "P(THREAT | CHECKPOINT, DECISION)" threat
                      (golden-inferred-prob bn e "STATUS" "THREAT"))
        (golden-close stage "P(BENIGN | CHECKPOINT, DECISION)" benign
                      (golden-inferred-prob bn e "STATUS" "BENIGN"))))
    (multiple-value-bind (threat benign)
        (golden-oracle-posterior 0.3d0 0.9d0 0.7d0 0.2d0)
      (let ((e (golden-hard-evidence bn "CONTEXT" "DECISION"
                                    "CHOICE" "BYPASS")))
        (golden-close stage "P(THREAT | BYPASS, DECISION)" threat
                      (golden-inferred-prob bn e "STATUS" "THREAT"))
        (golden-close stage "P(BENIGN | BYPASS, DECISION)" benign
                      (golden-inferred-prob bn e "STATUS" "BENIGN"))))
    (let ((e (golden-hard-evidence bn "CONTEXT" "ORDINARY"
                                  "CHOICE" "LEFT")))
      (golden-close stage "uninformative choice preserves threat prior" 0.3d0
                    (golden-inferred-prob bn e "STATUS" "THREAT")))
    (let ((e (golden-hard-evidence bn "STATUS" "THREAT"
                                  "CONTEXT" "DECISION")))
      (golden-close stage "P(CHECKPOINT | THREAT, DECISION)" 0.1d0
                    (golden-inferred-prob bn e "CHOICE" "CHECKPOINT"))
      (golden-close stage "P(BYPASS | THREAT, DECISION)" 0.9d0
                    (golden-inferred-prob bn e "CHOICE" "BYPASS")))
    (let* ((e (golden-evidence
               bn (list
                   (cons "CONTEXT" '(("DECISION" . 1.0d0)))
                   (cons "CHOICE" '(("CHECKPOINT" . 0.6d0)
                                    ("BYPASS" . 0.4d0))))))
           (like-benign (+ (* 0.6d0 0.8d0) (* 0.4d0 0.2d0)))
           (like-threat (+ (* 0.6d0 0.1d0) (* 0.4d0 0.9d0))))
      (multiple-value-bind (threat ignored)
          (golden-oracle-posterior 0.3d0 like-threat 0.7d0 like-benign)
        (declare (ignore ignored))
        (golden-close stage "soft CHOICE evidence" threat
                      (golden-inferred-prob bn e "STATUS" "THREAT"))))))

(defun golden-check-hard-update (stage bn &key (context-updated-p t))
  (golden-check-row stage (golden-cpd bn "STATUS") 101.0d0
                    `(("BENIGN" . ,(/ 71.0d0 101.0d0))
                      ("THREAT" . ,(/ 30.0d0 101.0d0))))
  (golden-check-row stage (golden-cpd bn "CONTEXT")
                    (if context-updated-p 101.0d0 100.0d0)
                    (if context-updated-p
                        `(("DECISION" . ,(/ 61.0d0 101.0d0))
                          ("ORDINARY" . ,(/ 40.0d0 101.0d0)))
                        '(("DECISION" . 0.6d0)
                          ("ORDINARY" . 0.4d0))))
  (golden-check-row stage (golden-cpd bn "CHOICE") 51.0d0
                    `(("CHECKPOINT" . ,(/ 41.0d0 51.0d0))
                      ("BYPASS" . ,(/ 10.0d0 51.0d0)))
                    '("STATUS" . "BENIGN") '("CONTEXT" . "DECISION"))
  (golden-check-row stage (golden-cpd bn "CHOICE") 10.0d0
                    '(("CHECKPOINT" . 0.1d0) ("BYPASS" . 0.9d0))
                    '("STATUS" . "THREAT") '("CONTEXT" . "DECISION"))
  (golden-check-row stage (golden-cpd bn "CHOICE") 20.0d0
                    '(("LEFT" . 0.5d0)
                      ("RIGHT" . 0.5d0))
                    '("STATUS" . "BENIGN") '("CONTEXT" . "ORDINARY")))

(defun golden-test-non-latent-insertion ()
  (let ((stage "non-latent public insertion"))
    (golden-reset-memory)
    (golden-insert (golden-model))
    (golden-insert (golden-hard-observation))
    (let ((bn (golden-memory-model)))
      (golden-check-hard-update stage bn)
      (dolist (var '("STATUS" "CONTEXT" "CHOICE"))
        (golden-close stage (format nil "~A CPD count increments once" var)
                      101.0d0 (rule-based-cpd-count (golden-cpd bn var)))))))

(defun golden-test-clamped-em ()
  (let* ((stage "clamped EM equivalence")
         (bn (golden-model :latent-status-p t))
         (status-id (rule-based-cpd-dependent-id (golden-cpd bn "STATUS")))
         (e (golden-hard-evidence bn "STATUS" "BENIGN"
                                  "CONTEXT" "DECISION"
                                  "CHOICE" "CHECKPOINT"))
         (updated
           (golden-quietly
             (online-em-step bn (list status-id) e
                             :step-size 1.0d0 :equivalent-sample-size 1.0d0
                             :latent-perturbation 0.0d0 :iteration 1
                             :current-sample-already-counted-p nil
                             :decay-statistics-p nil
                             :update-latent-child-cpds-p t))))
    (golden-close stage "E-step q(BENIGN)" 1.0d0
                  (golden-inferred-prob bn e "STATUS" "BENIGN"))
    ;; ONLINE-EM-STEP updates latent-affected CPDs. CONTEXT is updated later
    ;; by staged public insertion, so it stays at its original row here.
    (golden-check-hard-update stage updated :context-updated-p nil)))

(defun golden-hidden-values ()
  (multiple-value-bind (qb qt) (golden-hidden-bypass-responsibilities)
    (values qb qt
            (/ (+ 71.0d0 qb) 102.0d0)
            (/ (+ 30.0d0 qt) 102.0d0)
            (/ 41.0d0 (+ 51.0d0 qb))
            (/ (+ 10.0d0 qb) (+ 51.0d0 qb))
            (/ 1.0d0 (+ 10.0d0 qt))
            (/ (+ 9.0d0 qt) (+ 10.0d0 qt)))))

(defun golden-hidden-em-step (bn)
  (let* ((status-id (rule-based-cpd-dependent-id (golden-cpd bn "STATUS")))
         (e (golden-hard-evidence bn "CONTEXT" "DECISION"
                                  "CHOICE" "BYPASS")))
    (values
     (golden-quietly
       (online-em-step bn (list status-id) e
                       :step-size 1.0d0 :equivalent-sample-size 1.0d0
                       :latent-perturbation 0.0d0 :iteration 1
                       :current-sample-already-counted-p nil
                       :decay-statistics-p nil
                       :update-latent-child-cpds-p t))
     e)))

(defun golden-check-hidden-update (stage bn &key (context-updated-p nil))
  (multiple-value-bind (qb qt pb pt bcp bb tcp tb) (golden-hidden-values)
    (golden-check-row stage (golden-cpd bn "STATUS") 102.0d0
                      `(("BENIGN" . ,pb) ("THREAT" . ,pt)))
    (golden-check-row stage (golden-cpd bn "CHOICE") (+ 51.0d0 qb)
                      `(("CHECKPOINT" . ,bcp) ("BYPASS" . ,bb))
                      '("STATUS" . "BENIGN") '("CONTEXT" . "DECISION"))
    (golden-check-row stage (golden-cpd bn "CHOICE") (+ 10.0d0 qt)
                      `(("CHECKPOINT" . ,tcp) ("BYPASS" . ,tb))
                      '("STATUS" . "THREAT") '("CONTEXT" . "DECISION"))
    (golden-check-row stage (golden-cpd bn "CONTEXT")
                      (if context-updated-p 102.0d0 101.0d0)
                      (if context-updated-p
                          `(("DECISION" . ,(/ 62.0d0 102.0d0))
                            ("ORDINARY" . ,(/ 40.0d0 102.0d0)))
                          `(("DECISION" . ,(/ 61.0d0 101.0d0))
                            ("ORDINARY" . ,(/ 40.0d0 101.0d0)))))))

(defun golden-test-hidden-em ()
  (let* ((stage "hidden-status EM")
         (bn (golden-after-hard-model :latent-status-p t))
         (e (golden-hard-evidence bn "CONTEXT" "DECISION"
                                  "CHOICE" "BYPASS")))
    (multiple-value-bind (qb qt) (golden-hidden-bypass-responsibilities)
      (golden-close stage "E-step q(BENIGN)" qb
                    (golden-inferred-prob bn e "STATUS" "BENIGN"))
      (golden-close stage "E-step q(THREAT)" qt
                    (golden-inferred-prob bn e "STATUS" "THREAT")))
    (multiple-value-bind (updated ignored) (golden-hidden-em-step bn)
      (declare (ignore ignored))
      (golden-check-hidden-update stage updated)
      (multiple-value-bind (qb qt pb pt bcp bb tcp tb) (golden-hidden-values)
        (declare (ignore qb qt bcp tcp))
        (multiple-value-bind (expected-threat ignored-benign)
            (golden-oracle-posterior pt tb pb bb)
          (declare (ignore ignored-benign))
          (let ((post-e (golden-hard-evidence updated "CONTEXT" "DECISION"
                                              "CHOICE" "BYPASS")))
            (golden-close stage "post-EM threat inference" expected-threat
                          (golden-inferred-prob
                           updated post-e "STATUS" "THREAT"))))))))

(defun golden-test-uninformative-em ()
  (let ((stage "uninformative latent observation"))
    (multiple-value-bind (updated ignored)
        (golden-hidden-em-step
         (golden-after-hard-model :latent-status-p t))
      (declare (ignore ignored))
      (let* ((status (golden-cpd updated "STATUS"))
             (prior-b (golden-prob status "BENIGN"))
             (prior-t (golden-prob status "THREAT"))
             (status-id (rule-based-cpd-dependent-id status))
             (e (golden-hard-evidence updated "CONTEXT" "ORDINARY"
                                      "CHOICE" "LEFT")))
        (golden-close stage "E-step q(BENIGN) equals prior" prior-b
                      (golden-inferred-prob updated e "STATUS" "BENIGN"))
        (golden-close stage "E-step q(THREAT) equals prior" prior-t
                      (golden-inferred-prob updated e "STATUS" "THREAT"))
        (let* ((after
                 (golden-quietly
                   (online-em-step
                    updated (list status-id) e
                    :step-size 1.0d0 :equivalent-sample-size 1.0d0
                    :latent-perturbation 0.0d0 :iteration 1
                    :current-sample-already-counted-p nil
                    :decay-statistics-p nil
                    :update-latent-child-cpds-p t)))
               (after-status (golden-cpd after "STATUS")))
          (golden-close stage "BENIGN probability does not sharpen" prior-b
                        (golden-prob after-status "BENIGN"))
          (golden-close stage "THREAT probability does not sharpen" prior-t
                        (golden-prob after-status "THREAT")))))))

(defun golden-test-compression ()
  (let* ((stage "atomic/compressed equivalence")
         (atomic (golden-after-hard-model :latent-status-p t))
         (compressed (golden-compressed-copy atomic)))
    (dolist (spec '(("STATUS" "BENIGN" nil)
                    ("STATUS" "THREAT" nil)
                    ("CHOICE" "CHECKPOINT"
                     (("STATUS" . "BENIGN") ("CONTEXT" . "DECISION")))
                    ("CHOICE" "BYPASS"
                     (("STATUS" . "THREAT") ("CONTEXT" . "DECISION")))))
      (destructuring-bind (var label parents) spec
        (golden-close
         stage (format nil "pre-EM ~A=~A ~S" var label parents)
         (apply #'golden-prob (golden-cpd atomic var) label parents)
         (apply #'golden-prob (golden-cpd compressed var) label parents))))
    (multiple-value-bind (atomic-after ignored-a) (golden-hidden-em-step atomic)
      (declare (ignore ignored-a))
      (multiple-value-bind (compressed-after ignored-c)
          (golden-hidden-em-step compressed)
        (declare (ignore ignored-c))
        (dolist (spec '(("STATUS" "BENIGN" nil)
                        ("STATUS" "THREAT" nil)
                        ("CHOICE" "CHECKPOINT"
                         (("STATUS" . "BENIGN") ("CONTEXT" . "DECISION")))
                        ("CHOICE" "BYPASS"
                         (("STATUS" . "BENIGN") ("CONTEXT" . "DECISION")))
                        ("CHOICE" "CHECKPOINT"
                         (("STATUS" . "THREAT") ("CONTEXT" . "DECISION")))
                        ("CHOICE" "BYPASS"
                         (("STATUS" . "THREAT") ("CONTEXT" . "DECISION")))))
          (destructuring-bind (var label parents) spec
            (golden-close
             stage (format nil "post-EM ~A=~A ~S" var label parents)
             (apply #'golden-prob (golden-cpd atomic-after var) label parents)
             (apply #'golden-prob
                    (golden-cpd compressed-after var) label parents))))))))

(defun golden-test-public-latent-path ()
  (let ((stage "public latent insertion and retrieval"))
    (golden-reset-memory)
    ;; The ordinary-insertion test independently verifies the preceding hard
    ;; event. Seed that known state here so this stage isolates latent insertion.
    (golden-insert (golden-after-hard-model :latent-status-p t))
    (let ((before (golden-memory-model)))
      (golden-close stage "pre-latent CONTEXT row count" 101.0d0
                    (golden-count (golden-cpd before "CONTEXT")
                                  "DECISION"))
      (golden-close stage "pre-latent CONTEXT CPD count" 101.0d0
                    (rule-based-cpd-count (golden-cpd before "CONTEXT"))))
    (let ((event (golden-latent-bypass-observation)))
      (golden-close stage "incoming latent event CONTEXT row count" 1.0d0
                    (golden-count (golden-cpd event "CONTEXT")
                                  "DECISION"))
      (golden-close stage "incoming latent event CONTEXT CPD count" 1.0d0
                    (rule-based-cpd-count (golden-cpd event "CONTEXT")))
      (golden-insert event))
    (let ((bn (golden-memory-model)))
      (golden-check-hidden-update stage bn :context-updated-p t)
      (dolist (var '("STATUS" "CONTEXT" "CHOICE"))
        (golden-close stage (format nil "~A CPD count increments once" var)
                      102.0d0 (rule-based-cpd-count (golden-cpd bn var))))
      (multiple-value-bind (qb qt pb pt bcp bb tcp tb) (golden-hidden-values)
        (declare (ignore qb qt bcp tcp))
        (multiple-value-bind (expected-threat ignored-benign)
            (golden-oracle-posterior pt tb pb bb)
          (declare (ignore ignored-benign))
          (multiple-value-bind (families marginals)
              (golden-quietly
                (remember eltm* (golden-cue "DECISION" "BYPASS")
                          '+ 1.0d0 nil :type "observation"
                          :soft-likelihoods nil))
            (declare (ignore families))
            (golden-close
             stage "REMEMBER P(THREAT | BYPASS, DECISION)"
             expected-threat
             (golden-prob (golden-factor marginals "STATUS") "THREAT"))))))))

(defun golden-run-all ()
  (setq *golden-results* nil)
  (golden-run-stage "functional-node generation"
                    #'golden-test-functional-generation)
  (golden-run-stage "pre-insertion inference" #'golden-test-inference)
  (golden-run-stage "non-latent public insertion"
                    #'golden-test-non-latent-insertion)
  (golden-run-stage "clamped EM equivalence" #'golden-test-clamped-em)
  (golden-run-stage "hidden-status EM" #'golden-test-hidden-em)
  (golden-run-stage "uninformative latent observation"
                    #'golden-test-uninformative-em)
  (golden-run-stage "atomic/compressed equivalence"
                    #'golden-test-compression)
  (golden-run-stage "public latent insertion and retrieval"
                    #'golden-test-public-latent-path)
  (multiple-value-bind (passed failed) (golden-write-report)
    (format t "~&HEMS golden system tests: ~D passed, ~D failed.~%"
            passed failed)
    (format t "Report: ~A~%" (namestring *golden-report-path*))
    (values passed failed)))

(multiple-value-bind (passed failed) (golden-run-all)
  (declare (ignore passed))
  (uiop:quit (if (zerop failed) 0 1)))


