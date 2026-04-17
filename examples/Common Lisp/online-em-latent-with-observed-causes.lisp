(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :hems)

(in-package :hems)

;; Synthetic ground truth:
;;
;;   A -> H -> E
;;   D -> H
;;
;; A, D, H, and E are all percept nodes. A and D are observed causes of H.
;; H is hidden in each training instance. E is an observed effect of H.
;; The training-set CSV records H only for evaluation; online EM receives
;; evidence for A, D, and E only.

(defparameter *online-em-middle-latent-seed* 424242)

(defun middle-lcg-next (state)
  (mod (+ (* 1103515245 state) 12345) 2147483648))

(defun middle-lcg-random-unit (state)
  (let ((next (middle-lcg-next state)))
    (values (/ next 2147483648.0d0) next)))

(defun middle-bernoulli-label (state p true-label false-label)
  (multiple-value-bind (u next) (middle-lcg-random-unit state)
    (values (if (< u p) true-label false-label) next)))

(defun middle-demo-ground-truth-prob (var value &key a d h)
  (cond
    ((and (equal var "A") (equal value "HIGH")) 0.45d0)
    ((and (equal var "D") (equal value "ON")) 0.55d0)
    ((and (equal var "H") (equal value "HOT") (equal a "LOW") (equal d "OFF"))
     0.10d0)
    ((and (equal var "H") (equal value "HOT") (equal a "LOW") (equal d "ON"))
     0.30d0)
    ((and (equal var "H") (equal value "HOT") (equal a "HIGH") (equal d "OFF"))
     0.60d0)
    ((and (equal var "H") (equal value "HOT") (equal a "HIGH") (equal d "ON"))
     0.80d0)
    ((and (equal var "E") (equal value "YES") (equal h "COLD")) 0.15d0)
    ((and (equal var "E") (equal value "YES") (equal h "HOT")) 0.90d0)
    (t
     (error "No middle-demo ground-truth probability for ~S=~S with A=~S D=~S H=~S"
            var value a d h))))

(defun middle-cpd-by-var (bn var)
  (loop for cpd being the elements of (car bn)
        when (equal (rule-based-cpd-dependent-var cpd) var)
          return cpd
        finally (error "No CPD with dependent var ~S" var)))

(defun middle-cpd-index-by-var (bn var)
  (loop for cpd being the elements of (car bn)
        for i from 0
        when (equal (rule-based-cpd-dependent-var cpd) var)
          return i
        finally (error "No CPD with dependent var ~S" var)))

(defun middle-cpd-value-index (cpd ident label)
  (let* ((pos (gethash ident (rule-based-cpd-identifiers cpd)))
         (vvbm (gethash pos (rule-based-cpd-var-value-block-map cpd))))
    (loop for entry in vvbm
          when (equal (caar entry) label)
            return (cdar entry)
          finally
             (error "No value ~S for identifier ~S in CPD ~S"
                    label ident (rule-based-cpd-dependent-id cpd)))))

(defun middle-set-rule-probability-for-labels
    (cpd probability dep-label &rest parent-labels)
  (let* ((dep-id (rule-based-cpd-dependent-id cpd))
         (dep-idx (middle-cpd-value-index cpd dep-id dep-label))
         (parent-idxs
           (loop for (parent-id . label) in parent-labels
                 collect (cons parent-id
                               (middle-cpd-value-index cpd parent-id label)))))
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          when (and (equal (gethash dep-id (rule-conditions rule))
                           (list dep-idx))
                    (loop for (parent-id . idx) in parent-idxs
                          always (equal (gethash parent-id
                                                 (rule-conditions rule))
                                        (list idx))))
            do
               (setf (rule-probability rule) probability)
               (setf (rule-count rule) 2.0d0))))

(defun middle-cpd-rule-probability (cpd dep-label &rest parent-labels)
  (let* ((dep-id (rule-based-cpd-dependent-id cpd))
         (dep-idx (middle-cpd-value-index cpd dep-id dep-label))
         (query-rule (make-rule :conditions (make-hash-table :test #'equal))))
    (setf (gethash dep-id (rule-conditions query-rule)) (list dep-idx))
    (loop for (parent-id . label) in parent-labels
          do
             (setf (gethash parent-id (rule-conditions query-rule))
                   (list (middle-cpd-value-index cpd parent-id label))))
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          when (compatible-rule-p rule query-rule nil nil)
            sum (float (rule-probability rule) 1.0d0))))

(defun middle-expand-model-cpd (bn var identifiers)
  (let ((idx (middle-cpd-index-by-var bn var)))
    (multiple-value-bind (expanded ignored)
        (online-em-expand-cpd-over-identifiers (aref (car bn) idx) identifiers)
      (declare (ignore ignored))
      (setf (aref (car bn) idx) expanded)
      expanded)))

(defun middle-initialize-root-row (cpd true-label false-label true-prob)
  (middle-set-rule-probability-for-labels cpd true-prob true-label)
  (middle-set-rule-probability-for-labels cpd (- 1.0d0 true-prob) false-label)
  (update-cpd-rules cpd (rule-based-cpd-rules cpd) :check-prob-sum nil))

(defun middle-initialize-h-row (h-cpd a-id a-label d-id d-label hot-prob)
  (middle-set-rule-probability-for-labels
   h-cpd hot-prob "HOT" (cons a-id a-label) (cons d-id d-label))
  (middle-set-rule-probability-for-labels
   h-cpd (- 1.0d0 hot-prob) "COLD" (cons a-id a-label) (cons d-id d-label)))

(defun middle-initialize-effect-row (e-cpd h-id h-label yes-prob)
  (middle-set-rule-probability-for-labels
   e-cpd yes-prob "YES" (cons h-id h-label))
  (middle-set-rule-probability-for-labels
   e-cpd (- 1.0d0 yes-prob) "NO" (cons h-id h-label)))

(defun initialize-middle-demo-schema (bn)
  (let* ((a-cpd (middle-cpd-by-var bn "A"))
         (d-cpd (middle-cpd-by-var bn "D"))
         (h-cpd (middle-cpd-by-var bn "H"))
         (e-cpd (middle-cpd-by-var bn "E"))
         (a-id (rule-based-cpd-dependent-id a-cpd))
         (d-id (rule-based-cpd-dependent-id d-cpd))
         (h-id (rule-based-cpd-dependent-id h-cpd)))
    (middle-initialize-root-row
     a-cpd "HIGH" "LOW" (middle-demo-ground-truth-prob "A" "HIGH"))
    (middle-initialize-root-row
     d-cpd "ON" "OFF" (middle-demo-ground-truth-prob "D" "ON"))
    (setq h-cpd (middle-expand-model-cpd bn "H" (list h-id a-id d-id)))
    (setq e-cpd (middle-expand-model-cpd bn "E" (list (rule-based-cpd-dependent-id e-cpd) h-id)))
    (loop for a-label in '("LOW" "HIGH")
          do
             (loop for d-label in '("OFF" "ON")
                   do
                      (middle-initialize-h-row h-cpd a-id a-label d-id d-label 0.5d0)))
    (middle-initialize-effect-row
     e-cpd h-id "HOT" (middle-demo-ground-truth-prob "E" "YES" :h "HOT"))
    (middle-initialize-effect-row
     e-cpd h-id "COLD" (middle-demo-ground-truth-prob "E" "YES" :h "COLD"))
    (update-cpd-rules h-cpd (rule-based-cpd-rules h-cpd) :check-prob-sum nil)
    (update-cpd-rules e-cpd (rule-based-cpd-rules e-cpd) :check-prob-sum nil)))

(defun middle-demo-schema-bn ()
  "Build a model where latent H has observed parents and an observed child."
  (let ((bn
          (compile-program nil
            a = (percept-node a :values ((:value "HIGH" :probability 0.5d0 :count 1)
                                         (:value "LOW" :probability 0.5d0 :count 1)))
            d = (percept-node d :values ((:value "ON" :probability 0.5d0 :count 1)
                                         (:value "OFF" :probability 0.5d0 :count 1)))
            h = (percept-node h :latent-p t
                  :values ((:value "HOT") (:value "COLD")))
            e = (percept-node e :values ((:value "YES" :probability 0.5d0 :count 1)
                                         (:value "NO" :probability 0.5d0 :count 1)))
            a --> h
            d --> h
            h --> e)))
    (initialize-middle-demo-schema bn)
    bn))

(defun middle-demo-observed-instance (sample)
  "Compile one training instance. H is present as a latent domain only."
  (eval
   `(compile-program nil
      a = (percept-node a :value ,(getf sample :a))
      d = (percept-node d :value ,(getf sample :d))
      h = (percept-node h :latent-p t
            :values ((:value "HOT") (:value "COLD")))
      e = (percept-node e :value ,(getf sample :e))
      a --> h
      d --> h
      h --> e)))

(defun middle-demo-draw-sample (state)
  (multiple-value-bind (a state)
      (middle-bernoulli-label state
                              (middle-demo-ground-truth-prob "A" "HIGH")
                              "HIGH"
                              "LOW")
    (multiple-value-bind (d state)
        (middle-bernoulli-label state
                                (middle-demo-ground-truth-prob "D" "ON")
                                "ON"
                                "OFF")
      (multiple-value-bind (h state)
          (middle-bernoulli-label state
                                  (middle-demo-ground-truth-prob
                                   "H" "HOT" :a a :d d)
                                  "HOT"
                                  "COLD")
        (multiple-value-bind (e state)
            (middle-bernoulli-label state
                                    (middle-demo-ground-truth-prob
                                     "E" "YES" :h h)
                                    "YES"
                                    "NO")
          (values (list :a a :d d :h h :e e) state))))))

(defun middle-demo-training-set (n &key (seed *online-em-middle-latent-seed*))
  (loop with state = seed
        for i from 1 to n
        collect
        (multiple-value-bind (sample next-state)
            (middle-demo-draw-sample state)
          (setq state next-state)
          sample)))

(defun middle-demo-evidence-from-instance (model-bn instance-bn)
  "Use the compiled percept-node instance, then bind its labels to model IDs."
  (let ((evidence (make-hash-table :test #'equal)))
    (loop for var in '("A" "D" "E")
          for model-cpd = (middle-cpd-by-var model-bn var)
          for instance-cpd = (middle-cpd-by-var instance-bn var)
          for instance-evidence = (make-observations (vector instance-cpd))
          for instance-id = (rule-based-cpd-dependent-id instance-cpd)
          do
             (setf (gethash (rule-based-cpd-dependent-id model-cpd) evidence)
                   (gethash instance-id instance-evidence)))
    evidence))

(defun middle-h-hot-estimate (bn a-label d-label)
  (let* ((h-cpd (middle-cpd-by-var bn "H"))
         (a-id (rule-based-cpd-dependent-id (middle-cpd-by-var bn "A")))
         (d-id (rule-based-cpd-dependent-id (middle-cpd-by-var bn "D"))))
    (middle-cpd-rule-probability
     h-cpd "HOT" (cons a-id a-label) (cons d-id d-label))))

(defun middle-demo-abs-error (estimate a-label d-label &key (swap nil))
  (let ((truth (middle-demo-ground-truth-prob "H" "HOT" :a a-label :d d-label))
        (aligned (if swap (- 1.0d0 estimate) estimate)))
    (abs (- aligned truth))))

(defun write-middle-demo-training-set (samples path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "instance,a,d,h_hidden,e~%")
    (loop for sample in samples
          for i from 1
          do
             (format stream "~D,~A,~A,~A,~A~%"
                     i
                     (getf sample :a)
                     (getf sample :d)
                     (getf sample :h)
                     (getf sample :e)))))

(defun write-middle-demo-convergence
    (&key
       (n 1000)
       (seed *online-em-middle-latent-seed*)
       (training-path "online-em-middle-latent-training-set.csv")
       (convergence-path "online-em-middle-latent-convergence.csv")
       (target-a "HIGH")
       (target-d "ON")
       (step-size *online-em-default-step-size*)
       (equivalent-sample-size 2.0d0)
       (latent-perturbation 5.0d-2)
       (lr 1.0d0))
  (let* ((model (middle-demo-schema-bn))
         (samples (middle-demo-training-set n :seed seed))
         (h-id (rule-based-cpd-dependent-id (middle-cpd-by-var model "H")))
         (truth (middle-demo-ground-truth-prob
                 "H" "HOT" :a target-a :d target-d)))
    (write-middle-demo-training-set samples training-path)
    (with-open-file (stream convergence-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "seen_instances,target_a,target_d,truth_p_h_hot,raw_p_h_hot,aligned_p_h_hot,latent_label_swapped,abs_error_p_h_hot~%")
      (loop for sample in samples
            for seen from 1
            for instance-bn = (middle-demo-observed-instance sample)
            for evidence = (middle-demo-evidence-from-instance model instance-bn)
            do
               (setq model
                     (online-em-step model
                                     (list h-id)
                                     evidence
                                     :iteration seen
                                     :step-size step-size
                                     :equivalent-sample-size equivalent-sample-size
                                     :latent-perturbation latent-perturbation
                                     :update-latent-child-cpds-p nil
                                     :lr lr))
               (let* ((raw (middle-h-hot-estimate model target-a target-d))
                      (fixed-error (middle-demo-abs-error raw target-a target-d))
                      (swapped-error (middle-demo-abs-error
                                      raw target-a target-d :swap t))
                      (swap (< swapped-error fixed-error))
                      (aligned (if swap (- 1.0d0 raw) raw))
                      (error (min fixed-error swapped-error)))
                 (format stream "~D,~A,~A,~,8F,~,8F,~,8F,~A,~,8F~%"
                         seen
                         target-a
                         target-d
                         truth
                         raw
                         aligned
                         (if swap 1 0)
                         error))))
    (format t "~&Wrote ~D samples to ~A~%" n training-path)
    (format t "Wrote convergence trace to ~A~%" convergence-path)
    model))

(write-middle-demo-convergence :n 2000)
