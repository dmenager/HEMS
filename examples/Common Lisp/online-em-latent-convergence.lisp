(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :hems)

(in-package :hems)

;; Synthetic ground truth:
;;
;;   H -> B
;;   H -> C
;;
;; H, B, and C are all percept nodes. H is hidden in each training instance.
;; The training-set CSV records H only for evaluation; online EM receives
;; evidence for B and C only.

(defparameter *online-em-latent-seed* 8675309)

(defun lcg-next (state)
  (mod (+ (* 1103515245 state) 12345) 2147483648))

(defun lcg-random-unit (state)
  (let ((next (lcg-next state)))
    (values (/ next 2147483648.0d0) next)))

(defun bernoulli-label (state p true-label false-label)
  (multiple-value-bind (u next) (lcg-random-unit state)
    (values (if (< u p) true-label false-label) next)))

(defun online-em-demo-ground-truth-prob (var value &key h)
  (cond
    ((and (equal var "H") (equal value "HOT")) 0.35d0)
    ((and (equal var "B") (equal value "YES") (equal h "COLD")) 0.20d0)
    ((and (equal var "B") (equal value "YES") (equal h "HOT")) 0.85d0)
    ((and (equal var "C") (equal value "YES") (equal h "COLD")) 0.70d0)
    ((and (equal var "C") (equal value "YES") (equal h "HOT")) 0.25d0)
    (t
     (error "No ground-truth probability for ~S=~S with H=~S"
            var value h))))

(defun cpd-by-var (bn var)
  (loop for cpd being the elements of (car bn)
        when (equal (rule-based-cpd-dependent-var cpd) var)
          return cpd
        finally (error "No CPD with dependent var ~S" var)))

(defun cpd-index-by-var (bn var)
  (loop for cpd being the elements of (car bn)
        for i from 0
        when (equal (rule-based-cpd-dependent-var cpd) var)
          return i
        finally (error "No CPD with dependent var ~S" var)))

(defun cpd-value-index (cpd ident label)
  (let* ((pos (gethash ident (rule-based-cpd-identifiers cpd)))
         (vvbm (gethash pos (rule-based-cpd-var-value-block-map cpd))))
    (loop for entry in vvbm
          when (equal (caar entry) label)
            return (cdar entry)
          finally
             (error "No value ~S for identifier ~S in CPD ~S"
                    label ident (rule-based-cpd-dependent-id cpd)))))

(defun set-rule-probability-for-labels (cpd probability dep-label &rest parent-labels)
  (let* ((dep-id (rule-based-cpd-dependent-id cpd))
         (dep-idx (cpd-value-index cpd dep-id dep-label))
         (parent-idxs
           (loop for (parent-id . label) in parent-labels
                 collect (cons parent-id
                               (cpd-value-index cpd parent-id label)))))
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

(defun cpd-rule-probability (cpd dep-label &rest parent-labels)
  (let* ((dep-id (rule-based-cpd-dependent-id cpd))
         (dep-idx (cpd-value-index cpd dep-id dep-label))
         (query-rule (make-rule :conditions (make-hash-table :test #'equal))))
    (setf (gethash dep-id (rule-conditions query-rule)) (list dep-idx))
    (loop for (parent-id . label) in parent-labels
          do
             (setf (gethash parent-id (rule-conditions query-rule))
                   (list (cpd-value-index cpd parent-id label))))
    (loop for rule being the elements of (rule-based-cpd-rules cpd)
          when (compatible-rule-p rule query-rule nil nil)
            sum (float (rule-probability rule) 1.0d0))))

(defun expand-model-cpd (bn var identifiers)
  (let ((idx (cpd-index-by-var bn var)))
    (multiple-value-bind (expanded ignored)
        (online-em-expand-cpd-over-identifiers (aref (car bn) idx) identifiers)
      (declare (ignore ignored))
      (setf (aref (car bn) idx) expanded)
      expanded)))

(defun initialize-observed-child-row (child-cpd h-id h-label yes-prob)
  (set-rule-probability-for-labels child-cpd 0.0d0 "NA" (cons h-id h-label))
  (set-rule-probability-for-labels child-cpd yes-prob "YES" (cons h-id h-label))
  (set-rule-probability-for-labels child-cpd (- 1.0d0 yes-prob) "NO" (cons h-id h-label)))

(defun initialize-online-em-demo-schema (bn)
  (let* ((h-cpd (cpd-by-var bn "H"))
         (h-id (rule-based-cpd-dependent-id h-cpd))
         (b-cpd (expand-model-cpd bn "B" (list (rule-based-cpd-dependent-id (cpd-by-var bn "B")) h-id)))
         (c-cpd (expand-model-cpd bn "C" (list (rule-based-cpd-dependent-id (cpd-by-var bn "C")) h-id))))
    (set-rule-probability-for-labels h-cpd 0.0d0 "NA")
    (set-rule-probability-for-labels h-cpd 0.50d0 "HOT")
    (set-rule-probability-for-labels h-cpd 0.50d0 "COLD")
    (initialize-observed-child-row b-cpd h-id "NA" 0.5d0)
    (initialize-observed-child-row b-cpd h-id "HOT"
                                   (online-em-demo-ground-truth-prob "B" "YES" :h "HOT"))
    (initialize-observed-child-row b-cpd h-id "COLD"
                                   (online-em-demo-ground-truth-prob "B" "YES" :h "COLD"))
    (initialize-observed-child-row c-cpd h-id "NA" 0.5d0)
    (initialize-observed-child-row c-cpd h-id "HOT"
                                   (online-em-demo-ground-truth-prob "C" "YES" :h "HOT"))
    (initialize-observed-child-row c-cpd h-id "COLD"
                                   (online-em-demo-ground-truth-prob "C" "YES" :h "COLD"))
    (update-cpd-rules h-cpd (rule-based-cpd-rules h-cpd) :check-prob-sum nil)
    (update-cpd-rules b-cpd (rule-based-cpd-rules b-cpd) :check-prob-sum nil)
    (update-cpd-rules c-cpd (rule-based-cpd-rules c-cpd) :check-prob-sum nil)))

(defun online-em-demo-schema-bn ()
  "Build the model structure using only percept nodes and known domains."
  (let ((bn
          (compile-program nil
            h = (percept-node h :latent-p t
                  :values ((:value "HOT") (:value "COLD")))
            b = (percept-node b :values ((:value "NA" :probability 0.0d0 :count 0)
                                         (:value "YES" :probability 0.5d0 :count 1)
                                         (:value "NO" :probability 0.5d0 :count 1)))
            c = (percept-node c :values ((:value "NA" :probability 0.0d0 :count 0)
                                         (:value "YES" :probability 0.5d0 :count 1)
                                         (:value "NO" :probability 0.5d0 :count 1)))
            h --> b
            h --> c)))
    (initialize-online-em-demo-schema bn)
    bn))

(defun online-em-demo-observed-instance (sample)
  "Compile one training instance. H is present as a latent domain only."
  (eval
   `(compile-program nil
      h = (percept-node h :latent-p t
            :values ((:value "HOT") (:value "COLD")))
      b = (percept-node b :value ,(getf sample :b))
      c = (percept-node c :value ,(getf sample :c))
      h --> b
      h --> c)))

(defun online-em-demo-draw-sample (state)
  (multiple-value-bind (h state)
      (bernoulli-label state
                       (online-em-demo-ground-truth-prob "H" "HOT")
                       "HOT"
                       "COLD")
    (multiple-value-bind (b state)
        (bernoulli-label state
                         (online-em-demo-ground-truth-prob "B" "YES" :h h)
                         "YES"
                         "NO")
      (multiple-value-bind (c state)
          (bernoulli-label state
                           (online-em-demo-ground-truth-prob "C" "YES" :h h)
                           "YES"
                           "NO")
        (values (list :h h :b b :c c) state)))))

(defun online-em-demo-training-set (n &key (seed *online-em-latent-seed*))
  (loop with state = seed
        for i from 1 to n
        collect
        (multiple-value-bind (sample next-state)
            (online-em-demo-draw-sample state)
          (setq state next-state)
          sample)))

(defun online-em-demo-evidence-from-instance (model-bn instance-bn)
  "Use the compiled percept-node instance, then bind its labels to model IDs."
  (let ((evidence (make-hash-table :test #'equal)))
    (loop for var in '("B" "C")
          for model-cpd = (cpd-by-var model-bn var)
          for instance-cpd = (cpd-by-var instance-bn var)
          for instance-evidence = (make-observations (vector instance-cpd))
          for instance-id = (rule-based-cpd-dependent-id instance-cpd)
          do
             (setf (gethash (rule-based-cpd-dependent-id model-cpd) evidence)
                   (gethash instance-id instance-evidence)))
    evidence))

(defun h-hot-estimate (bn)
  (cpd-rule-probability (cpd-by-var bn "H") "HOT"))

(defun online-em-demo-abs-error (estimate &key (swap nil))
  (let ((truth (online-em-demo-ground-truth-prob "H" "HOT"))
        (aligned (if swap (- 1.0d0 estimate) estimate)))
    (abs (- aligned truth))))

(defun write-online-em-demo-training-set (samples path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "instance,h_hidden,b,c~%")
    (loop for sample in samples
          for i from 1
          do
             (format stream "~D,~A,~A,~A~%"
                     i
                     (getf sample :h)
                     (getf sample :b)
                     (getf sample :c)))))

(defun write-online-em-demo-convergence
    (&key
       (n 1000)
       (seed *online-em-latent-seed*)
       (training-path "online-em-latent-training-set.csv")
       (convergence-path "online-em-latent-convergence.csv")
       (step-size *online-em-default-step-size*)
       (equivalent-sample-size 2.0d0)
       (latent-perturbation 5.0d-2)
       (lr 1.0d0))
  (let* ((model (online-em-demo-schema-bn))
         (samples (online-em-demo-training-set n :seed seed))
         (h-id (rule-based-cpd-dependent-id (cpd-by-var model "H"))))
    (write-online-em-demo-training-set samples training-path)
    (with-open-file (stream convergence-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "seen_instances,truth_p_h_hot,raw_p_h_hot,aligned_p_h_hot,latent_label_swapped,abs_error_p_h_hot~%")
      (loop for sample in samples
            for seen from 1
            for instance-bn = (online-em-demo-observed-instance sample)
            for evidence = (online-em-demo-evidence-from-instance model instance-bn)
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
               (let* ((raw (h-hot-estimate model))
                      (fixed-error (online-em-demo-abs-error raw))
                      (swapped-error (online-em-demo-abs-error raw :swap t))
                      (swap (< swapped-error fixed-error))
                      (aligned (if swap (- 1.0d0 raw) raw))
                      (error (min fixed-error swapped-error)))
                 (format stream "~D,~,8F,~,8F,~,8F,~A,~,8F~%"
                         seen
                         (online-em-demo-ground-truth-prob "H" "HOT")
                         raw
                         aligned
                         (if swap 1 0)
                         error))))
    (format t "~&Wrote ~D samples to ~A~%" n training-path)
    (format t "Wrote convergence trace to ~A~%" convergence-path)
    model))

(write-online-em-demo-convergence :n 1000)
