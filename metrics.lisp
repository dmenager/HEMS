(in-package :hems)

(defparameter parents* (make-hash-table :test #'equal))
(defparameter id-cpd-hash* (make-hash-table :test #'equal))

; If key is in hashtable, return the corresponding value.
; If it isn't, create that entry using (compute-value key var-args-lst0 var-args-lst1 ...),
; then return it.
(defun hash-cache (hashtable key compute-value &rest var-args-lst)
  (multiple-value-bind (res foundp)
      (gethash key hashtable)
    (if foundp
	res
	(setf res (funcall compute-value key var-args-lst)))))

;; return hash table that maps each node id of episode onto its cpd
(defun get-cpd-from-id-aux (episode)
  (loop
    with bn = (car (episode-states episode))
    with hash = (make-hash-table :test #'equal)
    for cpd being the elements of (car bn)
    do
       (setf (gethash (rule-based-cpd-dependent-id cpd) hash) cpd)
    finally
       (return hash)))

;; return hash table that maps each cpd of episode onto a list of its parents
(defun get-parents-aux (episode)
  (loop
    for cpd being the elements of (caar (episode-states episode))
    do
       (loop
	 named finder
	 for parent being the hash-keys of (rule-based-cpd-identifiers cpd)
	   using (hash-value idx)
	 when (> idx 0)
	   collect (gethash parent id-cpd-hash*) into parents
	 finally
	    (return parents))))

;; Given a cpd from episode, return its list of parents 
(defun get-parents (episode cpd)
  (gethash cpd (hash-cache parents* (episode-id episode) #'get-parents-aux)))

;; Given a cpd from episode, return its dependent id 
(defun get-cpd-from-id (episode cpd-id)
  (gethash cpd (hash-cache id-cpd-hash* (episode-id episode) #'get-cpd-from-id-aux)))

#|Get the list of possible values dependent variable can take. renamed version of (get-values) |#

;; cpd = conditional probability distribution
(defun get-cpd-values (cpd)
  (gethash 0 (rule-based-cpd-var-values cpd)))

(defun get-nodes (episode)
  (caar (episode-states episode)))

(defun get-prob (cpd assns)
  (let ((rule (make-rule
	       :conditions (make-hash-table :test #'equal))))
    (loop
      for binding in assns
      do
	 (setf (gethash (car binding) (rule-conditions rule)) (cdr binding)))
    (rule-probability (car (get-compatible-rules cpd cpd rule :find-all nil)))))

#| Look up the probability of an assignment in a given CPD |#

;; cpd = conditional probability distribution
;; assn = list of variable value bindings
(defun P[X=x!Pa=a] (cpd dep-id-val parent-assns)
  (let ((assns (cons (cons (rule-based-cpd-dependent-id cpd) dep-id-val)
		     parent-assns)))
    (get-prob cpd assns)))

;; Takes a list of the parents and the possible values they can take, and outputs a list of all possible assignments, e.g.,
;; INPUT: (('foo . (1 2)) ('bar . (10 20 30 40)) ('baz . (100 200 300)))
;; OUTPUT: ((('baz . 300) ('bar . 40) ('foo . 2))
;;          (('baz . 200) ('bar . 40) ('foo . 2))
;;          ...
;;          (('baz . 100) ('bar . 10) ('foo . 1)))
(defun assignment-combinations (possible-vals)
  (let (results)
    (labels ((aux (pv assignments)
               (if pv
                   (let ((name (caar pv))
                         (vals (cdar pv)))
                     (loop for val in vals
                           do (aux (cdr pv)
                                   (cons (cons name val) assignments))))
                   (setq results (cons assignments results)))))
      (progn (aux possible-vals nil)
             results))))

;; Assumes that nil is not a valid value for the hash table
(defun lookup-or-nil (hash key)
  (multiple-value-bind (ret foundp)
      (gethash key hash)
    (declare (ignore foundp))
    ret))

;; Returns list of possible values `node` can take that are consistent with `observations`
;; Which is to say, it returns all values `node` can take if it hasn't been observed, and a list
;; containing just the observed value if it was.
;; `observations` is a hash table.
(defun valid-assignments (node observations)
  (let ((observed (lookup-or-nil observations (rule-based-cpd-dependent-id node))))
    (if observed
	(list observed)
	(get-cpd-values node))))

;; OUTPUT: same as assignment-combinations
(defun valid-parent-assignments (episode node observations)
  ;; parent-assignments: (('foo . (1 2)) ('bar . (10 20 30 40)) ('baz . (100 200 300)))
  ;; where 'foo, 'bar, 'baz are the parents of `node`
  (let ((parent-assignments 
          (loop for parent in (get-parents episode node)
                collect (cons parent (valid-assignments parent observations)))))
    (assignment-combinations parent-assignments)))


;; Entropy of node given parent assignment
;; H_P(X_i | pa_i) is the entropy of X_i given a specific instantiation pa_i of its parents
;; H_P(X_i | pa_i) = -\sum_{x \in X_i} P(X_i=x | pa_i) log P(X_i=x | pa_i)
(defun H[X!Pa=a] (X parent-assignments observations)
  (* -1                                                    
     (loop for val in (valid-assignments X observations)
           sum (let ((prob (P[X=x!Pa=a] X val parent-assignments observations)))
                 (* prob (log prob 2))))))


#| returns the posterior distribution of episode's bn given a set of observations |# 
(defun infer-posterior (episode obs)
  (loop
    with bn = (car (episode-states episode))
    with cpd and val
    with evidence = (make-hash-table :test #'equal)
    for cpd-id being the hash-keys of obs
      using (hash-value val-idx)
    do
       (setq cpd (get-cpd-from-id episode cpd-id))
       (setq val (caar (nth val-idx (gethash 0 (rule-based-cpd-var-value-block-map cpd)))))
       (setf (gethash cpd-id evidence) val)
    finally
       (return (loopy-belief-propagation bn evidence '+ 1))))



;; assns = assignments we want to know the posterior probability of 
;; obs = hash-table of observed assignments
(defun P[A=a!obs] (bn assns obs-posterior)
  (loop
    with marginal-posterior
    for cpd being the elements of (car obs-posterior)
    when (assoc (rule-based-cpd-dependent-id cpd) assns :test #'equal)
      collect (rule-based-cpd-dependent-id cpd) into nodes-to-keep
    else
      collect (rule-based-cpd-dependent-id cpd) into nodes-to-remove
    finally
       (setq marginal-posterior (factor-operation cpd nodes-to-keep nodes-to-remove '+))
       (loop
	 with prob = 1
	 for cpd in marginal-posterior
	 do
	    (setq prob (* (get-prob cpd assns) prob)))))

; Entropy of node given parents
; H_P(X_i | Pa_i) = \sum_{pa_i} P(pa_i) H_P(X_i | pa_i)
(defun H[X!Pa] (episode X observations)
  (loop
    with bn = (car (episode-states episode))
    with posterior = (infer-posterior episode observations)
    for pa in (valid-parent-assignments episode X observations)
          sum (* (P[A=a!obs] bn pa posterior)
                 (H[X!Pa=a] X pa observations))))

; Entropy of bayesian net
; H_P(X) = \sum_i H_P(X_i | Pa_i^G)
(defun H[bn] (episode observations)
  (loop
    for node being the elements of (get-nodes episode)
    sum (H[X!Pa] episode node observations)))
