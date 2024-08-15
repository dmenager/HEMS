(in-package :hems)

;;(defparameter parents* (make-hash-table :test #'equal))
;;(defparameter id-cpd-hash* (make-hash-table :test #'equal))

#|
; If key is in hashtable, return the corresponding value.
; If it isn't, create that entry using (compute-value key var-args-lst0 var-args-lst1 ...),
; then return it.
(defun hash-cache (hashtable key compute-value &rest var-args-lst)
  (multiple-value-bind (res foundp)
      (gethash key hashtable)
    (if foundp
	res
	(setf res (funcall compute-value key var-args-lst)))))
|#

;; return hash table that maps each node id of episode onto its cpd
(defun get-cpd-from-id-aux (episode)
  (loop
    with bn = (if (episode-temporal-p episode)
		  (episode-state-transitions episode)
		  (episode-observation episode))
    with hash = (make-hash-table :test #'equal)
    for cpd being the elements of (car bn)
    do
       (setf (gethash (rule-based-cpd-dependent-id cpd) hash) cpd)
    finally
       (return hash)))

;; TODO: use edges hash in network to get more efficient
(defun get-parents-aux (episode)
  (loop
    with parents-hash = (make-hash-table :test #'equal)
    with cpd1-ident
    for cpd1 being the elements of (car (if (episode-temporal-p episode)
					    (episode-state-transitions episode)
					    (episode-observation episode)))
    do
       (setq cpd1-ident (rule-based-cpd-dependent-id cpd1))
       (loop
	 with idx
	 for cpd2 being the elements of (car (if (episode-temporal-p episode)
						 (episode-state-transitions episode)
						 (episode-observation episode)))
	 do
	    (setq idx (gethash (rule-based-cpd-dependent-id cpd2)
			       (rule-based-cpd-identifiers cpd1)))
	 when (and idx (> idx 0))
	   collect cpd2 into cpd1-parents
	 finally
	    (setf (gethash cpd1-ident parents-hash) cpd1-parents))
    finally
    (return parents-hash)))
  
;; Given a cpd from episode, return its list of parents 
;;(defun get-parents (episode cpd)
;;  (gethash cpd (hash-cache parents* (episode-id episode) #'get-parents-aux)))

;; Given a cpd from episode, return its dependent id 
;;(defun get-cpd-from-id (episode cpd-id)
;;  (gethash cpd-id (hash-cache id-cpd-hash* (episode-id episode) #'get-cpd-from-id-aux)))

#|Get the list of possible values dependent variable can take. renamed version of (get-values) |#

;; cpd = conditional probability distribution
(defun get-cpd-values (cpd)
  (gethash 0 (rule-based-cpd-var-values cpd)))

(defun get-nodes (episode)
  (car (if (episode-temporal-p episode)
	   (episode-state-transitions episode)
	   (episode-observation episode))))

(defun get-prob (cpd assns)
  (let ((rule (make-rule
	       :conditions (make-hash-table :test #'equal))))
    (loop
      with val and compatible-rule
      for binding in assns
      do
	 (setf (gethash (car binding) (rule-conditions rule)) (cdr binding))
      finally
	 (setq compatible-rule (car (get-compatible-rules cpd cpd rule :find-all nil)))
	 (when nil
	   (print-cpd-rule compatible-rule))
      (return (rule-probability compatible-rule)))))

#| Look up the probability of an assignment in a given CPD |#

;; cpd = conditional probability distribution
;; assn = list of variable value bindings
(defun P[X=x!Pa=a] (cpd dep-id-val parent-assns)
  (let* ((assns (cons (cons (rule-based-cpd-dependent-id cpd) dep-id-val)
		     parent-assns))
	 (prob (get-prob cpd assns)))
    prob))

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
  ;;(format t "~%~%evidence:~%~S~%key:~%~S~%(gethash key hash): ~S" hash key (gethash key hash))
  (multiple-value-bind (ret foundp)
      (gethash key hash)
    (declare (ignore foundp))
    ret))

;; Returns list of possible values `node` can take that are consistent with `observations`
;; Which is to say, it returns all values `node` can take if it hasn't been observed, and a list
;; containing just the observed value if it was.
;; `observations` is a hash table.
(defun valid-assignments (bn node)
  (let ((observed-list (lookup-or-nil (getf bn :obs) (rule-based-cpd-dependent-id node))))
    (cond (observed-list
	   (loop
	     with res
	     for (observed . prob) in observed-list
	     do
		(setq res (assoc observed (gethash 0 (rule-based-cpd-var-value-block-map node)) :test #'equal :key #'car))
	     when (and res (> prob 0))
	       collect (cdar res)
		 into values
	     finally
		(if (null values)
		    (return (get-cpd-values node))
		    (return values))))
	  (t
	   (get-cpd-values node)))))

;; OUTPUT: same as assignment-combinations
(defun valid-parent-assignments (bn node)
  ;; parent-assignments: (('foo . (1 2)) ('bar . (10 20 30 40)) ('baz . (100 200 300)))
  ;; where 'foo, 'bar, 'baz are the parents of `node
  (let ((parent-assignments 
          (loop
	    with parents = (gethash (rule-based-cpd-dependent-id node) (getf bn :parents))
	    for parent in parents
            collect (cons (rule-based-cpd-dependent-id parent) (valid-assignments bn parent)))))
    (assignment-combinations parent-assignments)))


;; Entropy of node given parent assignment
;; H_P(X_i | pa_i) is the entropy of X_i given a specific instantiation pa_i of its parents
;; H_P(X_i | pa_i) = -\sum_{x \in X_i} P(X_i=x | pa_i) log P(X_i=x | pa_i)
(defun H[X!Pa=a] (bn X parent-assignments)
  (* -1                                                    
     (loop for val in (valid-assignments bn X)
           sum (let ((epsilon 1e-10)
		     (prob (P[X=x!Pa=a] X val parent-assignments)))
		 (if (< prob epsilon)
		     0.0
		     (* prob (log prob 2)))))))
  

#| returns the posterior distribution of episode's bn given a set of observations |# 
;; obs = array of cpds
(defun infer-posterior (cpds-hash episode obs)
  (let ((observation (make-hash-table :test #'equal))
	(net (if (episode-temporal-p episode)
		 (episode-state-transitions episode)
		 (episode-observation episode))))
    (multiple-value-bind (sol no-matches cost bindings)
	(new-maximum-common-subgraph obs net (make-hash-table) (make-hash-table))
      (declare (ignore sol no-matches cost))
      (loop
	 with mapped-id
	 for cpd being the elements of (car obs)
	 do
	   (setq mapped-id (gethash (rule-based-cpd-dependent-id cpd) bindings))
	   (when mapped-id
	     (setf (gethash mapped-id observation) (list (cons (caar (second (gethash 0 (rule-based-cpd-var-value-block-map cpd)))) 1))))
	 finally
	   (return
	     (loop
		with cpd and val
		with evidence = (make-hash-table :test #'equal)
		with res
		for cpd-id being the hash-keys of observation
		using (hash-value var)
		do
		  (setq cpd (gethash cpd-id cpds-hash))
		;;(setq val (caar (nth val-idx (gethash 0 (rule-based-cpd-var-value-block-map cpd)))))
		  (setf (gethash cpd-id evidence) var)
		finally
		  (setq res (loopy-belief-propagation net evidence '+ 1))
		  (return (loop
			     for cpd in res
			     when (rule-based-cpd-singleton-p cpd)
			     collect cpd into singletons
			     else
			     collect cpd into net
			     finally
			       (return (list net singletons observation))))))))))


;; assns = assignments we want to know the posterior probability of (bindings) 
;; obs = hash-table of observed assignments
(defun P[A=a] (bn assns)
  (loop
    with obs-posterior = (getf bn :net)
    with idents
    for cpd in obs-posterior
    collect
    (loop
      with marginalized-factor
      for ident being the hash-keys of (rule-based-cpd-identifiers cpd)
      when (and (equal ident (rule-based-cpd-dependent-id cpd))
		(assoc ident assns :test #'equal))
	collect ident into nodes-to-keep
      else
	collect ident into nodes-to-remove
      finally
	 (return
	      (cond (nodes-to-keep
		     (when nil
		       (format t "~%~%marginalizing:~%~S~%on variables to keep:~%~S~%variables to remove:~%~S" cpd nodes-to-keep nodes-to-remove))
		     (setq marginalized-factor (factor-operation cpd nodes-to-keep nodes-to-remove '+))
		     (normalize-rule-probabilities marginalized-factor (rule-based-cpd-dependent-id marginalized-factor)))
		    (t
		     nil))))
      into marginal-posterior
    finally
       (when nil
	   (format t "~%~%marginal posterior:~%~S" marginal-posterior))
       (return
	 (let ((rule (make-rule :conditions (make-hash-table :test #'equal))))
	   (loop
	     for (ident . val) in assns
	     do
		(setf (gethash ident (rule-conditions rule)) val))
	   (loop
	     with prob = 1 and summed-prob
	     with obs-posterior = (getf bn :net)
	     with compatible-rules
	     for cpd in marginal-posterior
	     when (and cpd (gethash (rule-based-cpd-dependent-id cpd) (rule-conditions rule)))
	       do
		  (setq compatible-rules (get-compatible-rules cpd cpd rule :find-all t))
		  (loop
		    for rule in compatible-rules
		    summing (rule-probability rule) into val
		    finally
		       (setq summed-prob val))
		  (when nil
		    (format t "~%assn:")
		    (print-cpd-rule rule)
		    (format t "~%compatible-rules:")
		    (map nil #'print-cpd-rule compatible-rules)
		    (break))
		  (setq prob (* prob summed-prob))
	     finally
		(return prob)))
	 #|
	 (loop
	   with prob = 1
	   for cpd in marginal-posterior
	   when cpd
	     do
		(setq prob (* (get-prob cpd assns) prob))
	   finally
	      (return prob))
	 |#
	 )))

; Entropy of node given parents
; H_P(X_i | Pa_i) = \sum_{pa_i} P(pa_i) H_P(X_i | pa_i)
(defun H[X!Pa] (bn X)
  ;;(format t "~%bn:~%~A" bn)
  ;;(break)
  (loop
    for pa in (valid-parent-assignments bn X)
    do
       (when nil
	 (format t "~%pa:~%~A~%P(A=a) = ~d" pa (P[A=a] bn pa)))
    when pa
      sum (* (P[A=a] bn pa)
             (H[X!Pa=a] bn X pa))
	into res
    else
      do
	 (setq res (H[X!Pa=a] bn X pa))
    finally
       (return res)))

; Entropy of bayesian net
; H_P(X) = \sum_i H_P(X_i | Pa_i^G)
(defun H[bn] (episode observations)
  (loop
    with parents = (get-parents-aux episode)
    with cpds = (get-cpd-from-id-aux episode)
    with res = (infer-posterior cpds episode observations)
    with net = (first res) and singletons = (second res) and obs = (third res)
    ;; bn is everything that is constant for all remaining computations
    with bn = (list :net net :singletons singletons :cpds cpds :parents parents :obs obs) 
    for node being the elements of (get-nodes episode)
    ;;do
    ;;   (format t "~%H[~A|Pa] = ~d~%" (rule-based-cpd-dependent-id node) (H[X!Pa] bn node)) 
    sum (H[X!Pa] bn node) into entropy
    finally
    (return (values net entropy))))

(defun condition-handler (condition)
  (log-message (list "~%~%lisp backtrace:~%~A~%error:~%~S" (sb-debug:list-backtrace) condition) "log.txt" :if-exists :supersede))

(defun get-entropy (episode observations)
  (multiple-value-bind (net entropy)
      (handler-bind ((condition #'condition-handler))
	(H[bn] episode observations))
    (log-message (list "~%entropy value: ~d" entropy) "log.txt")
    (values net entropy)))

#| TESTS

(ql:quickload :hems)
(hems:load-eltm-from-file "~/Code/itm/components/decision_analyzer/event_based_diagnosis/eltm.txt")

(let (bn)
  (setq bn (hems:compile-program
	    nil
	    c1 = (percept-node PAIN :value "unknown")
	    c2 = (percept-node RESPRATE :value "low")
	    c3 = (percept-node HEARTRATE :value "high")
	    c4 = (percept-node O2SAT :value "low")
	    c5 = (relation-node CHEST_COLLAPSE :value "T" :kb-concept-id "INJURY")
	    c6 = (relation-node AMPUTATION :value "T" :kb-concept-id "INJURY")
	    c7 = (percept-node TAG :value "delayed")))
  (multiple-value-bind (net entropy)
      (hems:get-entropy
       (car
	(hems:new-retrieve-episode
	 (hems:get-eltm)
	 (hems:create-episode
	  :observation bn)
	 nil))
       bn)
    (declare (ignore net))
    entropy))
  |#
