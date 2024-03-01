(in-package :hems)

#| Reference circular structures rather than print them all |#
(setf *print-circle* t)

#| Buffer for event cognition |#

;; obs = observation sequence in current event
;; h-model = hierarchical modle predicting future states to come
(defvar episode-buffer* (list ':obs (make-hash-table) ':h-model nil))

#| Episodic Long Term Memory |#
(defparameter eltm* nil)

#| Episodic memory episode |#

;; id = string of gensym for episode id
;; index-episode-id = id of first member episode in schema
;; parent = pointer to parent episode
;; observation = perceptions and inferred relations represented as a BN
;; state-transitions = state-transition graph and observation  model represented as a BN
;; temporal-p = flag for whether the episode represents a course of events (state transitions) or state of affairs (observation)
;; backlinks = hash table of episode ids to back-links references pointing to lower-level observation/state transition models in the event memory
;; abstraction-ptrs = list of pointers to higher level abstraction branch, if it exists
;; count = number of times episode occured
;; depth = depth of episode in episodic long-term memory. Fixed at insertion
;; lvl = abstraction level of the episode. Observations are lowest level, then, hierarchically abstracted state transition models are higher.
(defstruct episode id index-episode-id parent observation state-transitions temporal-p backlinks abstraction-ptrs count depth lvl)

#| Returns the episodic long-term memory structure |#

(defun get-eltm ()
  eltm*)



#| Performs deep copy on episode |#

;; ep = episode to copy
(defun copy-ep (ep &key (fresh-id t))
  (make-episode
   :id (if fresh-id (symbol-name (gensym "EPISODE-")) (episode-id ep))
   :index-episode-id (episode-index-episode-id ep)
   :parent (if (episode-parent ep) (episode-parent ep))
   :observation (copy-bn (episode-observation ep))
   :state-transitions (copy-bn (episode-state-transitions ep))
   :temporal-p (episode-temporal-p ep)
   :backlinks (copy-hash-table (episode-backlinks ep) :reference-values t)
   :abstraction-ptrs (copy-list (episode-abstraction-ptrs ep))
   :depth (episode-depth ep)
   :count (episode-count ep)
   :lvl (episode-lvl ep)))

#| Make a model for tracking state |#

;; ep = episode
;; cur-step = index pointing to current state in the stochastic process
;; scope = indicates number of consecutive states in obs window that model explains
;; model parent = pointer to parent model
(defun make-model (&key ep (cur-step 0) (scope 0) (model-parent nil) (evidence (make-hash-table :test #'equal)))
  (let (ep-copy)
    (when ep
      (setq ep-copy (copy-ep ep :fresh-id nil)))
    `(:model ,ep-copy
      :cur-step ,cur-step
      :scope ,scope
      :evidence ,evidence
      :model-parent ,model-parent)
    ))

#| Copy hierarchical model |#

;; model = model used for tracking state
(defun copy-model (model &key (scope-modifier 0) (step-modifier 0))
  (when model
    `(:model ,(getf model :model)
      :cur-step ,(+ (getf model :cur-step) step-modifier)
      :scope ,(+ (getf model :scope) scope-modifier)
      :evidence ,(copy-hash-table (getf model :evidence))
      :model-parent ,(copy-model (getf model :model-parent)))))

#| Print hierarchical model |#

;; model = model used for tracking state
(defun print-model-stack (model)
  (when model
    (format t "~%(model-id ~A level: ~d model-num-decompositions: ~d model-count: ~d cur-step: ~d scope: ~d model-parent: ~A)"
            (ignore-errors (episode-id (getf model :model)))
            (ignore-errors (episode-lvl (getf model :model)))
            (ignore-errors (hash-table-count (episode-id-ref-map (getf model :model))))
            (ignore-errors (episode-count (getf model :model)))
            (getf model :cur-step)
            (getf model :scope)
            (not (null (getf model :model-parent))))
    (print-model-stack (getf model :model-parent))))

#| Return names of models |#

;; model = model used for tracking state
(defun get-model-names-in-stack (model)
  (cond ((null model)
         nil)
        (t
         (cons (episode-id (getf model :model)) (get-model-names-in-stack (getf model :model-parent))))))

#| Incorporate contents of new episode into existing event memory content |#

;; ep1-states = states in episode1 (from event memory)
;; ep2-states = states in episode2 (from new episode)
;; mappings = matching with bindings (for each state) from ep2 nodes to ep1 nodes
;; unmatched = list of lists of cpd-index pairs. Each index represents an unmatched element in ep1 and cpd represents a dummy match
;; bbindings = list of bindings
(defun combine-states (ep1-states ep2-states ep1-count mappings unmatched bbindings)
  (loop
    with p and q
    with new-nodes and new-edges
    with states-ep1 = (mapcar #'(lambda (s) (copy-factors (car s))) ep1-states)
    and states-ep2 = (mapcar #'(lambda (s) (copy-factors (car s))) ep2-states)
    with state1-ptr = states-ep1 and state2-ptr = states-ep2
    with matches = mappings and bindings = bbindings and unmatched-qss = unmatched
    while (or state1-ptr state2-ptr)
    do
       (setq p (car state2-ptr))
       (setq q (car state1-ptr))
       (loop
         for (p-match . q-match) being the elements of (car matches)
         with node and nodes and p-cpd
         do
            (when nil (and (= cycle* 3) (equal "INTENTION2751" (rule-based-cpd-dependent-id (aref p p-match)))) nil q-match
                  (format t "~%~%p-cpd before subst:~%~S~%q-match:~%~S" (aref p p-match) (if q-match (aref q q-match))))
            (setq p-cpd (subst-cpd (aref p p-match) (when q-match (aref q q-match)) (car bindings) :deep nil))
            (when nil (and (= cycle* 3) (equal "INTENTION2751" (rule-based-cpd-dependent-id (aref p p-match)))) nil q-match
                  (format t "~%p-cpd after subst:~%~S" p-cpd))
            (when nil (and (equal "TURN_RIGHTNIL" (gethash 0 (rule-based-cpd-qualified-vars (aref p p-match))))
                           (equal "ACTIONNIL" (gethash 1 (rule-based-cpd-qualified-vars (aref p p-match)))))
                  (format t "~%p-match:~%~S~%p-cpd:~%~S~%q-cpd:~%~S" (aref p p-match) p-cpd (if q-match (aref q q-match)))
                  (break))
            (setq node (factor-merge p-cpd (if q-match (aref q q-match)) (car bindings) nodes ep1-count))
            ;;(format t "~%p-match:~%~S~%subst p-match:~%~S~%q-match:~%~S~%node:~%~S" (aref p p-match) p-cpd (if q-match (aref q q-match)) node)
            (setq nodes (nreverse (cons node (nreverse nodes))))
         finally
            (setq new-nodes nodes))
       (loop
         for (dummy-match . unmatched-q) in (car unmatched-qss)
         with dm and node
         do
            (when nil (and (= cycle* cycle*) (equal "NO_OP7332" (rule-based-cpd-dependent-id dummy-match)))
                  (format t "~%dummy-match:~%~S~%unmatched q:~%~S" dummy-match (aref q unmatched-q)))
            (setq dm (subst-cpd dummy-match (aref q unmatched-q) (car bindings) :deep nil))
            (setq node (factor-merge dm (aref q unmatched-q) (car bindings) new-nodes ep1-count))
            (when nil (and (= cycle* cycle*) (equal "NO_OP7332" (rule-based-cpd-dependent-id dummy-match)))
                  (format t "~%node:~%~S" node)
                  (break))
            (setq new-nodes (nreverse (cons node (nreverse new-nodes)))))
       (setq new-nodes (sort new-nodes #'higher-lvl-cpd))
       (setq new-nodes (make-array (length new-nodes) :initial-contents new-nodes :fill-pointer t))
       (setq new-edges (make-graph-edges new-nodes))
       (setq state1-ptr (rest state1-ptr))
       (setq state2-ptr (rest state2-ptr))
       (setq matches (rest matches))
       (setq bindings (rest bindings))
       (setq unmatched-qss (rest unmatched-qss))
    collect (cons new-nodes new-edges) into states
    finally
       (return states)))

#| Incorporate contents of new episode into existing event memory content |#

;; ep1-bn = bn in episode1 (from event memory)
;; ep2-bn = bn in episode2 (from new episode)
;; mappings = matching with bindings from ep2 nodes to ep1 nodes
;; unmatched = list of cpd-index pairs. Each index represents an unmatched element in ep1 and cpd represents a dummy match
;; bindings = bindings
(defun new-combine-bns (ep1-bn ep2-bn ep1-count mappings unmatched bindings)
  (let (p q new-nodes)
    (setq p (copy-factors (car ep2-bn)))
    (setq q (copy-factors (car ep1-bn)))
    (loop
      for (p-match . q-match) being the elements of mappings
      with node and nodes and p-cpd
      do
         (when nil
               (format t "~%~%p-cpd before subst:~%~S~%q-match:~%~S" (aref p p-match) (if q-match (aref q q-match))))
         (setq p-cpd (subst-cpd (aref p p-match) (when q-match (aref q q-match)) bindings :deep nil))
         (when nil
               (format t "~%p-cpd after subst:~%~S" p-cpd))
         (when nil
               (format t "~%p-match:~%~S~%p-cpd:~%~S~%q-cpd:~%~S" (aref p p-match) p-cpd (if q-match (aref q q-match)))
               (break)
	       )
         (setq node (factor-merge p-cpd (if q-match (aref q q-match)) bindings nodes ep1-count))
         ;;(format t "~%p-match:~%~S~%subst p-match:~%~S~%q-match:~%~S~%node:~%~S" (aref p p-match) p-cpd (if q-match (aref q q-match)) node)
         (setq new-nodes (cons node new-nodes)))
    (loop
      for (dummy-match . unmatched-q) in unmatched
      with dm and node
      do
         (when nil (and (= cycle* cycle*) (equal "NO_OP7332" (rule-based-cpd-dependent-id dummy-match)))
               (format t "~%dummy-match:~%~S~%unmatched q:~%~S" dummy-match (aref q unmatched-q)))
         (setq dm (subst-cpd dummy-match (aref q unmatched-q) bindings :deep nil))
         (setq node (factor-merge dm (aref q unmatched-q) bindings new-nodes ep1-count))
         (when nil (and (= cycle* cycle*) (equal "NO_OP7332" (rule-based-cpd-dependent-id dummy-match)))
               (format t "~%node:~%~S" node)
               (break))
         (setq new-nodes (cons node new-nodes)))
    (setq new-nodes (topological-sort new-nodes)) ;;(sort new-nodes #'higher-lvl-cpd))
    (setq new-nodes (make-array (length new-nodes) :initial-contents new-nodes :fill-pointer t))
    (cons new-nodes (make-graph-edges new-nodes))))

#| Update distribution over states |#

;; x = episode from event memory
;; y = new episode
;; decomp-bindings = bindings for merging decompositions
(defun merge-decompositions (x y decomp-bindings &aux q-first-bindings p-nodes)
  (when nil
    (format t "~%schema decompositions:~%~A~%num: ~d~%episode decompositions:~%~A~%num: ~d~%decomp bindings:~%~S" (episode-decompositions x) (episode-num-decompositions x) (episode-decompositions y) (episode-num-decompositions y) decomp-bindings))
  (setq q-first-bindings (make-hash-table :test #'equal))
  (setq p-nodes (make-hash-table :test #'equal))
  (loop
    for p being the hash-keys of decomp-bindings
      using (hash-value q)
    do
       (setf (gethash q q-first-bindings) p))
  (loop
    for cpd being the elements of (car (episode-decompositions y))
    for i from 0
    do
       (setf (gethash (rule-based-cpd-dependent-id cpd) p-nodes) i))
  (loop
    with merged and sol and no-matches
    with j
    for i from 0 to (- (episode-num-decompositions y) 1)
    do
       (cond ((< i (episode-num-decompositions x))
              (setq j i)
              (setf (gethash (rule-based-cpd-dependent-id (aref (car (episode-decompositions y)) i)) decomp-bindings)
                    (rule-based-cpd-dependent-id (aref (car (episode-decompositions x)) i))))
             (t
              (setq j nil)))
       (setq sol (cons (cons i j) sol))
    finally
       (setq decomp-bindings (list decomp-bindings))
       (setq sol (list (reverse sol)))
       (setq no-matches (list (make-na-matches-for-unmatched-cpds (episode-decompositions y) (episode-decompositions x) (car sol) (car decomp-bindings) q-first-bindings p-nodes)))
       (setq merged (car (combine-states (list (episode-decompositions x)) (list (episode-decompositions y)) (episode-count x) sol no-matches decomp-bindings)))
       (return merged)))

#| Combine episodes together to make probabilistic description |#

;; ep1 = existing episode in memory
;; ep2 = new episode
;; mappings = matching with bindings (for each state) from ep2 nodes to ep1 nodes
;; unmatched = list of cpd-index pairs. Each index represents an unmatched element in ep1 and cpd represents a dummy match
;; bindings = bindings
(defun combine (ep1 ep2 mappings unmatched bindings)
  ;;(format t "~%schema decopositions:~%~S~%num:~%~d~%episode decopositions:~%~S~%num:~%~d~%decomp bindings:~%~S" (episode-decompositions ep1) (episode-num-decompositions ep1) (episode-decompositions ep2) (episode-num-decompositions ep2) decomp-bindings)
  (make-episode :id (episode-id ep1)
                :index-episode-id (episode-index-episode-id ep1)
                :parent (if (episode-parent ep1) (episode-parent ep1))
                :count (+ (episode-count ep1) (episode-count ep2))
                :lvl (max (episode-lvl ep1) (episode-lvl ep2))
                :id-ref-map
                (loop
                  with new-id-ref-map  = (copy-hash-table (episode-id-ref-map ep1) :reference-values t)
                  for id being the hash-keys of (episode-id-ref-map ep2)
                    using (hash-value ref)
                  do
                     (setf (gethash id new-id-ref-map) ref)
                  finally
                     (return new-id-ref-map))
                :num-decompositions (max (episode-num-decompositions ep1) (episode-num-decompositions ep2))
                :states (combine-states (episode-states ep1) (episode-states ep2) (episode-count ep1) mappings unmatched bbindings)
                :decompositions (merge-decompositions ep1 ep2 decomp-bindings)
                :abstraction-ptrs (episode-abstraction-ptrs ep1)))

#| Combine episodes together to make probabilistic description |#

;; ep1 = existing episode in memory
;; ep2 = new episode
;; mappings = matching with bindings (for each state) from ep2 nodes to ep1 nodes
;; unmatched = list of cpd-index pairs. Each index represents an unmatched element in ep1 and cpd represents a dummy match
;; bindings = bindings
(defun new-combine (ep1 ep2 mappings unmatched bindings)
  ;;(format t "~%schema decopositions:~%~S~%num:~%~d~%episode decopositions:~%~S~%num:~%~d~%decomp bindings:~%~S" (episode-decompositions ep1) (episode-num-decompositions ep1) (episode-decompositions ep2) (episode-num-decompositions ep2) decomp-bindings)
  (make-episode :id (episode-id ep1)
                :index-episode-id (episode-index-episode-id ep1)
                :parent (if (episode-parent ep1) (episode-parent ep1))
                :count (+ (episode-count ep1) (episode-count ep2))
                :lvl (max (episode-lvl ep1) (episode-lvl ep2))
                :backlinks
                (loop
                  with new-id-ref-map  = (copy-hash-table (episode-backlinks ep1) :reference-values t)
                  for id being the hash-keys of (episode-backlinks ep2)
                    using (hash-value ref)
		  ;; check to see if id is bound already to ancestor id in ep1? If it is, then skip id
		  do
                     (setf (gethash id new-id-ref-map) ref)
                  finally
                     (return new-id-ref-map))
                :observation (if (episode-temporal-p ep2)
                                 (episode-observation ep1)
                                 (new-combine-bns (episode-observation ep1) (episode-observation ep2) (episode-count ep1) mappings unmatched bindings))
                :state-transitions (if (episode-temporal-p ep2)
                                       (new-combine-bns (episode-state-transitions ep1) (episode-state-transitions ep2) (episode-count ep1) mappings unmatched bindings)
                                       (episode-state-transitions ep1))
                :temporal-p (or (episode-temporal-p ep1) (episode-temporal-p ep2))
                :abstraction-ptrs (episode-abstraction-ptrs ep1)
		:depth (episode-depth ep1)))

#| Match decomposition states between episodes |#

;; x-decomp = event memory
;; y-decomp = cue
;; bic-p = flag for retrieval mode. T uses Bayesian Information Criterion, Nil uses likelihood
(defun match-decomp-states (x-decomp y-decomp bic-p)
  (loop
    with pattern-states = (ignore-errors (episode-states y-decomp))
    and base-states = (ignore-errors (episode-states (car x-decomp)))
    with pattern-state-pointer = pattern-states and base-state-pointer = base-states
    with costs
    while (or pattern-state-pointer base-state-pointer)
    do
       (multiple-value-bind (sol no-matches cost bindings unweighted-cost)
           (maximum-common-subgraph (car pattern-state-pointer) (car base-state-pointer) :cost-of-nil (if x-decomp (episode-count (car x-decomp)) 2) :bic-p bic-p)
         ;;(subgraph-greedy-monomorphism pattern-state base-state :cost-of-nil (if x-decomp (episode-count (car x-decomp)) 2) :bic-p bic-p)
         ;;(subgraph-optimal-monomorphism pattern-state base-state (if x (episode-count (car (dereference x)))))
         (declare (ignore bindings sol no-matches unweighted-cost))
         (setq costs (cons cost costs))
         ;;(setq unweighted-costs (cons unweighted-cost unweighted-costs))
         (setq pattern-state-pointer (rest pattern-state-pointer))
         (setq base-state-pointer (rest base-state-pointer)))
    finally
       (return (reduce '+ costs))))

(defun find-matching-decomp-state (ep-ref refs &aux match)
  (cond ((null ep-ref) nil)
        ((null refs) nil)
        ((setq match (car (member ep-ref refs
                                  :key #'car
                                  :test #'(lambda (ep ref)
                                            (string= (episode-id (car ep))
                                                     (episode-id (car ref)))))))
         (values (car match) (cdr match)))
        (t
         (find-matching-decomp-state (episode-parent (car ep-ref)) refs))))

#| Match decompositions between episodes |#

;; x = event memory
;; y = cue
;; costs = list of costs
;; bbindings = list of bindings
(defun match-decompositions (x y costs &key (bic-p t) (retrieve-p nil))
  (loop
    for i from 0 to (if retrieve-p
                        (- (episode-num-decompositions y) 1)
                        (- (max (episode-num-decompositions y)
                                (episode-num-decompositions (car x))) 1))
    with pattern-decomps = (car (episode-decompositions y))
    with base-decomps = (car (episode-decompositions (car x)))
    with pattern-decomp and base-decomp
    with p-ref-id and p-ref and base-refs
    with bindings = (make-hash-table :test #'equal)
    do
       (cond ((< i (array-dimension pattern-decomps 0))
              (setq pattern-decomp (aref pattern-decomps i))
              (setq p-ref-id (car (car (last (gethash 0 (rule-based-cpd-var-value-block-map pattern-decomp))))))
              (setq p-ref (gethash p-ref-id (episode-id-ref-map y))))
             (t
              (setq pattern-decomp nil)
              (setq p-ref-id nil)
              (setq p-ref nil)))
       (cond ((< i (array-dimension base-decomps 0))
              (setq base-decomp (aref base-decomps i))
              (setq base-refs (mapcan #'(lambda (att-val)
                                          (let (r)
                                            (setq r (gethash (car att-val) (episode-id-ref-map (car x))))
                                            (if r (list (cons r (car att-val))))))
                                      (gethash 0 (rule-based-cpd-var-value-block-map base-decomp)))))
             (t
              (setq base-decomp nil)
              (setq base-refs nil)))
       (multiple-value-bind (base-ep-ref base-ref-id)
           (find-matching-decomp-state p-ref base-refs)
         (when (and p-ref-id base-ref-id)
           (setf (gethash p-ref-id bindings) base-ref-id))
         (setq costs (cons (match-decomp-states p-ref base-ep-ref bic-p) costs)))
    finally
       (return (values costs bindings))))

#| Test branch to see if we should evaluate it |#

;; branch = event memory
;; ep = episode
;; reject-list = list of episode ids to reject and not expand/return
(defun reject-branch? (branch ep reject-list &key (check-decomps t) (check-abstraction-ptrs nil) (check-index-case nil))
  (cond ((and check-decomps
	      (= (hash-table-count (episode-backlinks (car branch))) 0)
	      (> (hash-table-count (episode-backlinks ep)) 0))
         (when nil
           (format t "~%rejecting ~A because backlinks don't match" (episode-id (car branch))))
         t)
        ((and check-decomps
	      (episode-parent (car branch))
	      (> (hash-table-count (episode-backlinks (car branch))) 0)
	      (= (hash-table-count (episode-backlinks ep)) 0))
         (when nil
           (format t "~%rejecting ~A because backlinks don't match" (episode-id (car branch))))
         t)
        #|
        ((and check-decomps (episode-parent (car branch)) (< (episode-num-decompositions (car branch)) (episode-num-decompositions ep)))
         (when nil
           (format t "~%rejecting ~A because branch has fewer than the required number of decompositions" (episode-id (car branch))))
         t)
        |#
        ((and check-abstraction-ptrs (episode-abstraction-ptrs (car branch)) (episode-abstraction-ptrs ep)
              (loop
                named matcher
                for ep-abstraction-ptr in (episode-abstraction-ptrs ep)
                when (member (episode-index-episode-id (car ep-abstraction-ptr)) (episode-abstraction-ptrs (car branch))
                             :test #'(lambda (id branch-abstr)
                                       (equal (episode-index-episode-id (car branch-abstr)) id)))
                  do
                     (return-from matcher nil)
                finally
                   (return-from matcher t)))
         (when nil
           (format t "~%rejecting ~A because it's not in episode's abstraction family." (episode-id (car branch))))
         t)
        ((and check-index-case (not (equal (episode-index-episode-id (car branch)) (episode-index-episode-id ep))))
         (when nil
           (format t "~%rejecting ~A because index cases are different." (episode-id (car branch))))
         t)
        ((and (car branch) (member (episode-id (car branch)) reject-list :test #'equal))
         (when nil
           (format t "~%rejecting ~A because its in forbidden list" (episode-id (car branch))))
         t)
        (t
         ;;(format t "~%passing ~A" (episode-id (car branch)))
         nil)))

#| Merge two episodes together|#

;; x = episodic memory
;; y = episode to merge into x
;; res = existing mapping and bindings
;; reject-list = list of episode ids to reject when merging
;; bic-p = flag to compute BIC
(defun ep-merge (x y res reject-list bic-p &aux generalized equivalent)
  ;;(format t "~%reject list: ~A~%root id: ~A" reject-list (if x (episode-id (car x))))
  (cond ((null x) (values x 0 (list (make-hash-table :test #'equal)) nil nil reject-list))
        ((reject-branch? x y reject-list :check-decomps nil)
         (values x 0 (list (make-hash-table :test #'equal)) nil nil (cons (episode-id (car x)) reject-list)))
        (res
         ;; combine the graphs according to the mapping
         (setq generalized (combine (car x) y (first res) (second res) (fourth res) (fifth res)))
         (setq equivalent (= 0 (third res)))
         (cond ((and equivalent (null (cdr x)))
                (setf (car x) generalized)
                (values x (third res) (fourth res) nil t reject-list))
               ((and equivalent (cdr x))
                (setf (car x) generalized)
                (values x (third res) (fourth res) t nil reject-list))
               ((and (not equivalent) (null (cdr x)))
                (setf (episode-id generalized) (symbol-name (gensym "EPISODE-")))
                (push (car x) (cdr (last x)))
                (setf (car x) generalized)
                (setf (second x) (list (second x)))
                (setf (episode-parent (car (second x))) x)
                (values x (third res) (fourth res) nil nil reject-list))
               ((and (not equivalent) (cdr x))
                (setf (car x) generalized)
                (values x (third res) (fourth res) t nil reject-list))))
        (t ;; when insert starts at root
         (loop
           with pattern-states = (episode-states y)
           and base-states = (ignore-errors (episode-states (car x)))
           with pattern-state-pointer = pattern-states and base-state-pointer = base-states
           with mappings and unmatched and costs and bbindings and decomp-bindings
           while (or pattern-state-pointer base-state-pointer)
           do
              (multiple-value-bind (sol no-matches cost bindings unweighted-cost)
                  (maximum-common-subgraph (car pattern-state-pointer) (car base-state-pointer) :cost-of-nil (episode-count (car x)) :bic-p bic-p)
                ;;(subgraph-greedy-monomorphism (car pattern-state-pointer) (car base-state-pointer) :cost-of-nil (episode-count (car x)) :bic-p bic-p)
                ;;(subgraph-optimal-monomorphism (car pattern-state-pointer) (car base-state-pointer) :cost-of-nil (episode-count (car x)) :bic-p bic-p)
                (declare (ignorable unweighted-cost))
                (setq mappings (cons sol mappings))
                (setq unmatched (cons no-matches unmatched))
                (setq costs (cons cost costs))
                ;;(setq unweighted-costs (cons unweighted-cost unweighted-costs))
                (setq bbindings (cons bindings bbindings)))
              (setq pattern-state-pointer (rest pattern-state-pointer))
              (setq base-state-pointer (rest base-state-pointer))
           finally
              (multiple-value-setq (costs decomp-bindings)
                (match-decompositions x y costs))
              (setq generalized (combine (car x) y (reverse mappings) (reverse unmatched) (nreverse bbindings) decomp-bindings))
              (when nil
                (format t "~%mappings:~%~S~%costs: ~S~%bindings:~%~S" mappings costs bbindings))
              (setq equivalent (= 0 (reduce #'+ costs)))
              ;;(break "(car x):~%~A~%(cdr x):~%~A~%equivalent: ~A" (car x) (cdr x) equivalent)
              (cond ((and equivalent (null (cdr x)))
                     (setf (car x) generalized)
                     (return (values x (reduce #'+ costs) bbindings nil t reject-list)))
                    ((and equivalent (cdr x))
                     (setf (car x) generalized)
                     (return (values x (reduce #'+ costs) bbindings t nil reject-list)))
                    ((and (not equivalent) (null (cdr x)))
                     (setf (episode-id generalized) (symbol-name (gensym "EPISODE-")))
                     (push (car x) (cdr (last x)))
                     (setf (car x) generalized)
                     (setf (second x) (list (second x)))
                     (setf (episode-parent (car (second x))) x)
                     (return (values x (reduce #'+ costs) bbindings nil nil reject-list)))
                    ((and (not equivalent) (cdr x))
                     (setf (car x) generalized)
                     (return (values x (reduce #'+ costs) bbindings t nil reject-list))))))))

#| Merge two episodes together|#

;; x = episodic memory
;; y = episode to merge into x
;; res = existing mapping and bindings
;; reject-list = list of episode ids to reject when merging
;; bic-p = flag to compute BIC
(defun new-ep-merge (x y res reject-list bic-p &aux generalized equivalent)
  ;;(format t "~%reject list: ~A~%root id: ~A" reject-list (if x (episode-id (car x))))
  (let (pattern base)
    (when x
      (cond ((episode-temporal-p y)
             (setq pattern (episode-state-transitions y))
             (setq base (episode-state-transitions (car x))))
            (t
             (setq pattern (episode-observation y))
             (setq base (episode-observation (car x))))))
    (cond ((null x) (values x 0 (list (make-hash-table :test #'equal)) nil nil reject-list -1))
          ((reject-branch? x y reject-list :check-decomps nil)
           (values x 0 (list (make-hash-table :test #'equal)) nil nil (cons (episode-id (car x)) reject-list -1)))
          (res
           ;; combine the graphs according to the mapping
           (setq generalized (new-combine (car x) y (first res) (second res) (fourth res)))
	   (setq equivalent (or (= 0 (third res))
				(and (= (hash-table-count (fifth res))
					(hash-table-count (fourth res)))
				     (= (array-dimension (car base) 0)
					(array-dimension (car pattern) 0)))))
	   (when nil
	     (format t "~%~%num bindings: ~d~%num q-first bindings: ~d~%size of base: ~d~%size of pattern: ~d~%equivalent-p: ~A" (hash-table-count (fourth res)) (hash-table-count (fifth res)) (array-dimension (car base) 0) (array-dimension (car pattern) 0) equivalent))
	   (cond ((and equivalent (null (cdr x)))
                  (setf (car x) generalized)
                  (values x (third res) (fourth res) nil t reject-list (sixth res)))
		 ((and equivalent (cdr x))
                  (setf (car x) generalized)
                  (values x (third res) (fourth res) t t reject-list (sixth res)))
		 ((and (not equivalent) (null (cdr x)))
                  (setf (episode-id generalized) (symbol-name (gensym "EPISODE-")))
                  (push (car x) (cdr (last x)))
                  (setf (car x) generalized)
                  (setf (second x) (list (second x)))
                  (setf (episode-parent (car (second x))) x)
                  (values x (third res) (fourth res) nil nil reject-list (sixth res)))
		 ((and (not equivalent) (cdr x))
                  (setf (car x) generalized)
                  (values x (third res) (fourth res) t nil reject-list (sixth res)))))
          (t ;; when insert starts at root
           (multiple-value-bind (sol no-matches cost bindings q-first-bindings num-local-preds)
               (new-maximum-common-subgraph pattern base (episode-backlinks y) (episode-backlinks (car x)) :cost-of-nil (episode-count (car x)) :bic-p bic-p)
	     (declare (ignore q-first-bindings))
	     (when nil
	       (format t "~%cost: ~d~%num local matches: ~d" cost num-local-preds))
	     (when nil t
	       (format t "~%matches:~%~A~%no matches:~%~A~%bindings:~%~A~%num-local-preds: ~d" sol no-matches bindings num-local-preds))
	     (setq generalized (new-combine (car x) y sol no-matches bindings))
	     (setq equivalent (or (= 0 cost)
				  (and (= (hash-table-count q-first-bindings)
					  (hash-table-count bindings))
				       (= (array-dimension (car base) 0)
					  (array-dimension (car pattern) 0)))))
	     (when nil
	       (format t "~%~%num bindings: ~d~%num q-first bindings: ~d~%size of base: ~d~%size of pattern: ~d~%equivalent-p: ~A" (hash-table-count bindings) (hash-table-count q-first-bindings) (array-dimension (car base) 0) (array-dimension (car pattern) 0) equivalent))
	     (cond ((and equivalent (null (cdr x)))
                    (setf (car x) generalized)
                    (values x cost bindings nil t reject-list num-local-preds))
                   ((and equivalent (cdr x))
                    (setf (car x) generalized)
                    (values x cost bindings t t reject-list num-local-preds))
                   ((and (not equivalent) (null (cdr x)))
                    (setf (episode-id generalized) (symbol-name (gensym "EPISODE-")))
                    (push (car x) (cdr (last x)))
                    (setf (car x) generalized)
                    (setf (second x) (list (second x)))
                    (setf (episode-parent (car (second x))) x)
                    (values x cost bindings nil nil reject-list num-local-preds))
                   ((and (not equivalent) (cdr x))
                    (setf (car x) generalized)
                    (values x cost bindings t nil reject-list num-local-preds))))))))

#| Rudamentary printer for showing the branching structure of eltm. |#

;; eltm = episodic long-term memory
(defun print-tree-structure (eltm)
  (cond ((episode-p eltm)
         (format t "x"))
        ((null (rest eltm))
         (format t " (x)"))
        (t
         (format t "(")
         (loop
           for item in eltm do
             (print-tree-structure item))
         (format t ")"))))

#| Inserts episode into episodic memory

tree = \lambda v b1 b2 ... bn l b. (b v b1 b2 ... bn)
tree = \lambda v b1 b2 ....bn l b. (l v)
|#

;; eltm = episodic memory
;; ep = episode to insert (list of (state . type-count), where state = (factors . edges)
;; reject-list = list of episode ids to reject when merging
;; res = optimal common subgraph between ep and eltm
(defun insert-episode (eltm ep reject-list &key res (depth 0) (bic-p t) &aux best-child (best-child-cost most-positive-fixnum))
  (multiple-value-bind (eltm p-cost p-bbindings branch absorb rejects)
      (ep-merge eltm ep res reject-list bic-p)
    (sb-ext:gc :full t)
    ;;(break)
    ;;(format t "~%branch-p: ~S" branch)
    (setq reject-list rejects)
    (when nil
      (format t "~%~%parent episode-id ~A parent episode-states: ~d parent lvl: ~d parent decompositions: ~A parent abstraction pointers: ~S~%episode-states: ~d episode lvl: ~d episode decompositions: ~A episode abstraction pointers: ~S~%parent cost of match: ~d"
              (ignore-errors (episode-id (car eltm))) (ignore-errors (length (episode-states (car eltm)))) (ignore-errors (episode-lvl (car eltm))) (ignore-errors (episode-num-decompositions (car eltm))) (ignore-errors (mapcar #'(lambda (abstr-ptr) (episode-id (car abstr-ptr))) (episode-abstraction-ptrs (car eltm)))) (length (episode-states ep)) (episode-lvl ep) (episode-num-decompositions ep) (ignore-errors (mapcar #'(lambda (abstr-ptr) (episode-id (car abstr-ptr))) (episode-abstraction-ptrs ep))) p-cost))
    (when branch
      (loop
        for branch in eltm
        for i from 0
        when (> i 0) do
          (cond ((not (reject-branch? branch ep reject-list :check-decomps t))
                 (when nil
                   (format t "~%~%branch episode-id ~A branch lvl: ~d branch episode-states: ~d branch decompositions: ~A"
                           (episode-id (car branch)) (episode-lvl (car branch)) (length (episode-states (car branch))) (episode-num-decompositions (car branch))))
                 (loop
                   with pattern-states = (episode-states ep)
                   and base-states = (episode-states (car branch))
                   with pattern-state-pointer = pattern-states and base-state-pointer = base-states
                   with mappings and unmatched and costs and bbindings and kost and decomp-bindings
                   while (or pattern-state-pointer base-state-pointer)
                   do
                      (multiple-value-bind (sol no-matches cost bindings unweighted-cost)
                          (maximum-common-subgraph (car pattern-state-pointer) (car base-state-pointer) :cost-of-nil (episode-count (car branch)) :bic-p bic-p)
                        ;;(subgraph-greedy-monomorphism pattern-state base-state :cost-of-nil (episode-count (car branch)) :bic-p bic-p)
                        ;;(subgraph-optimal-monomorphism pattern-state base-state :cost-of-nil (episode-count (car branch)) :bic-p bic-p)
                        (declare (ignorable unweighted-cost))
                        (setq mappings (cons sol mappings))
                        (setq unmatched (cons no-matches unmatched))
                        (setq costs (cons cost costs))
                        (setq bbindings (cons bindings bbindings)))
                      (setq pattern-state-pointer (rest pattern-state-pointer))
                      (setq base-state-pointer (rest base-state-pointer))
                   finally
                      (multiple-value-setq (costs decomp-bindings)
                        (match-decompositions branch ep costs))
                      (setq kost (reduce #'+ costs))
                      (when nil
                        (format t "~%cost of branch after matching decompositions: ~d~%size of bindings: ~d" kost (hash-table-count (car bbindings))))
                      (when (or (= i 1) (better-random-match? (list nil kost (car bbindings)) best-child)#|(< kost best-child-cost)|#)
                        (setq res (list (nreverse mappings) (nreverse unmatched) kost (nreverse bbindings) decomp-bindings))
                        (setq best-child-cost kost)
                        (setq best-child (list i kost (car bbindings))))))
                (t
                 (setq reject-list (cons (episode-id (car branch)) reject-list))))))
    ;;(format t "~%~%parent cost: ~d~%unweighted parent cost: ~d~%best-child cost: ~d" p-cost unweighted-p-cost best-child-cost)
    (when nil t
          (format t "~%~%parent cost: ~d~%size parent bindings: ~d~%best-child-weighted-cost: ~d~%size best-child bindings: ~d" p-cost (hash-table-count (car p-bbindings)) best-child-cost (if branch (hash-table-count (third best-child)) 0)))
    (cond ((null eltm)
           (setf eltm (list ep))
           (when nil t
                 (format t "~%Pushed to empty memory"))
           (values eltm eltm))
          (absorb
           (when nil t
             (format t "~%Absorbed"))
           (values eltm eltm))
          ((or (and branch (better-random-match? (list nil p-cost (car p-bbindings)) best-child) #|(<= p-cost best-child-cost)|#)
               (not branch))
           (setf (episode-parent ep) eltm)
           (push (list ep) (cdr (last eltm)))
           (when nil t
                 (format t "~%Added new child of parent"))
           (values eltm (car (last eltm))))
          (t ;;(and branch (> p-cost best-child-cost))
           (when nil t
                 (format t "~%Recursing on best child"))
           (multiple-value-bind (new-branch ref)
               (insert-episode (nth (car best-child) eltm) ep reject-list :res res :depth (+ depth 1) :bic-p bic-p)
             (setf (nth (car best-child) eltm) new-branch)
             (values eltm ref))))))

#| Inserts episode into episodic memory

tree = \lambda v b1 b2 ... bn l b. (b v b1 b2 ... bn)
tree = \lambda v b1 b2 ....bn l b. (l v)
|#

;; eltm = episodic memory
;; ep = episode to insert (list of (state . type-count), where state = (factors . edges)
;; reject-list = list of episode ids to reject when merging
;; res = optimal common subgraph between ep and eltm
(defun new-insert-episode (eltm ep reject-list &key res (depth 0) (bic-p t) &aux best-child (best-child-cost most-positive-fixnum))
  (multiple-value-bind (eltm p-cost p-bindings branch absorb rejects p-num-local-preds)
      (new-ep-merge eltm ep res reject-list bic-p)
    ;;(break)
    ;;(format t "~%branch-p: ~S" branch)
    (setq reject-list rejects)
    (when nil
      (format t "~%~%parent episode-id ~A parent episode-states: ~d parent lvl: ~d parent decompositions: ~A parent abstraction pointers: ~S~%episode-states: ~d episode lvl: ~d episode decompositions: ~A episode abstraction pointers: ~S~%parent cost of match: ~d"
              (ignore-errors (episode-id (car eltm))) (ignore-errors (length (episode-states (car eltm)))) (ignore-errors (episode-lvl (car eltm))) (ignore-errors (episode-num-decompositions (car eltm))) (ignore-errors (mapcar #'(lambda (abstr-ptr) (episode-id (car abstr-ptr))) (episode-abstraction-ptrs (car eltm)))) (length (episode-states ep)) (episode-lvl ep) (episode-num-decompositions ep) (ignore-errors (mapcar #'(lambda (abstr-ptr) (episode-id (car abstr-ptr))) (episode-abstraction-ptrs ep))) p-cost))
    (when branch
      (loop
        with pattern and base and pattern-backlinks and base-backlinks
        for branch in eltm
        for i from 0
        when (> i 0) do
          (cond (t (not (reject-branch? branch ep reject-list :check-decomps t))
                 (when nil
                   (format t "~%~%branch episode-id ~A branch lvl: ~d branch episode-states: ~d branch decompositions: ~A"
                           (episode-id (car branch)) (episode-lvl (car branch)) (length (episode-states (car branch))) (episode-num-decompositions (car branch))))
                 (cond ((episode-temporal-p ep)
                        (setq pattern (episode-state-transitions ep))
                        (setq base (episode-state-transitions (car branch))))
                       (t
                        (setq pattern (episode-observation ep))
                        (setq base (episode-observation (car branch)))))
		 (setq pattern-backlinks (episode-backlinks ep))
                 (setq base-backlinks (episode-backlinks (car branch)))
                 (multiple-value-bind (sol no-matches cost bindings q-first-bindings num-local-preds)
                     (new-maximum-common-subgraph pattern base pattern-backlinks base-backlinks :cost-of-nil (episode-count (car branch)) :bic-p bic-p)
                   ;;(subgraph-greedy-monomorphism pattern-state base-state :cost-of-nil (episode-count (car branch)) :bic-p bic-p)
                   ;;(subgraph-optimal-monomorphism pattern-state base-state :cost-of-nil (episode-count (car branch)) :bic-p bic-p)
		   (when nil
		     (format t "~%cost: ~d~%num local matches: ~d" cost num-local-preds))
		   (when nil
                     (format t "~%cost of branch after matching decompositions: ~d~%size of bindings: ~d" cost (hash-table-count bindings)))
                   (when (or (= i 1) (better-random-match? (list nil cost bindings nil num-local-preds) best-child)#|(< kost best-child-cost)|#)
                     (setq res (list sol no-matches cost bindings q-first-bindings num-local-preds))
                     (setq best-child-cost cost)
                     (setq best-child (list i cost bindings nil num-local-preds)))))
                (t
                 (setq reject-list (cons (episode-id (car branch)) reject-list))))))
    ;;(format t "~%~%parent cost: ~d~%unweighted parent cost: ~d~%best-child cost: ~d" p-cost unweighted-p-cost best-child-cost)
    (when nil t
          (format t "~%~%parent cost: ~d~%size parent bindings: ~d~%best-child-weighted-cost: ~d~%size best-child bindings: ~d" p-cost (hash-table-count p-bindings) best-child-cost (if branch (hash-table-count (third best-child)) 0)))
    (cond ((null eltm)
           (setf (episode-depth ep) depth)
	   (setf eltm (list ep))
           (when nil
                 (format t "~%Pushed to empty memory"))
           (values eltm eltm))
          ((and absorb
		(not branch))
           (when nil
                 (format t "~%Absorbed"))
           (values eltm eltm))
          ((and (not absorb)
		(or (and branch (better-random-match? (list nil p-cost p-bindings nil p-num-local-preds) best-child) #|(<= p-cost best-child-cost)|#)
		    (not branch)))
           (setf (episode-parent ep) eltm)
           (setf (episode-depth ep) depth)
           (push (list ep) (cdr (last eltm)))
           (when nil
             (format t "~%Added new child of parent"))
           (values eltm (car (last eltm))))
          (t ;;(and branch (> p-cost best-child-cost))
           (when nil
             (format t "~%Recursing on best child"))
           (multiple-value-bind (new-branch ref)
               (new-insert-episode (nth (car best-child) eltm) ep reject-list :res res :depth (+ depth 1) :bic-p bic-p)
             (setf (nth (car best-child) eltm) new-branch)
             (values eltm ref))))))

#| Match retrieval cue to event memory element |#

;; x = episodic memory
;; y = retrieval cue to match with root of x
;; res = optimal common subgraph between cue and eltm, with associated cost
;; reject-list = list of episode ids to reject when merging
;; bicp-p = retrieval mode for using BIC or likelihood for structure mapping
(defun match-cue (x y res reject-list bic-p check-decomps check-abstraction-ptrs check-index-case forbidden-types)
  (cond ((null x) (values (list nil most-positive-fixnum nil most-positive-fixnum) nil reject-list))
        ((reject-branch? x y reject-list :check-decomps check-decomps :check-abstraction-ptrs check-abstraction-ptrs :check-index-case check-index-case)
         (values (list nil most-positive-fixnum nil most-positive-fixnum) nil (cons (episode-id (car x)) reject-list)))
        (res
         (cond ((null (cdr x))
                (values res nil reject-list))
               ((cdr x)
                (values res t reject-list))))
        (t ;; when checking starts at the root
         (loop
           with pattern-states = (episode-states y)
           and base-states = (episode-states (car x))
           with pattern-state-pointer = pattern-states and base-state-pointer = base-states
           with mappings and weighted-costs and costs and bbindings
           while pattern-state-pointer
           for i from 1
           do
              (multiple-value-bind (sol no-matches weighted-kost bindings kost)
                  (maximum-common-subgraph (car pattern-state-pointer) (car base-state-pointer) :cost-of-nil (episode-count (car x)) :bic-p bic-p :forbidden-types forbidden-types)
                ;;(subgraph-greedy-monomorphism (car pattern-state-pointer) (car base-state-pointer) :cost-of-nil (episode-count (car x)) :bic-p bic-p :forbidden-types forbidden-types)
                ;;(subgraph-optimal-monomorphism pattern-state base-state)
                (declare (ignore no-matches))
                (setq mappings (cons sol mappings))
                (setq weighted-costs (cons weighted-kost weighted-costs))
                (setq costs (cons kost costs))
                (setq bbindings (cons bindings bbindings)))
              (setq pattern-state-pointer (rest pattern-state-pointer))
              (setq base-state-pointer (rest base-state-pointer))
           finally
              (setq weighted-costs (match-decompositions x y weighted-costs :bic-p bic-p :retrieve-p t))
              (let (weighted-cost cost)
                (setq weighted-cost (reduce #'+ weighted-costs))
                (setq cost (reduce #'+ costs))
                (setq res (list (reverse mappings) weighted-cost (reverse bbindings) cost))
                (cond ((null (cdr x))
                       (return (values res nil reject-list)))
                      ((cdr x)
                       (return (values res t reject-list)))))))))

#| Match retrieval cue to event memory element |#

;; x = episodic memory
;; y = retrieval cue to match with root of x
;; res = optimal common subgraph between cue and eltm, with associated cost
;; reject-list = list of episode ids to reject when merging
;; bicp-p = retrieval mode for using BIC or likelihood for structure mapping
(defun new-match-cue (x y res reject-list bic-p check-decomps check-abstraction-ptrs check-index-case forbidden-types)
  (cond ((null x)
	 (values (list nil most-positive-fixnum nil -1) nil reject-list))
        ((reject-branch? x y reject-list :check-decomps check-decomps :check-abstraction-ptrs check-abstraction-ptrs :check-index-case check-index-case)
	 (values (list nil most-positive-fixnum nil -1) nil (cons (episode-id (car x)) reject-list)))
        (res
         (cond ((null (cdr x))
                (values res nil reject-list))
               ((cdr x)
                (values res t reject-list))))
        (t ;; when checking starts at the root
	 (let (pattern base)
	   (cond ((episode-temporal-p y)
                  (setq pattern (episode-state-transitions y))
                  (setq base (episode-state-transitions (car x))))
                 (t
                  (setq pattern (episode-observation y))
                  (setq base (episode-observation (car x)))))
	   (multiple-value-bind (sol no-matches cost bindings q-first-bindings num-local-preds)
               (new-maximum-common-subgraph pattern base (episode-backlinks y) (episode-backlinks (car x)) :cost-of-nil (episode-count (car x)) :bic-p bic-p :forbidden-types forbidden-types)
	     (declare (ignore no-matches q-first-bindings))
	     (when nil t
	       (format t "~%retrieval mappings:~%~S~%retrieval cost: ~d~%retrieval bindings:~%~S" sol cost bindings))
	     (setq res (list sol cost bindings num-local-preds))
	     (cond ((null (cdr x))
                    (values res nil reject-list))
                   ((cdr x)
                    (values res t reject-list))))))))

#| Inserts episode into episodic memory

tree = \lambda v b1 b2 ... bn l b. (b v b1 b2 ... bn)
tree = \lambda v b1 b2 ....bn l b. (l v)
|#

;; eltm = episodic memory
;; cue = retrieval cue
;; reject-list = list of episode ids to reject and not expand/return
;; res = optimal common subgraph between cue and eltm, with associated cost
;; depth = optional depth of retrieval search
(defun retrieve-episode (eltm cue reject-list &key (res nil) (depth 0) (bic-p t) (lvl-func nil) (check-decomps t) (check-abstraction-ptrs nil) (check-index-case nil) (forbidden-types nil) &aux best-child (best-child-weighted-cost most-positive-fixnum) (best-child-cost most-positive-fixnum))
  (when nil
    (format t "~%reject list in retrieve: ~A" reject-list))
  (when (or (null eltm) (member (episode-id (car eltm)) reject-list :test #'equal))
    (return-from retrieve-episode (values nil nil nil depth most-positive-fixnum most-positive-fixnum nil reject-list)))
  ;; merge eltm and ep
  (multiple-value-bind (p-cost branch rejects)
      (match-cue eltm cue res reject-list bic-p check-decomps check-abstraction-ptrs check-index-case forbidden-types)
    ;; for each child, check the match result
    (setq reject-list rejects)
    (when nil
      (format t "~%~%parent episode-id ~A parent episode-states: ~d parent lvl: ~d parent decompositions: ~A parent abstraction pointers: ~S~%cue episode-states: ~d cue lvl: ~d cue decompositions: ~A cue abstraction pointers: ~S~%parent cost of match: ~d~%lvl-func: ~A~%reject list: ~S"
              (episode-id (car eltm)) (length (episode-states (car eltm))) (episode-lvl (car eltm)) (episode-num-decompositions (car eltm)) (ignore-errors (mapcar #'(lambda (abstr-ptr) (episode-id (car abstr-ptr))) (episode-abstraction-ptrs (car eltm)))) (if cue (length (episode-states cue)) 0) (if cue (episode-lvl cue) 0) (if cue (episode-num-decompositions cue) 0) (if cue (ignore-errors (mapcar #'(lambda (abstr-ptr) (episode-id (car abstr-ptr))) (episode-abstraction-ptrs cue)))) (second p-cost) lvl-func reject-list))
    (when branch
      (loop
        for branch in eltm
        for i from 0
        when (> i 0) do
          (cond ((not (reject-branch? branch cue reject-list :check-decomps check-decomps :check-abstraction-ptrs check-abstraction-ptrs :check-index-case check-index-case))
                 (when nil t
                   (format t "~%~%branch episode-id ~A branch lvl: ~d branch episode-states: ~d branch decompositions: ~A"
                           (episode-id (car branch)) (episode-lvl (car branch)) (length (episode-states (car branch))) (episode-num-decompositions (car branch))))
                 (loop
                   with pattern-states = (episode-states cue)
                   and base-states = (episode-states (car branch))
                   with pattern-state-pointer = pattern-states and base-state-pointer = base-states
                   with mappings and weighted-costs and costs and bbindings and weighted-cost and cost
                   while pattern-state-pointer
                   do
                      (multiple-value-bind (sol no-matches weighted-kost bindings kost)
                          (maximum-common-subgraph (car pattern-state-pointer) (car base-state-pointer) :cost-of-nil (episode-count (car branch)) :bic-p bic-p :forbidden-types forbidden-types)
                          ;;(subgraph-greedy-monomorphism (car pattern-state-pointer) (car base-state-pointer) :cost-of-nil (episode-count (car branch)) :bic-p bic-p :forbidden-types forbidden-types)
                        ;;(subgraph-optimal-monomorphism pattern-state base-state)
			(declare (ignore no-matches))
			(setq mappings (cons sol mappings))
                        (setq weighted-costs (cons weighted-kost weighted-costs))
                        (setq costs (cons kost costs))
                        (setq bbindings (cons bindings bbindings)))
                      (setq pattern-state-pointer (rest pattern-state-pointer))
                      (setq base-state-pointer (rest base-state-pointer))
                   finally
                      (setq weighted-costs (match-decompositions branch cue weighted-costs :bic-p bic-p :retrieve-p t))
                      (when nil t
                        (format t "~%cost of branch after matching decompositions: ~d" (reduce #'+ weighted-costs)))
                      (cond ((or (= i 1)
				 (and (not lvl-func)
				      (better-random-match? (list nil (reduce #'+ weighted-costs) (car bbindings)) best-child))
                                 ;;(< (reduce #'+ weighted-costs) best-child-weighted-cost)
                                 (and lvl-func
                                      (not (funcall lvl-func (episode-lvl (caar best-child)) (episode-lvl cue)))
                                      (funcall lvl-func (episode-lvl (car branch)) (episode-lvl cue))
				      (better-random-match? (list nil (reduce #'+ weighted-costs) (car bbindings)) best-child)
				      ;;(= (reduce #'+ weighted-costs) best-child-weighted-cost)
				      )
                                 (and lvl-func
                                      (funcall lvl-func (episode-lvl (car branch)) (episode-lvl cue))
                                      (not (funcall lvl-func (episode-lvl (car branch)) (episode-lvl (caar best-child))))
				      (better-random-match? (list nil (reduce #'+ weighted-costs) (car bbindings)) best-child)
				      ;;(= (reduce #'+ weighted-costs) best-child-weighted-cost)
				      ))
                             (setq weighted-cost (reduce #'+ weighted-costs))
                             (setq cost (reduce #'+ costs))
                             (setq res (list (reverse mappings) weighted-cost (reverse bbindings) cost))
                             ;; if current best child is an episode, reject it before replacing it with current branch
                             (when (and (car best-child) (= (episode-count (caar best-child)) 1)
                                        (= (episode-num-decompositions (caar best-child)) 0))
                               (setq reject-list (cons (episode-id (caar best-child)) reject-list)))
                             (setq best-child-weighted-cost weighted-cost)
                             (setq best-child-cost cost)
                             (setq best-child (list branch weighted-cost (car bbindings))))
                            ((and (= (episode-count (car branch)) 1) (= (episode-num-decompositions (car branch)) 0))
                             (setq reject-list (cons (episode-id (car branch)) reject-list))))))
                (t
                 (when nil t
                   (format t "~%~A is in the reject list" (episode-id (car branch))))))))
    (when nil t
      (format t "~%parent cost: ~d~%best-child: ~S~%branch-p: ~S" (second p-cost) (list 'subtree (second best-child) (third best-child)) branch))
    (cond ((and lvl-func (funcall lvl-func (episode-lvl (car eltm)) (episode-lvl cue))
                (if best-child (not (funcall lvl-func (episode-lvl (car eltm)) (episode-lvl (caar best-child)))) t)
		(if best-child
		    (better-random-match? (list nil (second p-cost) (car (third p-cost))) best-child)
		    (<= (second p-cost) best-child-weighted-cost)))
           (when nil t nil
             (format t "~%Returning ~A" (if (member (episode-id (car eltm)) reject-list :test #'equal) nil (episode-id (car eltm)))))
           (values (if (member (episode-id (car eltm)) reject-list :test #'equal) nil (car eltm)) (car p-cost) (third p-cost) depth (fourth p-cost) (second p-cost) nil reject-list))
          ((and (or (eq #'= lvl-func) (eq '= lvl-func)) (funcall lvl-func (episode-lvl (car eltm)) (episode-lvl cue))
                (if best-child (funcall lvl-func (episode-lvl (car eltm)) (episode-lvl (caar best-child))) t)
		(if best-child
		    (better-random-match? (list nil (second p-cost) (car (third p-cost))) best-child)
		    (<= (second p-cost) best-child-weighted-cost))
		;;(<= (- (second p-cost) best-child-weighted-cost) 0)
		)
           (when nil t nil
             (format t "~%Returning ~A" (if (member (episode-id (car eltm)) reject-list :test #'equal) nil (episode-id (car eltm)))))
           (values (if (member (episode-id (car eltm)) reject-list :test #'equal) nil (car eltm)) (car p-cost) (third p-cost) depth (fourth p-cost) (second p-cost) nil reject-list))
          ((and lvl-func (not (funcall lvl-func (episode-lvl (car eltm)) (episode-lvl cue))) (null (car best-child)))
           (when nil t nil
             (format t "~%Returning ~A" (if (equal (episode-id (car eltm)) (car reject-list)) nil (car eltm))))
           (values (if (member (episode-id (car eltm)) reject-list :test #'equal) nil (car eltm)) (car p-cost) (third p-cost) depth (fourth p-cost) (second p-cost) (episode-id (car eltm)) reject-list))
          ((and (not lvl-func)
		(or (and branch (better-random-match? (list nil (second p-cost) (car (third p-cost))) best-child))
		    (not branch))
		;;(<= (- (second p-cost) best-child-weighted-cost) 0)
		)
           (when nil t nil
             (format t "~%Returning ~A" (if (member (episode-id (car eltm)) reject-list :test #'equal) nil (episode-id (car eltm)))))
           (values (if (member (episode-id (car eltm)) reject-list :test #'equal) nil (car eltm)) (car p-cost) (third p-cost) depth (fourth p-cost) (second p-cost) nil reject-list))
          (t
           (when nil t nil
             (format t "~%Recursing on best child: ~A" (if (car best-child) (episode-id (caar best-child)))))
           (retrieve-episode (car best-child) cue reject-list :res res :depth (1+ depth) :bic-p bic-p :check-decomps check-decomps :forbidden-types forbidden-types :lvl-func lvl-func :check-abstraction-ptrs check-abstraction-ptrs :check-index-case check-index-case)))))

#| Inserts episode into episodic memory

tree = \lambda v b1 b2 ... bn l b. (b v b1 b2 ... bn)
tree = \lambda v b1 b2 ....bn l b. (l v)
|#

;; eltm = episodic memory
;; cue = retrieval cue
;; reject-list = list of episode ids to reject and not expand/return
;; res = optimal common subgraph between cue and eltm, with associated cost
;; depth = optional depth of retrieval search
(defun new-retrieve-episode (eltm cue reject-list &key (res nil) (depth 0) (bic-p t) (lvl-func nil) (check-decomps t) (check-abstraction-ptrs nil) (check-index-case nil) (forbidden-types nil) &aux best-child (best-child-weighted-cost most-positive-fixnum) (best-child-cost most-positive-fixnum))
  (setq best-child (list nil most-positive-fixnum (make-hash-table :test #'equal) nil -1))
  (when nil t
    (format t "~%reject list in retrieve: ~A" reject-list))
  (when (or (null eltm) (member (episode-id (car eltm)) reject-list :test #'equal))
    (return-from new-retrieve-episode (values nil nil nil depth most-positive-fixnum most-positive-fixnum nil reject-list)))
  ;; merge eltm and ep
  (multiple-value-bind (p-cost branch rejects)
      (new-match-cue eltm cue res reject-list bic-p check-decomps check-abstraction-ptrs check-index-case forbidden-types)
    ;; for each child, check the match result
    (setq reject-list rejects)
    (when nil
      (format t "~%~%parent episode-id ~A parent episode-states: ~d parent lvl: ~d parent decompositions: ~A parent abstraction pointers: ~S~%cue episode-states: ~d cue lvl: ~d cue decompositions: ~A cue abstraction pointers: ~S~%parent cost of match: ~d~%lvl-func: ~A~%reject list: ~S"
              (episode-id (car eltm)) (length (episode-states (car eltm))) (episode-lvl (car eltm)) (episode-num-decompositions (car eltm)) (ignore-errors (mapcar #'(lambda (abstr-ptr) (episode-id (car abstr-ptr))) (episode-abstraction-ptrs (car eltm)))) (if cue (length (episode-states cue)) 0) (if cue (episode-lvl cue) 0) (if cue (episode-num-decompositions cue) 0) (if cue (ignore-errors (mapcar #'(lambda (abstr-ptr) (episode-id (car abstr-ptr))) (episode-abstraction-ptrs cue)))) (second p-cost) lvl-func reject-list))
    (when branch
      (loop
	 with pattern and base and pattern-backlinks and base-backlinks
	 with default-check = nil
	 for branch in eltm
	 for i from 0
         when (> i 0) do
          (cond ((not (reject-branch? branch cue reject-list :check-decomps check-decomps :check-abstraction-ptrs check-abstraction-ptrs :check-index-case check-index-case))
                 (when nil t
                       (format t "~%~%branch episode-id ~A branch lvl: ~d branch episode-temporal-p: ~S~%episode num decompositions: ~d"
                               (episode-id (car branch)) (episode-lvl (car branch)) (episode-temporal-p (car branch)) (hash-table-count (episode-backlinks (car branch)))))
		 (cond ((episode-temporal-p cue)
                        (setq pattern (episode-state-transitions cue))
                        (setq base (episode-state-transitions (car branch))))
                       (t
                        (setq pattern (episode-observation cue))
                        (setq base (episode-observation (car branch)))))
		 (setq pattern-backlinks (episode-backlinks cue))
                 (setq base-backlinks (episode-backlinks (car branch)))
		 (multiple-value-bind (sol no-matches cost bindings q-first-bindings num-local-preds)
                     (new-maximum-common-subgraph pattern base pattern-backlinks base-backlinks :cost-of-nil (episode-count (car branch)) :bic-p bic-p :forbidden-types forbidden-types)
		   (declare (ignore q-first-bindings no-matches))
		   (when nil t
		     (format t "~%retrieval mappings:~%~S~%retrieval cost: ~d~%retrieval bindings:~%~S" sol cost bindings))
		   (cond ((or (= i 1)
			      (and (not lvl-func)
				   (better-random-match? (list nil cost bindings nil num-local-preds) best-child))
                              ;;(< (reduce #'+ weighted-costs) best-child-weighted-cost)
                              (and lvl-func
                                   (not (funcall lvl-func (episode-lvl (caar best-child)) (episode-lvl cue)))
                                   (funcall lvl-func (episode-lvl (car branch)) (episode-lvl cue))
				   (better-random-match? (list nil cost bindings nil num-local-preds) best-child)
				   ;;(= (reduce #'+ weighted-costs) best-child-weighted-cost)
				   )
                              (and lvl-func
                                   (funcall lvl-func (episode-lvl (car branch)) (episode-lvl cue))
                                   (not (funcall lvl-func (episode-lvl (car branch)) (episode-lvl (caar best-child))))
				   (better-random-match? (list nil cost bindings nil num-local-preds) best-child)
				   ;;(= (reduce #'+ weighted-costs) best-child-weighted-cost)
				   ))
                          (setq res (list sol cost bindings num-local-preds))
                          ;; if current best child is an episode, reject it before replacing it with current branch
			  (when (and (car best-child) (= (episode-count (caar best-child)) 1)
                                     (not (episode-temporal-p (caar best-child))))
                            (setq reject-list (cons (episode-id (caar best-child)) reject-list)))
                          (setq best-child-cost cost)
			  (when nil t
			    (format t "~%branch is new best child"))
                          (setq best-child (list branch cost bindings nil num-local-preds)))
                         ((and (= (episode-count (car branch)) 1) (not (episode-temporal-p (car branch))))
			  (setq reject-list (cons (episode-id (car branch)) reject-list))))))
                (t
                 (when nil t
                   (format t "~%~A is in the reject list" (episode-id (car branch))))))))
    (when nil t
      (format t "~%parent cost: ~d~%best-child: ~S~%branch-p: ~S" (second p-cost) (list 'subtree (second best-child) (third best-child)) branch))
    (cond ((and lvl-func (funcall lvl-func (episode-lvl (car eltm)) (episode-lvl cue))
                (if best-child (not (funcall lvl-func (episode-lvl (car eltm)) (episode-lvl (caar best-child)))) t)
		(if best-child
		    (better-random-match? (list nil (second p-cost) (third p-cost) nil (fourth p-cost)) best-child)
		    (<= (second p-cost) best-child-weighted-cost)))
           (when nil t
             (format t "~%Returning ~A" (if (member (episode-id (car eltm)) reject-list :test #'equal) nil (episode-id (car eltm)))))
           (values (if (member (episode-id (car eltm)) reject-list :test #'equal) nil eltm) (car p-cost) (third p-cost) depth (second p-cost) nil reject-list))
          ((and (or (eq #'= lvl-func) (eq '= lvl-func)) (funcall lvl-func (episode-lvl (car eltm)) (episode-lvl cue))
                (if best-child (funcall lvl-func (episode-lvl (car eltm)) (episode-lvl (caar best-child))) t)
		(if best-child
		    (better-random-match? (list nil (second p-cost) (third p-cost) nil (fourth p-cost)) best-child)
		    (<= (second p-cost) best-child-weighted-cost))
		;;(<= (- (second p-cost) best-child-weighted-cost) 0)
		)
           (when nil t
             (format t "~%Returning ~A" (if (member (episode-id (car eltm)) reject-list :test #'equal) nil (episode-id (car eltm)))))
           (values (if (member (episode-id (car eltm)) reject-list :test #'equal) nil eltm) (car p-cost) (third p-cost) depth (second p-cost) nil reject-list))
          ((and lvl-func (not (funcall lvl-func (episode-lvl (car eltm)) (episode-lvl cue))) (null (car best-child)))
           (when nil t
             (format t "~%Returning ~A" (if (equal (episode-id (car eltm)) (car reject-list)) nil (car eltm))))
           (values (if (member (episode-id (car eltm)) reject-list :test #'equal) nil eltm) (car p-cost) (third p-cost) depth (second p-cost) (episode-id (car eltm)) reject-list))
          ((and (not lvl-func)
		(or (and branch (better-random-match? (list nil (second p-cost) (third p-cost) nil (fourth p-cost)) best-child))
		    (not branch))
		;;(<= (- (second p-cost) best-child-weighted-cost) 0)
		)
           (when nil t
             (format t "~%Returning ~A" (if (member (episode-id (car eltm)) reject-list :test #'equal) nil (episode-id (car eltm)))))
           (values (if (member (episode-id (car eltm)) reject-list :test #'equal) nil eltm) (car p-cost) (third p-cost) depth (second p-cost) nil reject-list))
          (t
           (when nil t
             (format t "~%Recursing on best child: ~A" (if (car best-child) (episode-id (caar best-child)))))
           (new-retrieve-episode (car best-child) cue reject-list :res res :depth (1+ depth) :bic-p bic-p :check-decomps check-decomps :forbidden-types forbidden-types :lvl-func lvl-func :check-abstraction-ptrs check-abstraction-ptrs :check-index-case check-index-case)))))

#| Infer a retrieval cue from the first or last state of episode |#

;; episode = current state from which to supply evidence
;; i = binary variable for selecting first or last state in episode
(defun infer-state-from-episode (episode i)
  (let (state max-product-recs singletons new-cpds)
    (if (= i 0)
        (setq state (first (episode-states episode)))
        (setq state (second (episode-states episode))))
    (setq max-product-recs (get-recollections state nil nil nil 'max 1))
    (loop
      with rec
      with singleton
      with dep-var and vars and types-hash and id and dep-id and cid
      with qvars and vvm and cards and steps and assns and lvl and counts and count and rows and row-length and num-assignments
      for cpd being the elements of (car state)
      do
         (setq rec (assoc (rule-based-cpd-dependent-id cpd) max-product-recs :test #'equal))
         (when (and rec (not (equal "NA" (cdr rec))))
           (setq dep-id (rule-based-cpd-dependent-id cpd))
           (setq id (make-hash-table :test #'equal))
           (setf (gethash dep-id id) 0)
           (setq dep-var (rule-based-cpd-dependent-var cpd))
           (setq vars (make-hash-table))
           (setf (gethash 0 vars) dep-var)
           (setq types-hash (make-hash-table))
           (setf (gethash 0 types-hash) (gethash 0 (rule-based-cpd-types cpd)))
           (setq cid (make-hash-table))
           (setf (gethash 0 cid) (gethash 0 (rule-based-cpd-concept-ids cpd)))
           (setq qvars (make-hash-table))
           (setf (gethash 0 qvars) (gethash 0 (cpd-qualified-vars cpd)))
           (setq vvm (make-hash-table))
           (setf (gethash 0 vvm) (list (cons "NA" 0) (cons (cdr rec) 1)))
           (setq cards (make-array 1 :initial-element 2 :fill-pointer t))
           (setq steps (make-array 1 :initial-element 1 :fill-pointer t))
           ;;(setq assns (initialize-hash-table (aref (cpd-cardinalities cpd) 0) 1))
           (setq assns (make-hash-table))
           (setq lvl (cpd-lvl cpd))
           (setq num-assignments (reduce #'* cards))
           (setq row-length (aref cards 0))
           (setq rows (/ num-assignments row-length))
           (setq counts (make-counts rows row-length))
           (setq count 1)
           (setq singleton (make-cpd :dependent-id dep-id
                                     :identifiers id
                                     :dependent-var dep-var
                                     :vars vars
                                     :types types-hash
                                     :concept-ids cid
                                     :qualified-vars qvars
                                     :var-value-map vvm
                                     :cardinalities cards
                                     :step-sizes steps
                                     :assignments assns
                                     :counts counts
                                     :count count
                                     :lvl lvl))
           (setq singletons (cons singleton singletons)))
      finally
         (setq singletons (reverse singletons)))
    (loop
      with singleton
      for cpd being the elements of (car state)
      do
         (setq singleton (car (member (cpd-dependent-id cpd) singletons :key #'cpd-dependent-id :test #'equal)))
         (when singleton
           (loop
             with modifier-cpd
             for parent being the hash-keys of (cpd-identifiers cpd)
               using (hash-value pos)
             when (> pos 0) do
               (setq modifier-cpd (car (member parent singletons :key #'cpd-dependent-id :test #'equal)))
               (modify-cpd singleton cpd))))
    (setq new-cpds (make-array (length singletons) :initial-contents (update-cpd-assignments singletons) :fill-pointer t))
    (cons new-cpds (make-graph-edges new-cpds))))

#| Generates initial evidence table with variable-message mappings

;; factors = observed factors
(defun make-observations (factors &aux (evidence (make-hash-table :test #'equal)))
  (loop
    with idx and value
    for factor being the elements in factors
    when (> (hash-table-count (cpd-assignments factor)) 0) do
      (loop
        named indexer
        for assn-idx being the hash-keys of (cpd-assignments factor) do
          (setq idx assn-idx)
          (return-from indexer))
      (setq value (car (rassoc idx (gethash 0 (cpd-var-value-map factor)))))
      (when (null value)
        (error "~%Can't make nil observation:~%~%factor:~%~S~%index for vvm var: ~d~%evidence:~%~S" factor idx evidence))
      (setf (gethash (cpd-dependent-id factor) evidence) value))
  evidence)
|#

#| Generates initial evidence table with variable-message mappings |#

;; factors = observed factors
(defun make-observations (factors &aux (evidence (make-hash-table :test #'equal)))
  (loop
    with idx and var
    for factor being the elements in factors do
      (loop
        named indexer
        for rule being the elements of (rule-based-cpd-rules factor)
        when (= (rule-probability rule) 1) do
          (setq idx (gethash (rule-based-cpd-dependent-id factor) (rule-conditions rule)))
          (return-from indexer))
      (setq var (car (rassoc idx (mapcar #'car (gethash 0 (rule-based-cpd-var-value-block-map factor))))))
      (when (null value)
        (error "~%Can't make nil observation:~%~%factor:~%~S~%index for vvm var: ~d~%evidence:~%~S" factor idx evidence))
      (setf (gethash (rule-based-cpd-dependent-id factor) evidence) var))
  evidence)

#| Make partially observed retrieval cue |#

;; cue = retrieval cue
;; percentage = percentage of visible observations
(defun remove-observations (cue percentage)
  (loop
    for state in cue
    with keep and remove and size
    do
       (setq size (array-dimension (car state) 0))
       (setq keep (round (* size percentage)))
       (setq remove (- size keep))
       ;;(format t "~%observability: ~d~%total size of state: ~d~%keeping ~d elements and removing ~d" percentage size keep remove)
    collect
    (loop
      for i from 1 to remove
      with copy-observation = (copy-seq (car state))
      with index
      do
         (setq index (random (array-dimension copy-observation 0)))
         (setq copy-observation (concatenate 'vector (subseq copy-observation 0 index)
                                             (if (< (1+ index) (array-dimension copy-observation 0))
                                                 (subseq copy-observation (1+ index)))))
      #|
         (loop
      for j from saved-j to (array-dimension copy-observation 0)
      with removep
      do
         (setq removep (random 2))
         (when (= removep 1)
         (when (= j (- (array-dimension copy-observation 0) 1))
         (setq j 0))
         (setq saved-j j)
         (setq copy-observation (concatenate 'vector (subseq copy-observation 0 j)
         (if (< (1+ j) (array-dimension copy-observation 0))
         (subseq copy-observation (1+ j)))))
         (return))
         (when (= j (- (array-dimension copy-observation 0) 1))
         (setq j 0)))|#
      finally
      #|
         (when matt-testing* 
         (loop
      with predicate-cue
      with predicate
      with cur-percept
         ;;assumes these cpds are percepts
      for cpd being the elements of copy-observation
      do
         (cond ((action-cpd-p cpd)
         (cond ((equal (gethash cur-percept (rule-based-cpd-identifiers cpd)) 1)
         (setq predicate  (reverse (cons (gethash 0 (rule-based-cpd-vars cpd))
         (reverse predicate)))))
         (t
         (when predicate
				 (setq predicate-cue (cons (reverse predicate) predicate-cue))
				 (setq predicate nil))
         (setq cur-percept (rule-based-cpd-dependent-id cpd))
         (setq predicate (cons (rule-based-cpd-dependent-var cpd) predicate)))))
         ((percept-cpd-p cpd)
         (when predicate
         (setq predicate-cue (cons (reverse predicate) predicate-cue))
         (setq predicate nil))
         (setq cur-percept (rule-based-cpd-dependent-id cpd))
         (setq predicate (cons (rule-based-cpd-dependent-var cpd) predicate))
         (setq predicate (cons (caaar (last (gethash 0 (rule-based-cpd-var-value-block-map cpd)))) predicate)))
         ((attribute-cpd-p cpd)
         (cond ((equal (gethash cur-percept (rule-based-cpd-identifiers cpd)) 1)
         (setq predicate  (cons (gethash 0 (rule-based-cpd-vars cpd)) predicate))
         (setq predicate (cons (caaar (last (gethash 0 (rule-based-cpd-var-value-block-map cpd)))) predicate)))
         (t
         (when predicate
				 (setq predicate-cue (cons (reverse predicate) predicate-cue))
				 (setq predicate nil))
         (setq cur-percept nil)
         (maphash #'(lambda (k v)
         (when (= v 1)
         (setq cur-percept k)))
         (rule-based-cpd-identifiers cpd))
         (setf (rule-based-cpd-identifiers cpd) (make-hash-table :test #'equal))
			       (setf (gethash (rule-based-cpd-dependent-id cpd)
					      (rule-based-cpd-identifiers cpd))
				     0)
			       (setq predicate (cons (rule-based-cpd-dependent-var cpd) predicate))
			       (setq predicate (cons (caaar (last (gethash 0 (rule-based-cpd-var-value-block-map cpd)))) predicate))))))
	      finally
		 (when predicate
		   (setq predicate-cue (cons (reverse predicate) predicate-cue)))
		 (setq predicate-cue (reverse predicate-cue))
		 (let ((*print-circle* nil))
		   (format t "~%predicate cue:~%~A" predicate-cue)
		   (log-message (list "~%:observation~%~A" predicate-cue) (format nil "matt-trace/matt-trace-observability~d.txt" percentage)))))
	  |#
         (let (edges)
           (setq edges (make-graph-edges copy-observation))
           (return (cons copy-observation edges))))))

#| Produce recollections from model |#

;; model = state from event memory element from which to make predictions
;; episode = current state from which to supply evidence
;; mappings = list of cpd-to-cpd bindings
;; bbindings = list of bindings
;; mode = inference mode ('+ or 'max)
;; lr = learning rate
(defun get-recollections (model episode mappings bbindings mode lr)
  (loop
    for (p-match . q-match) being the elements of (car mappings)
    with bindings = (car bbindings)
    with p-copy and observed-factors and observed-factor and num-assignments
    with dep-id and dep-var and vars and idents and types-hash and cids and qvars and vvm and cards and steps and assns and lvl
    when (and q-match (not (member (cpd-dependent-id (aref (car model) q-match)) observed-factors :key #'cpd-dependent-id :test #'equal))) do
       ;;(setq p-copy (copy-cpd p-match))
       (setq p-copy (subst-cpd (aref (car episode) p-match) (aref (car model) q-match) bindings))
       (setq dep-id (cpd-dependent-id p-copy))
       (setq idents (make-hash-table :test #'equal))
       (setf (gethash dep-id idents) 0)
       (setq dep-var (cpd-dependent-var p-copy))
       (setq vars (make-hash-table))
       (setf (gethash 0 vars) dep-var)
       (setq types-hash (make-hash-table))
       (setf (gethash 0 types-hash) (gethash 0 (cpd-types p-copy)))
       (setq cids (make-hash-table))
       (setf (gethash 0 cids) (gethash 0 (cpd-concept-ids p-copy)))
       (setq qvars (make-hash-table))
       (setf (gethash 0 qvars) (gethash 0 (cpd-qualified-vars p-copy)))
       (setq vvm (make-hash-table))
       (setf (gethash 0 vvm) (gethash 0 (cpd-var-value-map p-copy)))
       (setq cards (make-array 1 :initial-element (aref (cpd-cardinalities p-copy) 0) :fill-pointer t))
       (setq num-assignments (reduce #'* (coerce cards 'list)))
       (setq steps (make-array 1 :initial-element (aref (cpd-step-sizes p-copy) 0) :fill-pointer t))
       ;;(setq assns (make-array (aref cards 0) :initial-element 0))
       (setq assns (make-hash-table))
       (setf (gethash (- num-assignments 1) assns) 1)
       (setq lvl (cpd-lvl p-copy))
       (setq observed-factor (make-cpd :dependent-id dep-id
                                       :identifiers idents
                                       :dependent-var dep-var
                                       :vars vars
                                       :types types-hash
                                       :concept-ids cids
                                       :qualified-vars qvars
                                       :var-value-map vvm
                                       :cardinalities cards
                                       :step-sizes steps
                                       :assignments assns
                                       :lvl lvl))
       (setq observed-factors (cons observed-factor observed-factors))
    finally
       (let (evidence-table recollection)
         (setq evidence-table (make-observations observed-factors))
         (setq recollection (loopy-belief-propagation model evidence-table mode lr))
         (return (values recollection bindings)))))

#| Recollect an experience |#

;; eltm = event memory
;; pstm = perceived objects in retrieval cue
;; cstm = given relations in the retrieval cue
;; cue-states = retrieval cue states (list (factors . edges))
;; mode = inference mode ('+ or 'max)
;; lr = learning rate
;; observability = percent of state observable
(defun remember (eltm cue-states mode lr bic-p &optional (observability 1))
  (let (partial-states cue bindings)
    ;;(log-message (list "~d," (array-dimension (caar cue-states) 0)) "vse.csv")
    ;;(state-count-element-types (caar cue-states))
    (setq partial-states (remove-observations cue-states observability))
    ;;(log-message (list "~d," (array-dimension (caar partial-states) 0)) "vse.csv")
    ;;(state-count-element-types (caar partial-states))
    ;;(log-message (list "~A," (specific-cue-p (caar partial-states))) "vse.csv")
    ;;(log-message (list "~d," observability) "vse.csv")
    ;;(log-message (list "~A~%" partial-states) "cues.txt")
    (setq cue (make-episode
                       :states partial-states
                       :decompositions (make-empty-graph)
                       :id-ref-map (make-hash-table :test #'equal)
                       :num-decompositions 0))
    ;;(format t "~%original cue:~%~A~%partial-cue:~%~A" cue partial-cue)
    (multiple-value-bind (eme sol bindings depth cost)
        (cond ((equalp (make-array 0) (caar cue-states))
               (values (car eltm) nil nil 0 most-positive-fixnum most-positive-fixnum))
              (t
               (retrieve-episode eltm cue nil :bic-p bic-p)))
      (declare (ignore depth cost))
      (setq bindings (car bbindings))
      #|
      (if (> (episode-count eme) 1)
          (log-message (list "schema,") "vse.csv")
          (log-message (list "episode,") "vse.csv"))
      |#
      ;;(log-message (list "~d,~d,~d,~d,~d," cost weighted-cost depth (episode-count eme) (array-dimension (caar (episode-states eme)) 0)) "vse.csv")
      ;;(state-count-element-types (caar (episode-states eme)))
      ;;(format t "~%partial cue:~%~A~%retrieved event memory element:~%~A~%bindings: ~A" partial-states eme bindings)
      (loop
        for (p-match . q-match) being the elements of (car mappings)
        with p-copy and observed-factors and observed-factor and num-assignments
        with dep-id and dep-var and vars and idents and types-hash and cids and qvars and vvbm and var-values and cards and steps and rules and lvl
        do
           (setq p-copy (copy-rule-based-cpd (aref (caar partial-states) p-match)))
           (when (and q-match (not (member (rule-based-cpd-dependent-id (aref (caar (episode-states eme)) q-match))
					   observed-factors :key #'rule-based-cpd-dependent-id :test #'equal)))
             ;;(setq p-copy (copy-cpd p-match))
	     (when nil (gethash "ACTION7333" (rule-based-cpd-identifiers (aref (caar (episode-states eme)) q-match))) 
	       (format t "~%~%p-cpd:~%~S~%q-cpd:~%~S~%episode id: ~S~%bindings:~%~S" (aref (caar partial-states) p-match)
		       (aref (caar (episode-states eme)) q-match)
		       (episode-id eme)
		       bindings))
             (setq p-copy (subst-cpd (aref (caar partial-states) p-match) (aref (caar (episode-states eme)) q-match) bindings))
	     (when nil (gethash "ACTION7333" (rule-based-cpd-identifiers (aref (caar (episode-states eme)) q-match)))
	       (format t "~%subst:~%~S~%likelihood(p,q) = ~d" p-copy (local-likelihood p-copy (aref (caar (episode-states eme)) q-match))))
             (setq dep-id (rule-based-cpd-dependent-id p-copy))
             (setq idents (make-hash-table :test #'equal))
             (setf (gethash dep-id idents) 0)
             (setq dep-var (rule-based-cpd-dependent-var p-copy))
             (setq vars (make-hash-table))
             (setf (gethash 0 vars) dep-var)
             (setq types-hash (make-hash-table))
             (setf (gethash 0 types-hash) (gethash 0 (rule-based-cpd-types p-copy)))
             (setq cids (make-hash-table))
             (setf (gethash 0 cids) (gethash 0 (rule-based-cpd-concept-ids p-copy)))
             (setq qvars (make-hash-table))
             (setf (gethash 0 qvars) (gethash 0 (rule-based-cpd-qualified-vars p-copy)))
             (setq vvbm (make-hash-table))
             (setf (gethash 0 vvbm) (gethash 0 (rule-based-cpd-var-value-block-map p-copy)))
	     (setq var-values (make-hash-table))
	     (setf (gethash 0 var-values) (gethash 0 (rule-based-cpd-var-values p-copy)))
	     (setq cards (make-array 1 :initial-element (aref (rule-based-cpd-cardinalities p-copy) 0) :fill-pointer t))
             ;;(setq num-assignments (reduce #'* (coerce cards 'list)))
             (setq steps (make-array 1 :initial-element (aref (rule-based-cpd-step-sizes p-copy) 0) :fill-pointer t))
             ;;(setq assns (make-array (aref cards 0) :initial-element 0))
             (setq rules (make-array (aref cards 0)))
             (loop
               with rule
	       with max-card = (apply #'max (gethash 0 var-values))
               for card in (gethash 0 var-values) ;;from 0 to (- (aref cards 0) 1)
	       for counter from 0
	       when (= card max-card)
                 do
                    (setq rule (make-rule :id (gensym "RULE-")
                                          :conditions (make-hash-table :test #'equal)
                                          :probability 1
                                          :count 1))
                    (setf (gethash dep-id (rule-conditions rule)) card)
                    (setf (aref rules counter) rule)
               else
                 do
                    (setq rule (make-rule :id (gensym "RULE-")
                                          :conditions (make-hash-table :test #'equal)
                                          :probability 0
                                          :count 1))
                    (setf (gethash dep-id (rule-conditions rule)) card)
                    (setf (aref rules counter) rule))
             (setq lvl (rule-based-cpd-lvl p-copy))
             (setq observed-factor (make-rule-based-cpd :dependent-id dep-id
                                                        :identifiers idents
                                                        :dependent-var dep-var
                                                        :vars vars
                                                        :types types-hash
                                                        :concept-ids cids
                                                        :qualified-vars qvars
                                                        :var-value-block-map vvbm
							:var-values var-values
							:cardinalities cards
                                                        :step-sizes steps
							:rules rules
							:singleton-p t
                                                        :lvl lvl))
             (setq observed-factors (cons observed-factor observed-factors)))
        finally
	   (let (evidence-table recollection max-card ground-marginals)
             (setq evidence-table (make-observations observed-factors))
             (setq max-card 0)
             (loop
	       with msg
               for ground-cpd being the elements of (caar (episode-states eme))
               when (and (> (aref (rule-based-cpd-cardinalities ground-cpd) 0) max-card))
                 do
                    (setq max-card (aref (rule-based-cpd-cardinalities ground-cpd) 0))
		    (setq msg (factor-operation ground-cpd (hash-keys-to-list (rule-based-cpd-identifiers ground-cpd))
					  (remove (rule-based-cpd-dependent-id ground-cpd)
                                                  (hash-keys-to-list (rule-based-cpd-identifiers ground-cpd)))
                                          '+))
               collect (normalize-rule-probabilities msg (rule-based-cpd-dependent-id msg))
                 into grounds
               finally
                  (setq ground-marginals grounds))
             ;;(log-message (list "Learning_Rate,Iteration,Conflicts,Deltas,Deltas_std~%") "learning-curves.csv")
             ;;(log-message (list "Learning_Rate,CPD,Value,Density,Error~%") "marginal-distribution.csv")
             ;;(log-message (list "CPD,Value,Density~%") "ground-marginals.csv")
	     #|
	     (loop
               for ground-cpd in ground-marginals
               do
                  (loop
                    with idx
                    for i from 0 to (- max-card 1)
                    do
                       (setq idx (make-rule :conditions (make-hash-table :test #'equal)))
                       (setf (gethash (rule-based-cpd-dependent-id ground-cpd) (rule-conditions idx)) i)
                       (setq idx (car (get-compatible-rules ground-cpd ground-cpd idx :find-all nil)))
                       (log-message (list "~A,~d,~d~%"
                                          (rule-based-cpd-dependent-id ground-cpd)
                                          i
                                          (float (rule-probability idx)))
                                    "ground-marginals.csv")))
	     |#
             (setq recollection (loopy-belief-propagation (car (episode-states eme)) evidence-table mode lr))
             #|
	     (setq copy-grounds (copy-list ground-marginals))
	     (loop
               with ground-cpd
               for cpd in recollection
               when (rule-based-cpd-singleton-p cpd)
                 do
                    (setq ground-cpd (car copy-grounds))
                    (setq copy-grounds (rest copy-grounds))
                    (loop
                      with idx with ground-idx
                      for i from 0 to (- max-card 1)
                      do
                         (setq idx (make-rule :conditions (make-hash-table :test #'equal)))
                         (setf (gethash (rule-based-cpd-dependent-id cpd) (rule-conditions idx)) i)
                         (setq idx (car (get-compatible-rules cpd cpd idx :find-all nil)))
                         (setq ground-idx (car (get-compatible-rules ground-cpd cpd idx :find-all nil)))
                         (log-message (list "~d,~A,~d,~d,~d~%"
                                            lr
                                            (rule-based-cpd-dependent-id cpd)
                                            i
                                            (float (rule-probability idx))
                                            (abs (float (- (rule-probability idx) (rule-probability ground-idx)))))
                                      "marginal-distribution.csv")))
	     |#
             (return (values recollection eme)))))))

#| Add a new experience to the episodic buffer and insert it into memory when appropriate |#

;; state = dotted list where cons is an array of cpds, and the cdr is a hash table of edges
;; cstm = relational beliefs that are true about the world
;; pstm = sensory perceptions of the world
;; bic-p = flag for using the Bayesian information criterion. If false, system just uses likelihood
(defun push-to-ep-buffer (&key (state nil) (cstm nil) (pstm nil) (insertp nil) (bic-p t) &aux ep)
  (let (p ref empty-decomp ep-id)
    (cond (state
           (setf (gethash 0 (getf episode-buffer* :obs))
                 (nreverse (cons state (nreverse (gethash 0 (getf episode-buffer* :obs)))))))
          (t
           (multiple-value-bind (p-elements p-edges)
               (state-to-graph pstm cstm :executing-intention executing-intention*)
             (setq p (cons p-elements p-edges)))
           (setf (gethash 0 (getf episode-buffer* :obs))
                 (nreverse (cons p (nreverse (gethash 0 (getf episode-buffer* :obs))))))))
    (when (and (or pstm state) insertp)
      (format t "~%Adding new episode.")
      (finish-output)
      (setq empty-decomp (make-empty-graph))
      (setq ep-id (symbol-name (gensym "EPISODE-")))
      (setq ep (make-episode :id ep-id
                             :index-episode-id ep-id
                             :states (mapcar #'copy-observation (last (gethash 0 (getf episode-buffer* :obs))))
                             :decompositions empty-decomp
                             :id-ref-map (make-hash-table :test #'equal)
                             :num-decompositions 0
                             :count 1
                             :lvl 1))
      (format t "~%inserting ~A" (episode-id ep))
      (multiple-value-setq (eltm* ref)
        (insert-episode eltm* ep nil :bic-p bic-p))
      (setf (gethash 1 (getf episode-buffer* :obs))
            (nreverse (cons (list ref (copy-observation (car (episode-states (car ref))))) (nreverse (gethash 1 (getf episode-buffer* :obs))))))
      ;;(eltm-to-pdf)
      ;;(break)
      )))

#| Find the lowest common ancestor of two episodes

;; ep1 = episode
;; ep2 = episode
(defun get-common-episode-class (ep1 ep2)
  (loop
    while (not (= (episode-depth ep1) (episode-depth ep2)))
    if (> (episode-depth ep1) (episode-depth ep2))
      do
	 (setq ep1 (car (episode-parent ep1)))
    else
      do
	 (setq ep2 (car (episode-parent ep2))))
  (loop
    while (not (equal (episode-id ep1) (episode-id ep2)))
    do
       (setq ep1 (car (episode-parent ep1)))
       (setq ep2 (car (episode-parent ep2))))
  ep1)
    |#

#| Find the lowest common ancestor of two episodes|#

;; ep1 = pattern episode
;; ep2 = base episode
(defun get-common-episode-class (ep1 ep2)
  (when (>= (episode-depth ep1) (episode-depth ep2))
    (loop
      while (not (= (episode-depth ep1) (episode-depth ep2)))
      do
         (setq ep1 (car (episode-parent ep1)))
      finally
         (when (equal (episode-id ep1) (episode-id  ep2))
           ep1))))

#| Add a new experience to the episodic buffer and insert it into memory when appropriate |#

;; state = dotted list where car is an array of cpds, and the cdr is a hash table of edges
;; bic-p = flag for using the Bayesian information criterion. If false, system just uses likelihood
(defun new-push-to-ep-buffer (&key (observation nil) (state nil) (action-name nil) (bic-p t) (insertp nil) (temporal-p t) (hidden-state-p nil))
  (let (obs st model)
    (setq obs observation)
    (if state
	(setq st state))
    (setf (gethash 0 (getf episode-buffer* :obs))
          (nreverse (cons (list obs st action-name) (nreverse (gethash 0 (getf episode-buffer* :obs))))))
    (cond (temporal-p
	   (unless insertp
	     (setq model (getf (getf episode-buffer* :h-model) :model))
	     (setq model (event-boundary-p model
					   (subseq (gethash 0 (getf episode-buffer* :obs)) (if model (getf model :scope) 0))
					   eltm*
					   nil
					   nil
					   bic-p
					   hidden-state-p))
	     (when (getf model :model)
	       (format t "~%retrieved model:~%~S" (episode-id (getf model :model))))
	     (unless (getf model :model)
	       (setq insertp t))))
	  (t
	   (setq insertp t)))
    (when t
      (format t "~%insertp?: ~A" insertp))
    (when insertp
      (loop
	with state-transitions = nil
        with ep and ep-id and obs-ref and st-ref
        with cur-st and cur-obs and cur-act and prev-act and prev-st and prev-obs and st-bn and id-ref-hash = (make-hash-table :test #'equal)
        for (o s act) in (butlast (gethash 0 (getf episode-buffer* :obs)))
	for i from 0
	do
           (setq ep-id (symbol-name (gensym "EPISODE-")))
           (setq ep (make-episode :id ep-id
                                  :index-episode-id ep-id
                                  :observation (copy-bn o)
				  :state-transitions (cons (make-array 0) (make-hash-table :test #'equal))
				  :backlinks (make-hash-table :test #'equal)
				  :count 1
                                  :lvl 1))
           (format t "~%inserting observation, ~A" (episode-id ep))
           (multiple-value-setq (eltm* obs-ref)
             (new-insert-episode eltm* ep nil :bic-p bic-p))
	   (when temporal-p
	     (when st
	       (setq ep-id (symbol-name (gensym "EPISODE-")))
               (setq ep (make-episode :id ep-id
                                      :index-episode-id ep-id
                                      :observation (copy-bn s)
				      :state-transitions (cons (make-array 0) (make-hash-table :test #'equal))
				      :backlinks (make-hash-table :test #'equal)
				      :count 1
                                      :lvl 1))
	       (format t "~%inserting state, ~A" (episode-id ep))
               (multiple-value-setq (eltm* st-ref)
		 (new-insert-episode eltm* ep nil :bic-p bic-p))
	       (setf (gethash (episode-id (car st-ref)) id-ref-hash) st-ref))
	     (when st
	       (setq cur-st (gensym "STATE-")))
	     (setq cur-obs (gensym "OBS-"))
	     (setq cur-act (gensym "ACT-"))	     
	     ;; you have to put these hash keys in after you've done all the insertions for observations, states, and (eventually) actions. Otherwise, if you update id-ref-hash, then do further insertions, the episode id of the reference will change, making the key obsolte. Worse, the cpd vvbm will have the name of the new ref hash key, but the id ref hash will have the name of the old ref. So, look ups will fail.
	     (setf (gethash (episode-id (car obs-ref)) id-ref-hash) obs-ref)
	     (when st
	       (setq state-transitions (concatenate 'list state-transitions `(,cur-st = (state-node state :value ,(episode-id (car st-ref)))))))
	     (setq state-transitions (concatenate 'list state-transitions `(,cur-obs = (observation-node observation :value ,(episode-id (car obs-ref))))))
	     (setq state-transitions (concatenate 'list state-transitions `(,cur-act = (percept-node action :value ,act))))
	     (when st
	       (setq state-transitions (concatenate 'list state-transitions `(,cur-st -> ,cur-obs))))
	     (setq state-transitions (concatenate 'list state-transitions `(,cur-obs -> ,cur-act)))
	     (cond (st
		    (when prev-st
		      (setq state-transitions (concatenate 'list state-transitions `(,prev-st -> ,cur-st)))))
		   (t
		    (when prev-obs
		      (setq state-transitions (concatenate 'list state-transitions `(,prev-obs -> ,cur-obs))))))
	     (when prev-act
	       (if st
		   (setq state-transitions (concatenate 'list state-transitions `(,prev-act -> ,cur-st)))
		   (setq state-transitions (concatenate 'list state-transitions `(,prev-act -> ,cur-obs)))))
	     (if st
		 (setq prev-st cur-st)
		 (setq prev-obs cur-obs))
	     (setq prev-act cur-act))
        finally
	   (when state-transitions
             ;;(setq state-transitions (concatenate 'list ,@state-transitions))
	     (setq st-bn (eval `(compile-program nil ,@state-transitions)))
	     (when (> (hash-table-count (rule-based-cpd-identifiers (aref (car st-bn) 0))) 1)
	       (format t "~%state transition model:~%~A" st-bn)
	       (break))
             ;; make temporal episode from state transitions
             (setq ep-id (symbol-name (gensym "EPISODE-")))
             (setq ep (make-episode :id ep-id
                                    :index-episode-id ep-id
				    :observation (cons (make-array 0) (make-hash-table :test #'equal))
                                    :state-transitions st-bn
				    :backlinks id-ref-hash
                                    :temporal-p t
                                    :count 1
                                    :lvl 2))
	     (format t "~%Inserting transition model, ~A" (episode-id ep))
             (setq eltm* (new-insert-episode eltm* ep nil :bic-p bic-p))))
      ;; clear buffer
      (clear-episodic-cache 0 1)
      #|
      (setf (gethash 1 (getf episode-buffer* :obs))
      (nreverse (cons (list ref (copy-observation (car (episode-states (car ref))))) (nreverse (gethash 1 (getf episode-buffer* :obs))))))
      |#
      ;;(eltm-to-pdf)
      ;;(break)
      )))

(defun print-h-buffer ()
  (loop
    with lvls = (reduce #'max (hash-keys-to-list (getf episode-buffer* :obs)))
    for i from 2 to lvls
    do
       (format t "~%lvl ~d: " i)
       (format t "~{~d~^ ~}" (mapcar #'(lambda (ref) (episode-id (caar ref))) (gethash i (getf episode-buffer* :obs))))))

#| Add abstracted episode to event memory |#

;; intention = executing intention
(defun push-higher-lvl-episode-to-buffer (intention-operator intention-active-cycles intention-executed-subskills &key (bic-p t))
  (let (higher-lvl-episode start-state end-state dcmps reject-list ref cur-lvl ep-id)
    (setq cur-lvl (sclause-height intention-operator))
    (setq ep-id (symbol-name (gensym "EPISODE-")))
    (setq higher-lvl-episode (make-episode :id ep-id :index-episode-id ep-id :count 1 :lvl cur-lvl))
    (when t
      (format t "~%intention-operator:~%~A~%intention active cycles: ~d~%intention-decompositions: ~d" intention-operator intention-active-cycles (length intention-executed-subskills))
      (format t "~%~%h-cache before insertion")
      (print-h-buffer))
    (loop
      with decomposition and skill-lvl and decompositions
      for subskill in intention-executed-subskills
      do
         (setq skill-lvl (sclause-height (intention-operator subskill)))
         (setq decomposition (car (last (gethash skill-lvl (getf episode-buffer* :obs)))))
         (setf (gethash skill-lvl (getf episode-buffer* :obs)) (butlast (gethash skill-lvl (getf episode-buffer* :obs))))
         (setq decompositions (cons decomposition decompositions))
      finally
         (cond ((null decompositions)
                (setq dcmps (last (gethash 1 (getf episode-buffer* :obs))
                                  intention-active-cycles))
                (setf (gethash 1 (getf episode-buffer* :obs))
                      (butlast (gethash 1 (getf episode-buffer* :obs))
                               intention-active-cycles)))
               (t
                (setq dcmps decompositions))))
    (when nil
      (format t "~%decomps are:~%~{~A~^, ~}" (mapcar #'episode-id (mapcar #'caar dcmps))))
    (let (last-decomp)
      (setq last-decomp (car (last dcmps)))
      (setq start-state (second (car dcmps)))
      (if (third last-decomp)
          (setq end-state (third last-decomp))
          (setq end-state (second last-decomp))))
    (when nil 
      (format t "~%precondition:~%~S~%~%post-condition:~%~S" start-state end-state))
    (setf (episode-states higher-lvl-episode) (list start-state end-state))
    (multiple-value-bind (factors edges)
        (get-cpds-for-decompositions (mapcar #'car dcmps) nil)
      (setf (episode-decompositions higher-lvl-episode) (cons factors edges)))
    (loop
      for dcmp in (mapcar #'car dcmps)
      with id-ref-map = (make-hash-table :test #'equal)
      do
         (setf (gethash (episode-id (car dcmp)) id-ref-map) dcmp)
         (setq reject-list (cons (episode-id (car dcmp)) reject-list))
      finally
         (setf (episode-id-ref-map higher-lvl-episode) id-ref-map)
         (setf (episode-num-decompositions higher-lvl-episode) (length dcmps)))
    (when nil
      (format t "~%episode states:~%~d~%episode decompositions:~%~d" (length (episode-states higher-lvl-episode)) (array-dimension (car (episode-decompositions higher-lvl-episode)) 0)))
    (when t
      (format t "~%adding level ~d episode: ~A" (episode-lvl higher-lvl-episode) (episode-id higher-lvl-episode)))
    ;;(break "~%higher-lvl-episode:~%~A" higher-lvl-episode)
    (when nil
      (format t "~%reject list: ~A" reject-list))
    (multiple-value-setq (eltm* ref)
      (insert-episode eltm* higher-lvl-episode reject-list :bic-p bic-p))
    (loop
      for dcmp in dcmps ;;(mapcar #'car dcmps)
      do
         (setf (episode-abstraction-ptrs (caar dcmp)) (cons ref (episode-abstraction-ptrs (caar dcmp)))))
    ;;(eltm-to-pdf)
    ;;(break)
    (setf (gethash cur-lvl (getf episode-buffer* :obs))
          (nreverse (cons (list ref (copy-observation start-state) (copy-observation end-state)) (nreverse (gethash cur-lvl (getf episode-buffer* :obs))))))
    (when t
      (format t "~%h cache after insertion:")
      (print-h-buffer))))

#| Clear contents of episodic cache |#

;; lvl = level to remove items from cache
;; n = number of items to remove from cache
(defun clear-episodic-cache (&optional lvl n)
  (cond ((and lvl n)
         (setf (gethash lvl (getf episode-buffer* :obs))
               (last (gethash lvl (getf episode-buffer* :obs)))))
        ((and lvl (not n))
         (setf (gethash lvl (getf episode-buffer* :obs)) nil))
	(t
         (setf (getf episode-buffer* :obs) (make-hash-table)))))

#| Add a higher-level episode to event memory |#

;; intention-operator = sclause of intention
;; intention-active-cycles = number of cycles that intention was active
;; intention-executed-subskills = the executed subskills in the intention
(defun push-high-lvl-ep-to-ep-buffer (intention-operator intention-active-cycles intention-executed-subskills &key (bic-p t))
  ;;(break "Current intention operator:~%~A~%Successfully completed subskills:~%~A" (intention-operator intention) (intention-successful-subskill-executions intention))
  (push-higher-lvl-episode-to-buffer intention-operator intention-active-cycles intention-executed-subskills :bic-p bic-p)
  #|
  (clear-episodic-cache (- (sclause-height (intention-operator intention)) 1)
                        (intention-active-cycles intention)))
  |#		  )

#| Print the previous state |#

;; state = state in episode
;; id = unique numeric suffix from episode id
;; label = state index, ex: State_1
(defun print-epi-state (state id label)
  (let (string-stream res)
    (setq string-stream (make-string-output-stream))
    (loop
      for cpd being the elements of (car state)
      for i from 0
      with name1
      do
         (setq name1 (combine-symbols "EPISODE" id (rule-based-cpd-dependent-id cpd) label))
         (write-string name1 string-stream)
         (write-string "[label=" string-stream)
         ;;(format string-stream "~S" (rule-based-cpd-dependent-var cpd))
	 (format string-stream "~S" (rule-based-cpd-dependent-id cpd))
         (write-string ",shape=oval,color=blue]" string-stream)
         (write-line ";" string-stream))
    (loop
       with name1 and name2
       for cpd being the elements of (car state) do
         (setq name2 (combine-symbols "EPISODE" id (rule-based-cpd-dependent-id cpd) label))
         (loop
           for ident being the hash-keys of (rule-based-cpd-identifiers cpd)
             using (hash-value i)
             when (> i 0) do
	      (setq name1 (combine-symbols "EPISODE" id ident label))
	      (write-string name1 string-stream)
	      (write-string " -> " string-stream)
	      (write-string name2 string-stream)
	      (write-line ";" string-stream)))
    (setq res (get-output-stream-string string-stream))
    res))

#| Helper function to graph the previous stte |#

;; state = state to print
;; id = unique numeric suffix from episode id
;; label = state index, ex: State_1
;; stream = stream to hold output
(defun print-episode-state (state id label stream)
  (format stream "subgraph cluster_~d {~%label = ~A;~%~A}~%" (gensym "STATE") label (print-epi-state state id label)))

#| Helper function for printing the episodic memory in pdf form |#

;; eps = episodes
;; parent = parent of episodes
;; stream = output stream
(defun pdf-helper (eps parent stream)
  (let* ((ep (car eps))
         (id (subseq (episode-id ep) (+ (search "-" (episode-id ep)) 1))))
    (format stream "subgraph cluster_~d {label = ~d~%" id (combine-symbols "EPISODE" id "count" (episode-count ep)))
    (print-episode-state (episode-observation ep) id "Observation" stream)
    (print-episode-state (episode-state-transitions ep) id "State" stream)
    ;;(format t "~%HERE!!!!")
    (format stream "~d[shape=point style=invis]~%" id)
    ;;(format stream "~d~%" id)
    (format stream "}~%")
    (when parent
      (format stream "~A -> ~A [ltail=~A lhead=~A];~%" parent id (concatenate 'string "cluster_" parent)  (concatenate 'string "cluster_" id)))
    ;;visit children
    (mapcar #'(lambda (e)
                (pdf-helper e id stream))
            (cdr eps))))

#| Graphically display the episodic memory |#

;; eltm = episodic memory
;; count = count for the file number
(defun eltm-to-pdf (&optional eltm count)
  (let ((ep (if eltm eltm eltm*))
        string-stream)
    (setq string-stream (make-string-output-stream))
    (write-string "eltm" string-stream)
    (when eltm
      (write-string (write-to-string count) string-stream))
    (write-string ".pdf" string-stream)
    (cond (ep
	   (with-open-file (eltm-file "eltm.dot"
				      :direction :output
				      :if-exists :supersede
				      :if-does-not-exist :create)
	     (format eltm-file "digraph G {~%compound=true;~%")
	     (pdf-helper ep nil eltm-file)
	     (format eltm-file "}~%"))
	   (uiop:run-program
	    `("dot" "-Tpdf" "eltm.dot" "-o" ,(get-output-stream-string string-stream))))
	  (t
	   (format t "~%failed to generate pdf. eltm is nil")))))
    
