(in-package :hems)

#| Reference circular structures rather than print them all |#
(setf *print-circle* t)
(setf *print-pretty* t)
(setf *print-lines* nil)

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
;; state = state variables represented as a BN
;; state-transitions = state-transition graph and observation  model represented as a BN
;; temporal-p = flag for whether the episode represents a course of events (state transitions) or state of affairs (observation)
;; backlinks = hash table of episode ids to back-links references pointing to lower-level observation/state transition models in the event memory
;; abstraction-ptrs = list of pointers to higher level abstraction branch, if it exists
;; count = number of times episode occured
;; depth = depth of episode in episodic long-term memory. Fixed at insertion
;; lvl = abstraction level of the episode. Observations are lowest level, then, hierarchically abstracted state transition models are higher.
(defstruct episode id index-episode-id parent observation state state-transitions temporal-p backlinks abstraction-ptrs count depth lvl)

#| Returns the episodic long-term memory structure |#

(defun get-eltm ()
  eltm*)

(defun init-eltm()
  (setq eltm* nil)
  (sb-ext:gc :full t)
  eltm*)

(defun generate-list()
  (loop
    with hash
    with prev = nil
    for i from 1 to 1000
    do
       (setq hash (make-hash-table :test #'equal))
       (setf (gethash i hash) i)
       (setq prev (cons (cons hash prev) prev))
    finally
       (return prev)))

(defun make-empty-episode ()
  (let ((ep-id (symbol-name (gensym "EPISODE-"))))
    (make-episode
     :id ep-id
     :index-episode-id ep-id
     :observation (cons (make-array 0) (make-hash-table :test #'equal))
     :state (cons (make-array 0) (make-hash-table :test #'equal))
     :state-transitions (cons (make-array 0) (make-hash-table :test #'equal))
     :backlinks (make-hash-table :test #'equal)
     :count 0
     :depth 0
     :lvl 0)))

#| Performs deep copy on episode |#

;; ep = episode to copy
(defun copy-ep (ep &key (fresh-id t))
  (make-episode
   :id (if fresh-id (symbol-name (gensym "EPISODE-")) (episode-id ep))
   :index-episode-id (episode-index-episode-id ep)
   :parent (if (episode-parent ep) (episode-parent ep))
   :observation (copy-bn (episode-observation ep))
   :state (copy-bn (episode-state ep))
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
;; bindings = variable bindings from p to q
;; q-first-bindings = variable bindings from q to p
(defun new-combine-bns (ep1-bn ep2-bn ep1-count mappings unmatched bindings q-first-bindings)
  (let (p q new-nodes)
    (setq p (copy-factors (car ep2-bn)))
    (setq q (copy-factors (car ep1-bn)))
    (when nil
      (format t "~%~%bindings:~%~S" bindings))
    (loop
      for (p-match . q-match) being the elements of mappings
      with node and nodes and p-cpd
      do
         (when (and nil print-special* (equal "SIX_483" (rule-based-cpd-dependent-id (aref p p-match))))
               (format t "~%~%p-cpd before subst:~%~S~%q-match:~%~S~%bindings:~%~S" (aref p p-match) (if q-match (aref q q-match)) bindings))
         (setq p-cpd (subst-cpd (aref p p-match) (when q-match (aref q q-match)) bindings :deep nil))
         (when (and nil print-special* (equal "SIX_483" (rule-based-cpd-dependent-id (aref p p-match))))
               (format t "~%p-cpd after subst:~%~S" p-cpd))
         (when (and nil print-special* (equal "SIX_483" (rule-based-cpd-dependent-id (aref p p-match))))
               (format t "~%p-match:~%~S~%p-cpd:~%~S~%q-cpd:~%~S" (aref p p-match) p-cpd (if q-match (aref q q-match)))
               ;;(break)
	       )
         (setq node (factor-merge p-cpd (if q-match (aref q q-match)) bindings q-first-bindings nodes ep1-count))
	 ;;(format t "~%p-match:~%~S~%subst p-match:~%~S~%q-match:~%~S~%node:~%~S" (aref p p-match) p-cpd (if q-match (aref q q-match)) node)
	 (setq nodes (nreverse (cons node (nreverse nodes))))
	 finally
            (setq new-nodes nodes))
    (loop
      for (dummy-match . unmatched-q) in unmatched
      with dm and node
      do
         (when nil (and print-special* (equal "SIX_483" (rule-based-cpd-dependent-id dummy-match)))
               (format t "~%dummy-match:~%~S~%unmatched q:~%~S" dummy-match (aref q unmatched-q)))
         (setq dm (subst-cpd dummy-match (aref q unmatched-q) bindings :deep nil))
         (setq node (factor-merge dm (aref q unmatched-q) bindings q-first-bindings new-nodes ep1-count))
         (when nil (and (equal "NO_OP7332" (rule-based-cpd-dependent-id dummy-match)))
               (format t "~%node:~%~S" node)
               (break))
         (setq new-nodes (nreverse (cons node (nreverse new-nodes)))))
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
;; bindings = variable bindings from p to q
;; q-first-bindings = variable bindings from q to p
(defun new-combine (ep1 ep2 mappings unmatched bindings q-first-bindings)
  ;;(format t "~%schema decopositions:~%~S~%num:~%~d~%episode decopositions:~%~S~%num:~%~d~%decomp bindings:~%~S" (episode-decompositions ep1) (episode-num-decompositions ep1) (episode-decompositions ep2) (episode-num-decompositions ep2) decomp-bindings)
  (when nil (episode-temporal-p ep2)
    (format t "~%~%schema ID: ~S~%Episode ID: ~S~%schema state transitions:~%~S~%episode state transitions:~%~S~%mappings:~%~S~%bindings:~%~S~%q-first-bindings:~%~S"
	    (episode-id ep1)
	    (episode-id ep2)
	    (episode-state-transitions ep1)
	    (episode-state-transitions ep2)
	    mappings
	    bindings
	    q-first-bindings)
    (break))
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
		:observation (if (= (array-dimension (car (episode-observation ep2)) 0) 0)
				 (episode-observation ep1)
				 (new-combine-bns (episode-observation ep1) (episode-observation ep2) (episode-count ep1) mappings unmatched bindings q-first-bindings))
		:state (if (= (array-dimension (car (episode-state ep2)) 0) 0)
			   (episode-state ep1)
			   (new-combine-bns (episode-state ep1) (episode-state ep2) (episode-count ep1) mappings unmatched bindings q-first-bindings))
                :state-transitions (if (episode-temporal-p ep2)
                                       (new-combine-bns (episode-state-transitions ep1) (episode-state-transitions ep2) (episode-count ep1) mappings unmatched bindings q-first-bindings)
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
  (cond ((and nil check-decomps
	      (= (hash-table-count (episode-backlinks (car branch))) 0)
	      (> (hash-table-count (episode-backlinks ep)) 0))
         (when t
           (format t "~%rejecting ~A because backlinks don't match" (episode-id (car branch))))
         t)
        ((and nil check-decomps
	      (episode-parent (car branch))
	      (> (hash-table-count (episode-backlinks (car branch))) 0)
	      (= (hash-table-count (episode-backlinks ep)) 0))
         (when t
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
         (when t
           (format t "~%rejecting ~A because it's not in episode's abstraction family." (episode-id (car branch))))
         t)
        ((and check-index-case (not (equal (episode-index-episode-id (car branch)) (episode-index-episode-id ep))))
         (when t
           (format t "~%rejecting ~A because index cases are different." (episode-id (car branch))))
         t)
        ((and (car branch) (member (episode-id (car branch)) reject-list :test #'equal))
         (when t
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
;; type = type of episode content (observation, state, state-transitions)
(defun new-ep-merge (x y res reject-list bic-p type &aux generalized equivalent)
  ;;(format t "~%reject list: ~A~%root id: ~A" reject-list (if x (episode-id (car x))))
  (let (pattern base func)
    (when x
      (cond ((string-equal type "observation") ;;(> (array-dimension (car (episode-observation y)) 0) 0)
	     (setq pattern (episode-observation y))
	     (setq base (episode-observation (car x)))
	     (setq func #'episode-observation))
	    ((string-equal type "state") ;;(> (array-dimension (car (episode-state y)) 0) 0)
	     (setq pattern (episode-state y))
	     (setq base (episode-state (car x)))
	     (setq func #'episode-state))
	    ((string-equal type "state-transitions") ;;(> (array-dimension (car (episode-state-transitions y)) 0) 0)
	     (setq pattern (episode-state-transitions y))
             (setq base (episode-state-transitions (car x)))
	     (setq func #'episode-state-transitions))
	    (t
	     (error "uh oh"))))
    (cond ((null x) (values x 0 (list (make-hash-table :test #'equal)) nil nil reject-list -1 func))
          ((reject-branch? x y reject-list :check-decomps nil)
           (values x 0 (list (make-hash-table :test #'equal)) nil nil (cons (episode-id (car x)) reject-list) -1 func))
          (res
           ;; combine the graphs according to the mapping
           (setq generalized (new-combine (car x) y (first res) (second res) (fourth res) (fifth res)))
	   (setq equivalent (or (= 0 (third res))
				(= (array-dimension (car base) 0)
				   (array-dimension (car pattern) 0)
				   (array-dimension (car (funcall func generalized)) 0)
				   ;;-1
				   )))
	   (when nil (not equivalent)
	     (format t "~%~%num bindings: ~d~%num q-first bindings: ~d~%base id: ~S~%size of base: ~d~%pattern id: ~S~%size of pattern: ~d~%equivalent-p: ~A" (hash-table-count (fourth res)) (hash-table-count (fifth res)) (episode-id (car x)) (array-dimension (car base) 0) (episode-id y) (array-dimension (car pattern) 0) equivalent)
	     (break))
	   (cond ((and equivalent (null (cdr x)))
                  (setf (car x) generalized)
                  (values x (third res) (fourth res) nil t reject-list (sixth res) func))
		 ((and equivalent (cdr x))
                  (setf (car x) generalized)
                  (values x (third res) (fourth res) t t reject-list (sixth res) func))
		 ((and (not equivalent) (null (cdr x)))
                  (setf (episode-id generalized) (symbol-name (gensym "EPISODE-")))
                  (push (car x) (cdr (last x)))
                  (setf (car x) generalized)
                  (setf (second x) (list (second x)))
                  (setf (episode-parent (car (second x))) x)
		  (setf (episode-depth (car (second x))) (+ (episode-depth (car (second x))) 1))
		  (values x (third res) (fourth res) nil nil reject-list (sixth res) func))
		 ((and (not equivalent) (cdr x))
                  (setf (car x) generalized)
                  (values x (third res) (fourth res) t nil reject-list (sixth res) func))))
          (t ;; when insert starts at root
           (multiple-value-bind (sol no-matches cost bindings q-first-bindings num-local-preds)
               (new-maximum-common-subgraph pattern base (episode-backlinks y) (episode-backlinks (car x)) :cost-of-nil (episode-count (car x)) :bic-p bic-p)
	     (when nil (and (string-equal type "state-transitions")
			print-special*)
	       (format t "~%pattern:~%~S~%base:~%~S" pattern base))
	     (when nil (and (string-equal type "state-transitions")
			print-special*)
	       (format t "~%matches:~%~A~%no matches:~%~A~%bindings:~%~A~%cost: ~d~%num-local-preds: ~d" sol no-matches bindings cost num-local-preds)
	       (break))
	     (setq generalized (new-combine (car x) y sol no-matches bindings q-first-bindings))
	     (setq equivalent (or (= 0 cost)
				  (= (array-dimension (car base) 0)
				     (array-dimension (car pattern) 0)
				     (array-dimension (car (funcall func generalized)) 0)
				     ;;-1
				     )))
	     (when nil (not equivalent)
	       (format t "~%~%num bindings: ~d~%num q-first bindings: ~d~%base id: ~S~%size of base: ~d~%episode id: ~S~%size of pattern: ~d~%equivalent-p: ~A" (hash-table-count bindings) (hash-table-count q-first-bindings) (episode-id (car x)) (array-dimension (car base) 0) (episode-id y) (array-dimension (car pattern) 0) equivalent)
	       (break))
	     (cond ((and equivalent (null (cdr x)))
                    (setf (car x) generalized)
                    (values x cost bindings nil t reject-list num-local-preds func))
                   ((and equivalent (cdr x))
                    (setf (car x) generalized)
                    (values x cost bindings t t reject-list num-local-preds func))
                   ((and (not equivalent) (null (cdr x)))
                    (setf (episode-id generalized) (symbol-name (gensym "EPISODE-")))
		    (push (car x) (cdr (last x)))
                    (setf (car x) generalized)
                    (setf (second x) (list (second x)))
                    (setf (episode-parent (car (second x))) x)
		    (setf (episode-depth (car (second x))) (+ (episode-depth (car (second x))) 1))
                    (values x cost bindings nil nil reject-list num-local-preds func))
                   ((and (not equivalent) (cdr x))
                    (setf (car x) generalized)
                    (values x cost bindings t nil reject-list num-local-preds func))))))))

#| print episode |#
(defun print-episode (ep &key (stream t))
  (let ((indent "    ")
	  (large-indent "        ")
	  (fields (list :episode-id :parent :observation :state :state-transitions :backlinks :count :depth :temporal-p)))
      (when (episode-p ep)
	(format stream "~%~aEPISODE:" indent)
	(loop
	  for field in fields
	  do
	     (case field
	       (:episode-id
		(format stream "~%~a  ID: ~a" indent (episode-id ep)))
	       (:parent
		(format stream "~%~a  Parent: ~a" indent (when (episode-parent ep)
						      (episode-id (car (episode-parent ep))))))
	       (:observation
		(format stream "~%~a  Observation:" indent)
		(print-bn (episode-observation ep) :indent large-indent :stream stream))
	       (:state
		(format stream "~%~a  State:" indent)
		(print-bn (episode-state ep) :indent large-indent :stream stream))
	       (:state-transitions
		(format stream "~%~a  State Transitions:" indent)
		(print-bn (episode-state-transitions ep) :indent large-indent :stream stream))
	       (:backlinks
		(format stream "~%~a  Backlinks:" indent)
		(loop
		  for pointer being the hash-keys of (episode-backlinks ep)
		    using (hash-value subtree)
		  do
		     (format stream "~%~a  (~a . ~a)" large-indent pointer (episode-id (car subtree)))))
	       (:depth
		(format stream "~%~a  Depth: ~a" indent (episode-depth ep)))
	       (:count
		(format stream "~%~a  Count: ~a" indent (episode-count ep)))
	       (:temporal-p
		(format stream "~%~a  Temporal-p: ~a" indent (episode-temporal-p ep))))))))

(defun print-backlinks (backlinks)
  (format t "~%~%Backlinks:")
  (loop
    for ep-id being the hash-keys of backlinks
      using (hash-value subtree)
    do
       (format t "~%key: ~A" ep-id)
       (print-episode (car subtree))))

#| Rudamentary printer for showing the branching structure of eltm. |#

;; eltm = episodic long-term memory
(defun print-tree-structure (eltm &key (stream t))
  (cond ((episode-p eltm)
	 (print-episode eltm :stream stream))
        ((null (rest eltm))
         (format stream "~%[")
	 (print-episode (car eltm) :stream stream)
	 (format stream "~%]"))
        (t
         (format stream "~%[")
         (loop
           for item in eltm do
             (print-tree-structure item :stream stream))
         (format stream "~%]"))))

#| Inserts episode into episodic memory

tree = \lambda v b1 b2 ... bn l b. (b v b1 b2 ... bn)
tree = \lambda v b1 b2 ....bn l b. (l v)
|#

;; eltm = episodic memory
;; ep = episode to insert (list of (state . type-count), where state = (factors . edges)
;; reject-list = list of episode ids to reject when merging
;; res = optimal common subgraph between ep and eltm
(defun new-insert-episode (eltm ep reject-list &key (type "state-transitions") res (depth 0) (bic-p t) &aux best-child (best-child-cost most-positive-fixnum))
  (when nil
    (when eltm
      (format t "~%eltm root: ~S" (episode-id (car eltm)))))
  (multiple-value-bind (eltm p-cost p-bindings branch absorb rejects p-num-local-preds ep-access-func)
      (new-ep-merge eltm ep res reject-list bic-p type)
    ;;(break)
    (setq reject-list rejects)
    (when nil
      (format t "~%~%parent episode-id ~A parent episode-states: ~d parent lvl: ~d parent decompositions: ~A parent abstraction pointers: ~S~%episode-states: ~d episode lvl: ~d episode decompositions: ~A episode abstraction pointers: ~S~%parent cost of match: ~d"
              (ignore-errors (episode-id (car eltm))) (ignore-errors (length (episode-states (car eltm)))) (ignore-errors (episode-lvl (car eltm))) (ignore-errors (episode-num-decompositions (car eltm))) (ignore-errors (mapcar #'(lambda (abstr-ptr) (episode-id (car abstr-ptr))) (episode-abstraction-ptrs (car eltm)))) (length (episode-states ep)) (episode-lvl ep) (episode-num-decompositions ep) (ignore-errors (mapcar #'(lambda (abstr-ptr) (episode-id (car abstr-ptr))) (episode-abstraction-ptrs ep))) p-cost))
    (when branch
      (loop
        with pattern and base and pattern-backlinks and base-backlinks
        for branch in eltm
        for i from 0
        when (listp branch) do ;;(> i 0) do
          (cond (t ;;(not (reject-branch? branch ep reject-list :check-decomps t))
                 (when nil
                   (format t "~%~%schema id: ~S~%episode id: ~S"
                           (episode-id (car branch)) (episode-id ep)))
		 (cond ((string-equal type "observation") ;;(> (array-dimension (car (episode-observation ep)) 0) 0)
			(when nil
			  (format t "~%~%observation"))
			(setq pattern (episode-observation ep))
			(setq base (episode-observation (car branch))))
		       ((string-equal type "state") ;;(> (array-dimension (car (episode-state ep)) 0) 0)
			(when nil
			  (format t "~%~%state"))
			(setq pattern (episode-state ep))
			(setq base (episode-state (car branch))))
		       ((string-equal type "state-transitions") ;;(> (array-dimension (car (episode-state-transitions ep)) 0) 0)
			(when nil
			  (format t "~%~%state transition"))
			(setq pattern (episode-state-transitions ep))
			(setq base (episode-state-transitions (car branch))))
		       (t
			(error "uh oh")))
		 (setq pattern-backlinks (episode-backlinks ep))
                 (setq base-backlinks (episode-backlinks (car branch)))
                 (multiple-value-bind (sol no-matches cost bindings q-first-bindings num-local-preds)
                     (new-maximum-common-subgraph pattern base pattern-backlinks base-backlinks :cost-of-nil (episode-count (car branch)) :bic-p bic-p)
                   ;;(subgraph-greedy-monomorphism pattern-state base-state :cost-of-nil (episode-count (car branch)) :bic-p bic-p)
                   ;;(subgraph-optimal-monomorphism pattern-state base-state :cost-of-nil (episode-count (car branch)) :bic-p bic-p)
		   (when nil
		     (format t "~%cost: ~d~%num local matches: ~d~%best child cost: ~d~%best child local preds: ~d" cost num-local-preds (second best-child) (fifth best-child)))
		   (when (or (= i 1)
			     (better-random-match? (list nil cost bindings nil num-local-preds (array-dimension (car base) 0)) best-child))
                     (setq res (list sol no-matches cost bindings q-first-bindings num-local-preds))
                     (setq best-child-cost cost)
                     (setq best-child (list i cost bindings nil num-local-preds (array-dimension (car base) 0))))))
                (nil t
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
		(or (and branch (better-random-match? (list nil p-cost p-bindings nil p-num-local-preds (array-dimension (car (funcall ep-access-func (car eltm))) 0)) best-child) #|(<= p-cost best-child-cost)|#)
		    (not branch)))
           (setf (episode-parent ep) eltm)
           (setf (episode-depth ep) (+ depth 1))
           (push (list ep) (cdr (last eltm)))
           (when nil
             (format t "~%Added new child of parent"))
           (values eltm (car (last eltm))))
          (t ;;(and branch (> p-cost best-child-cost))
           (when nil
             (format t "~%Recursing on best child"))
           (multiple-value-bind (new-branch ref)
               (new-insert-episode (nth (car best-child) eltm) ep reject-list :type type :res res :depth (+ depth 1) :bic-p bic-p)
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
;; type = type of the episodic memory content. Either, observation, state, or state transitions.
;; reject-list = list of episode ids to reject when merging
;; bicp-p = retrieval mode for using BIC or likelihood for structure mapping
(defun new-match-cue (x y res type reject-list bic-p check-decomps check-abstraction-ptrs check-index-case forbidden-types)
  (cond ((null x)
	 (values (list nil most-positive-fixnum nil -1 0 ) nil reject-list nil))
        ((reject-branch? x y reject-list :check-decomps check-decomps :check-abstraction-ptrs check-abstraction-ptrs :check-index-case check-index-case)
	 (values (list nil most-positive-fixnum nil -1 0) nil (cons (episode-id (car x)) reject-list) nil))
        (res
         (cond ((null (cdr x))
                (values res nil reject-list))
               ((cdr x)
                (values res t reject-list))))
        (t ;; when checking starts at the root
	 (let (pattern base)
	   (cond ((string-equal type "observation") ;;(> (array-dimension (car (episode-observation y)) 0) 0)
	     (setq pattern (episode-observation y))
	     (setq base (episode-observation (car x))))
	    ((string-equal type "state") ;;(> (array-dimension (car (episode-state y)) 0) 0)
	     (setq pattern (episode-state y))
	     (setq base (episode-state (car x))))
	    ((string-equal type "state-transitions") ;;(> (array-dimension (car (episode-state-transitions y)) 0) 0)
	     (setq pattern (episode-state-transitions y))
             (setq base (episode-state-transitions (car x))))
	    (t
	     (error "uh oh")))
	   (multiple-value-bind (sol no-matches cost bindings q-first-bindings num-local-preds)
               (new-maximum-common-subgraph pattern base (episode-backlinks y) (episode-backlinks (car x)) :cost-of-nil (episode-count (car x)) :bic-p bic-p :forbidden-types forbidden-types)
	     (declare (ignore no-matches))
	     (when nil
	       (format t "~%retrieval mappings:~%~S~%retrieval cost: ~d~%retrieval bindings:~%~S~%q-first-bindings:~%~S" sol cost bindings q-first-bindings))
	     (setq res (list sol cost bindings num-local-preds (array-dimension (car base) 0) q-first-bindings))
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
(defun new-retrieve-episode (eltm cue reject-list &key (type "state-transitions") (res nil) (depth 0) (bic-p t) (lvl-func nil) (check-decomps t) (check-abstraction-ptrs nil) (check-index-case nil) (forbidden-types nil) &aux best-child (best-child-weighted-cost most-positive-fixnum) (best-child-cost most-positive-fixnum))
  (setq best-child (list nil most-positive-fixnum (make-hash-table :test #'equal) nil -1 -1))
  (when nil t
    (format t "~%reject list in retrieve: ~A" reject-list))
  (when (or (null eltm) (member (episode-id (car eltm)) reject-list :test #'equal))
    (return-from new-retrieve-episode (values nil nil nil depth most-positive-fixnum most-positive-fixnum nil reject-list)))
  ;; merge eltm and ep
  (multiple-value-bind (p-cost branch rejects)
      (new-match-cue eltm cue res type reject-list bic-p check-decomps check-abstraction-ptrs check-index-case forbidden-types)
    ;; for each child, check the match result
    (setq reject-list rejects)
    (when nil t
      (format t "~%~%parent episode: ~A" (episode-id (car eltm))))
    (when nil
      (format t "~%~%parent episode-id ~A parent episode-states: ~d parent lvl: ~d parent decompositions: ~A parent abstraction pointers: ~S~%cue episode-states: ~d cue lvl: ~d cue decompositions: ~A cue abstraction pointers: ~S~%parent cost of match: ~d~%lvl-func: ~A~%reject list: ~S"
              (episode-id (car eltm)) (length (episode-states (car eltm))) (episode-lvl (car eltm)) (episode-num-decompositions (car eltm)) (ignore-errors (mapcar #'(lambda (abstr-ptr) (episode-id (car abstr-ptr))) (episode-abstraction-ptrs (car eltm)))) (if cue (length (episode-states cue)) 0) (if cue (episode-lvl cue) 0) (if cue (episode-num-decompositions cue) 0) (if cue (ignore-errors (mapcar #'(lambda (abstr-ptr) (episode-id (car abstr-ptr))) (episode-abstraction-ptrs cue)))) (second p-cost) lvl-func reject-list))
    (when branch
      (loop
	 with pattern and base and pattern-backlinks and base-backlinks
	 with default-check = nil and first-passing-branch-p = nil
	 with passing-branch-idx
	 for branch in eltm
	 for i from 0
         when (> i 0) do
          (cond ((not (reject-branch? branch cue reject-list :check-decomps check-decomps :check-abstraction-ptrs check-abstraction-ptrs :check-index-case check-index-case))
		 (when (and (null first-passing-branch-p)
			    (null passing-branch-idx))
		   (setq first-passing-branch-p t)
		   (setq passing-branch-idx i))
		 (when first-passing-branch-p
		   (setq first-passing-branch-p nil))
		 (when nil t
                       (format t "~%~%branch episode-id ~A"
                               (episode-id (car branch))))
		 (cond ((string-equal type "observation") ;;(> (array-dimension (car (episode-observation cue)) 0) 0)
			(setq pattern (episode-observation cue))
			(setq base (episode-observation (car branch))))
		       ((string-equal type "state") ;;(> (array-dimension (car (episode-state cue)) 0) 0)
			(setq pattern (episode-state cue))
			(setq base (episode-state (car branch))))
		       ((string-equal type "state-transitions") ;;(> (array-dimension (car (episode-state-transitions cue)) 0) 0)
			(setq pattern (episode-state-transitions cue))
			(setq base (episode-state-transitions (car branch))))
		       (t
			(error "uh oh")))
		 (setq pattern-backlinks (episode-backlinks cue))
                 (setq base-backlinks (episode-backlinks (car branch)))
		 (multiple-value-bind (sol no-matches cost bindings q-first-bindings num-local-preds)
                     (new-maximum-common-subgraph pattern base pattern-backlinks base-backlinks :cost-of-nil (episode-count (car branch)) :bic-p bic-p :forbidden-types forbidden-types)
		   (declare (ignore no-matches))
		   (when nil t
		     (format t "~%retrieval mappings:~%~S~%retrieval cost: ~d~%retrieval bindings:~%~S" sol cost bindings))
		   (cond ((or first-passing-branch-p;;(= i 1)
			      (and (not lvl-func)
				   (better-random-match? (list nil cost bindings nil num-local-preds (array-dimension (car base) 0)) best-child))
                              ;;(< (reduce #'+ weighted-costs) best-child-weighted-cost)
                              (and lvl-func
                                   (not (funcall lvl-func (episode-lvl (caar best-child)) (episode-lvl cue)))
                                   (funcall lvl-func (episode-lvl (car branch)) (episode-lvl cue))
				   (better-random-match? (list nil cost bindings nil num-local-preds (array-dimension (car base) 0)) best-child)
				   ;;(= (reduce #'+ weighted-costs) best-child-weighted-cost)
				   )
                              (and lvl-func
                                   (funcall lvl-func (episode-lvl (car branch)) (episode-lvl cue))
                                   (not (funcall lvl-func (episode-lvl (car branch)) (episode-lvl (caar best-child))))
				   (better-random-match? (list nil cost bindings nil num-local-preds (array-dimension (car base) 0)) best-child)
				   ;;(= (reduce #'+ weighted-costs) best-child-weighted-cost)
				   ))
                          (setq res (list sol cost bindings num-local-preds (array-dimension (car base) 0) q-first-bindings))
                          ;; if current best child is an episode, reject it before replacing it with current branch
			  (when (and (car best-child) (= (episode-count (caar best-child)) 1)
                                     (not (episode-temporal-p (caar best-child))))
                            (setq reject-list (cons (episode-id (caar best-child)) reject-list)))
                          (setq best-child-cost cost)
			  (when nil t
			    (format t "~%branch is new best child"))
                          (setq best-child (list branch cost bindings nil num-local-preds (array-dimension (car base) 0) q-first-bindings)))
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
		    (better-random-match? (list nil (second p-cost) (third p-cost) nil (fourth p-cost) (fifth p-cost)) best-child)
		    (<= (second p-cost) best-child-weighted-cost)))
           (when nil t
             (format t "~%Returning ~A" (if (member (episode-id (car eltm)) reject-list :test #'equal) nil (episode-id (car eltm)))))
           (values (if (member (episode-id (car eltm)) reject-list :test #'equal) nil eltm) (car p-cost) (third p-cost) depth (second p-cost) nil reject-list (sixth p-cost)))
          ((and (or (eq #'= lvl-func) (eq '= lvl-func)) (funcall lvl-func (episode-lvl (car eltm)) (episode-lvl cue))
                (if best-child (funcall lvl-func (episode-lvl (car eltm)) (episode-lvl (caar best-child))) t)
		(if best-child
		    (better-random-match? (list nil (second p-cost) (third p-cost) nil (fourth p-cost) (fifth p-cost)) best-child)
		    (<= (second p-cost) best-child-weighted-cost))
		;;(<= (- (second p-cost) best-child-weighted-cost) 0)
		)
           (when nil t
             (format t "~%Returning ~A" (if (member (episode-id (car eltm)) reject-list :test #'equal) nil (episode-id (car eltm)))))
           (values (if (member (episode-id (car eltm)) reject-list :test #'equal) nil eltm) (car p-cost) (third p-cost) depth (second p-cost) nil reject-list (sixth p-cost)))
          ((and lvl-func (not (funcall lvl-func (episode-lvl (car eltm)) (episode-lvl cue))) (null (car best-child)))
           (when nil t
             (format t "~%Returning ~A" (if (equal (episode-id (car eltm)) (car reject-list)) nil (car eltm))))
           (values (if (member (episode-id (car eltm)) reject-list :test #'equal) nil eltm) (car p-cost) (third p-cost) depth (second p-cost) (episode-id (car eltm)) reject-list (sixth p-cost)))
          ((and (not lvl-func)
	        (or (and branch (better-random-match? (list nil (second p-cost) (third p-cost) nil (fourth p-cost) (fifth p-cost)) best-child))
		    (and branch (= (second p-cost) (second best-child)))
		    (not branch))
		;;(<= (- (second p-cost) best-child-weighted-cost) 0)
		)
           (when nil t
             (format t "~%Returning ~A" (if (member (episode-id (car eltm)) reject-list :test #'equal) nil (episode-id (car eltm)))))
           (values (if (member (episode-id (car eltm)) reject-list :test #'equal) nil eltm) (car p-cost) (third p-cost) depth (second p-cost) nil reject-list (sixth p-cost)))
          (t
           (when nil t
             (format t "~%Recursing on best child: ~A" (if (car best-child) (episode-id (caar best-child)))))
           (new-retrieve-episode (car best-child) cue reject-list :res res :depth (1+ depth) :bic-p bic-p :check-decomps check-decomps :forbidden-types forbidden-types :lvl-func lvl-func :check-abstraction-ptrs check-abstraction-ptrs :check-index-case check-index-case :type type)))))

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
     for factor being the elements in factors
     do
       (setf (gethash (rule-based-cpd-dependent-id factor) evidence) nil)
       (loop
        named indexer
        for rule being the elements of (rule-based-cpd-rules factor)
          do
             (setq idx (car (gethash (rule-based-cpd-dependent-id factor) (rule-conditions rule))))
	    (setq var (caar (assoc idx (gethash 0 (rule-based-cpd-var-value-block-map factor)) :key #'cdr)))
	    (when (null var)
	      (error "Variable look-up failed on vvbm:~%~S~%idx: ~d" (gethash 0 (rule-based-cpd-var-value-block-map factor)) idx))
	    (setf (gethash (rule-based-cpd-dependent-id factor) evidence)
		  (cons (cons var (rule-probability rule))
			(gethash (rule-based-cpd-dependent-id factor) evidence)))))
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

(defun create-episode (&key (observation (cons (make-array 0) (make-hash-table :test #'equal)))
			 (state (cons (make-array 0) (make-hash-table :test #'equal)))
			 (transitions (cons (make-array 0) (make-hash-table :test #'equal)))
			 (backlinks (make-hash-table :test #'equal)))
  (make-episode
   :observation observation
   :state state
   :state-transitions transitions
   :backlinks backlinks
   :temporal-p (if (equal (cons (make-array 0) (make-hash-table :test #'equal)) transitions) nil t)
   :count 1
   :lvl (if (equal (cons (make-array 0) (make-hash-table :test #'equal)) transitions) 1 2)))

#| Print a distribution over possible values |#

;; distribution-hash = hash table. key: var from var value block map (possible outcome). value: bn
(defun print-dist-hash (distribution-hash)
  (loop
    for cond-key being the hash-keys of distribution-hash
      using (hash-value prob-rec)
    do
       (format t "~%outcome: ~S" cond-key)
       (format t "~%outcome probability: ~d" (car prob-rec))
       (format t "~%beliefs:")
       (map nil #'(lambda (cpd)
		    (print-cpd cpd))
	    (cadr prob-rec))))

#| Print a slice |#

;; slice = hash table. Key: ["STATE", "OBSERVATION", "ACTION"], value: distribution hash. distribution hash: hash table. key: var from var value block map (possible outcome). value: bn
(defun print-slice (slice)
  (loop
    for temporal-model-key being the hash-keys of slice
      using (hash-value distribution-hash)
    do
       (format t "~%~S" temporal-model-key)
       (print-dist-hash distribution-hash)))

#| Print inferred beliefs over time. |#

;; state-transitions = hash table. key: integer representing time step. value: slice. slice: hash table. key: ["STATE", "OBSERVATION", "ACTION"], value: distribution hash. distribution hash: hash table. key: var from var value block map (possible outcome). value: bn
(defun print-state-transitions (state-transitions)
  (loop
    for slice-idx being the hash-keys of state-transitions
      using (hash-value slice)
    do
       (format t "~%~%slice: ~d" slice-idx)
       (print-slice slice)))

(defun marginalize-messages (state-transitions)
  (labels ((marginalize-bn (bn)
	     (loop
	       with arr = (make-array (array-dimension (car bn) 0))
	       with keep and remove and copy-cpd
	       for cpd being the elements of (car bn)
	       for i from 0
	       do
		  (setq copy-cpd (copy-rule-based-cpd cpd :rule-counts 1))
		  (setq keep (list (rule-based-cpd-dependent-id cpd)))
		  (loop
		    for id being the hash-keys of (rule-based-cpd-identifiers copy-cpd)
		    when (not (equal id (car keep)))
		      collect id into rem
		    finally
		       (setq remove rem))
		  (setf (aref arr i) (normalize-rule-probabilities (factor-operation copy-cpd keep remove '+) (rule-based-cpd-dependent-id copy-cpd)))
	       finally
		  (return (cons arr (cdr bn)))))
	   (marginalize-dist-hash (distribution-hash)
	     (loop
	       with dist-hash = (make-hash-table :test #'equal)
	       for cond-key being the hash-keys of distribution-hash
		 using (hash-value prob-rec)
	       do
		  (setf (gethash cond-key dist-hash)
			(cons (car prob-rec)
			      (marginalize-bn (cdr prob-rec))))
	       finally
		  (return dist-hash)))
	   (marginalize-slice (slice)
	     (loop
	       with slice-hash = (make-hash-table :test #'equal)
	       for temporal-model-key being the hash-keys of slice
		 using (hash-value distribution-hash)
	       do
		  (setf (gethash temporal-model-key slice-hash)
			(marginalize-dist-hash distribution-hash))
	       finally
		  (return slice-hash))))
    (loop
      with marginalized-messages = (make-hash-table)
      for slice-idx being the hash-keys of state-transitions
	using (hash-value slice)
      do
	 (setf (gethash slice-idx marginalized-messages)
	       (marginalize-slice slice))
      finally
	 (return marginalized-messages))))

#| Recollect an experience.
   Returns multiple values.
   First value is a list twice as long as the number of variables in the network. The first half contains posterior CPDs and the last half contains marginalized singletons.
   Second value is the retrieved event memory element. |#

;; eltm = event memory
;; pstm = perceived objects in retrieval cue
;; cstm = given relations in the retrieval cue
;; cue = temporal retrieval cue ;;retrieval cue states (list (factors . edges))
;; mode = inference mode ('+ or 'max)
;; lr = learning rate
;; observability = percent of state observable
(defun remember (eltm cue-bn mode lr bic-p &key (backlinks (make-hash-table :test #'equal)) (type "state-transitions") (observability 1) (soft-likelihoods t))
  (let (partial-states cue bindings and bn priors)
    ;;(log-message (list "~d," (array-dimension (caar cue-states) 0)) "vse.csv")     
    ;;(log-message (list "~d," (array-dimension (caar partial-states) 0)) "vse.csv")
    ;;(state-count-element-types (caar partial-states))
    ;;(log-message (list "~A," (specific-cue-p (caar partial-states))) "vse.csv")
    ;;(log-message (list "~d," observability) "vse.csv")
    ;;(log-message (list "~A~%" partial-states) "cues.txt")
    (cond ((string-equal type "state-transitions")
	   (setq cue (make-episode
		      :observation (cons (make-array 0) (make-hash-table :test #'equal))
		      :state (cons (make-array 0) (make-hash-table :test #'equal))
		      :state-transitions cue-bn
		      :backlinks backlinks
		      :temporal-p t
		      :count 1
		      :lvl 2)))
	  ((string-equal type "observation")
	   (setq cue (make-episode
		      :observation cue-bn
		      :state (cons (make-array 0) (make-hash-table :test #'equal))
		      :state-transitions (cons (make-array 0) (make-hash-table :test #'equal))
		      :backlinks (make-hash-table :test #'equal)
		      :temporal-p nil
		      :count 1
		      :lvl 1)))
	  ((string-equal type "state")
	   (setq cue (make-episode
		      :observation (cons (make-array 0) (make-hash-table :test #'equal))
		      :state cue-bn
		      :state-transitions (cons (make-array 0) (make-hash-table :test #'equal))
		      :backlinks (make-hash-table :test #'equal)
		      :temporal-p nil
		      :count 1
		      :lvl 1))))
    (multiple-value-bind (eme sol bindings depth cost  dont-care reject-list q-first-bindings)
        (cond ((equalp (cons (make-array 0) (make-hash-table :test #'equal)) cue-bn)
               (values eltm nil nil 0 most-positive-fixnum most-positive-fixnum nil))
              (t
               (new-retrieve-episode eltm cue nil :bic-p bic-p :type type)))
      (declare (ignore depth cost dont-care))
      (when nil (not (string-equal type "state-transitions"))
	(format t "~%Did observation node successfully match to existing temporal episode?~%sol:~%~S" sol))      
      (cond ((string-equal type "state-transitions")
	     (setq bn (episode-state-transitions (car eme))))
	    ((string-equal type "observation")
	     (setq bn (episode-observation (car eme))))
	    ((string-equal type "state")
	     (setq bn (episode-state (car eme)))))
      (setq bn (copy-bn bn))
      (when nil t
	(format t"~%episode id: ~A~%retrieved model:~%~%~A~%" (episode-id (car eme)) bn)
	(print-bn bn)
	(break))
      (setq priors (make-hash-table :test #'equal))
      ;; update schema domain on all bn cpds that include prior variables
      (loop
	with prior-cpd and bindings = (make-hash-table :test #'equal)
	for cpd1 being the elements of (car bn)
	when (rule-based-cpd-prior cpd1)
	do
	   (setq prior-cpd
		 (aref (car (eval `(compile-program nil
				     c1 = ,(rule-based-cpd-prior cpd1))))
		       0))
	   (setf (rule-based-cpd-singleton-p prior-cpd) t)
	   (when nil
	     (format t "~%prior:~%")
	     (print-cpd prior-cpd))
	   ;; make bindings
	   (setf (gethash (rule-based-cpd-dependent-id prior-cpd) bindings)
		 (rule-based-cpd-dependent-id cpd1))
	   (loop
	     for value in (getf (rule-based-cpd-prior cpd1) :values)
	     do
		(setf (gethash (getf value :value) bindings)
		      (getf value :value)))
	   (setq prior-cpd (subst-cpd prior-cpd cpd1 bindings))
	   (cpd-update-schema-domain cpd1 prior-cpd nil)
	   (when nil
	     (format t "~%subst prior:~%")
	     (print-cpd prior-cpd)
	     (format t "~%updated cpd1:~%")
	     (print-cpd cpd1)
	     (break))
	   ;; update the domain of CPD1 in downstream cpds
	   (loop
	     for cpd2 being the elements of (car bn)
	     for i from 0
	     when (and (gethash (rule-based-cpd-dependent-id cpd1)
				(rule-based-cpd-identifiers cpd2))
		       (not (equal (rule-based-cpd-dependent-id cpd1)
				   (rule-based-cpd-dependent-id cpd2))))
	       do
		  (setf (aref (car bn) i)
			(cpd-update-existing-vvms cpd2 bindings (list cpd1)))
		  (when nil
		    (format t "~%updated downstream cpd:~%")
		    (print-cpd (aref (car bn) i))
		    (break)))
	   (setf (gethash (rule-based-cpd-dependent-id cpd1) priors) prior-cpd))
      (when soft-likelihoods
	(loop
	      for cpd being the elements of (car bn)
	      for i from 0
	      do
	      (loop
		    for rule being the elements of (rule-based-cpd-rules cpd)
		    do
		    (when (= (rule-probability rule) 0)
		      (setf (rule-probability rule) 1/1000)))
	      (normalize-rule-probabilities cpd (rule-based-cpd-dependent-id cpd))))
      (when nil
	(format t "~%~%episode id: ~S~%bn:~%~S" (episode-id (car eme)) bn))
      (loop
        for (p-match . q-match) being the elements of sol
        with p-copy and observed-factors and observed-factor and num-assignments
        with dep-id and dep-var and vars and idents and types-hash and cids and qvars and vvbm and var-values and cards and steps and rules and lvl
        do
           (setq p-copy (copy-rule-based-cpd (aref (car cue-bn) p-match)))
           (when (and q-match (not (member (rule-based-cpd-dependent-id (aref (car bn) q-match))
					   observed-factors :key #'rule-based-cpd-dependent-id :test #'equal)))
             ;;(setq p-copy (copy-cpd p-match))
	     (when nil (gethash "ACTION7333" (rule-based-cpd-identifiers (aref (car bn) q-match))) 
	       (format t "~%~%p-cpd:~%~S~%q-cpd:~%~S~%episode id: ~S~%bindings:~%~S" (aref (car (episode-state-transitions cue)) p-match)
		       (rule-based-cpd-identifiers (aref (car bn) q-match))
		       (episode-id (car eme))
		       bindings))
             (setq p-copy (subst-cpd (aref (car cue-bn) p-match) (aref (car bn) q-match) bindings))
	     (when nil (gethash "ACTION7333" (rule-based-cpd-identifiers (aref (car bn) q-match)))
		   (format t "~%subst:~%~S~%likelihood(p,q) = ~d" p-copy (local-likelihood p-copy (aref (car bn) q-match))))
             (setq dep-id (rule-based-cpd-dependent-id p-copy))
	     (loop
	       for ident being the hash-keys of (rule-based-cpd-identifiers p-copy)
	       when (not (equal ident dep-id))
		 collect ident into remove
	       finally
		  (setq observed-factor (normalize-rule-probabilities
					 (factor-operation p-copy (list dep-id) remove #'+)
					 dep-id)))
             (setq observed-factors (cons observed-factor observed-factors)))
        finally
	   (when nil t nil (not (string-equal type "state-transitions"))
	     (format t "~%observed factors:~%~S" observed-factors))
	   (let (evidence-table)
             (setq evidence-table (make-observations observed-factors))
	     (when nil t
		   (format t "~%evidence table:~%~S" evidence-table))
             (multiple-value-bind (posterior-distribution posterior-marginals)
		 (loopy-belief-propagation bn evidence-table priors mode lr)
               (return (values posterior-distribution posterior-marginals eme sol bindings q-first-bindings))))))))

#| Recollect a temporal experience.
   Returns list containing inferences for each slice of the temporal model.
   Slice is a hash table. Key: CPD Type, Value: posterior distribution hash table.
   Posterior distribution hash table: Key: backlink id :value posterior state/observation model|#

;; eltm = episodic long-term memory
;; temporal-evidence-bn = evidence network on the temporal model
;; backlinks = hash table of episode ids to back-links references pointing to lower-level observation/state transition models in the event memory. Key: episode id, Value: subtree
;; evidence-slices = hash table. Key: integer Key: hash tables of evidence observed for state and observation schemas. Keys: ["STATE", "OBSERVATION"], Value: evidence network
(defun remember-temporal (eltm temporal-evidence-bn backlinks evidence-slices &key (mode '+) (lr 1) (bic-p t) (alphas (make-hash-table)) hidden-state-p soft-likelihoods)
  (labels ((get-modes (cpd delta)
	     "Return the highest value and all values within DELTA of it in one pass."
	     (loop
	       named mode-finder
	       with rules = (rule-based-cpd-rules cpd)
	       with dep-id = (rule-based-cpd-dependent-id cpd) and vvbms = (gethash 0 (rule-based-cpd-var-value-block-map cpd))
	       with max-prob = (cons nil -1) and res
	       with sorted-rules = (sort rules #'(lambda (a b)
						   (> (with-input-from-string (s (caar (nth (car (gethash dep-id (rule-conditions a)))
											    vvbms)))
							(let ((num (read s)))
							  (if (numberp num)
							      (rule-probability a)
							      -1)))
						      (with-input-from-string (s (caar (nth (car (gethash dep-id (rule-conditions b)))
											    vvbms)))
							(let ((num (read s)))
							  (if (numberp num)
							      (rule-probability b)
							      -1))))))
	       for rule being the elements of sorted-rules
	       do
		  (cond ((> (rule-probability rule) (cdr max-prob))
			 (setq max-prob (cons (caar (nth (car (gethash dep-id (rule-conditions rule)))
							 vvbms))
					      (rule-probability rule)))
			 (setq res (list (caar (nth (car (gethash dep-id (rule-conditions rule)))
						    vvbms)))))
			((and (>= (rule-probability rule) (- (cdr max-prob) delta)))
			 (setq res (cons
				    (caar (nth (car (gethash dep-id (rule-conditions rule)))
					       vvbms))
				    res)))
			((and (< (rule-probability rule) (- (cdr max-prob) delta)))
			 (return-from mode-finder (remove-duplicates res :test #'equal))))
	       finally
		  (return-from mode-finder (remove-duplicates res :test #'equal))))
	   (smooth-posterior (posterior-distribution alpha &key (mixture-type "discrete-uniform") (delta .01))
	     (loop
	       with values
	       with smoothed-cpd and probability with mixture-probs and mixture-rules
	       with vvbms
	       for cpd in posterior-distribution
	       do
		  (setq vvbms (gethash 0 (rule-based-cpd-var-value-block-map cpd)))
		  (setq values (mapcar #'(lambda (vvbm)
					   (caar vvbm))
				       (gethash 0 (rule-based-cpd-var-value-block-map cpd))))
		  (cond ((string-equal mixture-type "discrete-uniform")
			 (setq mixture-probs (discrete-uniform :values values)))
			((string-equal mixture-type "discrete-normal-approximation")
			 (let* ((marginalized (factor-operation cpd
								(list (rule-based-cpd-dependent-id cpd))
								(loop
								  for ident being the hash-keys of (rule-based-cpd-identifiers cpd)
								  when (not (equal ident (rule-based-cpd-dependent-id cpd)))
								    collect ident into remove
								  finally
								     (return remove))
								'+))
				(modes (get-modes marginalized delta)))
			   (setq mixture-probs (discrete-normal-approximation :values values :modes modes))))
			(t
			 (error "Unsupported mixture distrubution ~S. Must be either 'discrete-uniform or 'discrete-normal-approximation" mixture-type)))
		  (setq mixture-rules (make-array (length mixture-probs)))
		  (loop
		    with r
		    for value in mixture-probs
		    for i from 0
		    do
		       (setq r (make-rule :id "RULE-"
					  :conditions (make-hash-table :test #'equal)
					  :probability (getf value :probability)))
		       (setf (aref mixture-rules i) r))
		  (setq probability (* alpha (/ 1 (length (gethash 0 (rule-based-cpd-var-value-block-map cpd))))))
		  (setq smoothed-cpd (copy-rule-based-cpd cpd :rule-counts 1))
		  (loop
		    with var
		    for r1 being the elements of (rule-based-cpd-rules smoothed-cpd)
		    do
		       (loop
			 named filter
			 for r2 being the elements of mixture-rules
			 when (compatible-rule-p r1 r2 nil nil)
			   do
			      (setf (rule-probability r1)
				    (+ (* (rule-probability r1)
					  (- 1 alpha))
				       (* alpha (rule-probability r2))))
			      (return-from filter nil)))
	       collect smoothed-cpd into smoothed
	       finally
		  (return smoothed))))
    (multiple-value-bind (conditioned-temporal sol bindings q-first-bindings)
	(condition-model eltm
			 temporal-evidence-bn
			 "state-transitions"
			 :backlinks backlinks
			 :keep-singletons t
			 :soft-likelihoods soft-likelihoods)
      (when nil print-special*
	    (format t "~%temporal evidence retrieval cue:~%")
	    (print-bn temporal-evidence-bn)
	    (format t"~%conditioned temporal model:~%")
	    (print-bn (episode-state-transitions conditioned-temporal))
	    (format t "~%sol:~%~S" sol)
	    ;;(format t "~%bindings:~%~A" bindings)
	    ;;(format t "~%conditioned model -> retrieval que bindings:~%~A" q-first-bindings)
	    #|
	    (loop
	    for i being the hash-keys of evidence-slices
	    using (hash-value evidence-hash)
	    do
	    (format t "~%slice: ~d" i)
	    (print-slice evidence-hash))
	    |#
	    ;;(break)
	    )
      (loop
	with temporal-bn = (car (episode-state-transitions conditioned-temporal))
	with marker and mod-len = (if hidden-state-p 3 2)
	with evidence-hash and dist-hash and marginals-dist-hash and js = (loop for k being the hash-keys of evidence-slices collect k into keys
										finally (return (sort keys #'<)))
	with slice and marginals-slice and node-type
	with match and state-transitions = (make-hash-table) and marginals-state-transitions = (make-hash-table)
	with cpd2 and p-match
	with alpha
	for cpd being the elements of temporal-bn
	for idx from 0
	when (singleton-cpd? cpd)
	  do
	     (setq marker (mod idx mod-len))
	#|
	     (loop
	named finder
	for binding being the elements of sol
	when (and (numberp (cdr binding))
	     (= (cdr binding) idx))
	do
	     (setq p-match (car binding))
	     (return-from finder nil)
	finally
	     (setq p-match nil))
	     (when p-match
	     (format t "~%cpd:")
	     (print-cpd cpd)
	     (setq cpd2 (aref (car temporal-evidence-bn) p-match))
	     (setq cpd (subst-cpd cpd cpd2 q-first-bindings))
	     (format t "~%subst-cpd:")
	     (print-cpd cpd))
	|#
	     (when nil (and print-special*
			    (= (car js) 0))
		   (format t "~%~%idx: ~d~%p-match: ~d" idx p-match))
	     (when (= marker 0)
	       (setq slice (make-hash-table :test #'equal))
	       (setq marginals-slice (make-hash-table :test #'equal)))
	     (setq evidence-hash (gethash (cond ((string-equal "state-node" (symbol-name (get-cpd-type cpd)))
						 "STATE")
						((string-equal "observation-node" (symbol-name (get-cpd-type cpd)))
						 "OBSERVATION")
						((string-equal "action-node" (symbol-name (get-cpd-type cpd)))
						 "ACTION"))
					  (gethash (car js) evidence-slices)))
	     (when (null evidence-hash)
	       (setq evidence-hash (make-hash-table :test #'equal)))
	     (when nil (and print-special*
			    (= (car js) 0))
		   (format t "~%j: ~d~%cpd:" (car js))
		   (print-cpd cpd)
		   ;;(format t "~%conditioned model backlinks:")
		   ;;(print-backlinks (episode-backlinks conditioned-temporal))
		   (format t "~%evidence hash table:")
		   (print-dist-hash evidence-hash))
	     (setq dist-hash (make-hash-table :test #'equal))
	     (setq marginals-dist-hash (make-hash-table :test #'equal))
	     (loop
	       for c being the hash-keys of evidence-hash
		 using (hash-value prob-obs)
	       do
		  (setf (gethash c dist-hash) (cons 0 (cdr prob-obs)))
		  ;; I need to have the marginals evidence hash to properly pre-fill marginals-dist-hash
		  (setf (gethash c marginals-dist-hash) (cons 0 (cdr prob-obs))))
	     (setq node-type (gethash 0 (rule-based-cpd-types cpd)))
	     (when (equal "PERCEPT" node-type)
	       (setq node-type "ACTION"))
	     (when (not (equal "ACTION" node-type))
	       (loop
		 with prob and cond
		 with backlink-episode and evidence-bn
		 for rule being the elements of (rule-based-cpd-rules cpd)
		 do
		    (setq prob (rule-probability rule))
		    (setq cond (car (gethash (rule-based-cpd-dependent-id cpd) (rule-conditions rule))))
	            (loop
		      named finder
		      for vvb in (gethash 0 (rule-based-cpd-var-value-block-map cpd))
		      when (= cond (cdar vvb))
			do
			   (setq cond (caar vvb))
			   (return-from finder nil))
		    (setq evidence-bn (cdr (gethash cond evidence-hash)))
		    (when (null evidence-bn)
		      (setq evidence-bn (make-empty-graph)))
		    (setq backlink-episode (car (gethash cond (episode-backlinks conditioned-temporal))))
		    (when nil (and print-special*
				   (= (car js) 0))
			  (format t "~%rule:")
			  (print-cpd-rule rule)
			  (format t "~%cond: ~S~%backlinks:" cond)
			  (loop
			    for key being the hash-keys of (episode-backlinks conditioned-temporal)
			      using (hash-value subtree)
			    do
			       (format t "~%~S : ~S" key (episode-id (car subtree))))
			  (format t "~%backlink-episode: ~S" (if backlink-episode (episode-id backlink-episode))))
	            (when backlink-episode
		      (when nil (and print-special*
				     (= (car js) 0))
			    (format t "~%Remembering from:")
			    (print-episode backlink-episode)
			    (format t "~%slice: ~d~%node type: ~S~%Retrieval cue:" (car js) node-type)
			    (print-bn evidence-bn)
			    ;;(break)
			    )
		      (when t
			(format t "~%calling (remember) from (remember-temporal)"))
		      (multiple-value-bind (posterior-distribution posterior-marginals eme )
			  (remember (list backlink-episode) evidence-bn mode lr bic-p :type node-type :soft-likelihoods soft-likelihoods)
			(declare (ignore eme))
			;; If we had hierarchical temporal episodes, you would do a recursive call here with the recollection and eme
			(when nil (and print-special*
				       (= (car js) 0))
			      (format t "~%posterior:")
			      (print-bn (cons (make-array (length posterior-distribution)
							  :initial-contents posterior-distribution)
					      (make-hash-table)))
			      ;;(break)
			      )
			(when t
			  (format t "~%smoothing posterior"))
			;; smooth the posterior here.
			(setq posterior-distribution (smooth-posterior posterior-distribution (gethash (car js) alphas) :mixture-type "discrete-normal-approximation"))
			(setq posterior-marginals (smooth-posterior posterior-marginals (gethash (car js) alphas) :mixture-type "discrete-normal-approximation"))
			(setf (gethash cond dist-hash)
			      (cons prob
				    (cons (make-array (length posterior-distribution)
						      :initial-contents posterior-distribution)
					  (make-hash-table))))
			(setf (gethash cond marginals-dist-hash)
			      (cons prob
				    (cons (make-array (length posterior-marginals)
						      :initial-contents posterior-marginals)
					  (make-hash-table))))))))
	     (setf (gethash node-type slice) dist-hash)
	     (setf (gethash node-type marginals-slice) marginals-dist-hash)
	     (when (= marker (- mod-len 1))
	       (when nil (and print-special*
			      (= (car js) 0))
		     (format t "~%adding slice for time step ~d:" (car js))
		     (print-slice slice)
		     (break))
	       (setf (gethash (car js) state-transitions) slice)
	       (setf (gethash (car js) marginals-state-transitions) marginals-slice)
	       (setq js (cdr js)))
	finally
	   (when nil print-special*
		 (format t "~%returning messages:")
		 (print-state-transitions state-transitions)
		 (break))
	   (return (values state-transitions marginals-state-transitions))))))

(defun model-var-vvbms (model hidden-state-p)
  (loop
    with vvbms-hash = (make-hash-table :test #'equal) 
    with cpds-per-slice = (if hidden-state-p 3 2)
    with num-cpds = (array-dimension (car model) 0)
    with num-slices = (/ num-cpds cpds-per-slice)
    with cpd and cpd-type
    for i from 0 below cpds-per-slice
    do
       (setq cpd (aref (car model) i))
       (setq cpd-type (gethash 0 (rule-based-cpd-types cpd))) 
       (setf (gethash cpd-type vvbms-hash) nil)
       (loop
	 with j = i
	 with cpd2 and vvbm
	 while (< j  (- num-cpds 1))
	 do
	    (setq cpd2 (aref (car model) j))
	    (setq vvbm (gethash 0 (rule-based-cpd-var-value-block-map cpd2)))
	    (loop
	      with value
	      for vvb in vvbm
	      do
		 (setq value (caar vvb))
		 (when (not (member value (gethash cpd-type vvbms-hash) :test #'equal))
		   (setf (gethash cpd-type vvbms-hash)
			 (cons value (gethash cpd-type vvbms-hash)))))
	    (setq j (+ j cpds-per-slice)))
    finally
       (return vvbms-hash)))

(defun make-messages (evidence-hash from to hidden-state-p)
  (loop
    with messages = (make-hash-table)
    with idx-list = (loop for i from from to to collect i)
    with marker and cpds-per-slice = (if hidden-state-p 3 2)
    with model = (episode-state-transitions (car eltm*))
    with vvbms-hash = (model-var-vvbms model hidden-state-p)
    with num-slices = (/ (array-dimension (car model) 0) cpds-per-slice)
    with slice and evidence-slice
    for cur-slice in idx-list
    do
       (setq evidence-slice (gethash cur-slice evidence-hash))
       (if (null evidence-slice)
	   (setq evidence-slice (make-hash-table :test #'equal)))
       (setq slice (make-hash-table :test #'equal))
       (setq marker (mod (* cur-slice cpds-per-slice) (array-dimension (car model) 0)))
       (when nil (= cur-slice 25)
	     (format t "~%~%cur slice (i): ~d~%num-slices per model: ~d~%mod slice length: ~d~%num-cpds: ~d~%model slice marker: ~d"
		     cur-slice num-slices cpds-per-slice (array-dimension (car model) 0) marker))
       (loop
	 with cpd and cpd-type and evidence
	 with dist-hash
	 ;;for j from marker to (+ marker (- cpds-per-slice 1))
	 for j from 0 to (- (array-dimension (car model) 0) 1)
	 do
	    (when nil (= cur-slice 25)
		  (format t "~%j: ~d" j))
	    (setq cpd (aref (car model) j))
	    (setq cpd-type (gethash 0 (rule-based-cpd-types cpd)))
	    (setq evidence (gethash cpd-type evidence-slice))
	    (setq dist-hash (make-hash-table :test #'equal))
	    (when nil (= cur-slice 25)
		  (format t "~%cpd:")
		  (print-cpd cpd)
		  (format t "~%cpd type: ~S" cpd-type)
		  (format t "~%evidence:")
		  (print-bn evidence))
	    (loop
	      with vvbm = (gethash (gethash 0 (rule-based-cpd-types cpd)) vvbms-hash)
	      with num-vvbm = (length vvbm)
	      for vvb in vvbm
	      do
		 (if evidence
		     (setf (gethash vvb dist-hash) (cons (float (/ 1 num-vvbm)) evidence))
		     (setf (gethash vvb dist-hash) (cons (float (/ 1 num-vvbm)) (make-empty-graph))))
	      finally
		 (setf (gethash cpd-type slice) dist-hash))
	    (when nil (= cur-slice 25)
		  (format t "~%dist hash:")
		  (print-dist-hash dist-hash)
		  ;;(break)
		  ))
       (setf (gethash cur-slice messages)
	     slice)
    finally
       (return messages)))

#| Assumes that the temporal models all have the same number of slices. |#

;; evidence-hash = hash table. key: integer representing time step. value: slice. slice: hash table. key: ["STATE", "OBSERVATION", "ACTION"], value: bn
;; from = integer representing starting time step
;; to = integer representing ending time step
;; n-passes = max number of passes to conduct
;; hidden-state-p = flag for if the temporal model has a state variable
;; soft-likelihoods = flag for if we add very small padding to 0-valued probablities
;; bic-p = flag for using the Bayesian information criterion. If false, system just uses likelihood
(defun calibrate-temporal-model (evidence-hash from to n-passes hidden-state-p soft-likelihoods bic-p)
  (labels ((check-one-way-convergence (messages new-messages)
	     "slice -> type [state, observation, action] -> distribution-hash [outcome_1,..., outcome_n] -> (prob, net)"
	     (loop
	       with converged = t and conflicts
	       with slice2
	       for slice-idx being the hash-keys of new-messages
		 using (hash-value slice1)
	       when slice-idx
		 do
		    (setq slice2 (gethash slice-idx messages))
		    (when nil
		      (format t "~%~%slice: ~d" slice-idx))
		    (loop
		      with distribution-hash2
		      for temporal-model-key being the hash-keys of slice1
			using (hash-value distribution-hash)
		      do
			 (when nil
			   (format t "~%type: ~S" temporal-model-key))
			 (setq distribution-hash2 (gethash temporal-model-key slice2))
			 (loop
			   with prob and rec and prob-rec2 and prob2 and rec2
			   for cond-key being the hash-keys of distribution-hash
			     using (hash-value prob-rec)
			   do
			      (setq prob (car prob-rec))
			      (setq rec (cadr prob-rec))
			      (setq prob-rec2 (gethash cond-key distribution-hash2))
			      (setq prob2 (car prob-rec2))
			      (setq rec2 (cadr prob-rec2))
			   if (or (not prob-rec2)
				  (not (= (read-from-string (format nil "~5$" prob))
                                          (read-from-string (format nil "~5$" prob2)))))
			     do
				(setq converged nil)
				(setq conflicts (cons (cons prob prob2) conflicts))
				;;(return-from check-one-way-convergence nil)
			   else do
			     (loop
			       for cpd1 being the elements of rec
			       do
				  (loop
				    named looper
				    for cpd2 being the elements of rec2
				    when (and (equal (rule-based-cpd-dependent-var cpd1)
						     (rule-based-cpd-dependent-var cpd2))
					      (not (same-message-p cpd1 cpd2 :round t)))
				      do
					 (setq converged nil)
					 (setq conflicts (cons (cons cpd1 cpd2) conflicts))
					 (return-from looper nil)
					 ;;(return-from check-one-way-convergence nil))
				    ))))
	       finally
		  (return conflicts)))
	   (check-convergence (messages new-messages)
	     "Check if the messages have converged."
	     (let (conflicts1 conflicts2)
	       (setq conflicts1 (check-one-way-convergence messages new-messages))
	       (setq conflicts2 (check-one-way-convergence new-messages messages))
	       (when t
		 (format t "~%~%num conflicts from messages to new messages: ~d" (length conflicts1))
		 (loop
		   for (c1 . c2) in conflicts1
		   do
		      (cond ((numberp c1)
			     (format t "~%outcome probability conflict: ~d != ~d" c1 c2))
			    (t
			     (format t "~%rule conflict:")
			     (print-cpd  c1)
			     (print-cpd c2))))
		 (format t "~%~%num conflicts from new messages to messages: ~d" (length conflicts2))
		 (loop
		   for (c1 . c2) in conflicts2
		   do
		      (cond ((numberp c1)
			     (format t "~%outcome probability conflict: ~d != ~d" c1 c2))
			    (t
			     (format t "~%rule conflict:")
			     (print-cpd  c1)
			     (print-cpd c2)))))
	       (not (or conflicts1 conflicts2))))
	   (sliding-groups (lst k)
	     "Return a list of sublists, each of length K, sliding forward by K-1 elements.
              Pads end of list with NIL as necessary."
	     (loop for i from 0 below (length lst) by (1- k)
		   for group = (subseq lst i (min (+ i k) (length lst)))
		   collect (coerce (append group (make-list (- k (length group)) :initial-element nil)) 'vector))
	     #|
	     (loop
	       for i from 0 below (- (length lst) (1- k)) by (1- k)
	       collect (coerce (subseq lst i (+ i k)) 'vector))
	     |#)
	   (make-index-list (forward-pass-p)
	     (cond ((and forward-pass-p (< from to))
		    (loop for i from from to to collect i))
		   ((and forward-pass-p (>= from to))
		    (loop for i from to to from collect i))
		   ((and (not forward-pass-p) (< from to))
		    (loop for i from to downto from collect i))
		   ((and (not forward-pass-p) (>= from to))
		    (loop for i from from downto to collect i))))
	   (model-var-vvbms (model)
	     (loop
	       with vvbms-hash = (make-hash-table :test #'equal) 
	       with cpds-per-slice = (if hidden-state-p 3 2)
	       with num-cpds = (array-dimension (car model) 0)
	       with num-slices = (/ num-cpds cpds-per-slice)
	       with cpd and cpd-type
	       for i from 0 below cpds-per-slice
	       do
		  (setq cpd (aref (car model) i))
		  (setq cpd-type (gethash 0 (rule-based-cpd-types cpd))) 
		  (setf (gethash cpd-type vvbms-hash) nil)
		  (loop
		    with j = i
		    with cpd2 and vvbm
		    while (< j  (- num-cpds 1))
		    do
		       (setq cpd2 (aref (car model) j))
		       (setq vvbm (gethash 0 (rule-based-cpd-var-value-block-map cpd2)))
		       (loop
			 with value
			 for vvb in vvbm
			 do
			    (setq value (caar vvb))
			    (when (not (member value (gethash cpd-type vvbms-hash) :test #'equal))
			      (setf (gethash cpd-type vvbms-hash)
				    (cons value (gethash cpd-type vvbms-hash)))))
		       (setq j (+ j cpds-per-slice)))
	       finally
		  (return vvbms-hash)))
	   (init-messages (messages forward-pass-p)
	     (let ((idx-list (make-index-list forward-pass-p)))
	       (loop
		 with marker and cpds-per-slice = (if hidden-state-p 3 2)
		 with model = (episode-state-transitions (car eltm*))
		 with vvbms-hash = (model-var-vvbms model)
		 with num-slices = (/ (array-dimension (car model) 0) cpds-per-slice)
		 with slice and evidence-slice
		 for cur-slice in idx-list
		 do
		    (setq evidence-slice (gethash cur-slice evidence-hash))
		    (if (null evidence-slice)
			(setq evidence-slice (make-hash-table :test #'equal)))
		    (setq slice (make-hash-table :test #'equal))
		    (setq marker (mod (* cur-slice cpds-per-slice) (array-dimension (car model) 0)))
		    (when nil (= cur-slice 0)
		      (format t "~%~%cur slice (i): ~d~%num-slices per model: ~d~%mod slice length: ~d~%num-cpds: ~d~%model slice marker: ~d"
			      cur-slice num-slices cpds-per-slice (array-dimension (car model) 0) marker))
		    (loop
		      with cpd and cpd-type and evidence
		      with dist-hash
		      for j from marker to (+ marker (- cpds-per-slice 1))
		      ;;for j from 0 below (array-dimension (car model) 0)
		      do
			 (when nil (= cur-slice 0)
			   (format t "~%j: ~d" j))
			 (setq cpd (aref (car model) j))
			 (setq cpd-type (gethash 0 (rule-based-cpd-types cpd)))
			 (setq evidence (gethash cpd-type evidence-slice))
			 (setq dist-hash (make-hash-table :test #'equal))
			 (when nil (= cur-slice 0)
			   (format t "~%cpd:")
			   (print-cpd cpd)
			   (format t "~%cpd type: ~S" cpd-type)
			   (format t "~%evidence:")
			   (print-bn evidence))
			 (loop
			   with vvbm = (gethash (gethash 0 (rule-based-cpd-types cpd)) vvbms-hash)
			   with num-vvbm = (length vvbm)
			   for vvb in vvbm
			   do
			      (if evidence
				  (setf (gethash vvb dist-hash) (cons (float (/ 1 num-vvbm)) evidence))
				  (setf (gethash vvb dist-hash) (cons (float (/ 1 num-vvbm)) (make-empty-graph))))
			   finally
			      (setf (gethash cpd-type slice) dist-hash))
			 (when nil(= cur-slice 0)
			   (format t "~%dist hash:")
			   (print-dist-hash dist-hash)
			   (break)
			   ))
		    (setf (gethash cur-slice messages)
			  slice)))
	     messages)
	   (pass (messages forward-pass-p)
	     "Do a forward/backward pass along the number of time steps, saving the result in pass-evidence."
	     (let* ((mod-len (if hidden-state-p 3 2))
		    (model (episode-state-transitions (car eltm*)))
		    (num-slices (/ (array-dimension (car model) 0) mod-len))
		    (slice-steps (loop for i from 0 below num-slices collect i))
		    (time-steps (make-index-list forward-pass-p))
		    (pass-evidence (make-hash-table))
		    (marginals-pass-evidence (make-hash-table))
		    (evidence-slices (make-hash-table))
		    (alphas (make-hash-table)))
	       (when nil
		 (format t "~%time steps:~%~A" time-steps))
	       (dolist (group (sliding-groups time-steps num-slices))
		 (setq evidence-slices (make-hash-table))
		 (when t
		   (format t "~%group: ~S" group))
		 (setq alphas (make-hash-table))
		 (loop
		   with slice and slice-idx
		   with smallest-negative-delta = most-negative-fixnum and smallest-positive-delta = most-positive-fixnum
		   with biggest-smaller-obs and smallest-bigger-obs
		   with denom and alpha
		   for i from 0 below num-slices
		   for idx = (aref group i)
		   do
		      (setq alpha 0)
		      (setq smallest-bigger-obs nil)
		      (setq biggest-smaller-obs nil)
		      (when idx
			(loop
			  with keys = (sort (loop for k being the hash-keys of evidence-hash collect k) #'<)
			  for k in keys
			  do
			     (cond ((< k idx)
				    (setq biggest-smaller-obs k))
				   ((and (= k idx)
					 (null biggest-smaller-obs))
				    (setq biggest-smaller-obs k))
				   ((and (= k idx)
					 biggest-smaller-obs)
				    (setq smallest-bigger-obs k))
				   ((and (> k idx)
					 (null smallest-bigger-obs))
				    (setq smallest-bigger-obs k))))
			(cond ((and biggest-smaller-obs
				    smallest-bigger-obs)
			       (setq denom (abs (- biggest-smaller-obs smallest-bigger-obs)))
			       (setq alpha (/ (min (abs (- biggest-smaller-obs idx))
						   (abs (- smallest-bigger-obs idx)))
					      denom)))
			      ((and (null biggest-smaller-obs)
				    smallest-bigger-obs)
			       (setq denom (abs (- (min from to) smallest-bigger-obs)))
			       (setq alpha (/ (min (abs (- (min from to) idx))
						   (abs (- smallest-bigger-obs idx)))
					      denom)))
			      ((and biggest-smaller-obs
				    (null smallest-bigger-obs))
			       (setq denom (abs (- (max from to) biggest-smaller-obs)))
			       (setq alpha (/ (min (abs (- biggest-smaller-obs idx))
						   (abs (- (max from to) idx)))
					      denom)))
			      (t
			       (setq denom (abs (- from to)))
			       (setq alpha (/ (min (abs (- (min from to) idx))
						   (abs (- (max from to) idx)))
					      denom)))))
		      (when nil
			(format t "~%latest prior evidence idx: ~d~%earliest posterior evidence idx: ~d~%denom: ~d~%alpha: ~d" biggest-smaller-obs smallest-bigger-obs denom alpha))
		      (when nil
			(if (null idx)
			    (setq print-special* t)
			    (setq print-special* nil)))
		      (when t
			(format t "~%~%slice: ~d" idx))
		      (setq slice (gethash idx messages))
		      (when (null slice)
			(setq slice (make-hash-table :test #'equal)))
		      (when nil print-special*
			(format t "~%Content for:")
			(print-slice slice))
		      ;;(setq evidence-slices (cons slice evidence-slices))
		      (if forward-pass-p
			  (setq slice-idx i)
			  (setq slice-idx (- (- num-slices 1) i)))
		      (setf (gethash slice-idx evidence-slices) slice)
		      (setf (gethash slice-idx alphas) alpha))
		 (multiple-value-bind (evidence-bn backlinks)
		     (make-temporal-episode-retrieval-cue
		      eltm*
		      evidence-slices
		      t)  
		   (when nil print-special*
		     (format t "~%evidence bn:")
		     (print-bn evidence-bn)
		     (format t "~%evidence slices")
		     (loop
		       for i being the hash-keys of evidence-slices
			 using (hash-value evidence-hash)
		       do
			  (format t "~%slice: ~d" i)
			  (print-slice evidence-hash))
		     ;;(format t "~%backlinks:")
		     ;;(print-backlinks backlinks)
		     ;;(break)
		     )
		   (multiple-value-bind (state-transitions marginals-state-transitions)
		       (remember-temporal eltm* evidence-bn backlinks evidence-slices :hidden-state-p hidden-state-p :soft-likelihoods soft-likelihoods :bic-p bic-p :alphas alphas)
		     (when nil print-special*
		       (format t "~%state transitions:")
		       (print-state-transitions state-transitions)
		       ;;(break)
		       )
		     (loop
		       ;;for i from 0 below num-slices
		       ;;for idx = (aref group i)
		       for i in (if forward-pass-p slice-steps (reverse slice-steps))
		       for idx being the elements of group
		       do
			  (when nil print-special*
			    (format t "~%~%group (2nd): ~S~%state-trasitions index: ~d~%ordered state transitions slices: ~S~%pass-evidence-index: ~d" group i slice-steps idx))
			  (setf (gethash idx pass-evidence)
				(gethash i state-transitions))
			  (setf (gethash idx marginals-pass-evidence)
				(gethash i marginals-state-transitions))))))
	       (values pass-evidence marginals-pass-evidence)))
	   (forward-pass (messages)
	     (pass messages t))
	   (backward-pass (messages)
	     (pass messages nil)))
    (let ((messages (make-hash-table))
	  (forward-pass-p (<= from to))
	  marginals-messages
	  new-messages
	  new-marginals-messages)
      (setq messages (init-messages messages forward-pass-p))
      (when nil
	(print-state-transitions messages)
	(break))
      (loop
	with converged-p = nil
	for i from 1
	while (and (not converged-p)
		   (not (= i (+ n-passes 1))))
	do
	   (when t
	     (format t "~%~%iteration: ~d~%forward-pass-p: ~a" i forward-pass-p)
	     )
	    (if forward-pass-p
		(multiple-value-setq (new-messages new-marginals-messages)
		  (forward-pass (marginalize-messages messages))
		  )
		(multiple-value-setq (new-messages new-marginals-messages)
		  (backward-pass (marginalize-messages messages))
		  ))
	    (if (check-convergence messages new-messages)
		(setq converged-p t)
		(setq forward-pass-p (not forward-pass-p)))
	    (setq messages new-messages)
	    (setq marginals-messages new-marginals-messages)
	    (when t
	     (format t "~%messages")
	     (print-state-transitions messages)
	     ;;(format t "~%~%marginals")
	     ;;(print-state-transitions marginals-messages)
	     ;;(break)
	     )
	finally
	   (if (= i n-passes)
	       (format t "~%reached max inference passes")
	       (format t "~%callibrated temporal model"))
	   (remhash nil messages)
	   (remhash nil marginals-messages)
	   (when t
	     (format t "~%messages")
	     (print-state-transitions messages)
	     ;;(format t "~%~%marginals")
	     ;;(print-state-transitions marginals-messages)
	     )
	   (return (values messages marginals-messages))))))

(defun py-remember (eltm cue-bn mode lr bic-p &key (backlinks (make-hash-table :test #'equal)) (type "state-transitions") (observability 1) (softlikelihoods nil))
  (remember eltm cue-bn mode lr bic-p :backlinks backlinks :type type :observability observability :soft-likelihoods softlikelihoods))

(defun py-remember-temporal (eltm temporal-evidence-bn backlinks evidence-bns &key (mode '+) (lr 1) (bicp t) hiddenstatep softlikelihoods (alphas (make-hash-table)))
  (let (alist marginal-alist)
    (multiple-value-bind (messages marginal-messages)
	(remember-temporal eltm temporal-evidence-bn backlinks evidence-bns :mode mode :lr lr :bic-p bicp :hidden-state-p hiddenstatep :soft-likelihoods softlikelihoods :alphas alphas)
      (maphash #'(lambda (k v)
		   (push (list k v) alist))
	       messages)
      (maphash #'(lambda (k v)
		   (push (list k v) marginal-alist))
	       marginal-messages))
    (setq alist (coerce alist 'vector))
    (setq marginal-alist (coerce marginal-alist 'vector))
    (values alist marginal-alist)))

#| Generates code that compiles to a temporal episode. 
   Returns multiple values. 
      1. list of program statements 
      2. episode backlinks hash table 
      3. current observation identifier 
      4. current state identifier 
      5. current action identiier |#

;; eltm = episodic long-term memory
;; state-p = flag for whether or not the temporal model has a hidden state or not
;; slice-idx = index of the temporal episode slice that is being made
;; observations = hash table: Key: string, Value: observation retrieval cue
;; states = hash table: Key: string, Value: state retrieval cue
;; action = action name string
;; state-transitions = list of program statements
;; id-ref-hash = backlinks
(defun make-temporal-episode-program (eltm state-p slice-idx &key observations states actions state-transitions prev-st prev-obs prev-act (id-ref-hash (make-hash-table :test #'equal)))
  (let (obs-ref state-ref cur-st cur-obs cur-act cue)
    (when state-p
      (when nil
	(format t "~%states:~%~S" states))
      (setq cur-st (gensym "STATE-"))
      (when (and states
		 (> (hash-table-count states) 0))
	(loop
	  with value-prob-hash = (make-hash-table :test #'equal) and old-prob
	  with state and prob
	  for outcome being the hash-keys of states
	    using (hash-value prob-state)
	  do
	     (setq state (cdr prob-state))
	     (setq prob (car prob-state))
	     (when (null state)
	       (setq state (cons (make-array 0) (make-hash-table))))
	     (setq cue (make-episode :state state
				     :observation (cons (make-array 0) (make-hash-table))
				     :state-transitions (cons (make-array 0) (make-hash-table))
				     :backlinks (make-hash-table :test #'equal)
				     :count 1
				     :lvl 1))
	     (setq state-ref (new-retrieve-episode eltm cue nil :type "state"))
	     (setf (gethash outcome id-ref-hash) state-ref)
	     (setq old-prob (gethash outcome value-prob-hash))
	     (when (null old-prob)
	       (setq old-prob 0))
	     (setf (gethash outcome value-prob-hash) (+ old-prob prob))
	  finally
	     (loop
	       for v being the hash-keys of value-prob-hash
		 using (hash-value p)
	       collect (list :value v :probability p :count 1) into values
	       finally 
		  (setq state-transitions (concatenate 'list state-transitions `(,cur-st = (state-node ,(intern (format nil "STATE_~d" slice-idx)) :values ,values))))))))
    (when (and observations
	       (> (hash-table-count observations) 0))
      (setq cur-obs (gensym "OBS-"))
      (loop
	with value-prob-hash = (make-hash-table :test #'equal) and old-prob
	with observation and prob
	for outcome being the hash-keys of observations
	  using (hash-value prob-obs)
	do
	   (setq observation (cdr prob-obs))
	   (when (null observation)
	     (setq observation (cons (make-array 0) (make-hash-table))))
	   (setq prob (car prob-obs))
	   (setq cue (make-episode :state (cons (make-array 0) (make-hash-table))
				   :observation observation
				   :state-transitions (cons (make-array 0) (make-hash-table))
				   :backlinks (make-hash-table :test #'equal)
				   :count 1
				   :lvl 1))
	   (when nil t
		 (format t "~%retrieval cue for observation:~%~S" cue))
	   (setq obs-ref (new-retrieve-episode eltm cue nil :type "observation"))
	   (setf (gethash outcome id-ref-hash) obs-ref)
	   (setq old-prob (gethash outcome value-prob-hash))
	   (when (null old-prob)
	     (setq old-prob 0))
	   (setf (gethash outcome value-prob-hash) (+ old-prob prob))
	finally
	   (loop
	     for v being the hash-keys of value-prob-hash
	       using (hash-value p)
	     collect (list :value v :probability p :count 1) into values
	     finally 
		(setq state-transitions (concatenate 'list state-transitions `(,cur-obs = (observation-node ,(intern (format nil "OBSERVATION_~d" slice-idx)) :values ,values)))))))
    (when (and actions
	       (> (hash-table-count actions) 0))
      (setq cur-act (gensym "ACT-"))
      (loop
	for action being the hash-keys of actions
	  using (hash-value prob-act)
	collect (list :value action :probability (car prob-act) :count 1) into values
	finally
	   (setq state-transitions (concatenate 'list state-transitions `(,cur-act = (action-node ,(intern (format nil "ACTION_~d" slice-idx)) :values ,values))))))
    #|
    (when (and observation state)
      (setq state-transitions (concatenate 'list state-transitions `(,cur-st --> ,cur-obs))))
    (when (and observation action)
      (setq state-transitions (concatenate 'list state-transitions `(,cur-obs --> ,cur-act))))
    (cond (state-p
	   (when prev-st
	     (setq state-transitions (concatenate 'list state-transitions `(,prev-st --> ,cur-st))))
	   (when prev-act
	     (setq state-transitions (concatenate 'list state-transitions `(,prev-act --> ,cur-st)))))
	  (t
	   (when prev-obs
	     (setq state-transitions (concatenate 'list state-transitions `(,prev-obs --> ,cur-obs))))
	   (when prev-act
    (setq state-transitions (concatenate 'list state-transitions `(,prev-act --> ,cur-obs))))))
    |#
    (values state-transitions id-ref-hash cur-obs cur-st cur-act)))

#| Generates a retrieval cue for a temporal episode.
   Returns multiple values:
      1. temporal retrieval cue bn: Cons where first element is an array of CPDs, and second element is a hash table of edges
      2. hash table of backlinks
            key: episode id
            value: eltm subtree |#

;; eltm = episodic long-term memory
;; evidence-slices = hash table. Key: integer. Value: hash table of evidence observed for state and observation schemas. Key: ["STATE", "OBSERVATION", "ACTION"], Value: evidence hash table network
;;   evidence hash table:
;;     Key: episode id representing possible outcome
;;     Value: evidence network
;; state-p = flag for whether or not the temporal model has a hidden state or not
(defun make-temporal-episode-retrieval-cue (eltm evidence-slices state-p)
  (when nil t
	(format t "~%~%making temporal episode retrieval cue"))
  (loop
	with prog-statements and backlinks = (make-hash-table :test #'equal)
	with prev-st and prev-obs and prev-act
	for slice-idx being the hash-keys of evidence-slices
	using (hash-value slice)
	;;for slice in evidence-slices
	;;for slice-idx from 0
	do
	   (when nil
	     (format t "~%~%slice for retrieval cue:")
	     (print-slice slice))
	(multiple-value-setq (prog-statements backlinks prev-obs prev-st prev-act)
	  (make-temporal-episode-program eltm state-p slice-idx
					 :states (gethash "STATE" slice)
					 :observations (gethash "OBSERVATION" slice)
					 :actions (gethash "ACTION" slice)
					 :state-transitions prog-statements
					 :id-ref-hash backlinks
					 :prev-st prev-st
					 :prev-obs prev-obs
					 :prev-act prev-act))
	finally
	   (when nil
	     (format t "~%program statements:~%~S" prog-statements)
	     ;;(break)
	     )
	(return (values (eval `(compile-program nil ,@prog-statements)) backlinks))))

(defun py-test-hash ()
  (let ((ht (make-hash-table))
	alist)
    (setf (gethash 1 ht) 42)
    (setf (gethash 2 ht) "hello")
    (maphash #'(lambda (k v)
		 (push (list k v) alist))
	     ht)
    (coerce alist 'vector)))

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
    (when nil (episode-temporal-p ep1)
      (format t "~%~%episode depth: ~d~%schema depth: ~d" (episode-depth ep1) (episode-depth ep2)))
    (loop
      while (not (= (episode-depth ep1) (episode-depth ep2)))
      do
         (setq ep1 (car (episode-parent ep1)))
	 (when nil (episode-temporal-p ep1)
	   (format t "~%episode id: ~A~%episode depth: ~d" (episode-id ep1) (episode-depth ep1)))
      finally
	 (when nil (episode-temporal-p ep1)
	   (format t "~%episode id: ~S~%schema id: ~S~%equal? ~S" (episode-id ep1) (episode-id ep2) (equal (episode-id ep1) (episode-id  ep2))))
         (when (equal (episode-id ep1) (episode-id  ep2))
           (return ep1))
	 (return nil))))

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
    #|
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
    |#
    (when t
      (format t "~%insertp?: ~A" insertp))
    (when insertp
      (loop
	with state-transitions = nil
        with ep and ep-id and obs-ref and st-ref
        with cur-st and cur-obs and cur-act and prev-act and prev-st and prev-obs and st-bn and id-ref-hash = (make-hash-table :test #'equal)
        for (o s act) in (gethash 0 (getf episode-buffer* :obs))
	for i from 0
	do
	   (setq ep-id (symbol-name (gensym "EPISODE-")))
           (setq ep (make-episode :id ep-id
                                  :index-episode-id ep-id
                                  :observation (copy-bn o)
				  :state (cons (make-array 0) (make-hash-table :test #'equal))
				  :state-transitions (cons (make-array 0) (make-hash-table :test #'equal))
				  :backlinks (make-hash-table :test #'equal)
				  :count 1
                                  :lvl 1))
           (format t "~%inserting observation, ~A" (episode-id ep))
	   (multiple-value-setq (eltm* obs-ref)
             (new-insert-episode eltm* ep nil :bic-p bic-p :type "OBSERVATION"))
	   (when temporal-p
	     (when st
	       (setq ep-id (symbol-name (gensym "EPISODE-")))
               (setq ep (make-episode :id ep-id
                                      :index-episode-id ep-id
				      :observation (cons (make-array 0) (make-hash-table :test #'equal))
                                      :state (copy-bn s)
				      :state-transitions (cons (make-array 0) (make-hash-table :test #'equal))
				      :backlinks (make-hash-table :test #'equal)
				      :count 1
                                      :lvl 1))
	       (format t "~%inserting state, ~A" (episode-id ep))
	       (multiple-value-setq (eltm* st-ref)
		 (new-insert-episode eltm* ep nil :bic-p bic-p :type "STATE"))
	       (setf (gethash (episode-id (car st-ref)) id-ref-hash) st-ref))
	     (when st
	       (setq cur-st (gensym "STATE-")))
	     (setq cur-obs (gensym "OBS-"))
	     (setq cur-act (gensym "ACT-"))	     
	     ;; you have to put these hash keys in after you've done all the insertions for observations, states, and (eventually) actions. Otherwise, if you update id-ref-hash, then do further insertions, the episode id of the reference will change, making the key obsolte. Worse, the cpd vvbm will have the name of the new ref hash key, but the id ref hash will have the name of the old ref. So, look ups will fail.
	     (setf (gethash (episode-id (car obs-ref)) id-ref-hash) obs-ref)
	     (when st
	       (setq state-transitions (concatenate 'list state-transitions `(,cur-st = (state-node ,(intern (format nil "STATE_~d" i)) :value ,(episode-id (car st-ref)))))))
	     (setq state-transitions (concatenate 'list state-transitions `(,cur-obs = (observation-node ,(intern (format nil "OBSERVATION_~d" i)) :value ,(episode-id (car obs-ref))))))
	     (setq state-transitions (concatenate 'list state-transitions `(,cur-act = (action-node ,(intern (format nil "ACTION_~d" i))  :value ,act))))
	     (when st
	       (setq state-transitions (concatenate 'list state-transitions `(,cur-st --> ,cur-obs))))
	     (setq state-transitions (concatenate 'list state-transitions `(,cur-obs --> ,cur-act)))
	     (cond (st
		    (when prev-st
		      (setq state-transitions (concatenate 'list state-transitions `(,prev-st --> ,cur-st)))))
		   (t
		    (when prev-obs
		      (setq state-transitions (concatenate 'list state-transitions `(,prev-obs --> ,cur-obs))))))
	     (when prev-act
	       (if st
		   (setq state-transitions (concatenate 'list state-transitions `(,prev-act --> ,cur-st)))
		   (setq state-transitions (concatenate 'list state-transitions `(,prev-act --> ,cur-obs)))))
	     (if st
		 (setq prev-st cur-st)
		 (setq prev-obs cur-obs))
	     (setq prev-act cur-act))
        finally
	   (when state-transitions
	     ;;(eltm-to-pdf)
	     ;;(setq state-transitions (concatenate 'list ,@state-transitions))
	     (setq st-bn (eval `(compile-program nil ,@state-transitions)))
	     (when nil (> (hash-table-count (rule-based-cpd-identifiers (aref (car st-bn) 0))) 1)
		   ;;(format t "~%state transition model:~%~A" st-bn)
		   (format t "~%~S" `(compile-program nil ,@state-transitions))
		   (print-bn st-bn)
	       (break))
             ;; make temporal episode from state transitions
             (setq ep-id (symbol-name (gensym "EPISODE-")))
             (setq ep (make-episode :id ep-id
                                    :index-episode-id ep-id
				    :observation (cons (make-array 0) (make-hash-table :test #'equal))
				    :state (cons (make-array 0) (make-hash-table :test #'equal))
                                    :state-transitions st-bn
				    :backlinks id-ref-hash
                                    :temporal-p t
                                    :count 1
                                    :lvl 2))
	     (format t "~%Inserting transition model, ~A" (episode-id ep))
	     (setq eltm* (new-insert-episode eltm* ep nil :bic-p bic-p :type "STATE-TRANSITIONS"))))
      ;; clear buffer
      (clear-episodic-cache 0)
      #|
      (setf (gethash 1 (getf episode-buffer* :obs))
      (nreverse (cons (list ref (copy-observation (car (episode-states (car ref))))) (nreverse (gethash 1 (getf episode-buffer* :obs))))))
      |#
      ;;(eltm-to-pdf)
      ;;(break)
      )))

#| Python wrapper for pushing new experience into event memory |#
(defun py-push-to-ep-buffer(&key (observation nil) (state nil) (actionname nil) (bicp t) (insertp nil) (temporalp t) (hiddenstatep nil))
  (let (ret)
    (setq ret (handler-bind ((condition #'condition-handler))
		(new-push-to-ep-buffer :observation observation :state state :action-name actionname :bic-p bicp :insertp insertp :temporal-p temporalp :hidden-state-p hiddenstatep)))
    ret))

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
    (print-episode-state (episode-state ep) id "State" stream)
    (print-episode-state (episode-state-transitions ep) id "Transition_Model" stream)
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

#| Helper function for printing the episodic memory in pdf form |#

;; eps = episodes
;; parent = parent of episodes
;; stream = output stream
(defun pdf-struct-helper (eps parent stream)
  (let* ((ep (car eps))
         (id (subseq (episode-id ep) (+ (search "-" (episode-id ep)) 1))))
    (when parent
      (format stream "~A -> ~A;~%" parent id))
    ;;visit children
    (mapcar #'(lambda (e)
                (pdf-struct-helper e id stream))
            (cdr eps))))

#| Helper function for printing the episodic memory in pdf form |#

;; eps = episodes
;; parent = parent of episodes
;; stream = output stream
(defun pdf-helper-ray (eps parent)
  (let* ((ep (car eps))
         (id (subseq (episode-id ep) (+ (search "-" (episode-id ep)) 1))))
    (with-open-file (stream (format nil "ray/~A.dot" (episode-id ep))
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream "digraph G {~%compound=true;~%")
      (print-episode-state (episode-observation ep) id "Observation" stream)
      (print-episode-state (episode-state ep) id "State" stream)
      (print-episode-state (episode-state-transitions ep) id "Transition_Model" stream)
      ;;(format t "~%HERE!!!!")
      ;;(format stream "~d[shape=point style=invis]~%" id)
      ;;(format stream "~d~%" id)
      (format stream "}~%"))
    ;;visit children
    (mapcar #'(lambda (e)
                (pdf-helper-ray e id))
            (cdr eps))))


#| Graphically display the episodic memory |#

;; eltm = episodic memory
;; count = count for the file number
(defun eltm-to-pdf-ray (&optional eltm count)
  (ensure-directories-exist "./ray/")
  (let ((ep (if eltm eltm eltm*)))
    (cond (ep
	   (with-open-file (eltm-file "ray/eltm-struct.dot"
				      :direction :output
				      :if-exists :supersede
				      :if-does-not-exist :create)
	     (format eltm-file "digraph G {~%compound=true;~%")
	     (pdf-struct-helper ep nil eltm-file)
	     (format eltm-file "}~%"))
	   (pdf-helper-ray ep nil))
	  (t
	   (format t "~%failed to generate pdf. eltm is nil")))))

#| TESTS
(ql:quickload :hems)
(hems::run-execution-trace "/home/david/Code/HARLEM/ep_data_10/ppo_CliffWalking-v0_data.csv")
(let (st-evidence evidence-slices slice inference-slices-hash)
  (setq st-evidence (hems:compile-program nil
		       c1 = (relation-node number_1 :value "1")
		       c2 = (percept-node one_1 :value "1")
		       c3 = (relation-node number_2 :value "5")
		       c4 = (percept-node five_2 :value "5")
		       c1 --> c2
		       c3 --> c4))
  (setq slice (make-hash-table :test #'equal))
  (setf (gethash "STATE" slice) st-evidence)
  (setq evidence-slices (cons slice evidence-slices))
  (multiple-value-bind (evidence-bn backlinks evidence-bns)
      (hems:make-temporal-episode-retrieval-cue
       (hems:get-eltm)
       evidence-slices)
(setq inference-slices-hash(hems::remember-temporal (hems:get-eltm) evidence-bn backlinks evidence-bns :hidden-state-p t))
(loop
for idx being the hash-keys of inference-slices-hash
using (hash-value slice)
do
(format t "~%slice: ~d" idx)
(format t "~%~A" slice))))
    
(let (evidence-bn)
  (setq evidence-bn (hems:compile-program nil
		       c1 = (relation-node number_1 :value "1")))
(multiple-value-bind (recollection eme sol)
(hems:remember (hems:get-eltm) evidence-bn '+ 1 t :type "state")
(values recollection sol)))

;;;; Inference with prior example
(ql:quickload :hems)
(let (bn eltm)
  (setq bn (hems:compile-program nil 
	     c2 = (percept-node b :value "10")
	     c2 ~ (discrete-uniform :values ("10" "20" "30" "40"))
	     c1 = (percept-node a :value "A")
	     c1 --> c2))
  (hems:new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil)
  (hems:new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil)
  (hems:remember (hems:get-eltm) (cons (make-array 0) (make-hash-table :test #'equal)) '+ 1 t :type "observation" :soft-likelihoods t))
|#
