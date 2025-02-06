#| Meek rules 1, 2, and 3 are used (away from collider, away from cycle, and double triangle), from (Meek, 1995). |#

;; graph = bayesian network that is being built
;; v = set of nodes in the graph
(defun apply-meek-rules-locally (v graph)
  (let ((R v))
    ;; Add x and y to R for any edge x->y or x—y that was oriented/unoriented
    (dolist (edge graph)
      (let ((x (first edge))
            (y (second edge)))
        (setf R (add-to-set x R))
        (setf R (add-to-set y R))))
    
    ;; Repeat Meek rules application
    (do ((changed t)
         (count 0 (1+ count)))
        ((not changed) (return R))
      (setf changed nil)
      (dolist (n v)
        (let ((adj-n (adj n graph)))
          ;; Check whether we need to apply Meek rules to n and its adjacents
          (when (condition-check)  ;; Add relevant conditions here
            (apply-meek-rule n adj-n)
            (setf changed t)))))))
#| We are considering whether to add x->y to G and what set T of nodes should be oriented into y in the process, 
creating mostly false colliders that will be shielded and corrected in the backward step. 
One "shields" a collider A->B<-C by adding an undirected edge A—C and reorienting A->B and C->B as undirected. 
x and y are non-adjacent nodes. |#

;; x = node in the graph
;; y = node in the graph
;; lookup-arrows = 
;; sorted-arrows = 
;; graph =
(defun calculate-arrows-forward (x y lookup-arrows sorted-arrows graph)
  (let ((T (set-difference (adj y graph) (adj x graph)))
        (NaYX (set-intersection (adj y graph) (adj x graph))))
    (dolist (subset-T (power-set T))
      (when (is-clique (set-union NaYX subset-T) graph)
        (let ((S (set-union NaYX subset-T (parents y graph))))
          (when (> (I x y S) 0)  ;; Assuming a function I(x, y, Z)
            (let ((arrow (list x y NaYX S (I x y S))))
              (push arrow sorted-arrows)
              (setf (gethash (list x y) lookup-arrows) arrow))))))))


#| The reevaluation function checks the edges and recalculates forward arrows. |#

;; R = set of nodes
;; graph =
;; sorted-arrows =
;; lookup-arrows =
(defun reevaluate-forward (R graph sorted-arrows lookup-arrows)
  (dolist (x R)
    (dolist (w (adj x graph))
      (when (> (I x w '()) 0)
        (dolist (arrow (gethash (list w x) lookup-arrows))
          (setf sorted-arrows (remove arrow sorted-arrows)))
        (calculate-arrows-forward w x lookup-arrows sorted-arrows graph)))))

#| Perform forward equivalence search. This is the main forward search procedure that uses sorted arrows. |#

;; sorted-arrows =
;; lookup-arrows =
;; graph = 
(defun fes (sorted-arrows lookup-arrows graph)
  (loop while sorted-arrows do
        (let ((arrow (pop sorted-arrows)))
          (let* ((x (first arrow))
                 (y (second arrow))
                 (NaYX (third arrow))
                 (S (fourth arrow)))
            (when (and (not (adjacent-p x y graph))
                       (is-clique (set-union NaYX S) graph)
                       (no-semidirected-path x y NaYX graph))
              (add-edge x y graph)
              (orient-nodes S y graph)
              (let ((R (apply-meek-rules-locally (list x y) graph)))
                (reevaluate-forward R graph sorted-arrows lookup-arrows)))))))

#| This function calculates arrows similarly to the forward search. |#

;; x =
;; y =
;; lookup-arrows =
;; sorted-arrows =
;; graph = 
(defun calculate-arrows-backward (x y lookup-arrows sorted-arrows graph)
  (let ((NaYX (set-intersection (adj y graph) (adj x graph))))
    (dolist (subset-H (power-set NaYX))
      (when (is-clique (set-difference NaYX subset-H) graph)
        (let ((S (set-union (set-difference NaYX subset-H) (parents y graph))))
          (when (> (- (I x y S)) 0)
            (let ((arrow (list x y NaYX subset-H (- (I x y S)))))
              (push arrow sorted-arrows)
              (setf (gethash (list x y) lookup-arrows) arrow))))))))

#| Helper function used in the backward search process. |#

;; R = List of nodes
;; graph = Graph being built
;; sorted-arrows =
;; lookup-arrows = 
(defun reevaluate-backward (R graph sorted-arrows lookup-arrows)
  (dolist (edge R)
    (let ((r (first edge))
          (w (second edge)))
      (if (directed-edge-p r w graph)
          (progn
            (setf sorted-arrows (remove-all lookup-arrows (list r w) sorted-arrows))
            (calculate-arrows-backward r w lookup-arrows sorted-arrows graph))
        (progn
          (setf sorted-arrows (remove-all lookup-arrows (list r w) sorted-arrows))
          (calculate-arrows-backward r w lookup-arrows sorted-arrows graph)
          (calculate-arrows-backward w r lookup-arrows sorted-arrows graph))))))

#| This search procedure handles removing edges. |#

;; sorted-arrows =
;; lookup-arrows =
;; graph = graph being built
(defun bes (sorted-arrows lookup-arrows graph)
  (loop while sorted-arrows do
        (let ((arrow (pop sorted-arrows)))
          (let* ((x (first arrow))
                 (y (second arrow))
                 (NaYX (third arrow))
                 (S (fourth arrow)))
            (when (and (adjacent-p x y graph)
                       (is-clique (set-difference NaYX S) graph))
              (remove-edge x y graph)
              (orient-nodes-backward NaYX y graph)
              (let ((R (apply-meek-rules-locally (set-union (list x y) NaYX) graph)))
                (reevaluate-backward R graph sorted-arrows lookup-arrows)))))))

#| Fast Greedy Equivalence Search |#

;; V = set of variables
(defun fges (V)
  (let ((sorted-arrows '())
        (lookup-arrows (make-hash-table))
        (graph (make-graph V)))
    ;; Step 3: Initial arrow scoring
    (dolist (pair (combinations V 2))
      (let ((x (first pair))
            (y (second pair)))
        (when (> (I x y '()) 0)
          (let ((arrow (list x y '() '() (I x y '()))))
            (push arrow sorted-arrows)
            (setf (gethash (list x y) lookup-arrows) arrow)))))
    ;; Step 5: Forward Equivalence Search
    (fes sorted-arrows lookup-arrows graph)
    ;; Step 6-7: Reset for backward search
    (setf sorted-arrows '())
    (setf lookup-arrows (make-hash-table))
    ;; Step 8: Backward Equivalence Search
    (bes sorted-arrows lookup-arrows graph)
    ;; Final adjustments
    (dolist (triple (triples graph))
      (let ((x (first triple))
            (y (second triple))
            (z (third triple)))
        (unless (and (adjacent-p x z graph)
                     (not (adjacent-p x y graph)))
          (calculate-arrows-forward x y lookup-arrows sorted-arrows))))
    ;; Final forward-backward passes
    (fes sorted-arrows lookup-arrows graph)
    (bes sorted-arrows lookup-arrows graph)
    ;; Return final graph
    graph))
