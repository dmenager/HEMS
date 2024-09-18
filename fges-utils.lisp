(defun add-to-set (elem set)
  (if (member elem set)
      set
      (cons elem set)))

(defun remove-from-set (elem set)
  (remove elem set))

(defun set-union (set1 set2)
  (union set1 set2))

(defun set-intersection (set1 set2)
  (intersection set1 set2))

(defun set-difference (set1 set2)
  (set-difference set1 set2))

(defun is-clique (nodes graph)
  ;; Check if nodes form a clique in graph
  (every (lambda (node)
           (every (lambda (other-node)
                    (or (equal node other-node)
                        (member other-node (gethash node graph))))
                  nodes))
         nodes))

(defun parents (x graph)
  ;; Return the set of parents of node x in the graph
  (remove-if-not (lambda (edge)
                   (equal (second edge) x))
                 graph))

(defun adj (x graph)
  ;; Return the set of adjacent nodes of x in graph
  (remove-if-not (lambda (edge)
                   (or (equal (first edge) x)
                       (equal (second edge) x)))
                 graph))

#| Enumerates all possible combinations of lst choose n. |#

;; n = number of elements in each list
;; lst = set of elements
(defun combinations (lst n)
  (cond
    ((zerop n) nil)
    ((= n 1) (mapcar #'list lst))
    (t (mapcan #'(lambda (x)
		   (mapcar #'(lambda (y) (append (list x) y))
			   (combinations (1- n) (setf lst (delete x lst)))))
	       lst))))
