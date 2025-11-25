(in-package :hems)

#| Given index i of a sequence of variables, returns a list of all n indexes as the neighbors of i |# 

;; i = index of interest
;; n = number of variables in problem
(defun default-neighborhood (i n)
  (loop
    for j from 0 to (- n 1)
    when (not (= i j))
      collect j))

#| Given an index i into a 1D array, return a list of neighboring indeces |#

;; i = index of interest
;; size = size of the array
;; radius = radius of the neighborhood
(defun array-neighborhood (i size radius)
  (loop
    with start = (max 0 (- i radius))
    with end = (min (- size 1) (+ i radius)) 
    for n from start to end
    when (not (= i n))
      collect n into neighbors
    finally
       (return neighbors)))

;; i = index of interest
;; shape = list of array dimensions
;; radius = radius of the neighborhood
(defun eight-pixel-neighborhood (i shape radius)
  (labels ((idx-to-coord (idx shape)
	     (let (coord)
	       (loop
		 for dim in shape
		 do
		    (setq coord (cons (mod idx dim) coord))
		    (setq idx (floor (/ idx dim)))
		 finally
		    (return (reverse coord))))))
    (destructuring-bind (row col)
	(idx-to-coord i shape)
      (let ((row-start (max 0 (- row radius)))
	    (row-end (min (- (first shape) 1) (+ row radius)))
	    (col-start (max 0 (- col radius)))
	    (col-end (min (- (second shape) 1) (+ col radius))))
	(loop
	  with neighbors
	  for r from row-start to row-end
	  do
	     (loop
	       with ix
	       for c from col-start to col-end
	       do
		  (setq ix (+ (* (first shape) r) col))
	       when (not (= ix i))
		 do
		    (setq neighbors (cons ix neighbors)))
	  finally
	      (return (reverse neighbors)))))))

(defun bernoulli ())
(defun binomial ())

#| Create a discrete uniform prior distribution |#

;; values = list of values defining the cpd domain
(defun discrete-uniform (&key values)
  (let ((p (/ 1 (length values))))
    (mapcar #'(lambda (value)
		(list :value value :probability p :count nil))
	    values)))

(defun discrete-normal-approximation (&key values modes)
  (labels ((parse-string-numerics (l)
	     (mapcan #'(lambda (v)
			 (let (parsed)
			   (with-input-from-string (s v)
			     (setq parsed (read s))
			     (when (numberp parsed)
			       (list parsed)))))
		     l))
	   (get-distances (domain)
	     (mapcar #'(lambda (val)
			 ;;(cons val (apply #'min (mapcar #'(lambda (m) (abs (- m val))) modes)))
			 ;;(cons val (abs (- val (apply #'max modes))))
			 (cons val (abs (- val (mean modes)))))
		     domain))
	   (get-unnormalized-distribution (distances std)
	     (loop
	       with prob
	       for d in distances
	       do
	       (setq prob (* (/ 1 (* std (sqrt (* 2 pi))))
			     (exp (* -1/2 (expt (/ (cdr d) std) 2)))))
	       collect (cons (car d) prob) into unnormalized
	       summing prob into total
	       finally
		  (return (values unnormalized total))))
	   (get-distribution (distances std)
	     (multiple-value-bind (unnormalized total)
		 (get-unnormalized-distribution distances std)
	       (mapcar #'(lambda (pair)
			   (list :value  (write-to-string (car pair))
				 :probability (/ (cdr pair) total)
				 :count 1))
		       unnormalized))))
    (when nil
      (format t "~%values: ~S~%modes: ~S" values modes))
    (when (null modes)
      (error "Modes list must be non empty."))
    (when (null values)
      (error "Values list must be non empty."))
    (when (not (subsetp modes values :test #'equal))
      (error "Modes list needs to be a proper subset of values (case sensitive)."))
    (setq values (parse-string-numerics values))
    (setq modes (parse-string-numerics modes))
    (cons '(:value "NA" :probability 0.0 :count 1)
	  (get-distribution (get-distances values) (if (> (length modes) 1) (stdev modes) 1)))))

(defun get-cpd-type (ref-cpd)
  (cond ((equal (gethash 0 (rule-based-cpd-types ref-cpd)) "PERCEPT")
	 'percept-node)
	((equal (gethash 0 (rule-based-cpd-types ref-cpd)) "OBSERVATION")
	 'observation-node)
	((equal (gethash 0 (rule-based-cpd-types ref-cpd)) "ACTION")
	 'action-node)
	((equal (gethash 0 (rule-based-cpd-types ref-cpd)) "BELIEF")
	 'relation-node)
	((equal (gethash 0 (rule-based-cpd-types ref-cpd)) "STATE")
	 'state-node)))

;; node-def = list describing node contents in attribute-value pair format. :kb-concept-id is optional.
(defun make-bn-node (node-def)
  (labels ((n-cpd-populate-vvbm-sva-vals (value idx vvbm sva vals)
	     (when (null (gethash 0 vvbm))
	       (setf (gethash 0 vvbm) nil))
	     (when (null (gethash 0 sva))
	       (setf (gethash 0 sva) nil))
	     (when (null (gethash 0 vals))
	       (setf (gethash 0 vals) nil))
	     (setf (gethash 0 vvbm)
		   (reverse (cons (list (cons value idx) (make-hash-table))
				  (reverse (gethash 0 vvbm)))))
	     (setf (gethash 0 sva)
		   (reverse (cons (list idx)
				  (reverse (gethash 0 sva)))))
	     (setf (gethash 0 vals)
		   (reverse (cons idx
				  (reverse (gethash 0 vals))))))
	   (n-cpd-add-book-keeping-variables (cpd vvbm sva vals)
	     (let (cards steps)
	       (setq cards (generate-cpd-cardinalities vvbm))
	       (setq steps (generate-cpd-step-sizes cards))
	       (setf (rule-based-cpd-var-value-block-map cpd) vvbm)
	       (setf (rule-based-cpd-set-valued-attributes cpd) sva)
	       (setf (rule-based-cpd-lower-approx-var-value-block-map cpd)
		     (copy-hash-table vvbm))
	       (setf (rule-based-cpd-characteristic-sets cpd)
		     (make-hash-table))
	       (setf (rule-based-cpd-characteristic-sets-values cpd)
		     (make-hash-table))
	       (setf (rule-based-cpd-var-values cpd) vals)
	       (setf (rule-based-cpd-cardinalities cpd) cards)
	       (setf (rule-based-cpd-step-sizes cpd) steps))))
    (cond ((symbolp (car node-def))
	   (let ((cpd (make-rule-based-cpd))
		 (type-identifier)
		 (identifiers (make-hash-table :test #'equal))
		 (vars (make-hash-table))
		 (types-hash (make-hash-table))
		 (cids (make-hash-table))
		 (vvbm (make-hash-table))
		 (sva (make-hash-table))
		 (vals (make-hash-table))
		 (cards)
		 (steps)
		 (type)
		 (concept-id (getf node-def :kb-concept-id))
		 (value (getf node-def :value))
		 (values (getf node-def :values)) ;;(percept-node p :values ((a . 1) (b . 2)))
		 (arguments (getf node-def :arguments))
		 (generator (getf node-def :generator))
		 (supported-keywords '(:value :kb-concept-id :values :generator :arguments))
		 (node-def-kwds (remove-if-not #'keywordp node-def))
		 (unsupported))
	     (setq unsupported (set-difference node-def-kwds supported-keywords :test #'equal))
	     (when unsupported
	       (error "Unsupported keywords ~{~A~^, ~} in node defintion list." unsupported))
	     (cond ((or (string-equal "FUNCTIONAL-NODE" (symbol-name (car node-def))))
		    (cond ((and (null value)
				(null values))
			   (error "No 'value' or 'values' field found in node definition list.~%Received: ~S" node-def))
			  ((and value values)
			   (error "Found 'value' and 'values' field in node definition, but they are mutually exclusive.~%Received: ~S" node-def))))
		   ((or (string-equal "PERCEPT-NODE" (symbol-name (car node-def)))
			(string-equal "OBSERVATION-NODE" (symbol-name (car node-def)))
			(string-equal "RELATION-NODE" (symbol-name (car node-def)))
			(string-equal "STATE-NODE" (symbol-name (car node-def)))
			(string-equal "ACTION-NODE" (symbol-name (car node-def))))
		    (cond ((and (null value)
				(null values))
			   (error "No 'value' or 'values' field found in node definition list.~%Received: ~S" node-def))
			  ((and value values)
			   (error "Found 'value' and 'values' field in node definition, but they are mutually exclusive.~%Received: ~S" node-def)))
		    (cond ((and (symbolp (second node-def))
				(not (null (second node-def))))
			   (cond ((equal "PERCEPT-NODE" (symbol-name (car node-def)))
				  (setf (gethash 0 types-hash) "PERCEPT"))
				 ((equal "OBSERVATION-NODE" (symbol-name (car node-def)))
				  (setf (gethash 0 types-hash) "OBSERVATION"))
				 ((equal "RELATION-NODE" (symbol-name (car node-def)))
				  (setf (gethash 0 types-hash) "BELIEF"))
				 ((equal "STATE-NODE" (symbol-name (car node-def)))
				  (setf (gethash 0 types-hash) "STATE"))
				 ((equal "ACTION-NODE" (symbol-name (car node-def)))
				  (setf (gethash 0 types-hash) "ACTION")))
			   (setq type (symbol-name (second node-def)))
			   (setf (rule-based-cpd-dependent-var cpd) type)
			   (setf (gethash 0 vars) type)
			   (setf (rule-based-cpd-vars cpd) vars)
			   (setq type-identifier (symbol-name (gensym (concatenate 'string type "_"))))
			   (setf (rule-based-cpd-dependent-id cpd) type-identifier)
			   (setf (gethash type-identifier identifiers) 0)
			   (setf (rule-based-cpd-identifiers cpd) identifiers)
			   (setf (rule-based-cpd-types cpd) types-hash)
			   (setf (rule-based-cpd-concept-blocks cpd) (make-hash-table))
			   (setf (rule-based-cpd-count cpd) 1)
			   (when value
			     (setq values (list (list :value "NA" :probability 0 :count 0)
						(list :value value :probability 1 :count 1))))
			   (when (not (member "NA" values :test #'equal :key #'(lambda (lst)
										 (getf lst :value))))
			     (let (count-val)
			       (if (numberp (getf (car values) :count))
				   (setq count-val 0))
			       (setq values (cons (list :value "NA" :probability 0 :count count-val) values))))
			   (loop
			     with greatest-idx-val = 1 and idx
			     with rule and rules = (make-array (length values))
			     with value and prob and count
			     for value-list in values
			     ;;for idx from 0
			     do
				(setq value (getf value-list :value))
				(setq prob (getf value-list :probability))
				(setq count (getf value-list :count))
				(if (equal "NA" value)
				    (setq idx 0)
				    (setq idx greatest-idx-val))
				(setq rule (make-rule
					    :id (gensym "RULE-")
					    :conditions (make-hash-table :test #'equal)
					    :probability prob
					    :certain-block (make-hash-table)
					    :count count))
				(setf (gethash (rule-based-cpd-dependent-id cpd)
					       (rule-conditions rule))
				      (list idx))
				(setf (aref rules idx) rule)
				(cond ((stringp value)
				       (n-cpd-populate-vvbm-sva-vals value idx vvbm sva vals))
				      ((numberp value)
				       (error "Numeric value, ~A, not yet supported in node definition list. Expected string." value))
				      (t
				       (error "Unsupported value, ~A, for 'value' in node definition list.~%Received type ~A. Expected string or numeric." value (type-of value))))
				(if (not (equal "NA" value))
				    (setq greatest-idx-val (+ greatest-idx-val 1)))
			     finally
				(setf (rule-based-cpd-rules cpd) rules))
			   (n-cpd-add-book-keeping-variables cpd vvbm sva vals)
			   (cond ((or (null concept-id)
				      (stringp concept-id))
				  (if concept-id
				      (setf (gethash 0 cids) concept-id)
				      (setf (gethash 0 cids) "NIL"))
				  (setf (rule-based-cpd-concept-ids cpd) cids)
				  (setf (rule-based-cpd-qualified-vars cpd)
					(generate-cpd-vars identifiers vars cids)))
				 (t
				  (error "Unsupported value, ~A, for concept id in node definition list.~%Received type ~A. Expected string." concept-id (type-of concept-id))))
			   cpd)
			  (t
			   (raise-identifier-type-error (second node-def)))))
		   (t
		    (error "Unsupported type, ~A for node definition list." (car node-def))))))
	  (t
	   (raise-identifier-type-error (car node-def))))))

(defun raise-identifier-type-error (recieved)
  (error "Unsupported identifier ~A. Expected type of non-nil symbol." recieved))

(defun directed-edge (cpd1 cpd2 causal-discovery)
  (let ((cpd1-copy (make-rule-based-cpd
		    :dependent-id (rule-based-cpd-dependent-id cpd1)
		    :dependent-var (rule-based-cpd-dependent-var cpd1)
		    :identifiers (make-hash-table :test #'equal)
		    :vars (make-hash-table)
		    :types (make-hash-table)
		    :concept-ids (make-hash-table)
		    :qualified-vars (make-hash-table)
		    :var-value-block-map (make-hash-table)
		    :set-valued-attributes (make-hash-table)
		    :lower-approx-var-value-block-map (make-hash-table)
		    :var-values (make-hash-table)
		    :rules (make-array 2)))
	rule)
    (setq rule (make-rule
		:id (gensym "RULE-")
		:conditions (make-hash-table :test #'equal)
		:probability 0
		:certain-block (make-hash-table)
		:count 0))
    (setf (gethash (rule-based-cpd-dependent-id cpd1)
		   (rule-conditions rule))
	  (list 0))
    (setf (aref (rule-based-cpd-rules cpd1-copy) 0) rule)
    (setq rule (make-rule
		:id (gensym "RULE-")
		:conditions (make-hash-table :test #'equal)
		:probability 1
		:certain-block (make-hash-table)
		:count 1))
    (setf (gethash (rule-based-cpd-dependent-id cpd1)
		   (rule-conditions rule))
	  (list 1))
    (setf (aref (rule-based-cpd-rules cpd1-copy) 1) rule)
    
    (setf (gethash (rule-based-cpd-dependent-id cpd1-copy)
		   (rule-based-cpd-identifiers cpd1-copy))
	  0)
    (setf (gethash 0 (rule-based-cpd-vars cpd1-copy))
	  (gethash 0 (rule-based-cpd-vars cpd1)))
    (setf (gethash 0 (rule-based-cpd-concept-ids cpd1-copy))
	  (gethash 0 (rule-based-cpd-concept-ids cpd1)))
    (setf (gethash 0 (rule-based-cpd-qualified-vars cpd1-copy))
	  (gethash 0 (rule-based-cpd-qualified-vars cpd1)))
    (setf (gethash 0 (rule-based-cpd-var-value-block-map cpd1-copy))
	  (gethash 0 (rule-based-cpd-var-value-block-map cpd1)))
    (setf (gethash 0 (rule-based-cpd-set-valued-attributes cpd1-copy))
	  (gethash 0 (rule-based-cpd-set-valued-attributes cpd1)))
    (setf (gethash 0 (rule-based-cpd-lower-approx-var-value-block-map cpd1-copy))
	  (gethash 0 (rule-based-cpd-lower-approx-var-value-block-map cpd1)))
    (setf (gethash 0 (rule-based-cpd-var-values cpd1-copy))
	  (gethash 0 (rule-based-cpd-var-values cpd1)))
    (if (> (length (gethash 0 (rule-based-cpd-var-values cpd1))) 2)
	(error "multiple variable values currently unsupported"))
    (modify-cpd cpd2 cpd1-copy :causal-discovery causal-discovery)))

#| Sort a list of factors in topological order. Returns a list. |#

;; factors-list = list of cpds
(defun topological-sort (factors-list)
  (let (l s copy)
    (setq copy (copy-factors (make-array (length factors-list) :initial-contents factors-list)))
    (loop
      for cpd in factors-list
      when (= (hash-table-count (rule-based-cpd-identifiers cpd)) 1)
	do
	   (setq s (cons cpd s)))
    (loop
      with selection
      while (> (length s) 0)
      do
	 (setq selection (car s))
	 (setq l (reverse (cons selection (reverse l))))
	 (setq s (rest s))
	 (loop
	   for cpd being the elements of copy
	   for cpd1 in factors-list
	   do
	      (when nil (equal "AGE" (rule-based-cpd-dependent-var cpd))
		(format t "~%~%processing used cpd:~%~S~%checking for parent:~%~S" cpd selection))
	   when (and (not (equal (rule-based-cpd-dependent-id selection)
				 (rule-based-cpd-dependent-id cpd)))
		     (gethash (rule-based-cpd-dependent-id selection)
			      (rule-based-cpd-identifiers cpd)))
	     do
		(remhash (rule-based-cpd-dependent-id selection)
			 (rule-based-cpd-identifiers cpd))
		(when nil (equal "AGE" (rule-based-cpd-dependent-var cpd))
		  (format t "~%found match!~%updated used cpd:~%~S" cpd))
		(when (= (hash-table-count (rule-based-cpd-identifiers cpd)) 1)
		  (setq s (reverse (cons cpd1 (reverse s)))))))
    (loop
      for cpd1 in (reverse l)
      do
	 (loop
	   for cpd2 in l
	   when (and (not (equal (rule-based-cpd-dependent-id cpd1)
				 (rule-based-cpd-dependent-id cpd2)))
		     (gethash (rule-based-cpd-dependent-id cpd1)
			      (rule-based-cpd-identifiers cpd2)))
	     collect (rule-based-cpd-lvl cpd2) into lvls
	   finally
	      (setf (rule-based-cpd-lvl cpd1) (+ (apply #'max (cons 0 lvls)) 1))))
    l))

(defun divides (i j)
  (cond ((= j 1)
	 nil)
	(nil (= i 0)
	 nil)
	(t
	 (ignore-errors (integerp (/ i j))))))

(defun add-invariants (neighborhood-func nbr-func-args cpd-arr inv-hash invariants-list)
  (when (null neighborhood-func)
    (setq neighborhood-func #'default-neighborhood))
  (loop
    with reflexive-hash = (make-hash-table)
    with var-i and new-body and invariant-id
    for cpd-i being the elements of cpd-arr
    for i from 0
    do
       (setq var-i (caar (nth 1 (gethash 0 (rule-based-cpd-var-value-block-map cpd-i)))))
       (loop
	 for invariant in invariants-list
	 do
	    ;; unary invariants
	    (when (or (and (eq invariant 'nothing)
			   (= (parse-integer var-i) 0))
		      (and (eq invariant 'single)
			   (= (parse-integer var-i) 1))
		      (and (eq invariant 'couple)
			   (= (parse-integer var-i) 2))
		      (and (eq invariant 'triple)
			   (= (parse-integer var-i) 3))
		      (and (eq invariant 'multiple)
			   (> (parse-integer var-i) 3))
		      (and (eq invariant 'even)
			   (= (mod (parse-integer var-i) 2) 0)))
	      (setq invariant-id (gensym))
	      (setq new-body (concatenate 'list new-body `(,invariant-id = (relation-node ,invariant :value ,(rule-based-cpd-dependent-id cpd-i)))))
	      (setq new-body (concatenate 'list new-body `(,invariant-id -> ,(gethash (rule-based-cpd-dependent-id cpd-i) inv-hash))))))
       (loop
	 with cpd-j and var-j
	 for j in (eval `(funcall ,neighborhood-func ,i ,@nbr-func-args))
	 do
	    (setq cpd-j (aref cpd-arr j))
	    (setq var-j (caar (nth 1 (gethash 0 (rule-based-cpd-var-value-block-map cpd-j)))))
	    (loop
	      with invariant-alias and invariant-role1 and invariant-role2 and skip-test
	      with num-j and num-i and role1-id and role2-id and number-id
	      for invariant in invariants-list
	      when (or (eq invariant '>)
		       (eq invariant '=)
		       (eq invariant 'divides)
		       (eq invariant '+)
		       (eq invariant '-))
		do
		 (setq skip-test nil)
		 (setq invariant-id (gensym))
		 (setq role1-id (gensym))
		 (setq role2-id (gensym))
		 (setq number-id (gensym))
		 ;; binary invariants
		 (cond ((eq invariant '>)
			(setq invariant-alias 'greater_than)
			(setq invariant-role1 'greater)
			(setq invariant-role2 'lesser))
		       ((eq invariant '<)
			(setq invariant-alias 'less_than)
			(setq invariant-role1 'lesser)
			(setq invariant-role2 'greater))
		       ((eq invariant '=)
			(setq invariant-alias 'equal_to)
			(setq invariant-role1 'equal)
			(setq invariant-role2 'equal))
		       ((eq invariant '+)
			(setq invariant-alias (make-symbol (substitute #\_ #\- (string-upcase (format nil "~r" (+ (parse-integer var-j) (parse-integer var-i)))))))
			(setq invariant-role1 'augend)
			(setq invariant-role2 'addend))
		       ((eq invariant '-)
			(setq invariant-alias (make-symbol (substitute #\_ #\space (substitute #\_ #\- (string-upcase (format nil "~r" (- (parse-integer var-i) (parse-integer var-j))))))))
			(setq invariant-role1 'minuend)
			(setq invariant-role2 'substrahend))
		       ((eq invariant 'divides)
			(setq invariant-alias 'divides)
			(setq invariant-role1 'dividend)
			(setq invariant-role2 'divisor)))
		 (with-input-from-string (stream-j var-j)
		   (with-input-from-string (stream-i var-i)
		     (setq num-j (read stream-j))
		     (setq num-i (read stream-i))
		     (when (and (funcall invariant num-i num-j))
		       (cond ((and (funcall invariant num-j num-i))
			      (when (null (gethash invariant reflexive-hash))
				(setf (gethash invariant reflexive-hash) (make-hash-table)))
			      (when (not (member num-i (gethash num-j (gethash invariant reflexive-hash))))			     
				(cond ((eq invariant '+)
				       (setq new-body (concatenate 'list new-body `(,invariant-id = (relation-node ,invariant-alias :value "T"))))
				       (setq new-body (concatenate 'list new-body `(virtual_add_number = (relation-node additive_number :value ,(write-to-string (+ num-i num-j))))))
				       (setq new-body (concatenate 'list new-body `(virtual_add_number -> ,invariant-id))))
				      ((eq invariant '-)
				       (setq new-body (concatenate 'list new-body `(,invariant-id = (relation-node ,invariant-alias :value "T"))))
				       (setq new-body (concatenate 'list new-body `(virtual_sub_number = (relation-node substractive_number :value ,(write-to-string (- num-i num-j))))))
				       (setq new-body (concatenate 'list new-body `(virtual_sub_number -> ,invariant-id))))
				      ((eq invariant 'divides)
				       (setq new-body (concatenate 'list new-body `(,invariant-id = (relation-node ,invariant-alias :value ,(write-to-string (/ num-i num-j))))))
				       (setq new-body (concatenate 'list new-body `(virtual_add_number -> ,invariant-id)))
				       (setq new-body (concatenate 'list new-body `(virtual_sub_number -> ,invariant-id))))
				      (t
				       (setq new-body (concatenate 'list new-body `(,invariant-id = (relation-node ,invariant-alias :value ,(caar (second (gethash 0 (rule-based-cpd-var-value-block-map cpd-i))))))))
				       (setq new-body (concatenate 'list new-body `(virtual_add_number -> ,invariant-id)))
				       (setq new-body (concatenate 'list new-body `(virtual_sub_number -> ,invariant-id)))))
				;;(setq new-body (concatenate 'list new-body `(,number-id = (relation-node number :value ,(symbol-name invariant-alias)))))
				(setq new-body (concatenate 'list new-body `(,role1-id = (relation-node ,invariant-role1 :value ,(rule-based-cpd-dependent-id cpd-i)))))
				(setq new-body (concatenate 'list new-body `(,role2-id = (relation-node ,invariant-role2 :value ,(rule-based-cpd-dependent-id cpd-j)))))
				(setq new-body (concatenate 'list new-body `(,invariant-id -> ,role1-id)))
				(setq new-body (concatenate 'list new-body `(,invariant-id -> ,role2-id)))
				;;(setq new-body (concatenate 'list new-body `(,number-id -> ,invariant-id)))
				(setq new-body (concatenate 'list new-body `(,invariant-id -> ,(gethash (rule-based-cpd-dependent-id cpd-i) inv-hash))))
				(setq new-body (concatenate 'list new-body `(,invariant-id -> ,(gethash (rule-based-cpd-dependent-id cpd-j) inv-hash))))
				(setq new-body (concatenate 'list new-body `(,role1-id -> ,(gethash (rule-based-cpd-dependent-id cpd-i) inv-hash))))
				(setq new-body (concatenate 'list new-body `(,role2-id -> ,(gethash (rule-based-cpd-dependent-id cpd-j) inv-hash))))
				(setf (gethash num-i (gethash invariant reflexive-hash))
				      (cons num-j (gethash num-i (gethash invariant reflexive-hash))))))
			     (t ;;(not skip-test)
			      (cond ((eq invariant '+)
				     (setq new-body (concatenate 'list new-body `(,invariant-id = (relation-node ,invariant-alias :value "T"))))
				     (setq new-body (concatenate 'list new-body `(virtual_add_number = (relation-node additive_number :value ,(write-to-string (+ num-i num-j))))))
				     (setq new-body (concatenate 'list new-body `(virtual_add_number -> ,invariant-id))))
				    ((eq invariant '-)
				     (setq new-body (concatenate 'list new-body `(,invariant-id = (relation-node ,invariant-alias :value "T"))))
				     (setq new-body (concatenate 'list new-body `(virtual_sub_number = (relation-node substractive_number :value ,(write-to-string (- num-i num-j))))))
				     (setq new-body (concatenate 'list new-body `(virtual_sub_number -> ,invariant-id))))
				    ((eq invariant 'divides)
				     (setq new-body (concatenate 'list new-body `(,invariant-id = (relation-node ,invariant-alias :value ,(write-to-string (/ num-i num-j))))))
				     (setq new-body (concatenate 'list new-body `(virtual_add_number -> ,invariant-id)))
				     (setq new-body (concatenate 'list new-body `(virtual_sub_number -> ,invariant-id))))
				    (t
				     (setq new-body (concatenate 'list new-body `(,invariant-id = (relation-node ,invariant-alias :value ,(caar (second (gethash 0 (rule-based-cpd-var-value-block-map cpd-i))))))))
				     (setq new-body (concatenate 'list new-body `(virtual_add_number -> ,invariant-id)))
				     (setq new-body (concatenate 'list new-body `(virtual_sub_number -> ,invariant-id)))))
			      ;;(setq new-body (concatenate 'list new-body `(,number-id = (relation-node number :value ,(symbol-name invariant-alias)))))
			      (setq new-body (concatenate 'list new-body `(,role1-id = (relation-node ,invariant-role1 :value ,(rule-based-cpd-dependent-id cpd-i)))))
			      (setq new-body (concatenate 'list new-body `(,role2-id = (relation-node ,invariant-role2 :value ,(rule-based-cpd-dependent-id cpd-j)))))
			      (setq new-body (concatenate 'list new-body `(,invariant-id -> ,role1-id)))
			      (setq new-body (concatenate 'list new-body `(,invariant-id -> ,role2-id)))
			      ;;(setq new-body (concatenate 'list new-body `(,number-id -> ,invariant-id)))
			      (setq new-body (concatenate 'list new-body `(,invariant-id -> ,(gethash (rule-based-cpd-dependent-id cpd-i) inv-hash))))
			      (setq new-body (concatenate 'list new-body `(,invariant-id -> ,(gethash (rule-based-cpd-dependent-id cpd-j) inv-hash))))
			      (setq new-body (concatenate 'list new-body `(,role1-id -> ,(gethash (rule-based-cpd-dependent-id cpd-i) inv-hash))))
			      (setq new-body (concatenate 'list new-body `(,role2-id -> ,(gethash (rule-based-cpd-dependent-id cpd-j) inv-hash))))))
		       )))))
    finally
       ;;(format t "~%new body:~%~S" new-body)
       (return new-body)))

#| Compiles a hems program into a Bayesian Network. Returns a cons where the first element is an array of factors, and the second element is a nested hash-table of edges |#

;; body = HEMS program
;; relational-invariants = Flag for whether to augment the state with relational comparators that are true.
;; neighborhood-func = function that returns the indeces of the neighbors of the given variable index
;; nbr-func-args = a list of arguments for neighborhood function
(defmacro compile-program ((&key relational-invariants neighborhood-func nbr-func-args (sort-p t) causal-discovery) &body body)
  (let ((hash (gensym))
	(args (gensym))
	(prior-args (gensym))
	(inv-hash (gensym))
        (ident (gensym))
	(cpd (gensym))
	(cpd-list (gensym))
	(cpd-arr (gensym))
	(edge-type (gensym))
	(factors (gensym))
	(edges (gensym))
	(recurse-p (gensym))
	(invariant-list (gensym))
	(new-body (gensym)))
    (cond ((and neighborhood-func
		(not (eq t relational-invariants)))
	   (error "Relational Invariants flag must be set to t to define the neighborhood function: ~A." neighborhood-func))
	  ((and (not (eq t relational-invariants))
		(not (eq nil relational-invariants)))
	   (error "Relational Invariants flag is boolean. Must be set to t or nil. Value~%~A~% is invalid" relational-invariants))
	  ;;((and neighborhood-func relational-invariants (not (functionp neighborhood-func)))
	   ;;(error "Invalid neighborhood function. Received~%~A~%Expected a function." neighborhood-func))
	  ((and (eq nil relational-invariants) nbr-func-args)
	   (error "Cannot supply arguments~%~A~%when relational-invariants is nil." nbr-func-args)))
    `(labels ((compile-hems-program (,hash ,args ,invariant-list ,recurse-p ,edge-type)
		(cond (,args
		       (if (and (symbolp (first ,args))
				(not (null (first ,args))))
			   (cond ((equal (symbol-name '=) (symbol-name (second ,args)))
				  (cond ((listp (third ,args))
					 (when (gethash (first ,args) ,hash)
					   (warn "Attempting to overwrite value for identifyer ~A in statement ~{~A~^ ~}." (first ,args) (subseq ,args 0 3)))
					 (setf (gethash (first ,args) ,hash)
					       (make-bn-node (third ,args))))
					(t
					 (error "Expected assignment to node definition (list) in statement ~{~A~^ ~}." (subseq ,args 0 3)))))
				 ((equal (symbol-name '~) (symbol-name (second ,args)))
				  (cond ((listp (third ,args))
					 (when (not (gethash (first ,args) ,hash))
					   (error "Reference to ~A before assignment in statement ~{~A~^ ~}." (first ,args) (subseq ,args 0 3)))
					 (let* ((raw-symbol (first (third ,args)))
						(prior-fn (or (find-symbol (symbol-name raw-symbol) "HEMS")
							   (error "~A does not name a defined function" raw-symbol)))
						(,prior-args (apply prior-fn
								   (loop
								     for (key arg) on (cdr (third ,args)) by #'cddr
								     append (list key arg)))))
					 (setf (rule-based-cpd-prior (gethash (first ,args) ,hash))
					       (list (get-cpd-type (gethash (first ,args) ,hash))
						     'prior
						     :values ,prior-args))))
					(t
					 (error "Expected distributional identity to prior definition (list) in statement ~{~A~^ ~}." (subseq ,args 0 3)))))
				 ((or (equal "---" (symbol-name (second ,args)))
				      (equal "-->" (symbol-name (second ,args))))
				  (cond ((null ,edge-type)
					 (setq ,edge-type (symbol-name (second ,args))))
					((not (equal (symbol-name (second ,args)) ,edge-type))
					 (error "Multigraphs are not supported. Cannot reset edge type from ~S to ~S." ,edge-type (second ,args))))				  
				  (when (not (gethash (first ,args) ,hash))
				    (error "Reference to ~A before assignment in statement ~{~A~^ ~}." (first ,args) (subseq ,args 0 3)))
				  (cond ((and (symbolp (third ,args))
					      (not (null (third ,args))))
					 (when (not  (gethash (third ,args) ,hash))
					   (error "Reference to ~A before assignment in statement ~{~A~^ ~}." (third ,args) (subseq ,args 0 3)))
					 (directed-edge (gethash (first ,args) ,hash)
							(gethash (third ,args) ,hash)
							,causal-discovery))
					(t
					 (raise-identifier-type-error (third ,args)))))
				 (t
				  (error "Unrecognized operator in statement ~{~A~^ ~}.~%Received ~A.~%Expected assignment, distributional identity, or three-part edge with a startpoint, connector, and endpoint." (subseq ,args 0 3) (second ,args))))
			   (raise-identifier-type-error (first ,args))
		           )
		       (compile-hems-program ,hash (nthcdr 3 ,args) ,invariant-list ,recurse-p ,edge-type))
		      (t
		       (let (,inv-hash)
			 (when (and ,relational-invariants ,recurse-p)
			   (setq ,inv-hash (make-hash-table :test #'equal)))
			 (loop
			   with ,factors and ,edges and ,cpd-arr and ,new-body and ,invariant-list = '(+ - > = divides) ;;'(> = divides nothing single couple triple multiple)
			   for ,ident being the hash-keys of ,hash
			     using (hash-value ,cpd)
			   do
			      (setq ,cpd (normalize-rule-probabilities ,cpd (rule-based-cpd-dependent-id ,cpd)))
			   collect
			   (if ,causal-discovery
			       ,cpd
			       (get-local-coverings
				(update-cpd-rules ,cpd (rule-based-cpd-rules ,cpd))))
			     into ,cpd-list
			   when (and ,relational-invariants ,recurse-p)
			     do
				(setf (gethash (rule-based-cpd-dependent-id ,cpd) ,inv-hash) ,ident)
			   finally
			      (when ,sort-p
				(setq ,cpd-list (topological-sort ,cpd-list)))
			      (when (and ,relational-invariants ,recurse-p)
				(setq ,cpd-arr (make-array (hash-table-count ,hash) :initial-contents ,cpd-list))
				(setq ,new-body (add-invariants ,neighborhood-func ',nbr-func-args ,cpd-arr ,inv-hash ,invariant-list))
				(return-from compile-hems-program (compile-hems-program ,hash ,new-body ,invariant-list nil ,edge-type)))
			      (setq ,factors (make-array (hash-table-count ,hash)
							 :initial-contents ,cpd-list))
			      (setq ,edges (make-graph-edges ,factors :edge-type ,edge-type))
			      (return (cons ,factors ,edges))))))))
       (compile-hems-program (make-hash-table :test #'equal) ',body ',invariant-list t nil))))

(defun compile-program-from-file (prog-file &key args)
  (with-open-file (in (merge-pathnames prog-file) :direction :input
						  :if-does-not-exist :error)
    (loop
      with prog
      for line = (read-line in nil)
      while line
      do
	 (setq line (subseq line 0 (search ";" line)))
      when (> (length line) 0)
	do
	   (loop
	     with len = (length line)
	     with s and i = 0
	     do
		(multiple-value-setq (s i)
		  (read-from-string line nil nil :start i))
	        (setq prog (reverse (cons s (reverse prog))))
	     while (< i len))
      finally
	 (return (eval `(compile-program ,args ,@prog))))))
  
#| TESTS

(hems:compile-program nil 
c2 = (percept-node b :value "10")
c2 ~ (discrete-uniform :values ("10" "20" "30" "40")))
|#
