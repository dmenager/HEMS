(in-package :hems)

#| Given index i of a sequence of variables, returns a list of all n indexes as the neighbors of i |# 

;; i = index of interest
;; n = number of variables in problem
(defun default-neighborhood (i n)
  (loop
    for j from 0 to (- n 1)
    when (not (= i j))
     collect j))

;; node-def = list describing node contents in attribute-value pair format. :kb-concept-id is optional.
(defun make-bn-node (node-def)
  (cond ((symbolp (car node-def))
	 (let ((cpd (make-rule-based-cpd))
	       (type-identifier)
	       (identifiers (make-hash-table :test #'equal))
	       (vars (make-hash-table))
	       (types-hash (make-hash-table))
	       (cids (make-hash-table))
	       (vvbm (make-hash-table))
	       (sva (make-hash-table))
	       (svna (make-hash-table))
	       (vals (make-hash-table))
	       (cards)
	       (steps)
	       (type)
	       (concept-id (getf node-def :kb-concept-id))
	       (value (getf node-def :value))
	       (supported-keywords '(:value :kb-concept-id))
	       (node-def-kwds (remove-if-not #'keywordp node-def))
	       (unsupported))
	   (setq unsupported (set-difference node-def-kwds supported-keywords :test #'equal))
	   (when unsupported
	     (error "Unsupported keywords ~{~A~^, ~} in node defintion list." unsupported))
	   (when (null value)
	     (error "No value field found in node definition list.~%Received: ~S" node-def))
	   (cond ((or (equal "PERCEPT-NODE" (symbol-name (car node-def)))
		      (equal "OBSERVATION-NODE" (symbol-name (car node-def)))
		      (equal "RELATION-NODE" (symbol-name (car node-def)))
		      (equal "STATE-NODE" (symbol-name (car node-def))))
		  (cond ((and (symbolp (second node-def))
			      (not (null (second node-def))))
			 (cond ((equal "PERCEPT-NODE" (symbol-name (car node-def)))
				(setf (gethash 0 types-hash) "PERCEPT"))
			       ((equal "OBSERVATION-NODE" (symbol-name (car node-def)))
				(setf (gethash 0 types-hash) "OBSERVATION"))
			       ((equal "RELATION-NODE" (symbol-name (car node-def)))
				(setf (gethash 0 types-hash) "BELIEF"))
			       ((equal "STATE-NODE" (symbol-name (car node-def)))
				(setf (gethash 0 types-hash) "STATE")))
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
			 (setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table))))
			 (setf (gethash 0 sva) (list (list 0)))
			 (setf (gethash 0 svna) (list nil))
			 (setf (gethash 0 vals) (list 0))
			 (setq cards (generate-cpd-cardinalities vvbm))
			 (setq steps (generate-cpd-step-sizes cards))
			 (cond ((or (null concept-id)
				    (stringp concept-id))
				(if concept-id
				    (setf (gethash 0 cids) concept-id)
				    (setf (gethash 0 cids) "NIL"))
				(setf (rule-based-cpd-concept-ids cpd) cids)
				(setf (rule-based-cpd-qualified-vars cpd)
				      (generate-cpd-vars identifiers vars cids))
				(cond ((stringp value)
				       (when (not (string-equal "NA" value))
					 (cond ((equal "PERCEPT-NODE" (symbol-name (car node-def)))
						(setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table))
									     (list (cons value 1) (make-hash-table)))))
					       ((or (equal "OBSERVATION-NODE" (symbol-name (car node-def)))
						    (equal "STATE-NODE" (symbol-name (car node-def))))
						(setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table))
									     (list (cons value 1) (make-hash-table)))))
					       ((equal "RELATION-NODE" (symbol-name (car node-def)))
						(when (not (equal "T" value))
						  (error "Relation node values must be \"T\" or \"NA\". ~S is unsupported." value))
						(setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table))
									     (list (cons "T" 1) (make-hash-table))))))
					 (setf (gethash 0 sva) (list (list 0) (list 1)))
					 (setf (gethash 0 svna) (list (list 1) (list 0)))
					 (setf (gethash 0 vals) (list 0 1))
					 (setq cards (generate-cpd-cardinalities vvbm))
					 (setq steps (generate-cpd-step-sizes cards)))
				       (setf (rule-based-cpd-var-value-block-map cpd) vvbm)
				       (setf (rule-based-cpd-negated-vvbms cpd)
					     (copy-hash-table vvbm))
				       (setf (rule-based-cpd-set-valued-attributes cpd) sva)
				       (setf (rule-based-cpd-set-valued-negated-attributes cpd) svna)
				       (setf (rule-based-cpd-lower-approx-var-value-block-map cpd)
					     (copy-hash-table vvbm))
				       (setf (rule-based-cpd-lower-approx-negated-vvbms cpd)
					     (copy-hash-table vvbm))
				       (setf (rule-based-cpd-characteristic-sets cpd)
					     (make-hash-table))
				       (setf (rule-based-cpd-characteristic-sets-values cpd)
					     (make-hash-table))
				       (setf (rule-based-cpd-var-values cpd) vals)
				       (setf (rule-based-cpd-cardinalities cpd) cards)
				       (setf (rule-based-cpd-step-sizes cpd) steps)
				       cpd)
				      ((numberp value)
				       (error "Numeric value, ~A, not yet supported in node definition list. Expected string." value))
				      (t
				       (error "Unsupported value, ~A, for value in node definition list.~%Received type ~A. Expected string or numeric." value (type-of value)))))
			       (t
				(error "Unsupported value, ~A, for concept id in node definition list.~%Received type ~A. Expected string." concept-id (type-of concept-id)))))
			(t
			 (raise-identifier-type-error (second node-def)))))
		 (t
		  (error "Unsupported type, ~A for node definition list." (car node-def))))))
	(t
	 (raise-identifier-type-error (car node-def)))))
  

(defun raise-identifier-type-error (recieved)
  (error "Unsupported identifier ~A. Expoected type of non-nil symbol." recieved))

(defun directed-edge (cpd1 cpd2)
  (modify-cpd cpd2 cpd1))

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
    l))

(defun add-invariants (neighborhood-func nbr-func-args cpd-arr inv-hash invariants-list)
  (when (null neighborhood-func)
    (setq neighborhood-func #'default-neighborhood))
  (loop
    with var-i and new-body
    for cpd-i being the elements of cpd-arr
    for i from 0
    do
       (setq var-i (caar (nth 1 (gethash 0 (rule-based-cpd-var-value-block-map cpd-i)))))
       (loop
	 with cpd-j and var-j
	 for j in (eval `(funcall ,neighborhood-func ,i ,@nbr-func-args))
	 do
	    (setq cpd-j (aref cpd-arr j))
	    (setq var-j (caar (nth 1 (gethash 0 (rule-based-cpd-var-value-block-map cpd-j)))))
	    (loop
	      with num-j and num-i and invariant-id
	      for invariant in invariants-list
	      do
		 (setq invariant-id (gensym))
		 (with-input-from-string (stream-j var-j)
		   (with-input-from-string (stream-i var-i)
		     (setq num-j (read stream-j))
		     (setq num-i (read stream-i))
		     (when (funcall invariant num-i num-j)
		       (setq new-body (concatenate 'list new-body `(,invariant-id = (relation-node ,invariant :value "T"))))
		       (setq new-body (concatenate 'list new-body `(,invariant-id -> ,(gethash (rule-based-cpd-dependent-id cpd-i) inv-hash))))
		       (setq new-body (concatenate 'list new-body `(,invariant-id -> ,(gethash (rule-based-cpd-dependent-id cpd-j) inv-hash))))
		       (setq new-body (concatenate 'list new-body `(,(gethash (rule-based-cpd-dependent-id cpd-i) inv-hash) -> ,(gethash (rule-based-cpd-dependent-id cpd-j) inv-hash)))))))))
    finally
       (format t "~%new body:~%~S" new-body)
       (return new-body)))

#| Compiles a hems program into a Bayesian Network. Returns a cons where the first element is an array of factors, and the second element is a nested hash-table of edges |#

;; body = HEMS program
;; relational-invariants = Flag for whether to augment the state with relational comparators that are true.
;; neighborhood-func = function that returns the indeces of the neighbors of the given variable index
;; nbr-func-args = a list of arguments for neighborhood function
(defmacro compile-program ((&key relational-invariants neighborhood-func nbr-func-args) &body body)
  (let ((hash (gensym))
	(args (gensym))
	(inv-hash (gensym))
        (ident (gensym))
	(cpd (gensym))
	(cpd-list (gensym))
	(cpd-arr (gensym))
	(factors (gensym))
	(edges (gensym))
	(recurse-p (gensym))
	(invariant-list (gensym))
	(new-body (gensym)))
    (cond ((and neighborhood-func
		(not (eq t relational-invariants)))
	   (error "Relational Invariants flag must be set to t to define the neighborhood function: ~A."))
	  ((and (not (eq t relational-invariants))
		(not (eq nil relational-invariants)))
	   (error "Relational Invariants flag is boolean. Must be set to t or nil. Value~%~A~% is invalid" relational-invariants))
	  ((and neighborhood-func relational-invariants (not (function-p neighborhood-func)))
	   (error "Invalid neighborhood function. Received~%~A~%Expected a function." neighborhood-func))
	  ((and (eq nil relational-invariants) nbr-func-args)
	   (error "Cannot supply arguments~%~A~%when relational-invariants is nil." nbr-func-args)))
    `(labels ((compile-hems-program (,hash ,args ,invariant-list ,recurse-p)
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
					 (error "Expected assignment to list in statement ~{~A~^ ~}." (subseq ,args 0 3)))))
				 ((equal (symbol-name '->) (symbol-name (second ,args)))
				  (when (not (gethash (first ,args) ,hash))
				    (error "Reference to ~A before assignment in statement ~{~A~^ ~}." (first ,args) (subseq ,args 0 3)))
				  (cond ((and (symbolp (third ,args))
					      (not (null (third ,args))))
					 (when (not  (gethash (third ,args) ,hash))
					   (error "Reference to ~A before assignment in statement ~{~A~^ ~}." (third ,args) (subseq ,args 0 3)))
					 (directed-edge (gethash (first ,args) ,hash)
							(gethash (third ,args) ,hash)))
					(t
					 (raise-identifier-type-error (third ,args)))))
				 (t
				  (error "Unrecognized operator in statement ~{~A~^ ~}.~%Received ~A.~%Expected assignment or directed edge." (subseq ,args 0 3) (second ,args))))
			   (raise-identifier-type-error (first ,args))
		        
			   )
		       (compile-hems-program ,hash (nthcdr 3 ,args) ,invariant-list ,recurse-p))
		      (t
		       (let (,inv-hash)
			 (when (and ,relational-invariants ,recurse-p)
			   (setq ,inv-hash (make-hash-table :test #'equal)))
			 (loop
			   with ,factors and ,edges and ,cpd-arr and ,new-body and ,invariant-list = '(< > =)
			   for ,ident being the hash-keys of ,hash
			     using (hash-value ,cpd)
			   collect ,cpd into ,cpd-list
			   when (and ,relational-invariants ,recurse-p)
			     do
				(setf (gethash (rule-based-cpd-dependent-id ,cpd) ,inv-hash) ,ident)
			   finally
			      (setq ,cpd-list (topological-sort ,cpd-list))
			      (when (and ,relational-invariants ,recurse-p)
				(setq ,cpd-arr (make-array (hash-table-count ,hash) :initial-contents ,cpd-list))
				(setq ,new-body (add-invariants ,neighborhood-func ',nbr-func-args ,cpd-arr ,inv-hash ,invariant-list))
				(return-from compile-hems-program (compile-hems-program ,hash ,new-body ,invariant-list nil)))
			      (setq ,factors (make-array (hash-table-count ,hash)
							 :initial-contents (finalize-factors ,cpd-list)))
			      (setq ,edges (make-graph-edges ,factors))
			      (return (cons ,factors ,edges))))))))
       (compile-hems-program (make-hash-table :test #'equal) ',body ',invariant-list t))))

(defun compile-program-from-file (prog-file)
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
	 (return (eval `(compile-program ,@prog))))))
  
