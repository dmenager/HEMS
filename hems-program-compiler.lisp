(in-package :hems)

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
		      (equal "RELATION-NODE" (symbol-name (car node-def))))
		  (cond ((and (symbolp (second node-def))
			      (not (null (second node-def))))
			 (if (equal "PERCEPT-NODE" (symbol-name (car node-def)))
			     (setf (gethash 0 types-hash) "PERCEPT")
			     (setf (gethash 0 types-hash) "BELIEF"))
			 (setq type (symbol-name (second node-def)))
			 (setf (rule-based-cpd-dependent-var cpd) type)
			 (setf (gethash 0 vars) type)
			 (setf (rule-based-cpd-vars cpd) vars)
			 (setq type-identifier (symbol-name (gensym type)))
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

(defmacro compile-program (&body body)
  (let ((hash (gensym))
	(args (gensym))
	(ident (gensym))
	(cpd (gensym))
	(cpd-list (gensym))
	(factors (gensym))
	(edges (gensym)))
    `(labels ((compile-hems-program (,hash ,args)
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
		       (compile-hems-program ,hash (nthcdr 3 ,args)))
		      (t
		       (loop
			 with ,factors and ,edges
			 for ,ident being the hash-keys of ,hash
			   using (hash-value ,cpd)
			 collect ,cpd into ,cpd-list
			 finally
			    (setq ,factors (make-array (hash-table-count ,hash)
						       :initial-contents (finalize-factors (topological-sort ,cpd-list))))
			    (setq ,edges (make-graph-edges ,factors))
			    (return (cons ,factors ,edges)))))))
       (compile-hems-program (make-hash-table :test #'equal) ',body))))


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
  
