(load "graph-representation")

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
	   (Format t "~%node definition:~%~S" node-def)
	   (setq unsupported (set-difference node-def-kwds supported-keywords :test #'equal))
	   (when unsupported
	     (error "Unsupported keywords ~{~A~^, ~} in node defintion list." unsupported))
	   (when (null value)
	     (error "No value found in node definition list."))
	   (cond ((or (equal "PERCEPT-NODE" (symbol-name (car node-def)))
		      (equal "RELATION-NODE" (symbol-name (car node-def))))
		  (cond ((and (symbolp (second node-def))
			      (not (null (second node-def))))
			 (if (equal "PERCEPT-NODE" (symbol-name (car node-def)))
			     (setf (gethash 0 types-hash) "PERCEPT")
			     (setf (gethash 0 types-hash) "BELIEF"))
			 (setq type (second node-def))
			 (setf (rule-based-cpd-dependent-var cpd) type)
			 (setf (gethash 0 vars) type)
			 (setf (rule-based-cpd-vars cpd) vars)
			 (setq type-identifier (symbol-name (gensym (symbol-name type))))
			 (setf (rule-based-cpd-dependent-id cpd) type-identifier)
			 (setf (gethash type-identifier identifiers) 0)
			 (setf (rule-based-cpd-identifiers cpd) identifiers)
			 (setf (rule-based-cpd-types cpd) types-hash)
			 (cond ((or (null concept-id)
				    (stringp concept-id))
				(if concept-id
				    (setf (gethash 0 cids) concept-id)
				    (setf (gethash 0 cids) "NIL"))
				(setf (rule-based-cpd-concept-ids cpd) cids)
				(setf (rule-based-cpd-qualified-vars cpd)
				      (generate-cpd-vars identifiers vars cids))
				(cond ((stringp value)
				       cpd)
				      (t
				       (error "Unsupported value, ~A, for value in node definition list.~%Received type ~A. Expected string." value (type-of value)))))
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

(defun directed-edge (cpd1 cpd2))

(defmacro compile-program (&body body)
  (let ((hash (gensym))
	(args (gensym))
	(ident (gensym))
	(cpd (gensym))
	(cpd-list (gensym))
	)
    `(labels ((compile-hems-program (,hash ,args)
		(cond (,args
		       (if (and (symbolp (first ,args))
				(not (null (first ,args))))
			   (cond ((equal '= (second ,args))
				  (cond ((listp (third ,args))
					 (when (gethash (first ,args) ,hash)
					   (warn "Attempting to overwrite value for identifyer ~A." (first ,args)))
					 (setf (gethash (first ,args) ,hash)
					       (make-bn-node (third ,args))))
					(t
					 (error "Expected assignment to list.~%Received: ~{~A~}." (subseq ,args 0 3)))))
				 ((equal '-> (second ,args))
				  (when (not (gethash (first ,args) ,hash))
				    (error "Reference to ~A before assignment." (first ,args)))
				  (cond ((and (symbolp (third ,args))
					      (not (null (third ,args))))
					 (when (not  (gethash (third ,args) ,hash))
					   (error "Reference to ~A before assignment." (third ,args)))
					 (directed-edge (gethash (first ,args) ,hash)
							(gethash (third ,args) ,hash)))
					(t
					 (raise-identifier-type-error (quote (third ,args)))
					 ;;(error "Unsupported identifier type ~A. Expected non-nil symbol." (type-of (third ,args)))
					 ))
				  (when (not (gethash (third ,args) ,hash))))
				 (t
				  (error "Unrecognized operator. Received ~A.~%Expected assignment or directed edge." (second ,args))))
			   (raise-identifier-type-error (quote (first ,args)))
		        
			   )
		       (compile-hems-program ,hash (nthcdr 3 ,args)))
		      (t
		       (loop
			 for ,ident being the hash-keys of ,hash
			   using (hash-value ,cpd)
			 collect ,cpd into ,cpd-list
			 finally
			    (return (make-array (hash-table-count ,hash) :initial-contents ,cpd-list)))))))
       (compile-hems-program (make-hash-table :test #'equal) ',body))))
