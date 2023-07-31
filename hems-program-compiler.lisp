(defun make-bn-node (node-def) t)

(defun directed-edge (cpd1 cpd2))

(defmacro compile-program (&body body)
  (let ((identifiers-hash (gensym))
	(hash (gensym))
	(args (gensym))
	(ident (gensym))
	(i (gensym))
	(cpd (gensym))
	(cpd-list (gensym))
	(new-args (gensym)))
    (format t "~%identifiers-hash: ~A~%hash: ~A~%args: ~S~%ident: ~A~%i: ~A~%cpd: ~A~%cpd-list: ~A~%" identifiers-hash hash args ident i cpd cpd-list)
    `(labels ((compile-hems-program (,hash ,args)
		(cond (,args
		       (if (and (symbolp (first ,args))
				(not (null (first ,args))))
			   (cond ((equal '= (second ,args))
				  (cond ((listp (third ,args))
					 (when (gethash (first ,args) ,hash)
					   (warn "Attempting to overwrite value for identifyer ~A." (first ,args)))
					 (setf (gethash (first ,args) ,hash)
					       (make-bn-node (quote (third ,args)))))
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
					 (error "Unsupported identifier type ~A. Expected non-nil symbol." (type-of (third ,args)))))
				  (when (not (gethash (third ,args) ,hash))))
				 (t
				  (error "Unrecognized operator. Received ~A.~%Expected assignment or directed edge." (second ,args))))
			   (error "Unsupported identifier type ~A. Expoected non-nil symbol." (type-of (first ,args))))
		       (setq ,new-args (nthcdr 3 ,args))
		       (compile-hems-program ,hash ,new-args))
		      (t
		       (loop
			 for ,ident being the hash-keys of ,hash
			   using (hash-value ,cpd)
			 collect ,cpd into ,cpd-list
			 finally
			    (return (make-array (hash-table-count ,hash) :initial-contents ,cpd-list)))))))
       (setq ,identifiers-hash (make-hash-table :test #'equal))
       (compile-hems-program ,identifiers-hash ',body)
       )))
