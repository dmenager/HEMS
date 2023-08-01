(in-package :hems)
;;(load "graph-representation")

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
	     (error "No value field found in node definition list."))
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
      for cpd being the elements of copy
      when (= (hash-table-count (rule-based-cpd-identifiers cpd)) 1)
	do
	   (setq s (cons cpd s)))
    (loop
      while s
      do
	 (setq l (reverse (cons (car s) (reverse l))))
	 (setq s (rest s))
	 (loop
	   for cpd being the elements of copy
	   for cpd1 in factors-list
	   when (gethash (rule-based-cpd-dependent-id (car l))
			 (rule-based-cpd-identifiers cpd))
	     do
		(remhash (rule-based-cpd-dependent-id (car l))
			 (rule-based-cpd-identifiers cpd))
		(when (= (hash-table-count (rule-based-cpd-identifiers cpd)) 1)
		  (setq l (reverse (cons cpd1 (reverse l)))))))
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

(defun test-compiler ()
  (let (bn1 bn2)
    (setq bn1 (compile-program
	       c1 = (percept-node casualty :value "CASUALTY-A" :kb-concept-id "CNPT-1")
	       c2 = (percept-node age :value "22" :kb-concept-id "CNPT-2")
	       c3 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")
	       c4 = (percept-node rank :value "MILITARY" :kb-concept-id "CNPT-4")
	       c5 = (percept-node hrpmin :value "145" :kb-concept-id "CNPT-5")
	       c6 = (percept-node mmHG :value "60" :kb-concept-id "CNPT-6")
	       c7 = (percept-node Spo2 :value "85" :kb-concept-id "CNPT-7")
	       c8 = (percept-node RR :value "40" :kb-concept-id "CNPT-8")
	       c9 = (percept-node pain :value "0" :kb-concept-id "CNPT-9")
	       c1 -> c2
	       c1 -> c3
	       c1 -> c4
	       c1 -> c5
	       c1 -> c6
	       c1 -> c7
	       c1 -> c8
		c1 -> c9))
    (setq bn2 (compile-program
	       c10 = (percept-node casualty :value "CASUALTY-B" :kb-concept-id "CNPT-1")
	       c11 = (percept-node age :value "25" :kb-concept-id "CNPT-2")
	       c12 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")
	       c13 = (percept-node rank :value "MILITARY" :kb-concept-id "CNPT-4")
	       c14 = (percept-node hrpmin :value "120" :kb-concept-id "CNPT-5")
	       c15 = (percept-node mmHG :value "80" :kb-concept-id "CNPT-6")
	       c16 = (percept-node Spo2 :value "98" :kb-concept-id "CNPT-7")
	       c17 = (percept-node RR :value "18" :kb-concept-id "CNPT-8")
	       c18 = (percept-node pain :value "6" :kb-concept-id "CNPT-9")
	       c10 -> c11
	       c10 -> c12
	       c10 -> c13
	       c10 -> c14
	       c10 -> c15
	       c10 -> c16
	       c10 -> c17
		c10 -> c18))
    #|
	       c19 = (percept-node casualty :value "CASUALTY-D" :kb-concept-id "CNPT-1")
	       c20 = (percept-node age :value "40" :kb-concept-id "CNPT-2")
	       c21 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")
	       c22 = (percept-node rank :value "VIP" :kb-concept-id "CNPT-4")
	       c23 = (percept-node hrpmin :value "105" :kb-concept-id "CNPT-5")
	       c24 = (percept-node mmHG :value "120" :kb-concept-id "CNPT-6")
	       c25 = (percept-node Spo2 :value "99" :kb-concept-id "CNPT-7")
	       c26 = (percept-node RR :value "15" :kb-concept-id "CNPT-8")
	       c27 = (percept-node pain :value "2" :kb-concept-id "CNPT-9")
	       c19 -> c20
	       c19 -> c21
	       c19 -> c22
	       c19 -> c23
	       c19 -> c24
	       c19 -> c25
	       c19 -> c26
	       c19 -> c27

	       c28 = (percept-node casualty :value "casualty-E" :kb-concept-id "CNPT-1")
	       c29 = (percept-node age :value "26" :kb-concept-id "CNPT-2")
	       c30 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")
	       c31 = (percept-node rank :value "Military" :kb-concept-id "CNPT-4")
	       c32 = (percept-node hrpmin :value "120" :kb-concept-id "CNPT-5")
	       c33 = (percept-node mmHG :value "100" :kb-concept-id "CNPT-6")
	       c34 = (percept-node Spo2 :value "95" :kb-concept-id "CNPT-7")
	       c35 = (percept-node RR :value "15" :kb-concept-id "CNPT-8")
	       c36 = (percept-node pain :value "10" :kb-concept-id "CNPT-9")
	       c28 -> c29
	       c28 -> c30
	       c28 -> c31
	       c28 -> c32
	       c28 -> c33
	       c28 -> c34
	       c28 -> c35
	       c28 -> c36

	       c37 = (percept-node casualty :value "CASUALTY-F" :kb-concept-id "CNPT-1")
	       c38 = (percept-node age :value "12" :kb-concept-id "CNPT-2")
	       c39 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")
	       c40 = (percept-node rank :value "CIVILIAN" :kb-concept-id "CNPT-4")
	       c41 = (percept-node hrpmin :value "120" :kb-concept-id "CNPT-5")
	       c42 = (percept-node mmHG :value "30" :kb-concept-id "CNPT-6")
	       c43 = (percept-node Spo2 :value "99" :kb-concept-id "CNPT-7")
	       c44 = (percept-node RR :value "25" :kb-concept-id "CNPT-8")
	       c45 = (percept-node pain :value "3" :kb-concept-id "CNPT-9")
	       c37 -> c38
	       c37 -> c39
	       c37 -> c40
	       c37 -> c41
	       c37 -> c42
	       c37 -> c43
	       c37 -> c44
	       c37 -> c45
|#
    (mapcar #'(lambda (bn)
		(format t "~%bn:~%~S" bn)
		(push-to-ep-buffer :state bn :insert-episode-p t)
		(eltm-to-pdf)
		(break))
	    (list bn1 bn2))
    ;;(eltm-to-pdf)
    ))
  
