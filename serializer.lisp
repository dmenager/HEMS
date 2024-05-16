(in-package :hems)

#| Load event memory contents from file |#

;; filename = string handle to file to load
(defun load-eltm-from-file (file-name)
  (format t "~%loading event memory...")
  (with-open-file (eltm-stream file-name 
                               :direction :input
                               :if-does-not-exist nil)
    (if eltm-stream
        (setq eltm* (read eltm-stream))
        (setq eltm* nil))
    (format t "~%done!")))

(defun a-list-to-hash (a-list)
  (if (listp a-list)
      (loop
	with hash-table =
			(if (stringp (caar a-list))
			    (make-hash-table :test #'equal)
			    (make-hash-table))
	for (att . val) in a-list
	if (listp val)
	  do
	     (setf (gethash att hash-table) (a-list-to-hash val))
	else
	  do
	     (setf (gethash att hash-table) val)
	finally
	   (return hash-table))
      a-list))

(defun hash-to-a-list (hash)
  (if (hash-table-p hash)
      (loop
	for key being the hash-keys of hash
	  using (hash-value val)
	if (hash-table-p val)
	  collect (cons key (hash-to-a-list val)) into a-list
	else
	  collect (cons key val) into a-list
	finally
	   (return a-list))
      hash))

(defun hash-rule (rule)
  (setf (rule-conditions rule)
	(a-list-to-hash (rule-conditions rule)))
  (setf (rule-block rule)
	(a-list-to-hash (rule-block rule)))
  (if (listp (rule-certain-block rule))
      (setf (rule-certain-block rule)
	    (a-list-to-hash (rule-certain-block rule))))
  (if (listp (rule-avoid-list rule))
      (setf (rule-avoid-list rule)
	    (a-list-to-hash (rule-avoid-list rule))))
  (if (listp (rule-redundancies rule))
      (setf (rule-redundancies rule)
	    (a-list-to-hash (rule-redundancies rule)))))

(defun unhash-rule (rule)
  (setf (rule-conditions rule)
	(hash-to-a-list (rule-conditions rule)))
  (setf (rule-block rule)
	(hash-to-a-list (rule-block rule)))
  (if (hash-table-p (rule-certain-block rule))
      (setf (rule-certain-block rule)
	    (hash-to-a-list (rule-certain-block rule))))
  (if (hash-table-p (rule-avoid-list rule))
      (setf (rule-avoid-list rule)
	    (hash-to-a-list (rule-avoid-list rule))))
  (if (hash-table-p (rule-redundancies rule))
      (setf (rule-redundancies rule)
	    (hash-to-a-list (rule-redundancies rule)))))

(defun nhash-var-values ())
(defun nunhash-var-values ())

(defun vvbm-to-hash (a-list)
  (if (listp a-list)
      (loop
	with hash-table = (make-hash-table)
	for (idx vvbm) in a-list
	do
	   (setf (gethash idx hash-table)
		 (loop
		   for vvb in vvbm
		   collect (list (car vvb)
				 (a-list-to-hash (second vvb)))
		     into vvb-list
		   finally
		      (return vvb-list)))
	finally
	   (return hash-table))
      a-list))

(defun vvbm-to-a-list (hash)
  (if (hash-table-p hash)
      (loop
	for idx being the hash-keys of hash
	  using (hash-value vvbm)
	collect
	(cons idx (loop
		    for vvb in vvbm
		    collect (list (car vvb) (hash-to-a-list (second vvb))) into vvb-list
		    finally
		       (return (list vvb-list))))
	  into vvbm-list
	finally
	   (return vvbm-list))
      hash))

(defun nhash-cpds (bn)
  (loop
    with cpd-arr = (car bn)
    for cpd being the elements of cpd-arr
    do
       (setf (rule-based-cpd-identifiers cpd)
	     (a-list-to-hash (rule-based-cpd-identifiers cpd)))
       (setf (rule-based-cpd-vars cpd)
	     (a-list-to-hash (rule-based-cpd-vars cpd)))
       (setf (rule-based-cpd-types cpd)
	     (a-list-to-hash (rule-based-cpd-types cpd)))
       (setf (rule-based-cpd-concept-ids cpd)
	     (a-list-to-hash (rule-based-cpd-concept-ids cpd)))
       (setf (rule-based-cpd-qualified-vars cpd)
	     (a-list-to-hash (rule-based-cpd-qualified-vars cpd)))
       (setf (rule-based-cpd-var-value-block-map cpd)
	     (vvbm-to-hash (rule-based-cpd-var-value-block-map cpd)))
       (setf (rule-based-cpd-set-valued-attributes cpd)
	     (vvbm-to-hash (rule-based-cpd-set-valued-attributes cpd)))
       (setf (rule-based-cpd-set-valued-negated-attributes cpd)
	     (vvbm-to-hash (rule-based-cpd-set-valued-negated-attributes cpd)))
       (setf (rule-based-cpd-negated-vvbms cpd)
	     (vvbm-to-hash (rule-based-cpd-negated-vvbms cpd)))
       (setf (rule-based-cpd-lower-approx-var-value-block-map cpd)
	     (vvbm-to-hash (rule-based-cpd-lower-approx-var-value-block-map cpd)))
       (setf (rule-based-cpd-lower-approx-negated-vvbms cpd)
	     (vvbm-to-hash (rule-based-cpd-lower-approx-negated-vvbms cpd)))
       (setf (rule-based-cpd-characteristic-sets cpd)
	     (vvbm-to-hash (rule-based-cpd-characteristic-sets cpd)))
       (setf (rule-based-cpd-characteristic-sets-values cpd)
	     (a-list-to-hash (rule-based-cpd-characteristic-sets-values cpd)))
       (setf (rule-based-cpd-var-values cpd)
	     (a-list-to-hash (rule-based-cpd-var-values cpd)))
       (loop
	 for rule being the elements of (rule-based-cpd-rules cpd)
	 do
	    (unhash-rule rule))
       (setf (rule-based-cpd-concept-blocks cpd)
	     (a-list-to-hash (rule-based-cpd-concept-blocks cpd))))
  (setf (cdr bn)
	(a-list-to-hash (cdr bn))))

(defun nunhash-cpds (bn)
  (loop
    with cpd-arr = (car bn)
    for cpd being the elements of cpd-arr
    do
       (setf (rule-based-cpd-identifiers cpd)
	     (hash-to-a-list (rule-based-cpd-identifiers cpd)))
       (setf (rule-based-cpd-vars cpd)
	     (hash-to-a-list (rule-based-cpd-vars cpd)))
       (setf (rule-based-cpd-types cpd)
	     (hash-to-a-list (rule-based-cpd-types cpd)))
       (setf (rule-based-cpd-concept-ids cpd)
	     (hash-to-a-list (rule-based-cpd-concept-ids cpd)))
       (setf (rule-based-cpd-qualified-vars cpd)
	     (hash-to-a-list (rule-based-cpd-qualified-vars cpd)))
       (setf (rule-based-cpd-var-value-block-map cpd)
	     (vvbm-to-a-list (rule-based-cpd-var-value-block-map cpd)))
       (setf (rule-based-cpd-set-valued-attributes cpd)
	     (vvbm-to-a-list (rule-based-cpd-set-valued-attributes cpd)))
       (setf (rule-based-cpd-set-valued-negated-attributes cpd)
	     (vvbm-to-a-list (rule-based-cpd-set-valued-negated-attributes cpd)))
       (setf (rule-based-cpd-negated-vvbms cpd)
	     (vvbm-to-a-list (rule-based-cpd-negated-vvbms cpd)))
       (setf (rule-based-cpd-lower-approx-var-value-block-map cpd)
	     (vvbm-to-a-list (rule-based-cpd-lower-approx-var-value-block-map cpd)))
       (setf (rule-based-cpd-lower-approx-negated-vvbms cpd)
	     (vvbm-to-a-list (rule-based-cpd-lower-approx-negated-vvbms cpd)))
       (setf (rule-based-cpd-characteristic-sets cpd)
	     (vvbm-to-a-list (rule-based-cpd-characteristic-sets cpd)))
       (setf (rule-based-cpd-characteristic-sets-values cpd)
	     (hash-to-a-list (rule-based-cpd-characteristic-sets-values cpd)))
       (setf (rule-based-cpd-var-values cpd)
	     (hash-to-a-list (rule-based-cpd-var-values cpd)))
       (loop
	 for rule being the elements of (rule-based-cpd-rules cpd)
	 do
	    (unhash-rule rule))
       (setf (rule-based-cpd-concept-blocks cpd)
	     (hash-to-a-list (rule-based-cpd-concept-blocks cpd))))
  (setf (cdr bn)
	(hash-to-a-list (cdr bn))))

(defun hash-eltm (eltm)
  (loop
    with branch and episode and visited
    with stack = (list eltm)
    while stack
    do
       (setq branch (car stack))
       (setq episode (car branch))
       (setq stack (rest stack))
       (setf (episode-backlinks episode) (a-list-to-hash (episode-backlinks episode)))
       ;;(format t "~%visiting: ~S" (episode-id episode))
       (loop
	 for slot in (list 'episode-observation 'episode-state 'episode-state-transitions)
	 do
	    (nhash-cpds (funcall slot episode)))
       ;;(format t "~%episode:~%~S"episode)
       (setq visited (cons (episode-id episode) visited))
       (loop
	 for child in (rest branch)
	 when (not (member (episode-id (car child)) visited
			   :test #'equal))
	   do
	      (setq stack (cons child stack))))
  eltm)

(defun unhash-eltm (eltm)
  (loop
    with branch and episode and visited
    with stack = (list eltm)
    while stack
    do
       (setq branch (car stack))
       (setq episode (car branch))
       (setq stack (rest stack))
       (setf (episode-backlinks episode) (hash-to-a-list (episode-backlinks episode)))
       ;;(format t "~%visiting: ~S" (episode-id episode))
       (loop
	 for slot in (list 'episode-observation 'episode-state 'episode-state-transitions)
	 do
	    (nunhash-cpds (funcall slot episode)))
       ;;(format t "~%episode:~%~S"episode)
       (setq visited (cons (episode-id episode) visited))
       (loop
	 for child in (rest branch)
	 when (not (member (episode-id (car child)) visited
			   :test #'equal))
	   do
	      (setq stack (cons child stack))))
  eltm)

#| Save contents of event memory to file which can be read later |#

;; eltm = episodic long-term memory
;; file-name = file to contain episodic memory
(defun save-eltm-to-file (eltm &key (path "./") (filename "eltm.txt"))
  (setq filename (concatenate 'string path filename))
  (log-message (list "~S" (unhash-eltm eltm)) filename :if-exists :supersede))

(defun read-from-file (fname)
  (with-open-file (s fname)
    (hash-eltm (read s))))
