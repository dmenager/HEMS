(ql:quickload :alexandria)
(defparameter printer-special nil)
(defparameter calls-to-cost* 0)
;;(setf *print-circle* nil)

;; dependent-id = dependent variable identifier of CPD
;; indentifiers = hash table of instance number for each var. If A is dependent var, and B C are free parameters (A B C) is P(A | B C)
;; dependent-var = dependent variable of CPD
;; vars = hash table of variables in the conditional probability density.
;; types = hash table of variables taking values of percept, belief, action, intention, or goal
;; concept-ids = hash table of concept ids for vars
;; qualified-vars = hash table of fully qualified variables in conditional probability density
;; var-value-map = hash table  of bindings from variable value to a number
;; cardinalities = array of cardinalities for each variable
;; step-sizes = array of number of steps to take before reaching variable's next assignment, for each variable
;; assignments = hash table of all possible variable assignments
;; counts = hash table of counts for assignments
;; count = total number of times this CPD was observed
;; lvl = height of CPD in bayesian network
(defstruct table-cpd
  dependent-id
  identifiers
  dependent-var
  vars
  types
  concept-ids
  qualified-vars
  var-value-map
  cardinalities
  step-sizes
  assignments
  counts
  count
  (lvl 1))

;; id = unique identifier for rule
;; condiitons = container for rule left-hand side.
;; probability = probability associated with left-hand side. This is the rule right-hand side
;; block = block denoting cases covered by rule
;; certain-block = block denoting cases always covered by rule
;; avoid-list = cases covered by the rule block that are outside of the concept block
;; redundancies = cases that are in the rule block that are already covered by the rule set
;; count = total number of times this rule was observed
(defstruct rule id conditions probability block certain-block avoid-list redundancies count)

;; dependent-id = dependent variable identifier of CPD
;; indentifiers = hash table of instance number for each var. If A is dependent var, and B C are free parameters (A B C) is P(A | B C)
;; dependent-var = dependent variable of CPD
;; vars = hash table of variables in the conditional probability density. 
;; types = hash table of variables taking values of percept, belief, action, intention, or goal
;; concept-ids = hash table of concept ids for vars
;; qualified-vars = hash table of fully qualified variables in conditional probability density
;; var-value-block map = hash table of bindings from variable value to a number and a block denoting cases where var is true
;; negated-vvbms = hash table of bindings from variable value to a negated number and a block denoting cases where var is true
;; set-valued-attributes = hash table of associations that restore attribute-value pairs to their set-valued equivalents
;; lower-approx-var-value-block = hash table of lower approximations for var-value-block-map
;; lower-approx-negated-vvbms = hash table of lower approximations for negated-vvbms
;; characteristic-sets = hash table of local characteristic sets for attributes in rules
;; characteristic-sets-values = hash table of variable values for each characteristic set
;; var-values = hash table of values for each variable in CPD
;; cardinalities = array of cardinalities for each variable
;; step-sizes = array of number of steps to take before reaching variable's next assignment, for each variable
;; rules = list of rules = <context; probability, count>
;; concept-blocks = rule probability-block map. For each probability in rules, associate it with the rules with the same probability
;; singleton-p = flag for whether this is a singleton cpd or not. If so, then rules represent potential fields, not probability
;; count = total number of times this CPD was observed
;; lvl = height of CPD in bayesian network
(defstruct rule-based-cpd
  dependent-id
  identifiers
  dependent-var
  vars
  types
  concept-ids
  qualified-vars
  var-value-block-map
  negated-vvbms
  set-valued-attributes
  set-valued-negated-attributes
  lower-approx-var-value-block-map
  lower-approx-negated-vvbms
  characteristic-sets
  characteristic-sets-values
  var-values
  cardinalities
  step-sizes
  rules
  concept-blocks
  (singleton-p nil)
  count
  (lvl 1))

#| Custom printer for hash tables |#

;; object = hash table
;; stream = output stream
(defmethod print-object ((object hash-table) stream)
  (format stream "#H(~{~{(~S . ~S)~}~^ ~})"
          (loop for key being the hash-keys of object
                  using (hash-value value)
                collect (list key value))))

(defun hash-reader (stream char n)
  (declare (ignore char n))
  (let* ((body (read stream t nil t)))
    ;;(append (list '\#H) body)
    ;;(format t "#H~A"body)
    (loop
      with hash = (make-hash-table :test #'equal)
      for (key . val) in body do
        (setf (gethash key hash) val)
      finally (return hash))))

(set-dispatch-macro-character #\# #\H #'hash-reader)

(defun mean (x)
  (/ (reduce #'+ x)
     (length x)))

#| Get standard deviation of a list |#

;; x = list
(defun stdev (x)
  (let ((avg (mean x)))
  (sqrt (/ (apply '+ (mapcar #'(lambda (xi) (expt (- xi avg) 2) ) x))
           (length x)))))



#| Access arbitrarily nested hash table |#

;; hash = hash table
;; default = return value if index not found in hash
;; cpdp = generalized boolean. Either a CPD is provided or nil. Indicates if hash contains cpd probabilities or not
;; indexes = list of hash table indexes
;; sparsep = flag for if we are access sparse hash table
(defun hash-access (hash default cpdp indexes &optional (sparsep t))
  (loop
    with  hash-table-value = hash
    for idx in indexes
    do
       (multiple-value-bind (val bool)
           (gethash idx hash-table-value)
         (when (and cpdp (cpd-counts cpdp) sparsep)
           (let (na-posp rest-prob row-length)
             (setq row-length (aref (cpd-cardinalities cpdp) 0))
             (setq na-posp (= (mod idx row-length) 0))
             (when na-posp
               ;; get the probabilities in the rest of row
               (setq rest-prob (reduce #'+ (gethash-range default (+ idx 1) (+ idx row-length) hash-table-value cpdp)))
               ;; set na-prob to be the difference 1 and the rest of the rows
               (setq val (- 1 rest-prob))
               (setq bool t))))
         (cond (bool (setq hash-table-value val))
               (t (return-from hash-access default))))
     finally
       (return hash-table-value)))

#| Get the count associated with index in cpd |#

;; counts-hash = hash table with counts information
;; i = index of interest
;; row-length = length of row in cpd
(defun access-counts (counts-hash i row-length)
  (let (count)
    (setq count (gethash (* (floor (/ i row-length)) row-length) counts-hash))
    (if count count 0)))

#| Retrieve a range of values from hash table |#

;; default = return value if index not found in hash
;; start = lower bound of range
;; end = exclusive upper bound of range
;; cpdp = generalized boolean. Either a CPD is provided or nil
(defun gethash-range (default start end hash cpdp)
  (loop
     for i from start
     while (< i end)
     collect (hash-access hash default cpdp (list i)) into range
     finally (return range)))

#| Normalize a hash table |#

;; num-assignments = number of assignments in cpd
;; hash = hash table to normalize
;; counts = counts hash table in cpd
;; cpdp = generalized boolean. Either a CPD is provided or nil
(defun normalize-hash-table (hash counts cpdp &optional (op #'/))
  (loop
     ;;with new-hash = (make-hash-table)
     for i being the hash-keys of counts
       using (hash-value count)
     with val and new-assn
     do
       (loop
         for count-idx from i to (+ i (- (aref (cpd-cardinalities cpdp) 0) 1))
         do
            (setq val (hash-access hash 0 cpdp (list count-idx)))
            (setq new-assn (funcall op val count))
            (when (and (> new-assn 1) (eq op #'/))
              (error "Normalization error. New probability is greater than 1!~%Unnormalized assignments:~%~A~%counts hash for normalizing:~%~A~%index: ~d~%retrieved probability: ~d~%retrieved count: ~d~%new probability: ~d~%operator: ~A" hash counts count-idx val count new-assn op))
            (if (> new-assn 0)
                (setf (gethash count-idx hash) new-assn)
                (remhash count-idx hash)))
     finally (return hash)))

#| Make a hash table with keys initilized to value |#

;; num-assignments = number of keys in hash table
;; value = value to initialize hash table with
(defun initialize-hash-table (num-assignments value)
  (loop
     with hash = (make-hash-table)
     for i from 0 to (- num-assignments 1)
     do
       (setf (gethash i hash) value)
     finally
       (return hash)))
#| Make a ruleset whose probabilities are initilized to value |#

;; factor = conditional probability distribution
;; value = value to initialize ruleset with
(defun initialize-rule-potentials (factor value)
  (loop
    with num-rules = (aref (rule-based-cpd-cardinalities factor) 0)
    with rule
    for i in (gethash 0 (rule-based-cpd-var-values factor)) ;;from 0 to (- num-rules 1)
    do
       (setq rule (make-rule :id (gensym "RULE-")
                             :conditions (make-hash-table :test #'equal)
                             :probability (float value)
                             :block (make-hash-table)
                             :certain-block (make-hash-table)
                             :count nil))
       (setf (gethash (rule-based-cpd-dependent-id factor) (rule-conditions rule)) i)
    collect rule into rules
    finally (return (make-array (length rules) :initial-contents rules))))

#| Substitute bindings in hash table keys

;; hash = hash table
;; bindings = variable bindings for keys hash table
;; var-val-mappings = map for consistently swapping the cdr of a binding
(defun swap-hash-keys (hash bindings &key (var-val-mappings))
  (loop
    with new-hash
    for id being the hash-keys of hash
      using (hash-value pos)
    do
       (multiple-value-bind (new-ident present-p)
           (gethash id bindings)
         (when (and present-p (not (equal id new-ident)))
           (remhash id hash)
	   (if var-val-mappings
	       (setf (gethash new-ident hash) (cdr (assoc pos
							  (gethash new-ident var-val-mappings))))
               (setf (gethash new-ident hash) pos))
           ;;(format t "~%original id: ~A gets swapped with new id: ~A~%new identifiers:~%~A" id new-ident new-hash)
           ))
    finally
       (return hash)))
|#
#| Substitute bindings in hash table keys |#

;; hash = hash table
;; bindings = variable bindings for keys hash table
;; var-val-mappings = map for consistently swapping the cdr of a binding
(defun swap-hash-keys (hash bindings &key (var-val-mappings))
  (loop
    with new-hash
    for id being the hash-keys of hash
      using (hash-value pos)
    do
       (multiple-value-bind (new-ident present-p)
           (gethash id bindings)
	 (cond ((and present-p (not (equal id new-ident)))
		(when (and present-p (not (equal id new-ident)))
		  (remhash id hash)
		  (if var-val-mappings
		      (setf (gethash new-ident hash) (cdr (assoc pos
								 (gethash new-ident var-val-mappings))))
		      (setf (gethash new-ident hash) pos))
		  ;;(format t "~%original id: ~A gets swapped with new id: ~A~%new identifiers:~%~A" id new-ident new-hash)
		  ))
	       (var-val-mappings
		(multiple-value-bind (present-p new-vvb)
		    (gethash id var-val-mappings)
		  (when present-p
		    (setf (gethash id hash)
			  (cdr (assoc pos
				      (gethash id var-val-mappings)))))))))
    finally
       (return hash)))

(defun swap-rule-conditions (rules bindings var-val-mappings)
  (loop
    with new-rule
    for rule being the elements of rules
    do
       (setq new-rule (make-rule :id (rule-id rule)
                                 :conditions (swap-hash-keys (rule-conditions rule) bindings :var-val-mappings var-val-mappings)
                                 :probability (rule-probability rule)
                                 :block (rule-block rule)
                                 :certain-block (rule-certain-block rule)
                                 :avoid-list (rule-avoid-list rule)
                                 :redundancies (rule-redundancies rule)
                                 :count (rule-count rule)))
    collect new-rule into new-rules
    finally
       (return (make-array (array-dimension rules 0) :initial-contents new-rules))))

#| Perform variable binding substituion in var value map |#

;; cpd = conditional probability distribution
;; cpd2 = conditional probability distribution
;; bindings = variable bindings for matches
(defun subst-var-value-block-map (cpd cpd2 bindings)
  (when nil (and (= cycle* cycle*) (equal "TURN_LEFT12794" (rule-based-cpd-dependent-id cpd)))
    (format t "~%cpd:~%~S~%cpd2:~%~S~%bindings:~%~S" cpd cpd2 bindings))
  (loop
    with var-val-mappings = (make-hash-table :test #'equal) and domain and domain2
    with var-values = nil
    for ident being the hash-keys of (rule-based-cpd-identifiers cpd)
      using (hash-value idx)
    do
       (setq var-values nil)
       (setq domain (gethash idx (rule-based-cpd-var-value-block-map cpd)))
       (when cpd2
	 (setq domain2 (gethash (gethash ident (rule-based-cpd-identifiers cpd2))
				(rule-based-cpd-var-value-block-map cpd2))))
       (when nil (and (= cycle* cycle*) (equal "TURN_LEFT12794" (rule-based-cpd-dependent-id cpd)))
	 (format t "~%~%domain:~%~S~%domain2:~%~S" domain domain2))
       (loop
	 with last-idx = (length domain2)
         for (binding block) in domain
         with val and new-binding
         do
	    (when nil (and (= cycle* cycle*) (equal "TURN_LEFT12794" (rule-based-cpd-dependent-id cpd)))
	      (format t "~%vvm:~%~S~%bindings:~%~S" binding bindings))
            (cond ((and cpd2 (gethash ident (rule-based-cpd-identifiers cpd2)))
		   (when (or (equal (gethash (gethash ident (rule-based-cpd-identifiers cpd2))
					     (rule-based-cpd-vars cpd)) "ACTION")
			     (equal (gethash (gethash ident (rule-based-cpd-identifiers cpd2))
					     (rule-based-cpd-vars cpd)) "GOAL"))
		     (when (gethash (car binding) bindings)
		       (setq binding (cons (gethash (car binding) bindings) (cdr binding)))))
		   (setq val (cdar (assoc (car binding) domain2 :key #'car :test #'equal)))
		   (when nil (and (= cycle* cycle*) (equal "TURN_LEFT12794" (rule-based-cpd-dependent-id cpd)))
		     (format t "~%val:~%~S" val))
		   (when (not val)
		     (setq val last-idx)
		     (setq last-idx (+ 1 last-idx)))
		   (when (null (gethash ident var-val-mappings))
		     (setf (gethash ident var-val-mappings) nil))
		   (setf (gethash ident var-val-mappings)
			 (cons (cons (cdr binding) val) (gethash ident var-val-mappings)))
		   (setq var-values (cons val var-values))
		   (setq new-binding (list (cons (car binding) val) block)))
		  (t
		   (setq var-values (cons (cdr binding) var-values))
                   (setq new-binding (list binding block))
		   (when (null (gethash ident var-val-mappings))
		     (setf (gethash ident var-val-mappings) nil))
		   (setf (gethash ident var-val-mappings)
			 (cons (cons (cdr binding) (cdr binding))
			       (gethash ident var-val-mappings)))))
         collect new-binding into new-domain
         finally
            (setf (gethash idx (rule-based-cpd-var-value-block-map cpd)) new-domain)
	    (setf (gethash idx (rule-based-cpd-var-values cpd)) (nreverse var-values)))
    finally
       (when nil (and (= cycle* cycle*) (equal "TURN_LEFT12794" (rule-based-cpd-dependent-id cpd)))
	      (format t "~%var-val-mappings:~%~S" var-val-mappings))
       (return var-val-mappings)))

#| Return the keys of the hash table as a list |#

;; hash = hash table
(defun hash-keys-to-list (hash)
  (loop for key being the hash-keys of hash collect key))

#| Take the intersection of hash table keys |#

;; hash1 = hash table
;; hash2 = hash-table
(defun hash-intersection (hash1 hash2 &key (output-hash-p nil) (test #'eql))
  (let (smaller greater result)
    (cond ((<= (hash-table-count hash1) (hash-table-count hash2))
           (setq smaller hash1)
           (setq greater hash2))
          (t
           (setq smaller hash2)
           (setq greater hash1)))
    (if output-hash-p
        (setq result (make-hash-table :size (ceiling (* 2.3 (hash-table-count smaller))) :test test)))
    (loop
      for k1 being the hash-keys of smaller
      do
         (multiple-value-bind (val present-p)
             (gethash k1 greater)
           (when present-p
             (if output-hash-p
                 (setf (gethash k1 result) k1)
                 (setq result (cons k1 result))))))
    result))

#| Indicate if two hash tables have elements in common |#

;; hash1 = hash table
;; hash2 = hash-table
(defun hash-intersection-p (hash1 hash2)
  (loop
    for k1 being the hash-keys of hash1
    do
       (multiple-value-bind (val present-p)
           (gethash k1 hash2)
         (when present-p
           (return-from hash-intersection-p t))))
  nil)

#| Returns key associated with value in hash table |#

;; value = value in hash table to find
;; hash = hash table
(defun hash-rassoc (value hash)
  (loop for key being the hash-keys of hash
          using (hash-value v)
        when (funcall (hash-table-test hash) v value) do
          (remhash key hash)
          (return-from hash-rassoc (values key t)))
  (values nil nil))

#| Find keys/values in hash1 that are not in hash2 |#

;; hash1 = hash table
;; hash2 = hash-table
;; cpd1 = conditional probability distribution
;; forbidden-types = cpd types to ignore
(defun hash-difference-p (hash1 hash2 cpd1 &optional forbidden-types)
  (when hash1
    (loop
      with diff-count = 0 and var-type and forbidden-p and forbidden-likelihood
      for k1 being the hash-keys of hash1
        using (hash-value pos)
      do
         (when forbidden-types
           (setq var-type (gethash pos (rule-based-cpd-types cpd1)))
           (setq forbidden-p (member var-type forbidden-types :test #'equal))
           (if forbidden-p (setq forbidden-likelihood t)))
         (multiple-value-bind (val present-p)
             (gethash k1 hash2)
           (when (and (not present-p) (not forbidden-p))
             (incf diff-count)))
      finally
         (return (values diff-count forbidden-likelihood)))))

#| Find keys/values in hash1 that are not in hash2 |#

;; hash1 = hash table
;; hash2 = hash-table
;; cpd1 = conditional probability distribution
;; forbidden-types = cpd types to ignore
(defun hash-difference (hash1 hash2 cpd1 &optional forbidden-types)
  (when hash1
    (loop
      with result and var-type and forbidden-p and forbidden-likelihood
      for k1 being the hash-keys of hash1
        using (hash-value pos)
      do
         (when forbidden-types
           (setq var-type (gethash pos (rule-based-cpd-types cpd1)))
           (setq forbidden-p (member var-type forbidden-types :test #'equal))
           (if forbidden-p (setq forbidden-likelihood t)))
         (multiple-value-bind (val present-p)
             (gethash k1 hash2)
           (when (and (not present-p) (not forbidden-p))
             (setq result (cons k1 result))))
      finally
         (return (values result forbidden-likelihood)))))

#| Return the difference of two hash tables |#

;; block1 = hash table
;; block2 = hash table
(defun block-difference (block1 block2 &key (output-hash-p nil) (test #'eql))
  (loop
    with result = (if output-hash-p (make-hash-table :size (ceiling (* 2.3 (hash-table-count block1))) :test test))
    for b1 being the hash-keys of block1
    do
       (multiple-value-bind (val present-p)
           (gethash b1 block2)
         (when (not present-p)
           (if output-hash-p
               (setf (gethash b1 result) b1)
               (setq result (cons b1 result)))))
    finally
       (return result)))

#| Return true if the parents of hash1 are a subset of the parents in hash2 |#

;; hash1 = hash table
;; hash2 = hash-table
(defun hash-subset-parents (hash1 hash2)
  (loop
    for k1 being the hash-keys of hash1
      using (hash-value i)
    when (not (= i 0))
    do
       (multiple-value-bind (val present-p)
           (gethash k1 hash2)
         (when (not present-p)
           (return-from hash-subset-parents nil))))
  t)

#| Make an efficient copy of an array |#

;; array = array to copy
(defun copy-array (array &key
                           (element-type (array-element-type array))
                           (fill-pointer (and (array-has-fill-pointer-p array)
                                              (fill-pointer array)))
                           (adjustable (adjustable-array-p array)))
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (cond ((listp i)
                   (row-major-aref array (copy-list i)))
                  (t
                   (row-major-aref array i)))))
    new-array))

#| Make an efficient copy of hash table |#

;; hash = hash-table to copy
(defun copy-hash-table (hash &key reference-values)
  (when hash
    (let ((h (make-hash-table
              :test (hash-table-test hash)
              :rehash-size (hash-table-rehash-size hash)
              :rehash-threshold (hash-table-rehash-threshold hash)
              :size (hash-table-size hash))))
      (loop for key being the hash-keys of hash
              using (hash-value value)
            do
               (setf (gethash key h)
                     (cond ((and (not reference-values) (listp value))
                            (deep-copy-list value))
                           ((and (not reference-values) (arrayp value))
                            (copy-array value))
                           (t
                            value)))
	 finally (return h)))))

#| Copy a list and all its elements |#

;; l = list to copy
(defun deep-copy-list (l)
  (cond ((null l)
         nil)
        ((and (consp (car l)) (not (listp (cdar l))))
         (cons (cons (caar l) (cdar l)) (deep-copy-list (rest l))))
        ((listp (car l))
         (cons (deep-copy-list (car l)) (deep-copy-list (rest l))))
        ((arrayp (car l))
         (cons (copy-array (car l)) (deep-copy-list (rest l))))
        ((hash-table-p (car l))
         (cons (copy-hash-table (car l)) (deep-copy-list (rest l))))
        (t
         (cons (car l) (deep-copy-list (rest l))))))

#| Make an efficient copy of hash table |#

;; hash = hash-table to copy
(defun make-hash-table-like (hash &key (rehash-size 1))
  (when hash
    (make-hash-table
              :test (hash-table-test hash)
              :rehash-size (if (= 1 rehash-size) (hash-table-rehash-size hash) (float rehash-size)) 
              :rehash-threshold (hash-table-rehash-threshold hash)
              :size (hash-table-size hash))))

#| Remove key from hash table and adjust keys to proper ordinal values |#

;; hash = ordinal hash table
;; key = key to remove
(defun reduce-ordinal-hash (hash key &aux copy-hash)
  (setq copy-hash (copy-hash-table hash))
  (remhash key copy-hash)
  (loop
    with val
    for i from (+ key 1) to (- (hash-table-count hash) 1)
    do
       (setq val (gethash i copy-hash))
       (setf (gethash (- i 1) copy-hash) val)
       (remhash i copy-hash))
  copy-hash)

#| Remove key from hash table and adjust keys to proper ordinal values |#

;; hash = categorical hash table
;; key = key to remove
;; key-idx = index associated with key
(defun reduce-categorical-hash (hash key key-idx)
  (loop
    with new-hash = (make-hash-table :test #'equal) and dep-id
    for k being the hash-keys of hash
      using (hash-value idx)
    do
       (cond ((< idx key-idx)
              (if (= 0 idx)
                  (setq dep-id k))
              (setf (gethash k new-hash) idx))
             ((> idx key-idx)
              (if (and (= 0 key-idx) (= 1 idx))
                  (setq dep-id k))
              (setf (gethash k new-hash) (- idx 1))))
    finally
       (return (values new-hash dep-id))))

#| Copy a conditional probability distribution represented as a table |#

;; cpd = conditional probability density
(defun copy-table-cpd (cpd)
  (make-cpd
   :dependent-id (cpd-dependent-id cpd)
   :identifiers (copy-hash-table (cpd-identifiers cpd))
   :dependent-var (cpd-dependent-var cpd)
   :vars (copy-hash-table (cpd-vars cpd))
   :types (copy-hash-table (cpd-types cpd))
   :concept-ids (copy-hash-table (cpd-concept-ids cpd))
   :qualified-vars (copy-hash-table (cpd-qualified-vars cpd))
   :var-value-map (copy-hash-table (cpd-var-value-map cpd))
   :cardinalities (copy-array (cpd-cardinalities cpd))
   :step-sizes (copy-array (cpd-step-sizes cpd))
   :assignments (copy-hash-table (cpd-assignments cpd))
   :counts (copy-hash-table (cpd-counts cpd))
   :count (cpd-count cpd)
   :lvl (cpd-lvl cpd)))

#| Copy a rule from a cpd |#

;; rule = rule in rule-based cpd
;; fresh-id = flag for whether we give the rule a new id
;; count = count we wish to assign to rule, if not default value
(defun copy-cpd-rule (rule &key (fresh-id nil) (count nil))
  (make-rule :id (if fresh-id (symbol-name (gensym "RULE-")) (rule-id rule))
             :conditions (copy-hash-table (rule-conditions rule))
             :probability (rule-probability rule)
             :block (copy-hash-table (rule-block rule))
             :certain-block (copy-hash-table (rule-certain-block rule))
             :avoid-list (copy-hash-table (rule-avoid-list rule))
             :redundancies (copy-hash-table (rule-redundancies rule))
             :count (if count count (rule-count rule))))

(defun string-to-float (str)
  (with-input-from-string (in str)
    (read in)))

#| Return true if two rules are the same |#

;; rule1 = rule in rule-based cpd
;; rule2 = rule in rule-based cpd
(defun same-rule-p (rule1 rule2 cpd1 cpd2 &key (check-probability t) (check-num-conditions t) (check-count t) (check-conditions t) (exact nil) (round nil))
  (cond ((and check-probability round (not (= (read-from-string (format nil "~5$" (rule-probability rule1)))
                                              (read-from-string (format nil "~5$" (rule-probability rule2))))))
         nil)
        ((and check-probability (not round) (not (= (rule-probability rule1) (rule-probability rule2))))
         nil)
        ((and check-num-conditions (not (= (hash-table-count (rule-conditions rule1)) (hash-table-count (rule-conditions rule2)))))
         nil)
        ((and check-count (not (eq (rule-count rule1) (rule-count rule2))))
         nil)
        (check-conditions
         (multiple-value-bind (compatible-p num-compatible)
             (compatible-rule-p rule1 rule2 cpd1 cpd2 :exact exact)
           (declare (ignore num-compatible))
           (and compatible-p
                (null (hash-difference (rule-conditions rule1) (rule-conditions rule2) nil))
                (null (hash-difference (rule-conditions rule2) (rule-conditions rule1) nil)))))
        (t
         t)))

#| Copy a conditional probability distribution represented as a set of rules |#

;; cpd = conditional probability density
(defun copy-rule-based-cpd (cpd &key (rule-counts nil))
  (make-rule-based-cpd
   :dependent-id (rule-based-cpd-dependent-id cpd)
   :identifiers (copy-hash-table (rule-based-cpd-identifiers cpd))
   :dependent-var (rule-based-cpd-dependent-var cpd)
   :vars (copy-hash-table (rule-based-cpd-vars cpd))
   :types (copy-hash-table (rule-based-cpd-types cpd))
   :concept-ids (copy-hash-table (rule-based-cpd-concept-ids cpd))
   :qualified-vars (copy-hash-table (rule-based-cpd-qualified-vars cpd))
   :var-value-block-map (copy-hash-table (rule-based-cpd-var-value-block-map cpd))
   :negated-vvbms (copy-hash-table (rule-based-cpd-negated-vvbms cpd))
   :set-valued-attributes (copy-hash-table (rule-based-cpd-set-valued-attributes cpd))
   :set-valued-negated-attributes (copy-hash-table (rule-based-cpd-set-valued-negated-attributes cpd))
   :lower-approx-var-value-block-map (copy-hash-table (rule-based-cpd-lower-approx-var-value-block-map cpd))
   :lower-approx-negated-vvbms (copy-hash-table (rule-based-cpd-lower-approx-negated-vvbms cpd))
   :characteristic-sets (copy-hash-table (rule-based-cpd-characteristic-sets cpd))
   :characteristic-sets-values (copy-hash-table (rule-based-cpd-characteristic-sets-values cpd))
   :var-values (copy-hash-table (rule-based-cpd-var-values cpd))
   :cardinalities (copy-array (rule-based-cpd-cardinalities cpd))
   :step-sizes (copy-array (rule-based-cpd-step-sizes cpd))
   :rules (map 'vector #'(lambda (rule) (copy-cpd-rule rule :count rule-counts)) (rule-based-cpd-rules cpd))
   :concept-blocks (copy-hash-table (rule-based-cpd-concept-blocks cpd))
   :singleton-p (rule-based-cpd-singleton-p cpd)
   :count (rule-based-cpd-count cpd)
   :lvl (rule-based-cpd-lvl cpd)))

#| Copy some book-keeping variables in a conditional probability distribution |#

;; cpd = conditional probability density
(defun partial-copy-table-cpd (cpd)
  (make-cpd
   :dependent-id (cpd-dependent-id cpd)
   :identifiers (copy-hash-table (cpd-identifiers cpd))
   :dependent-var (cpd-dependent-var cpd)
   :vars (copy-hash-table (cpd-vars cpd))
   :types (copy-hash-table (cpd-types cpd))
   :concept-ids (copy-hash-table (cpd-concept-ids cpd))
   :qualified-vars (copy-hash-table (cpd-qualified-vars cpd))
   :var-value-map (copy-hash-table (cpd-var-value-map cpd))
   :cardinalities (copy-array (cpd-cardinalities cpd))
   :step-sizes (copy-array (cpd-step-sizes cpd))
   :assignments (cpd-assignments cpd)
   :counts (cpd-counts cpd)
   :count (cpd-count cpd)
   :lvl (cpd-lvl cpd)))

#| Copy some book-keeping variables in a conditional probability distribution |#

;; cpd = conditional probability density
(defun partial-copy-rule-based-cpd (cpd &key (rule-counts nil))
  (make-rule-based-cpd
   :dependent-id (rule-based-cpd-dependent-id cpd)
   :identifiers (copy-hash-table (rule-based-cpd-identifiers cpd))
   :dependent-var (rule-based-cpd-dependent-var cpd)
   :vars (copy-hash-table (rule-based-cpd-vars cpd))
   :types (copy-hash-table (rule-based-cpd-types cpd))
   :concept-ids (copy-hash-table (rule-based-cpd-concept-ids cpd))
   :qualified-vars (copy-hash-table (rule-based-cpd-qualified-vars cpd))
   :var-value-block-map (copy-hash-table (rule-based-cpd-var-value-block-map cpd))
   :negated-vvbms (copy-hash-table (rule-based-cpd-negated-vvbms cpd))
   :set-valued-attributes (copy-hash-table (rule-based-cpd-set-valued-attributes cpd))
   :set-valued-negated-attributes (copy-hash-table (rule-based-cpd-set-valued-negated-attributes cpd))
   :lower-approx-var-value-block-map (copy-hash-table (rule-based-cpd-lower-approx-var-value-block-map cpd))
   :lower-approx-negated-vvbms (copy-hash-table (rule-based-cpd-lower-approx-negated-vvbms cpd))
   :characteristic-sets (copy-hash-table (rule-based-cpd-characteristic-sets cpd))
   :characteristic-sets-values (copy-hash-table (rule-based-cpd-characteristic-sets-values cpd))
   :var-values (copy-hash-table (rule-based-cpd-var-values cpd))
   :cardinalities (copy-array (rule-based-cpd-cardinalities cpd))
   :step-sizes (copy-array (rule-based-cpd-step-sizes cpd))
   :rules (map 'vector #'(lambda (rule) (copy-cpd-rule rule :count rule-counts)) (rule-based-cpd-rules cpd))
   :concept-blocks (rule-based-cpd-concept-blocks cpd)
   :singleton-p (rule-based-cpd-singleton-p cpd)
   :count (rule-based-cpd-count cpd)
   :lvl (rule-based-cpd-lvl cpd)))

#| Copy some book-keeping variables in a conditional probability distribution |#

;; cpd = conditional probability density
(defun partial-copy-table-cpd-2 (cpd)
  (make-cpd
   :dependent-id (cpd-dependent-id cpd)
   :identifiers (copy-hash-table (cpd-identifiers cpd))
   :dependent-var (cpd-dependent-var cpd)
   :vars (cpd-vars cpd)
   :types (cpd-types cpd)
   :concept-ids (cpd-concept-ids cpd)
   :qualified-vars (cpd-qualified-vars cpd)
   :var-value-map (copy-hash-table (cpd-var-value-map cpd))
   :cardinalities (cpd-cardinalities cpd)
   :step-sizes (cpd-step-sizes cpd)
   :assignments (cpd-assignments cpd)
   :counts (cpd-counts cpd)
   :count (cpd-count cpd)
   :lvl (cpd-lvl cpd)))

#| Copy some book-keeping variables in a conditional probability distribution |#

;; cpd = conditional probability density
(defun partial-copy-rule-based-cpd-2 (cpd &key (rule-counts nil))
  (make-rule-based-cpd
   :dependent-id (rule-based-cpd-dependent-id cpd)
   :identifiers (copy-hash-table (rule-based-cpd-identifiers cpd))
   :dependent-var (rule-based-cpd-dependent-var cpd)
   :vars (rule-based-cpd-vars cpd)
   :types (rule-based-cpd-types cpd)
   :concept-ids (rule-based-cpd-concept-ids cpd)
   :qualified-vars (rule-based-cpd-qualified-vars cpd)
   :var-value-block-map (copy-hash-table (rule-based-cpd-var-value-block-map cpd))
   :negated-vvbms (copy-hash-table (rule-based-cpd-negated-vvbms cpd))
   :set-valued-attributes (copy-hash-table (rule-based-cpd-set-valued-attributes cpd))
   :set-valued-negated-attributes (copy-hash-table (rule-based-cpd-set-valued-negated-attributes cpd))
   :lower-approx-var-value-block-map (rule-based-cpd-lower-approx-var-value-block-map cpd)
   :lower-approx-negated-vvbms (rule-based-cpd-lower-approx-negated-vvbms cpd)
   :characteristic-sets (rule-based-cpd-characteristic-sets cpd)
   :characteristic-sets-values (rule-based-cpd-characteristic-sets-values cpd)
   :var-values (copy-hash-table (rule-based-cpd-var-values cpd))
   :cardinalities (rule-based-cpd-cardinalities cpd)
   :step-sizes (rule-based-cpd-step-sizes cpd)
   :rules (map 'vector #'(lambda (rule) (copy-cpd-rule rule :count rule-counts)) (rule-based-cpd-rules cpd))
   :concept-blocks (rule-based-cpd-concept-blocks cpd)
   :singleton-p (rule-based-cpd-singleton-p cpd)
   :count (rule-based-cpd-count cpd)
   :lvl (rule-based-cpd-lvl cpd)))

(defun subst-cpd (cpd cpd2 bindings &key (deep nil) &aux new-cpd dep-id)
  ;;(format t "~%original cpd:~%~A" cpd)
  ;;(setq dep-id (cdr (assoc (cpd-dependent-id cpd) bindings :test #'equal)))
  (let (var-val-mappings)
    (if deep
	(setq new-cpd (copy-rule-based-cpd cpd))
	(setq new-cpd (partial-copy-rule-based-cpd cpd)))
    (setq dep-id (gethash (rule-based-cpd-dependent-id new-cpd) bindings))
    ;;(setq dep-id (fset:lookup bindings (cpd-dependent-id cpd)))
    (when dep-id
      (setf (rule-based-cpd-dependent-id new-cpd) dep-id))
    (setf (rule-based-cpd-identifiers new-cpd) (swap-hash-keys (rule-based-cpd-identifiers new-cpd) bindings))
    (setq var-val-mappings (subst-var-value-block-map new-cpd cpd2 bindings))
    (setf (rule-based-cpd-rules new-cpd) (swap-rule-conditions (rule-based-cpd-rules new-cpd) bindings var-val-mappings))
    ;;(format t "~%subst cpd:~%~A" cpd)
    new-cpd))

(defun subst-cpd-2 (cpd cpd2 bindings &key (deep nil) &aux new-cpd dep-id)
  ;;(format t "~%original cpd:~%~A" cpd)
  ;;(setq dep-id (cdr (assoc (cpd-dependent-id cpd) bindings :test #'equal)))
  (let (var-val-mappings)
    (if deep
	(setq new-cpd (copy-rule-based-cpd cpd))
	(setq new-cpd (partial-copy-rule-based-cpd-2 cpd)))
    (setq dep-id (gethash (rule-based-cpd-dependent-id new-cpd) bindings))
    ;;(setq dep-id (fset:lookup bindings (cpd-dependent-id cpd)))
    (when dep-id
      (setf (rule-based-cpd-dependent-id new-cpd) dep-id))
    (setf (rule-based-cpd-identifiers new-cpd) (swap-hash-keys (rule-based-cpd-identifiers new-cpd) bindings))
    (setq var-val-mappings (subst-var-value-block-map new-cpd cpd2 bindings))
    (setf (rule-based-cpd-rules new-cpd) (swap-rule-conditions (rule-based-cpd-rules new-cpd) bindings var-val-mappings))
    ;;(format t "~%subst cpd:~%~A" cpd)
    new-cpd))

#| Combine two or more symbols together |#

;; objects = list of symbols
(defun combine-symbols (&rest objects)
  (format nil "~{~a~}" objects))

#| Return true if cpd is a singleton |#

;; cpd = conditional probability density
(defun singleton-cpd? (cpd)
  (rule-based-cpd-singleton-p cpd))

#| Return true if cpd1 is a child of cpd2 |#

;; cpd1 = conditional probability density
;; cpd2 = conditional probability density
(defun cpd-child-p (cpd1 cpd2)
  (let (pos)
    (setq pos (gethash (rule-based-cpd-dependent-id cpd2) (rule-based-cpd-identifiers cpd1)))
    (when (and pos (> pos 0)) t)))

#| Remove the nth element from a list |#

;; pos = position of element
;; l = list
(defun remove-nth (pos l)
  (cond ((null l)
         l)
        ((> pos 0)
         (cons (car l) (remove-nth (- pos 1) (rest l))))
        ((= pos 0)
         (rest l))))

#| Replace the nth element in a list with item |#

;; pos = position of element
;; l = list
(defun replace-nth (pos item l)
  (cond ((null l)
         l)
        ((> pos 0)
         (cons (car l) (replace-nth (- pos 1) item (rest l))))
        ((= pos 0)
         (cons item (rest l)))))

#| Insert an element inplace after index |#

;; lst = list of elements
;; pos = index to insert after
;; ele = element to insert
(defun insert-after (lst pos ele)
  (push ele (cdr (nthcdr pos lst)))
  lst)

#| Insert an element inplace |#

;; lst = list of elements
;; pos = index to insert after
;; ele = element to insert
(defun inline-replace (lst pos ele)
  (remove-nth pos (insert-after lst pos ele)))

#| Return the scope of a conditional probability density |#

;; idents = hash table of identifiers
;; vars = hash table of cpd variable names
;; cids = hash table of concept ids
(defun generate-cpd-vars (idents vars cids)
  (loop
    with qvar-hash = (make-hash-table)
    with var and cid and qvar
    for ident being the hash-keys of idents
      using (hash-value idx)
    do
       (setq var (gethash idx vars))
       (setq cid (gethash idx cids))
       (setq qvar (combine-symbols var cid))
       (setf (gethash idx qvar-hash) qvar)
    finally
       (return qvar-hash)))

#| Generate size of the domain for conditional probability density |#

;; vvm = variable value map of conditional-probability density
(defun generate-cpd-cardinalities (vvm &aux count)
  (setq count (hash-table-count vvm))
  (make-array count :initial-contents (loop for i from 0 to (- count 1) collect (length (gethash i vvm))) :fill-pointer t))

#| Generate an ordered list of step-sizes for the variables |#

;; cardinalities = conditional probability density cardinalities
(defun generate-cpd-step-sizes (cardinalities &aux step-sizes)
  (setq step-sizes (maplist #'(lambda (cards) (reduce '* (rest cards)))
                            (reverse (coerce cardinalities 'list))))
  (make-array (length step-sizes) :initial-contents (reverse step-sizes) :fill-pointer t))

#| Generate an index to flattened cpd |#

;; cpd = conditional probability density
;; assignments = variable assignments for each var (ordered)
(defun get-cpd-index (cpd assignments)
  (reduce #'+ (mapcar '* assignments (coerce (rule-based-cpd-step-sizes cpd) 'list))))

#| Get variable assignment of variable i in cpd for given index |#

;; cpd = conditional probability density
;; index = index into flattened cpd
;; i = variable position in cpd-vars
(defun get-cpd-assignment-for-var (cpd index i)
  (mod  (floor (/ index (aref (rule-based-cpd-step-sizes cpd) i)))
        (aref (rule-based-cpd-cardinalities cpd) i)))

#| Get variable assignment of all variables in cpd for given index |#

;; cpd = conditional probability density
;; index = index into flattened cpd
(defun get-cpd-assignment-from-index (cpd index)
  (loop
    with assn = (make-array (hash-table-count (rule-based-cpd-identifiers cpd)) :fill-pointer t)
    for i being the hash-values of (rule-based-cpd-identifiers cpd)
    do
       (setf (aref assn i) (get-cpd-assignment-for-var cpd index i))
    finally
       (return assn)))

#| Get values associated with the scope of a factor|#

;; cpd = conditional probability density, aka factor
(defun get-cpd-scope-values (cpd)
  (mapcan #'(lambda (bindings)
	      (mapcar 'car bindings))
	  (cpd-var-value-map cpd)))

#| Find matching cpd in list of cpds |#

;; id = conditional probability variable identifier
;; matches = list of matching conditional probability densities
(defun get-corresponding-cpd-by-id (id matches)
  (caar (member id matches
                :key #'(lambda (pair)
                         (when (cdr pair)
                           (cpd-identifiers (cdr pair))))
                :test #'(lambda (identifier ids)
                          (equal identifier (car ids))))))

#| Find matching cpd in list of cpds |#

;; id = conditional probability variable identifier
;; cpds = list of conditional probability densities
(defun get-cpd-by-id (id cpds)
  (car (member id cpds :key #'rule-based-cpd-dependent-id :test #'equal)))

#| Find cpd in list of cpds by matching against identifiers |#

;; id = conditional probability variable identifier
;; cpds = list of conditional probability densities
(defun lookup-cpd-by-id (id cpds)
  (car (member id cpds
               :test #'(lambda (identifier cpd)
                         (and (= (hash-table-count (cpd-identifiers cpd)) 1)
                              (equal identifier (cpd-dependent-id cpd)))))))

#| Sum variable cardinalities from multiple conditional probability densities |#

;; vvm = variable value map of combined factor
(defun get-var-cardinalities (vvm)
  (generate-cpd-cardinalities vvm))

#| Convert assignment into a varible-values priority-queue |#

;; assignment = list of variable-values lists
(defun make-priority-q (assignment)
  (sort (copy-list assignment) (lambda (a b) (< (length (cdr a)) (length (cdr b))))))

;; assignment = list of variable-values lists
(defun priority-q-pop (assignment)
  (car assignment))

#| Checks to see if candidate assignment satisfies variable domains |#

;; candidate-assn = candidate assignment to check
;; unassigned = list of variable-values lists
;; assignment = list of variable-value bindings
;; constraint = constraint in csp
(defun valid-assignment (candidate-assn unassigned assignment constraint)
  ;;(format t "~%candidate assignment: ~A~%current assignment: ~A~%constraint: ~A" candidate-assn assignment constraint)
  ;;(maphash #'print-hash-entry (cpd-assignments constraint))
  (loop
     for assn in candidate-assn
     for ident in (cpd-identifiers constraint)
     for vvm in (cpd-var-value-map constraint)
     with var and vals
     do
       (setq var (find ident unassigned :key #'car))
       (cond (var
              (setq vals (mapcar #'(lambda (val)
                                     (cdr (assoc val vvm :test #'equal)))
                                 (rest var)))
              ;;(format t "~%var: ~A~%available vals: ~A~%assn: ~A" var vals assn)
              (when (not (member assn vals))
                ;;(format t "~%candidate assignment violates arc consistent variable domain")
                (return-from valid-assignment nil)))
             (t
              (setq var (find ident assignment :key #'car))
              ;;(format t "~%var: ~A~%assn: ~A" var assn)
              (when (not (= assn (cdr (assoc (cdr var) vvm :test #'equal))))
                ;;(format t "~%candidate assignment violates existing assignment!")
                (return-from valid-assignment nil))))
     finally
        ;;(format t "~%valid assignment!")
       (return t)))

#| Prune cardinalities of relevant variables via forward checking |#

;; unassigned = array of variable-value-lists
;; constraints = array of constraints for csp
;; arcs = list of variable-constriant pairs to process
;; assignment = list of variable-value bindings
(defun generalized-arc-consistency (unassigned constraints arcs assignment)
  ;;(format t "~%~%arcs:~%~A" arcs)
  (loop
    with arc and c-scope and new-domain and constraint
    with var and var-pos and poses
    do
       (setq arc (car arcs))
       (setq arcs (rest arcs))
       (setq constraint (aref constraints (cdr arc)))
       (setq c-scope (cpd-identifiers constraint))
       (setq var (aref unassigned (car arc)))
       (setq var-pos (position (car var) c-scope))
       (setq new-domain nil)
       (setq poses nil)
       ;;(format t "~%~%X: ~d~%Constraint: ~A~%unassigned: ~A~%current assignment: ~A" var constraint unassigned assignment)
       ;;(format t "~%Domain of X: ~A" (cdr var))
       (loop
         for ele in c-scope
         for i from 0
         do
            (setq poses (cons i poses))
         finally
            (setq poses (reverse poses)))
       ;;(format t "~%constraint scope: ~A~%scope variable positions in constraint: ~A" c-scope poses)
       (loop
         with assignments = (cpd-assignments constraint)
         with num-assignments = (reduce #'* (cpd-cardinalities constraint))
         with index and value
         for i from 0 to (- num-assignments 1)
         when (= (hash-access assignments 0 constraint (list i) nil) 1)
           do
              ;;(format t "~%cpd assignments index, ~d satisfies constraint assignments:." i)
              ;;(maphash #'print-hash-entry (cpd-assignments constraint))
              (loop
                for pos in poses
                collect (get-cpd-assignment-for-var constraint i pos) into assn
                finally
                   ;;(format t "~%corresponding variable assignments for constraint: ~A. testing validity..." assn)
                   (when (valid-assignment assn unassigned assignment constraint)
                     (setq index (nth var-pos assn))
                     (setq value (car (rassoc index (nth var-pos (cpd-var-value-map constraint)))))
                     (setq new-domain (union new-domain (list value))))))
       ;;(format t "~%new domain of X: ~A" new-domain)
       (if (null new-domain)
           (return-from generalized-arc-consistency (values 'fail unassigned)))
       (when (set-difference (cdr var) new-domain)
         (loop
           for i from 1 to (- (array-dimension unassigned 0) 1)
           do
              (loop
                for j from 0 to (- (array-dimension constraints 0) 1)
                with identifiers
                do
                   (setq identifiers (cpd-identifiers (aref constraints j)))
                   (when (and (= (length identifiers) 1)
                              (not (equalp constraint (aref constraints j)))
                              (gethash (car var) identifiers)
                              (gethash (car (aref unassigned i)) identifiers))
                     (setq arcs (union arcs (list (cons i j)))))))
         (setq var (cons (car var) new-domain))
         (setf (aref unassigned (car arc)) var))
    while (not (null arcs))
    finally
       (return (values t unassigned))))

#| Produces variable cardinality reductions via Maitaining Arc Consistency agorithm |#

;; unassigned = array of variable-values lists
;; csp = constraint satisfaction problem
;; assignment = list of variable-values bindings
(defun csp-inference (unassigned csp assignment)
  (cond ((> (array-dimension unassigned 0) 1)
         (loop
           for i from 1 to (- (array-dimension unassigned 0) 1)
           with todo and constraints = (getf csp :constraints) and var
           do
              (setq var (aref unassigned i))
              (loop
                for j from 0 to (- (array-dimension constraints 0) 1)
                with identifiers
                do
                   (setq identifiers (cpd-identifiers (aref constraints j)))
                   (when (and (= (length identifiers) 1)
                              (member (car var) identifiers :test #'equal))
                     (setq todo (cons (cons (- i 1) j) todo))))
           finally
              (return (generalized-arc-consistency (subseq unassigned 1) constraints todo assignment))))
        (t t)))

#| Prune variable values to be consistent with unary constraints |#

;; unassigned = list of unassigned variables
;; constraints = list of constraints
(defun node-consistency (unassigned constraints)
  (loop
    with cpd and domain
    for variable in unassigned
    do
       (setq cpd (lookup-cpd-by-id (car variable) constraints))
       (loop
         with vvm = (gethash 0 (cpd-var-value-map cpd))
         for key being the hash-keys of (cpd-assignments cpd)
         collect (car (nth key vvm)) into dom
         finally
            (setq domain dom))
    collect (cons (car variable) domain) into new-unassigned
    finally (return new-unassigned)))

#|Picks variable value according to least-constraining-value heuristic |#

;; assn = variable-values list
(defun order-variable-values (assn)
  (rest assn))

#| Picks next free variable according to minimum-remaining-values heuristic |#

;; unassigned = list of variable-values lists
(defun select-unassigned-variable (unassigned)
  (priority-q-pop (coerce unassigned 'list)))

;; assignment = list of variable-values bindings
;; csp = constraint satisfaction problem
(defun complete? (assignment csp)
  (= (length assignment) (length (getf csp :vars))))

#| Checks to see if candidate assignment violates constraints |#

;; var = selected variable
;; value = variable value for var
;; assignment = list of variable-values bindings
;; csp = constraint satisfaction problem
(defun consistent-p (var value assignment csp &aux candidate)
  (setq candidate (cons (cons var value) assignment))
  ;;(format t "~%~%candidate assignment: ~A" candidate)
  (loop
    for factor being the elements of (getf csp :constraints)
    ;;do (format t "~%checking costraint:~%~A" factor)
    ;;   (maphash #'print-hash-entry (cpd-assignments factor))
    if (subsetp (hash-keys-to-list (cpd-identifiers factor)) (mapcar 'car candidate) :test #'equal)
      do
         (loop
           for v being the hash-keys of (cpd-identifiers factor)
             using (hash-value pos)
           with sym-assn and bindings
           do
              (setq bindings (gethash pos (cpd-var-value-map factor)))
              (setq sym-assn (assoc v candidate :test #'equal))
              ;;(format t "~%constraint variable: ~A~%constraint variable vvm: ~A~%candidatate assignment for ~A: ~A" v bindings v sym-assn)
           when sym-assn
             collect (cdr (assoc (cdr sym-assn) bindings :test #'equal)) into assn
           finally
              ;;(format t "~%constraint assignment in candidate: ~A" assn)
              (let (assn-val)
                (setq assn-val (hash-access (cpd-assignments factor) 0 factor (list (get-cpd-index factor assn)) nil))
                ;;(format t "~%assignment value: ~A" assn-val)
                (when (= assn-val 0)
                  ;;(format t "~%invalid candidatate")
                  (return-from consistent-p nil)))))
  ;;(format t "~%success!")
  t)

#|Carries out backtracking depth first search |#

;; assignment = list of variable-values bindings
;; unassigned = list of variable-values lists
;; csp = constraint satisfaction problem
(defun csp-backtrack (assignment unassigned csp)
  #|
  (loop
    for constraint being the elements of (getf csp :constraints)
    do
       (format t "~%constraint: ~A" constraint))
  |#
  ;;(format t "~%~%partial solution:~%~A~%remaining assignments:~%~A" assignment unassigned)
  (cond ((complete? assignment csp)
         assignment)
        (t
         (let (var)
           (setq var (select-unassigned-variable unassigned))
           ;;(format t "~%    attempting to assign var ~A" var)
           (loop
             for val in (order-variable-values var)
             with inferences and result
             do
                ;;(format t " with value: ~A;" val)
                (when (consistent-p (car var) val assignment csp)
                  ;;(format t "~%        success! recursing")
                  (setq assignment (cons (cons (car var) val) assignment))
                  ;;(setq inferences (csp-inference csp var val))
                  ;;(break)
                  (when (not (eq 'fail inferences))
                    (setq assignment (append inferences assignment))
                    (setq result (csp-backtrack assignment (subseq unassigned 1) csp))
                    ;;(format t "~%        returned from recursion with result: ~A~%" result)
                    (if (not (eq 'fail result))
                        (return-from csp-backtrack result)
                        (setq assignment (nthcdr (+ (length inferences) 1) assignment)))))
                ;;(format t "~%failure")
                ;;(break)
             )
           'fail))))

#| Determines if there is an assignment of variable satisfying constraints |#

;; csp = constraints max factors from max-product alg.
;; unassigned = list of variable-values lists
(defun backtracking-search (csp unassigned)
  (format t "~%~%solving csp ...")
  (csp-backtrack nil (make-array (length unassigned) :initial-contents (make-priority-q unassigned) :fill-pointer t) csp))

(defun initialize-csp-assignments (csp)
  (loop
    for cpd being the elements of (getf csp :constraints)
    for var in (getf csp :vars)
    for i from 1
    collect (car (nth (random (length (gethash 0 (cpd-var-value-map cpd)))) (gethash 0 (cpd-var-value-map cpd)))) into assns
    finally
       (if (> (array-dimension (getf csp :constraints) 0) 0)
           (return (make-array i :initial-contents assns :fill-pointer t))
           (return (make-array 0 :fill-pointer t)))))

(defun csp-solution? (assignment csp)
  (when nil
    (format t "~%~%assignment: ~A" assignment))
  (loop
    with new-assn = (make-array (array-dimension assignment 0) :initial-element 'conflict :fill-pointer t)
    with conflicted and num-factors = (length (getf csp :vars))
    for factor being the elements of (getf csp :constraints)
    for i from 0
    do
       (when nil
         (format t "~%checking constraint:~%~A" factor))
       (loop
         for v being the hash-keys of (cpd-identifiers factor)
           using (hash-value pos)
         with sym-assn and sym-val and bindings
         do
            (setq bindings (gethash pos (cpd-var-value-map factor)))
            (loop
              named assoc-array
              for j from 0
              for factorj being the elements of (getf csp :constraints)
              when (equal v (cpd-dependent-id factorj)) do
                (setq sym-assn j)
                (setq sym-val (aref assignment j))
                (return-from assoc-array))
            (when nil
              (format t "~%constraint variable: ~A~%constraint variable vvm: ~A~%candidatate assignment for ~A: ~A" v bindings v (cons sym-assn sym-val)))
         when sym-assn
           collect (cdr (assoc sym-val bindings :test #'equal)) into assn
         finally
            (when nil
              (format t "~%constraint assignment in candidate: ~A" assn))
            (let (assn-val)
              (setq assn-val (hash-access (cpd-assignments factor) 0 factor (list (get-cpd-index factor assn)) nil))
              ;;(format t "~%assignment value: ~A" assn-val)
              #|
              (if (< i num-factors)
              (setf (aref new-assn i) (aref assignment i))
              (setf (aref new-assn (- i num-factors)) (aref assignment (- i num-factors))))
              (when (= assn-val 0)
              ;;(format t "~%invalid candidatate")
              (if (< i num-factors)
              (setq conflicted (cons i conflicted))
              (setq conflicted (cons (- i num-factors) conflicted))))
              |#
              (cond ((= assn-val 0)
                     ;;(format t "~%invalid candidatate")
                     (cond ((< i num-factors)
                            (setq conflicted (cons i conflicted))
                            (setf (aref new-assn i) 'conflict))
                           (t
                            (setq conflicted (cons (- i num-factors) conflicted))
                            (setf (aref new-assn (- i num-factors)) 'conflict))))
                    (t
                     (if (< i num-factors)
                         (setf (aref new-assn i) (aref assignment i))
                         (setf (aref new-assn (- i num-factors)) (aref assignment (- i num-factors))))))
	      ))
    finally
       ;;(format t "~%success!")
       (return (values (make-array (length conflicted) :initial-contents conflicted :fill-pointer t) new-assn))))

(defun generate-final-csp-assignment (csp conflicted-vars assn)
  (loop
    for var-val being the elements of assn
    for cpd being the elements of (getf csp :constraints)
    for i from 0
    when (or (not (member i conflicted-vars)) #|(not (eq var-val 'conflict))|#)
      collect (cons (cpd-dependent-id cpd) var-val) into assignment
    finally
       (when nil
         (format t "~%inferred assignment:~%~A" assignment)
         (break))
       (return assignment)))

;; idx = index to cpd of interest in csp
;; assn = current assignment
;; csp = constraint satisfaction problem
(defun conflicts (idx assn csp)
  (loop
    with cpd = (aref (getf csp :constraints) idx)
    with min-conflicts = most-positive-fixnum and num-conflicts
    with assn-copy = (copy-array assn)
    with candidates and candidates-size
    for value in (gethash 0 (cpd-var-value-map cpd)) do
      (setf (aref assn-copy idx) (car value))
      (setq num-conflicts (array-dimension (csp-solution? assn-copy csp) 0))
      (cond ((= num-conflicts min-conflicts)
             (setq candidates (cons (car value) candidates))
             (setq candidates-size (1+ candidates-size)))
            ((< num-conflicts min-conflicts)
             (setq min-conflicts num-conflicts)
             (setq candidates (list (car value)))
             (setq candidates-size 1)))
    finally
       (return (nth (random candidates-size) candidates))))

#| Constraint satisfaction problem solver using local search |#

;; csp = constraints max factors from max-product alg.
;; max-steps = number of steps allowed before giving up
(defun min-conflicts (csp &optional (max-steps 75))
  (when t
    (format t "~%engaging CSP"))
  (loop
    with current = (initialize-csp-assignments csp)
    with conflicted-vars and var and value
    for i from 1 to max-steps do
      (setq conflicted-vars (csp-solution? current csp))
      (when nil
        (format t "~%current solution: ~A~%conflicts:~%~A" current conflicted-vars))
      (when (= (array-dimension conflicted-vars 0) 0)
        (when nil
          (format t "~%Recovered consistent state at ~d steps..." i))
        (return-from min-conflicts (generate-final-csp-assignment csp (reverse (coerce conflicted-vars 'list)) current)))
      (setq var (aref conflicted-vars (random (array-dimension conflicted-vars 0))))
      (setq value (conflicts var current csp))
      (setf (aref current var) value)
    finally
       (when nil
         (format t "~%Recovered conflicted state..."))
       (multiple-value-bind (conflicted new-conflicts)
           (csp-solution? current csp)
         (when nil
           (format t "~%conflicted variables:~%~S~%new-conflicts:~%~S" (reverse (coerce conflicted 'list)) new-conflicts))
         (return (generate-final-csp-assignment csp (coerce conflicted 'list) new-conflicts)))))

(defun print-cpd-rule (rule)
  (format t "~%        <")
  (loop
    for att being the hash-keys of (rule-conditions rule)
      using (hash-value val)
    do
       (format t "~a:=~d " att val)
    finally
       (format t "; ~d, ~d>" (float (rule-probability rule)) (rule-count rule))))

#| Print key value pair from hashtable |#

;; key = hashtable key
;; value = hashtable value
(defun print-hash-entry (key value)
  (cond ((rule-based-cpd-p value)
         (format t "~%    ~d:" key)
         (map nil #'print-cpd-rule (rule-based-cpd-rules value)))
        (t
         (format t "~%    ~d: ~A" key value))))

#| Print all current messages in factor graph |#

;; messages = hashtable of messages
(defun print-messages (messages)
  (maphash #'(lambda (key value)
               (format t "~%The messages from node ~d are:" key)
               (maphash #'print-hash-entry value))
           messages))

#| Create list of fully qualified cpd variables|#

;; cpd = conditional probability density
(defun get-fully-qualified-cpd-vars (cpd)
  (generate-cpd-vars (cpd-vars cpd) (cpd-concept-ids cpd)))

#| Append unique phi2 variables to the end of phi1 variables |#

;; phi1 = conditional probability density 1
;; phi2 = conditional probability density 2
(defun ordered-union (phi1 phi2)
  (loop
    with idents = (copy-hash-table (rule-based-cpd-identifiers phi1)) and var-union = (rule-based-cpd-vars phi1)
    with concept-ids = (rule-based-cpd-concept-ids phi1) and qvars = (rule-based-cpd-qualified-vars phi1)
    with var-value-block-map = (rule-based-cpd-var-value-block-map phi1) and types = (rule-based-cpd-types phi1)
    with negated-vvbms = (rule-based-cpd-negated-vvbms phi1)
    with sva = (rule-based-cpd-set-valued-attributes phi1) and svna = (rule-based-cpd-set-valued-negated-attributes phi1)
    with lower-vvbms = (rule-based-cpd-lower-approx-var-value-block-map phi1)
    with lower-nvvbms = (rule-based-cpd-lower-approx-negated-vvbms phi1)
    with vals = (rule-based-cpd-var-values phi1)
    for ident2 being the hash-keys of (rule-based-cpd-identifiers phi2)
      using (hash-value pos2)
    do
       (when (not (gethash ident2 idents))
         (setf (gethash ident2 idents) (hash-table-count idents))
         (setf (gethash (hash-table-count var-union) var-union) (gethash pos2 (rule-based-cpd-vars phi2)))
         (setf (gethash (hash-table-count types) types) (gethash pos2 (rule-based-cpd-types phi2)))
         (setf (gethash (hash-table-count concept-ids) concept-ids) (gethash pos2 (rule-based-cpd-concept-ids phi2)))
         (setf (gethash (hash-table-count qvars) qvars) (gethash pos2 (rule-based-cpd-qualified-vars phi2)))
         (setf (gethash (hash-table-count var-value-block-map) var-value-block-map) (gethash pos2 (rule-based-cpd-var-value-block-map phi2)))
         (setf (gethash (hash-table-count negated-vvbms) negated-vvbms) (gethash pos2 (rule-based-cpd-negated-vvbms phi2)))
         (setf (gethash (hash-table-count sva) sva) (gethash pos2 (rule-based-cpd-set-valued-attributes phi2)))
         (setf (gethash (hash-table-count svna) svna) (gethash pos2 (rule-based-cpd-set-valued-negated-attributes phi2)))
         (setf (gethash (hash-table-count lower-vvbms) lower-vvbms) (gethash pos2 (rule-based-cpd-lower-approx-var-value-block-map phi2)))
         (setf (gethash (hash-table-count lower-nvvbms) lower-nvvbms) (gethash pos2 (rule-based-cpd-lower-approx-negated-vvbms phi2)))
         (setf (gethash (hash-table-count vals) vals) (gethash pos2 (rule-based-cpd-var-values phi2))))
     finally
        (return (values idents var-union types concept-ids qvars var-value-block-map negated-vvbms sva svna lower-vvbms lower-nvvbms vals))))

#| Normalize factor rules to maintain probability measure |#

;; assignments = array of assignments in conditional probability density
;; row-length = length of row in multi-dementional cpd
;; input-cpdp = generalized boolean stating whether the input distribution shows values for NA or not. (CPD or nil)
;; output-cpdp = generalized boolean stating whether the output distribution is shows values for NA or not. (CPD or nil)
(defun normalize-rule-probabilities (phi new-dep-id)
  (loop
    with dep-id-pos = (gethash new-dep-id (rule-based-cpd-identifiers phi))
    with rules = (rule-based-cpd-rules phi)
    with new-rules and block = 0 and new-rule
    for r1 being the elements of rules
    do
       (when nil (and (equal "TOWER546" (rule-based-cpd-dependent-id phi)))
         (format t "~%~%normalizing:~%~S" r1))
         (loop
           with copy-rule = (copy-cpd-rule r1)
           with compatible-rules and compatible-rule and norm-const
           for i in (gethash dep-id-pos (rule-based-cpd-var-values phi))
           do
              (setf (gethash new-dep-id (rule-conditions copy-rule)) i)
              ;;(setq compatible-rules (get-compatible-rules phi phi copy-rule :check-count nil))
              (setq compatible-rule (car (get-compatible-rules phi phi copy-rule :find-all nil)))
              ;;(setq compatible-rule (car compatible-rules))
              (when nil (and (equal "TOWER546" (rule-based-cpd-dependent-id phi)))
                    (format t "~%getting rule for assignment:~%~S~%candidatate matches:~%~S~%selection:~%~S" (rule-conditions copy-rule) compatible-rules compatible-rule))
           when (or (null (rule-count compatible-rule))
                    (> (rule-count compatible-rule) 0))
                collect compatible-rule into row
           finally
              ;;(setq norm-const (reduce #'(lambda (rule1 rule2) (+ (rule-probability rule1) (rule-probability rule2))) row))
              (setq norm-const (apply #'+ (mapcar #'rule-probability row)))
              (when nil (and (equal "TOWER546" (rule-based-cpd-dependent-id phi)))
                (format t "~%normalizing constant: ~d" norm-const))
               (setq new-rule (copy-cpd-rule r1))
               (setf (rule-probability new-rule) (if (> norm-const 0)
                                                     (/ (rule-probability r1) norm-const)
                                                     0))
               (setf (rule-block new-rule) (make-hash-table))
               (setf (gethash block (rule-block new-rule)) block)
               (setq block (+ block 1))
               (when nil (and (equal "TOWER546" (rule-based-cpd-dependent-id phi)))
                     (format t "~%normalized rule:~%~S" new-rule))
               (when (or (> (rule-probability new-rule) 1)
                         (< (rule-probability new-rule) 0))
                 (format t "~%identifiers:~%~S~%normalizing rule:~%~S~%row:~%~S~%norm const: ~d~%normalized rule:~%~S" (rule-based-cpd-identifiers phi) r1 row norm-const new-rule)
                 (error "Normalization error"))
               (setq new-rules (cons new-rule new-rules)))
    finally
       (setf (rule-based-cpd-rules phi) (make-array block :initial-contents (reverse new-rules))))
  phi)

#| Normalize factor assignments to maintain probabilities in assignments

;; assignments = array of assignments in conditional probability density
;; row-length = length of row in multi-dementional cpd
;; input-cpdp = generalized boolean stating whether the input distribution shows values for NA or not. (CPD or nil)
;; output-cpdp = generalized boolean stating whether the output distribution is shows values for NA or not. (CPD or nil)
(defun normalize-assignments (assignments row-length input-cpdp output-cpdp)
  (loop
    with row-idx-start and row-idx-end
    with normalizing-constant = 0
    with old-assn and new-assn and new-assignments = (make-hash-table)
    for assn-idx being the hash-keys of assignments
      using (hash-value prob)
    when (or (not (= (mod assn-idx row-length) 0))
             (not input-cpdp))
      do
         (setq row-idx-start (- assn-idx (mod assn-idx row-length)))
         (setq row-idx-end (+ row-idx-start (- row-length 1)))
         (loop
           for j from row-idx-start to row-idx-end
           sum (hash-access assignments 0 input-cpdp (list j)) into const
           finally (setq normalizing-constant const))
         (when (> normalizing-constant 0)
           (setq old-assn (hash-access assignments 0 input-cpdp (list assn-idx)))
           (setq new-assn (/ old-assn  normalizing-constant))
           (when (> new-assn 1)
             (error "Normalization error in normalize-assignments. New probability is greater than 1!~%Unnormalized assignments:~%~A~%normalizing constant:~%~d~%row start index: ~A~%row end index: ~d~%index in row: ~d~%retrieved probability: ~d~%new probability: ~d" assignments normalizing-constant row-idx-start row-idx-end assn-idx old-assn new-assn))
           (cond ((and (> new-assn 0) (not output-cpdp))
                  (setf (gethash assn-idx new-assignments) new-assn))
                 ((and (> new-assn 0) output-cpdp (not (= (mod assn-idx row-length) 0)))
                  (setf (gethash assn-idx new-assignments) new-assn))))
     finally
       (return new-assignments)))
|#

#|
#| Prevent conflicts between compatible rules by breaking compatibilities |#

;; cpd = conditional probability density
;; new-rule = rule to disambiguate against.
(defun disambiguate-rules-against (cpd new-rule)
  (when nil (and (= cycle* 3) (equal "GO_HOLD925" (rule-based-cpd-dependent-id cpd)))
    (format t "~%~%in disambiguate rules against:~%~S" new-rule))
  (loop
    with blk = 0 and new-rules
    for rule being the elements of (rule-based-cpd-rules cpd)
    do
       (when nil (and (= cycle* 3) (equal "GO_HOLD925" (rule-based-cpd-dependent-id cpd)))
         (format t "~%rule in schema:~S"rule))
       (cond ((compatible-rule-p rule new-rule cpd cpd)
              (multiple-value-bind (stripped-rule stripped-new-rule rule-avoid-list new-rule-avoid-list)
                  (disambiguate-rules (copy-cpd-rule rule) (copy-cpd-rule new-rule) cpd :incorporate-reference-conditions nil)
                (declare (ignore new-rule-avoid-list))
                ;; The avoid list needs to have the domain of the variable you want to split on
                (when nil (and (= cycle* 3) (equal "GO_HOLD925" (rule-based-cpd-dependent-id cpd)))
                  (format t "~%stripped rule:~%~S~%stripped disambiguate:~%~S~%stripped rule avoid list:~%~S" stripped-rule stripped-new-rule rule-avoid-list)
                  (break))
                (setq new-r1-rules (rule-split stripped-rule (rule-conditions stripped-new-rule) cpd cpd :enforce-compatible nil :avoid-hash rule-avoid-list)))
              (loop
                for new-r1-rule in new-r1-rules
                do
                   (setf (rule-block new-r1-rule) (list blk))
                   (setq new-rules (cons new-r1-rule new-rules))
                   (setq blk (+ blk 1))))
             (t
              (setf (rule-block rule) (list blk))
              (setq new-rules (cons rule new-rules))
              (setq blk (+ blk 1))))
    finally
       (setf (rule-block new-rule) (list blk))
       (setq new-rules (cons new-rule new-rules))
       (setq blk (+ blk 1))
       (return (values (reverse new-rules) blk))))
|#

#| Update schema domains with contents from episode |#

;; phi1 = conditional probability density from base
;; phi2 = conditional probability density from pattern
(defun cpd-update-schema-domain (phi1 phi2)
  (when nil (and (= cycle* 3) (equal "INTENTION2751" (rule-based-cpd-dependent-id phi1)))
          (format t "~%~%updating schema:~%~A~%with episode:~%~A" phi1 phi2))
    (loop
      for ident2 being the hash-keys of (rule-based-cpd-identifiers phi2)
        using (hash-value idx)
      with var and type and vvbm2 and nvvbm2 and cid2 and qvar2
      with new-rule
      with var2s and binding
      with pos and vvbm1 and nvvbm1 and lower-vvbm1 and lower-nvvbm1 and sva1 and svna1 and vals1
      do
         (setq var (gethash idx (rule-based-cpd-vars phi2)))
         (setq type (gethash idx (rule-based-cpd-types phi2)))
         (setq vvbm2 (gethash idx (rule-based-cpd-var-value-block-map phi2)))
         (setq nvvbm2 (gethash idx (rule-based-cpd-negated-vvbms phi2)))
         (setq cid2 (gethash idx (rule-based-cpd-concept-ids phi2)))
         (setq qvar2 (gethash idx (rule-based-cpd-qualified-vars phi2)))
         (setq pos (gethash ident2 (rule-based-cpd-identifiers phi1)))
         (when nil (and (= cycle* 3) (equal "INTENTION2751" (rule-based-cpd-dependent-id phi1)))
               (format t "~%identifier in episode: ~A~%position of identifier in schema: ~d" ident2 pos))
         (when pos
           (setq vvbm1 (gethash pos (rule-based-cpd-var-value-block-map phi1)))
           (setq nvvbm1 (gethash pos (rule-based-cpd-negated-vvbms phi1)))
           (setq sva1 (gethash pos (rule-based-cpd-set-valued-attributes phi1)))
           (setq svna1 (gethash pos (rule-based-cpd-set-valued-negated-attributes phi1)))
           (setq lower-vvbm1 (gethash pos (rule-based-cpd-lower-approx-var-value-block-map phi1)))
           (setq lower-nvvbm1 (gethash pos (rule-based-cpd-lower-approx-negated-vvbms phi1)))
           (setq vals1 (gethash pos (rule-based-cpd-var-values phi1)))
           (setq var2s (set-difference vvbm2 vvbm1 :key #'caar :test #'equal))
           (loop
             with var2
             for att-block in var2s
             ;;for count from (+ (cdaar (last vvbm1)) 1)
             do
	        (setq var2 (caar att-block))
                ;;(setq binding (cons (caar att-block) count))
		(setq binding (cons  (caar att-block) (cdar att-block)))
		(when nil (and (= cycle* 3) (equal "INTENTION2751" (rule-based-cpd-dependent-id phi1)))
                  (format t "~%schema vvbm: ~S~%episode vvbm: ~S~%var2s:~%~S~%var2: ~S~%binding:~%~S" vvbm1 vvbm2 var2s var2 binding))
                (setq vvbm1 (reverse (cons (list binding (make-hash-table)) (reverse vvbm1))))
                (setq nvvbm1 (reverse (cons (list binding (make-hash-table)) (reverse nvvbm1))))
                (setq sva1 (reverse (cons (list (cdr binding)) (reverse sva1))))
                (loop
                  for svna in svna1
                  for i from 0
                  collect (cons (cdr binding) svna) into new-svna
                  collect i into last-elem
                  finally
                     (setq svna1 (reverse (cons last-elem (reverse new-svna)))))
                (setq lower-vvbm1 (reverse (cons (list binding (make-hash-table)) (reverse lower-vvbm1))))
                (setq lower-nvvbm1 (reverse (cons (list binding (make-hash-table)) (reverse lower-nvvbm1))))
                (setq vals1 (reverse (cons (cdr binding) (reverse vals1))))
                (cond ((= pos 0)
                       (loop
                         with rule-condition
                         with new-rules = nil
                         for rule being the elements of (rule-based-cpd-rules phi1)
                         do
                            (setq new-rule (copy-cpd-rule rule))
                            (setf (gethash ident2 (rule-conditions new-rule)) (cdr binding))
                            (setf (rule-probability new-rule) 0)
                            (when nil (and (= cycle* 9) (equal "Y526" (rule-based-cpd-dependent-id phi1)))
                              (format t "~%new rule:~%~S~%existing rule:~%~S" new-rule rule))
                            (when (notany #'(lambda (r) (same-rule-p new-rule r phi1 phi1)) new-rules)
                              (setq new-rules (cons new-rule new-rules)))
                            (setq rule-condition (gethash ident2 (rule-conditions rule)))
                            (cond ((listp rule-condition)
                                   (loop
                                     for i from 0 to (- (aref (rule-based-cpd-cardinalities phi1) pos) 1)
                                     when (and (not (= i (cdr binding)))
                                              (if (second rule-condition) (not (= i (second rule-condition))) t))
                                       do
                                          (setq new-rule (copy-cpd-rule rule))
                                          (setf (gethash ident2 (rule-conditions new-rule)) i)
                                          (when (notany #'(lambda (r) (same-rule-p new-rule r phi1 phi1)) new-rules)
                                            (setq new-rules (cons new-rule new-rules)))))
                                  ((numberp rule-condition)
                                   (when (notany #'(lambda (r) (same-rule-p rule r phi1 phi1)) new-rules)
                                     (setq new-rules (cons rule new-rules)))))
                            (when nil (and (= cycle* 9) (equal "Y526" (rule-based-cpd-dependent-id phi1)))
                              (format t "~%new rules:~%~S" new-rules)
                              (break)
                              )
                         finally
                            (setf (rule-based-cpd-rules phi1) (make-array (length new-rules) :initial-contents new-rules)))
                       #|
                       (setq new-rule (make-rule :id (symbol-name (gensym "RULE-"))
                                                 :conditions (make-hash-table :test #'equal)
                                                 :probability 0
                                                 :count (rule-count (aref (rule-based-cpd-rules phi1) 0)) #|(rule-based-cpd-count phi1)|#))
                       (setf (gethash ident2 (rule-conditions new-rule)) (cdr binding))
                       (when nil (and (= cycle* 3) (equal "GO_HOLD925" (rule-based-cpd-dependent-id phi2)))
                       (format t "~%disambiguate against:~%~S" new-rule))
                       (multiple-value-bind (new-rules length)
                       (disambiguate-rules-against phi1 new-rule)
                       (when nil (and (= cycle* 3) (equal "GO_HOLD925" (rule-based-cpd-dependent-id phi2)))
                       (format t "~%new rules:~%~S" new-rules))
                       (setf (rule-based-cpd-rules phi1) (make-array length :initial-contents new-rules)))
                       |#
                       )
                      (t
                       (loop
                         with new-rules = (coerce (rule-based-cpd-rules phi1) 'list)
                         for i from 0 to 1
                         do
                            ;; change the count here to zero in the future. For now, because we require rules to have the same count,
                            ;; we're going to use the count of the first rule
                            (setq new-rule (make-rule :id (symbol-name (gensym "RULE-"))
                                                      :conditions (make-hash-table :test #'equal)
                                                      :block (make-hash-table);;(list (array-dimension (rule-based-cpd-rules phi1) 0))
                                                      :probability i
                                                      :count 0 #|(rule-count (aref (rule-based-cpd-rules phi1) 0))|#))
                            (setf (gethash (array-dimension (rule-based-cpd-rules phi1) 0) (rule-block new-rule))
                                  (array-dimension (rule-based-cpd-rules phi1) 0))
                            (cond ((= i 0)
                                   #|
                                   (setf (gethash (rule-based-cpd-dependent-id phi1) (rule-conditions new-rule)) (list 'not 0))
                                   (setf (gethash ident2 (rule-conditions new-rule)) (cdr binding))
                                   (setq new-rules (cons new-rule new-rules))
                                   |#
                                   (loop
                                     with loop-rule
                                     for vvb in (gethash 0 (rule-based-cpd-var-value-block-map phi1))
                                     for j from 0
                                     when (not (= j 0))
                                       do
                                          (setq loop-rule (copy-cpd-rule new-rule))
                                          (setf (gethash (rule-based-cpd-dependent-id phi1) (rule-conditions loop-rule)) j)
                                          (setf (gethash ident2 (rule-conditions loop-rule)) (cdr binding))
                                          (setq new-rules (cons loop-rule new-rules))))
                                  (t
                                   (setf (gethash (rule-based-cpd-dependent-id phi1) (rule-conditions new-rule)) 0)
                                   (setf (gethash ident2 (rule-conditions new-rule)) (cdr binding))
                                   (setq new-rules (cons new-rule new-rules))))                            
                         #|
                            (when nil (and (= cycle* 3) (equal "GO_HOLD925" (rule-based-cpd-dependent-id phi2)))
                            (format t "~%disambiguate against:~%~S" new-rule))
                            (multiple-value-bind (new-rules length)
                            (disambiguate-rules-against phi1 new-rule)
                            (when nil (and (= cycle* 3) (equal "GO_HOLD925" (rule-based-cpd-dependent-id phi2)))
                            (format t "~%new rules:~%~S" new-rules))
                            (setf (rule-based-cpd-rules phi1) (make-array length :initial-contents new-rules)))
                         |#
                            (setf (rule-based-cpd-rules phi1) (make-array (length new-rules) :initial-contents new-rules)))))
                (setf (gethash pos (rule-based-cpd-var-value-block-map phi1)) vvbm1)
                (setf (gethash pos (rule-based-cpd-negated-vvbms phi1)) nvvbm1)
                (setf (gethash pos (rule-based-cpd-set-valued-attributes phi1)) sva1)
                (setf (gethash pos (rule-based-cpd-set-valued-negated-attributes phi1)) svna1)
                (setf (gethash pos (rule-based-cpd-lower-approx-var-value-block-map phi1)) lower-vvbm1)
                (setf (gethash pos (rule-based-cpd-lower-approx-negated-vvbms phi1)) lower-nvvbm1)
                (setf (gethash pos (rule-based-cpd-var-values phi1)) vals1)
                (setf (rule-based-cpd-cardinalities phi1) (get-var-cardinalities (rule-based-cpd-var-value-block-map phi1)))
                (setf (rule-based-cpd-step-sizes phi1) (generate-cpd-step-sizes (rule-based-cpd-cardinalities phi1)))
                (when nil (and (= cycle* 3) (equal "GO_HOLD885" (rule-based-cpd-dependent-id phi1)))
                  (format t "~%updated schema:~%~S" phi1)
                  (break))))
      finally
         (when nil (equal "GO_HOLD1195" (rule-based-cpd-dependent-id phi1))
               (format t "~%updated schema:~%~S" phi1))
         (return phi1)))

#| Transform cpd rules to match transformed domain |#

;; cpd = conditional probability distribution with rules to modify
;; attribute = target attribute
;; transform = conses where each cons maps old value to new value for attribute
(defun cpd-transform-rule-conditions (cpd attribute transform additional-rules)
  (when nil (and (= cycle* 3) (equal "GO_HOLD925" (rule-based-cpd-dependent-id cpd)))
    (format t "~%rules to transform:~%~S~%on attribute:~%~S~%using transform:~%~S" (rule-based-cpd-rules cpd) attribute transform))
  (loop
    with condition and map
    for rule being the elements of (rule-based-cpd-rules cpd)
    for count from 1
    do
       (setq condition (gethash attribute (rule-conditions rule)))
       (when condition
         (if (numberp condition)
             (setq map (gethash condition transform))
             (setq map (gethash (second condition) transform)))
         (when nil (and (= cycle* 3) (equal "GO_HOLD925" (rule-based-cpd-dependent-id cpd)))
           (format t "~%rule:~%~S~%condition:~%~S~%transform:~%~S~%map:~%~S" rule condition transform map))
         (when map
           (if (listp (gethash attribute (rule-conditions rule)))
               (setf (gethash attribute (rule-conditions rule)) (list 'not map))
               (setf (gethash attribute (rule-conditions rule)) map))
           (when nil (and (= cycle* 3) (equal "GO_HOLD925" (rule-based-cpd-dependent-id cpd)))
                 (format t "~%updated rule:~%~S" rule))))
       (setq additional-rules (reverse (cons rule (reverse additional-rules))))
    ;;collect rule into new-rules
    finally
       (setf (rule-based-cpd-rules cpd)
             (make-array (length additional-rules)
                         :initial-contents (reverse additional-rules)))
       #|
       (when nil (= cycle* 15) nil
         (format t "~%additional rules for cpd:~%~S" additional-rules))
       (setf (rule-based-cpd-rules cpd) (make-array count :initial-contents new-rules))
       (loop
         with len = count
         for add-rule in additional-rules
         do
            (multiple-value-bind (n-r length)
                (disambiguate-rules-against cpd add-rule)
              (setf (rule-based-cpd-rules cpd) (make-array length :initial-contents n-r))))
       (when (and (= cycle* 13) (equal "STATE8417" (rule-based-cpd-dependent-id cpd)))
         (format t "~%transformed rules:~%~S" (rule-based-cpd-rules cpd)))
       |#
       (return (rule-based-cpd-rules cpd))))

#| Update the episode domain and vvbm indexes to be consistent with schema |#

;; phi1 = conditional probability density from pattern
;; phi2 = conditional probability density from base
(defun cpd-transform-episode-domain (phi1 phi2)
  (when nil (and (= cycle* 5) (equal "ACTION601" (rule-based-cpd-dependent-id phi1)))
    (format t "~%~%episode:~%~S" phi1)
    (format t "~%~%trasnforming episode domain:~%~S~%with~%~S" (rule-based-cpd-var-value-block-map phi1) (rule-based-cpd-var-value-block-map phi2)))
  (loop
    with transform
    with vvbms1 and nvvbms1
    with vvbms2 and nvvbms2 and sva2 and svna2 and vals2 and lower-vvbms2 and lower-nvvbms2
    with pos2 and binding2 and len and additional-rules
    for ident being the hash-keys of (rule-based-cpd-identifiers phi1)
      using (hash-value pos1)
    do
       (setq additional-rules nil)
       (setq transform (make-hash-table))
       (setq vvbms1 (gethash pos1 (rule-based-cpd-var-value-block-map phi1)))
       (setq nvvbms1 (gethash pos1 (rule-based-cpd-negated-vvbms phi1)))
       (setq pos2 (gethash ident (rule-based-cpd-identifiers phi2)))
       (when pos2
         (setq vvbms2 (gethash pos2 (rule-based-cpd-var-value-block-map phi2)))
         (setq nvvbms2 (gethash pos2 (rule-based-cpd-negated-vvbms phi2)))
         (setq sva2 (gethash pos2 (rule-based-cpd-set-valued-attributes phi2)))
         (setq svna2 (gethash pos2 (rule-based-cpd-set-valued-negated-attributes phi2)))
         (setq vals2 (gethash pos2 (rule-based-cpd-var-values phi2)))
	 ;; delete this loop. Not necessary.
	 (loop
           for binding1 in (rest vvbms1)
           do
              ;;(setq nbinding1 (car (last nvvbms1)))
              (setq binding2 (car (assoc (caar binding1) vvbms2 :key #'car :test #'equal)))
              (when nil (and (= cycle* 5) (equal "ACTION601" (rule-based-cpd-dependent-id phi1)))
                (format t "~%ident: ~S~%position in phi2: ~d~%vvbms1:~%~S~%vvbms2:~%~S~%binding1: ~S~%bingding2: ~S" ident pos2 vvbms1 vvbms2 binding1 binding2))
	      ;; revist this cond when structure mapping works. I may not need it with the changes i made to subst-cpd
	      (cond (binding2
                     ;;(setf (gethash (cdar binding1) transform) (cdr binding2))
                     #|
                     (setf (gethash pos1 (rule-based-cpd-var-value-block-map phi1))
                     (reverse (cons (list (cons (caar binding1) (cdr binding2)) (copy-list (second binding1)))
                     (reverse (butlast (gethash pos1 (rule-based-cpd-var-value-block-map phi1)))))))
                     (setf (gethash pos1 (rule-based-cpd-negated-vvbms phi1))
                     (reverse (cons (list (cons (caar nbinding1) (cdr binding2)) (copy-list (second nbinding1)))
                     (reverse (butlast (gethash pos1 (rule-based-cpd-negated-vvbms phi1)))))))
                     |#)
                    (t
		     (format t "~%this should never happen")
		     (break "this should never happen")
		     (setq len (aref (rule-based-cpd-cardinalities phi2) pos2))
                     (setf (gethash (cdar binding1) transform) len)
                     #|
                     (setf (gethash pos1 (rule-based-cpd-var-value-block-map phi1))
                     (reverse (cons (list (cons (caar binding1) len) (copy-list (second binding1)))
                     (reverse (butlast (gethash pos1 (rule-based-cpd-var-value-block-map phi1)))))))
                     (setf (gethash pos1 (rule-based-cpd-negated-vvbms phi1))
                     (reverse (cons (list (cons (caar nbinding1) len) (copy-list (second nbinding1)))
                     (reverse (butlast (gethash pos1 (rule-based-cpd-negated-vvbms phi1)))))))
                     |#)))
         (when nil (and (= cycle* 5) (equal "ACTION601" (rule-based-cpd-dependent-id phi1)))
           (format t "~%vvbms1:~%~S~%vvbms2:~%~S" vvbms1 vvbms2)
           (break))
         (loop
           with new-rule
           for vvbm in vvbms2
           when (not (member (caar vvbm) vvbms1 :key #'caar :test #'equal))
             do
                (when nil (and (= cycle* 5) (equal "ACTION601" (rule-based-cpd-dependent-id phi1)))
                  (format t "~%no rule for ~S in ~S" (caar vvbm) (rule-based-cpd-rules phi1)))
                (cond ((= pos1 0)
                       (loop
                         for rule being the elements of (rule-based-cpd-rules phi1)
                         do
                            (setq new-rule (copy-cpd-rule rule))
                            (setf (gethash ident (rule-conditions new-rule)) (cdar vvbm))
                            (setf (rule-probability new-rule) 0)
                            (when (notany #'(lambda (r) (same-rule-p new-rule r phi1 phi1)) additional-rules)
                              (setq additional-rules (cons new-rule additional-rules)))
                            (when (not (gethash ident (rule-conditions rule)))
                              ;;(setf (gethash ident (rule-conditions rule)) (list 'not (cdar vvbm)))
                              (loop
                                for i from 0 to (- (aref (rule-based-cpd-cardinalities phi1) pos1) 1)
                                when (not (= i (cdar vvbm)))
                                  do
                                     (setq new-rule (copy-cpd-rule rule))
                                     (setf (gethash ident (rule-conditions new-rule)) i)
                                     (when (notany #'(lambda (r) (same-rule-p new-rule r phi1 phi1)) additional-rules)
                                       (setq additional-rules (cons new-rule additional-rules))))))
                       #|
                       (setq new-rule (make-rule :id (symbol-name (gensym "RULE-"))
                                                 :conditions (make-hash-table :test #'equal)
                                                 :probability 0
                                                 :count (rule-count (aref (rule-based-cpd-rules phi1) 0))))
                       (setf (gethash ident (rule-conditions new-rule)) (cdar vvbm))
                       (setq additional-rules (cons new-rule additional-rules))
                       |#)
                      (t
                       (loop
                         for i from 0 to 1
                         do
                            (setq new-rule (make-rule :id (symbol-name (gensym "RULE-"))
                                                      :conditions (make-hash-table :test #'equal)
                                                      :probability i
                                                      :count 0 #|(rule-based-cpd-count phi1)|#))
                            (cond ((= i 0)
                                   #|
                                   (setf (gethash (rule-based-cpd-dependent-id phi1) (rule-conditions new-rule)) (list 'not 0))
                                   (setf (gethash ident (rule-conditions new-rule)) (cdar vvbm))
                                   (setq additional-rules (cons new-rule additional-rules))
                                   |#
                                   (loop
                                     with loop-rule
                                     for vvb in (gethash 0 (rule-based-cpd-var-value-block-map phi1))
                                     for j from 0
                                     when (not (= j 0))
                                       do
                                          (setq loop-rule (copy-cpd-rule new-rule))
                                          (setf (gethash (rule-based-cpd-dependent-id phi1) (rule-conditions loop-rule)) j)
                                          (setf (gethash ident (rule-conditions loop-rule)) (cdar vvbm))
                                          (setq additional-rules (cons loop-rule additional-rules))))
                                  (t
                                   (setf (gethash (rule-based-cpd-dependent-id phi1) (rule-conditions new-rule)) 0)
                                   (setf (gethash ident (rule-conditions new-rule)) (cdar vvbm))
                                   (setq additional-rules (cons new-rule additional-rules)))))))
           #|finally
              (setf (gethash pos1 (rule-based-cpd-var-value-block-map phi1)) vvbms2)
              (setf (aref (rule-based-cpd-cardinalities phi1) pos1) (aref (rule-based-cpd-cardinalities phi2) pos2))
              |#
           )
         (when nil (and (= cycle* 5) (equal "ACTION601" (rule-based-cpd-dependent-id phi1)))
           (format t "~%additional rules:~%~S~%final transform:~%~S" (reverse additional-rules) transform))
         (setf (rule-based-cpd-rules phi1)
               (cpd-transform-rule-conditions phi1 ident transform (reverse additional-rules)))
         (setf (gethash pos1 (rule-based-cpd-var-value-block-map phi1)) vvbms2)
         (setf (gethash pos1 (rule-based-cpd-negated-vvbms phi1)) nvvbms2)
         (setf (gethash pos1 (rule-based-cpd-set-valued-attributes phi1)) sva2)
         (setf (gethash pos1 (rule-based-cpd-set-valued-negated-attributes phi1)) svna2)
         (setf (gethash pos1 (rule-based-cpd-lower-approx-var-value-block-map phi1)) lower-vvbms2)
         (setf (gethash pos1 (rule-based-cpd-lower-approx-negated-vvbms phi1)) lower-nvvbms2)
         (setf (gethash pos1 (rule-based-cpd-var-values phi1)) vals2)
         (setf (aref (rule-based-cpd-cardinalities phi1) pos1) (aref (rule-based-cpd-cardinalities phi2) pos2))
         (when nil (and (= cycle* 5) (equal "ACTION601" (rule-based-cpd-dependent-id phi1)))
           (format t "~%updated-cpd vvbms:~%~S~%updated cpd cardinalities:~%~S" (rule-based-cpd-var-value-block-map phi1) (rule-based-cpd-cardinalities phi1))
           (break))
         ))
  (setf (rule-based-cpd-step-sizes phi1)
        (generate-cpd-step-sizes (rule-based-cpd-cardinalities phi1)))
  phi1)

#| Update episode domains with contents from schema |#

;; phi1 = conditional probability density from pattern
;; phi2 = conditional probability density from base
;; attribute = identifier in phi1 to modify
;; id-pos = identifier position in phi1 to modify
(defun cpd-update-episode-domain (phi1 phi2 attribute id-pos)
  (let (original-binding transform vvbms1 vvbms2 binding2 card ident additional-rules)
    (setq ident (rule-based-cpd-dependent-id phi2))
    (setq transform (make-hash-table))
    (setq vvbms1 (copy-list (gethash id-pos (rule-based-cpd-var-value-block-map phi1))))
    (setq original-binding (caar (last vvbms1)))
    (setf (gethash id-pos (rule-based-cpd-var-value-block-map phi1))
          (gethash 0 (rule-based-cpd-var-value-block-map phi2)))
    (setf (gethash id-pos (rule-based-cpd-negated-vvbms phi1))
          (gethash 0 (rule-based-cpd-negated-vvbms phi2)))
    (setf (aref (rule-based-cpd-cardinalities phi1) id-pos)
          (aref (rule-based-cpd-cardinalities phi2) 0))
    (setq vvbms2 (gethash id-pos (rule-based-cpd-var-value-block-map phi1)))
    (setq binding2 (car (assoc (car original-binding) vvbms2
                               :key #'car
                               :test #'equal)))
    (when nil (equal "INTENTION1278" (rule-based-cpd-dependent-id phi1))
      (format t "~%original binding:~%~S~%binding2:~%~S" original-binding binding2))
    (cond ((not (= (cdr binding2) (cdr original-binding)))
           (setf (gethash (cdr original-binding) transform) (cdr binding2)))
          ((null binding2)
           (setq card (aref (rule-based-cpd-cardinalities phi1) id-pos))
           (setf (gethash (cdr original-binding) transform) card)
           (setf (gethash id-pos (rule-based-cpd-var-value-block-map phi1))
                 (reverse (cons (list (cons (car original-binding) card) nil)
                                (reverse (gethash id-pos (rule-based-cpd-var-value-block-map phi1))))))
           (setf (gethash id-pos (rule-based-cpd-negated-vvbms phi1))
                 (reverse (cons (list (cons (car original-binding) card) nil)
                                (reverse (gethash id-pos (rule-based-cpd-negated-vvbms phi1))))))
           ;;(setf (aref (rule-based-cpd-cardinalities phi1) id-pos) (+ (aref (rule-based-cpd-cardinalities phi1) id-pos) 1))
           ))
    (when nil (equal "INTENTION1278" (rule-based-cpd-dependent-id phi1))
      (format t "~%original vvbms:~%~S~%new vvbms:~%~S" vvbms1 vvbms2))
    (loop
      with new-rule
      for vvbm in vvbms2
      when (not (member (caar vvbm) vvbms1 :key #'caar :test #'equal))
        do
           (when nil (equal "INTENTION1278" (rule-based-cpd-dependent-id phi1))
             (format t "~%vvbm: ~S not in vvbms1" (caar vvbm)))
           (cond ((= id-pos 0)
                  (setq new-rule (make-rule :id (symbol-name (gensym "RULE-"))
                                            :conditions (make-hash-table :test #'equal)
                                            :probability 0
                                            :count (rule-based-cpd-count phi1)))
                 (setf (gethash attribute (rule-conditions new-rule)) (cdar vvbm))
                 (setq additional-rules (cons new-rule additional-rules)))
                 (t
                  (loop
                    for i from 0 to 1
                    do
                       (setq new-rule (make-rule :id (symbol-name (gensym "RULE-"))
                                                 :conditions (make-hash-table :test #'equal)
                                                 :probability i
                                                 :count (rule-based-cpd-count phi1)))
                       (cond ((= i 0)
                              (setf (gethash (rule-based-cpd-dependent-id phi1) (rule-conditions new-rule)) (list 'not 0))
                              (setf (gethash attribute (rule-conditions new-rule)) (cdar vvbm))
                              (setq additional-rules (cons new-rule additional-rules)))
                             (t
                              (setf (gethash (rule-based-cpd-dependent-id phi1) (rule-conditions new-rule)) 0)
                              (setf (gethash attribute (rule-conditions new-rule)) (cdar vvbm))
                              (setq additional-rules (cons new-rule additional-rules))))))))
    (setf (rule-based-cpd-cardinalities phi1)
          (generate-cpd-cardinalities (rule-based-cpd-var-value-block-map phi1)))
    (setf (rule-based-cpd-step-sizes phi1)
          (generate-cpd-step-sizes (rule-based-cpd-cardinalities phi1)))
    (setf (rule-based-cpd-rules phi1)
          (cpd-transform-rule-conditions (rule-based-cpd-rules phi1) ident transform additional-rules))
    phi1))

#| Update variable value map of variables in cpd that have already been merged |#

;; cpd = conditional probability density
;; bindings = variable bindings
;; new-nodes = list of updated conditional probability distributions
(defun cpd-update-existing-vvms (cpd bindings new-nodes)
  (loop
    for identifier being the hash-keys of (rule-based-cpd-identifiers cpd)
    with p-cpd and trunc-p-cpd and cpd-copy
    with trunc-idents and trunc-vars and trunc-types and trunc-vvbm and trunc-nvvbm and trunc-cids and trunc-qvars
    with trunc-sva and trunc-svna and trunc-lower-approx-var-value-block-map and trunc-lower-approx-negated-vvbms
    do
       (setq p-cpd (get-cpd-by-id identifier new-nodes))
       (when p-cpd
         (setq trunc-idents (make-hash-table :test #'equal))
         (setf (gethash (rule-based-cpd-dependent-id p-cpd) trunc-idents) 0)
         (setq trunc-vars (make-hash-table))
         (setf (gethash 0 trunc-vars)
               (gethash 0 (rule-based-cpd-vars p-cpd)))
         (setq trunc-types (make-hash-table))
         (setf (gethash 0 trunc-types)
               (gethash 0 (rule-based-cpd-types p-cpd)))
         (setq trunc-vvbm (make-hash-table))
         (setf (gethash 0 trunc-vvbm)
               (gethash 0 (rule-based-cpd-var-value-block-map p-cpd)))
         (setq trunc-nvvbm (make-hash-table))
         (setf (gethash 0 trunc-nvvbm)
               (gethash 0 (rule-based-cpd-negated-vvbms p-cpd)))
         (setq trunc-sva (make-hash-table))
         (setf (gethash 0 trunc-sva)
               (gethash 0 (rule-based-cpd-set-valued-attributes p-cpd)))
         (setq trunc-svna (make-hash-table))
         (setf (gethash 0 trunc-svna)
               (gethash 0 (rule-based-cpd-set-valued-negated-attributes p-cpd)))
         (setq trunc-lower-approx-var-value-block-map (make-hash-table))
         (setf (gethash 0 trunc-lower-approx-var-value-block-map)
               (gethash 0 (rule-based-cpd-lower-approx-var-value-block-map p-cpd)))
         (setq trunc-lower-approx-negated-vvbms (make-hash-table))
         (setf (gethash 0 trunc-lower-approx-negated-vvbms)
               (gethash 0 (rule-based-cpd-lower-approx-negated-vvbms p-cpd)))
         (setq trunc-cids (make-hash-table))
         (setf (gethash 0 trunc-cids)
               (gethash 0 (rule-based-cpd-concept-ids p-cpd)))
         (setq trunc-qvars (make-hash-table))
         (setf (gethash 0 trunc-qvars)
               (gethash 0 (rule-based-cpd-qualified-vars p-cpd)))
         (setq trunc-p-cpd
               (make-rule-based-cpd
                :dependent-id (rule-based-cpd-dependent-id p-cpd)
                :identifiers trunc-idents
                :vars trunc-vars
                :types trunc-types
                :var-value-block-map trunc-vvbm
                :negated-vvbms trunc-nvvbm
                :set-valued-attributes trunc-sva
                :set-valued-negated-attributes trunc-svna
                :lower-approx-var-value-block-map trunc-lower-approx-var-value-block-map
                :lower-approx-negated-vvbms trunc-lower-approx-negated-vvbms
                :concept-ids trunc-cids
                :qualified-vars trunc-qvars))
         (when nil (equal "GO_HOLD27" (rule-based-cpd-dependent-id cpd))
               (format t "~%marginalized p-cpd:~%~S" p-cpd))
         ;;(format t "~%marginalized p-cpd:~%~A" p-cpd)
         ;;(format t "~%marginalized p-cpd assignments:")
         ;;(maphash #'print-hash-entry (cpd-assignments p-cpd))
         ;;(format t "~%marginalized p-cpd counts:")
         ;;(maphash #'print-hash-entry (cpd-counts p-cpd))
	 (when nil (and (= cycle* cycle*) (equal "TURN_LEFT12794" (rule-based-cpd-dependent-id cpd)))
	   (format t "~%updating schema vvm.~%schema before update:~%~S~%trunc parent cpd:~%~S" cpd trunc-p-cpd))
	 (setq cpd-copy (subst-cpd cpd trunc-p-cpd bindings))
	 (when nil (and (= cycle* cycle*) (equal "TURN_LEFT12794" (rule-based-cpd-dependent-id cpd)))
	   (format t "~%schema after update:~%~S" cpd-copy))
	   
	 (setq cpd (cpd-update-schema-domain cpd-copy trunc-p-cpd))
	 (when nil (and (= cycle* cycle*) (equal "TURN_LEFT12794" (rule-based-cpd-dependent-id cpd)))
	   (format t "~%~S~%done." cpd)
	   (break))
	 ;;(sb-ext:gc :full t)
         ;;(format t "~%new-phi:~%~A" phi)
         ;;(format t "~%new-phi assignments:")
         ;;(maphash #'print-hash-entry (cpd-assignments phi))
         ))
  cpd)

#| Reinitialize attribute blocks and concept blocks |#

;; cpd = conditional probability distribution
(defun reset-attribute-and-concept-blocks (cpd)
  (setf (rule-based-cpd-concept-blocks cpd) (make-hash-table))
  (loop
    with new-vvbms = (make-hash-table) and new-nvvbms = (make-hash-table)
    for vvbms being the hash-values of (rule-based-cpd-var-value-block-map cpd)
      using (hash-key idx)
    for nvvbms being the hash-values of (rule-based-cpd-negated-vvbms cpd)
    do
       (setf (gethash idx new-vvbms) (mapcar #'(lambda (vvbm)
                                                 (list (first vvbm) (make-hash-table)))
                                             vvbms))
       (setf (gethash idx new-nvvbms) (mapcar #'(lambda (nvvbm)
                                                  (list (first nvvbm) (make-hash-table)))
                                              nvvbms))
    finally
       (setf (rule-based-cpd-var-value-block-map cpd) new-vvbms)
       (setf (rule-based-cpd-negated-vvbms cpd) new-nvvbms))
  cpd)

(defun get-compatibilities (cpd rules)
  (loop
    with compatibilities = (make-hash-table)
    with r1
    for i from 0 to (- (array-dimension rules 0) 1)
    do
       (setq r1 (aref rules i))
       (loop
         with r2
         for j from 0 to (- (array-dimension rules 0) 1)
         when (not (= i j)) do
           (setq r2 (aref rules j))
           (when (compatible-rule-p r1 r2 cpd cpd)
             (when (null (gethash i compatibilities))
               (setf (gethash i compatibilities) (make-hash-table)))
             (setf (gethash j (gethash i compatibilities)) t)
             (when (null (gethash j compatibilities))
               (setf (gethash j compatibilities) (make-hash-table)))
             (setf (gethash i (gethash j compatibilities)) t)))
    finally
       (return compatibilities)))

#| For a given attribute value, get the set of assignments that satify it |#

;; value = value from rule condition
;; cpd = conditional probability distribution
;; idx = cpd index of attribute in condition
(defun get-set-valued-block-from-attribute-value (value cpd idx)
  (cond ((numberp value)
         (nth value (gethash idx (rule-based-cpd-set-valued-attributes cpd))))
        ((null value)
         (gethash idx (rule-based-cpd-var-values cpd)))
        ((listp value)
         (nth (second value) (gethash idx (rule-based-cpd-set-valued-negated-attributes cpd))))))

#| Supply the lower approximation for each attribute-value in CPD |#

;; cpd = conditional probability distribution
;; flag = accessor flag for extracting vvbms or negated vvbms
;; features = hash table of identifiers and their positions in the CPD that need updating
(defun cpd-add-lower-approximations (cpd flag &key features)
  (when nil (equal "INTENTION2406" (rule-based-cpd-dependent-id cpd))
        (format t "~%~%updating lower approximations for cpd:~%~S.~%flag: ~S" cpd flag))
  (when (null features)
    (setq features (rule-based-cpd-identifiers cpd)))
  (loop
    with vvbm and nvvbm and lower-approx
    for ident being the hash-keys of features
      using (hash-value idx)
    do
       (cond ((equal "VVBM" flag)
              (setq vvbm (gethash idx (rule-based-cpd-var-value-block-map cpd)))
              (setq nvvbm (gethash idx (rule-based-cpd-negated-vvbms cpd)))
              (setq lower-approx (gethash idx (rule-based-cpd-lower-approx-var-value-block-map cpd))))
             ((equal "NEGATED" flag)
              (setq vvbm (gethash idx (rule-based-cpd-negated-vvbms cpd)))
              (setq nvvbm (gethash idx (rule-based-cpd-var-value-block-map cpd)))
              (setq lower-approx (gethash idx (rule-based-cpd-lower-approx-negated-vvbms cpd))))
             (t
              (error "Unsupported flag argument")))
       #|
       (when nil (equal "INTENTION2406" (rule-based-cpd-dependent-id cpd))
             (format t "~%~%identifier:~%~S~%vvbm:~%~S~%current lower approx:~%~S~%characteristic sets:~%~S~%characteristic sets values" ident vvbm lower-approx c-set a-x-sets))
    |#
       (loop
         with val
         for vvb in vvbm
         for nvvb in nvvbm
         for lower in lower-approx
         for i from 0
         do
            ;;(setf (cadr lower) (set-difference (second vvb) (second nvvb)))
            (setf (cadr lower) (block-difference (second vvb) (second nvvb) :output-hash-p t))
         #|
            (when nil (equal "INTENTION2406" (rule-based-cpd-dependent-id cpd))
            (format t "~%ident: ~S~%Getting lower approximation for vvb:~%~S~%current lower approximation:~%~S" ident vvb lower))
            (when (equal "NEGATED" flag)
            (setq val (list 'not val)))
            (setq a-bigx-set (get-set-valued-block-from-attribute-value val cpd idx))
         |#
            (when nil (equal "INTENTION2406" (rule-based-cpd-dependent-id cpd))
                  (format t "~%value block:~%~S~%updated lower approximation:~%~S" (cadr vvb) (cadr lower)))))
  cpd)

#| Get local characteristic sets for each variable in CPD |#

;; cpd = conditional probability distribution
(defun cpd-add-characteristic-sets (cpd)
  (when nil
    (format t "~%~%updating cpd characteristic sets"))
  (loop
    for ident being the hash-keys of (rule-based-cpd-identifiers cpd)
      using (hash-value idx)
    do
       (setf (gethash idx (rule-based-cpd-characteristic-sets cpd))
             (make-array (array-dimension (rule-based-cpd-rules cpd) 0)))
       (setf (gethash idx (rule-based-cpd-characteristic-sets-values cpd))
             (make-array (array-dimension (rule-based-cpd-rules cpd) 0)))
       (loop
         for i from 0 to (- (array-dimension (rule-based-cpd-rules cpd) 0) 1)
         do
            (when nil
              (format t "~%~%ident: ~S~%i: ~d" ident i))
            (loop
              with c-set and a-x-set
              for vvb in (gethash idx (rule-based-cpd-var-value-block-map cpd))
              when (member i (cadr vvb))
                do
                   (when nil
                     (format t "~%vvb:~%~S~%c-set before update:~%~S" vvb c-set))
                   (setq c-set (union c-set (cadr vvb)))
                   (setq a-x-set (union (nth (cdar vvb) (gethash idx (rule-based-cpd-set-valued-attributes cpd))) a-x-set))
                   (when nil
                     (format t "~%c-set after update:~%~S" c-set))
              finally
                 (setf (aref (gethash idx (rule-based-cpd-characteristic-sets cpd)) i)
                       c-set)
                 (setf (aref (gethash idx (rule-based-cpd-characteristic-sets-values cpd)) i)
                       a-x-set)
                 (when nil
                   (format t "~%identifier: ~S~%index: ~d~%new characteristic set:~%~S" ident i c-set)))))
  (cpd-add-lower-approximations (cpd-add-lower-approximations cpd "VVBM")
                                "NEGATED"))

#| Change cpd rules, attribute blocks, and concept-blocks |#

;; cpd = conditional probability to modify
;; new-rules = array of new rules to replace current rules
(defun update-cpd-rules (cpd new-rules &key (disambiguate-p nil) (check-uniqueness nil))
  (setf (rule-based-cpd-rules cpd) new-rules)
  ;;(check-cpd cpd :check-uniqueness nil)
  (when nil (and (= cycle* 5) (equal "HAND565" (rule-based-cpd-dependent-id cpd)))
        (format t "~%~%updating cpd rules for cpd:~%~S" cpd)
        (break))
  (setq cpd (reset-attribute-and-concept-blocks cpd))
  (loop
    for rule being the elements of (rule-based-cpd-rules cpd)
    for i from 0
    do
       (when nil (and (= cycle* 5) (equal "HAND565" (rule-based-cpd-dependent-id cpd)))
         (format t "~%~%Rule: ~S~%index: ~d" rule i))
       (loop
         with val and nvvbm and vvbm and #|lower-vvbm and lower-nvvbm and c-sets and|# card
         for attribute being the hash-keys of (rule-based-cpd-identifiers cpd)
         using (hash-value idx)
         do
            (setq card (aref (rule-based-cpd-cardinalities cpd) idx))
            (setq vvbm (gethash idx (rule-based-cpd-var-value-block-map cpd)))
            (setq nvvbm (gethash idx (rule-based-cpd-negated-vvbms cpd)))
            ;;(setq lower-vvbm (gethash idx (rule-based-cpd-lower-approx-var-value-block-map cpd)))
            ;;(setq lower-nvvbm (gethash idx (rule-based-cpd-lower-approx-negated-vvbms cpd)))
            ;;(setq c-sets (gethash idx (rule-based-cpd-characteristic-sets cpd)))
            (setq val (gethash attribute (rule-conditions rule)))
            (when nil (and (= cycle* 5) (equal "HAND565" (rule-based-cpd-dependent-id cpd)))
                  (format t "~%Attribute: ~S~%value: ~S~%vvbm:~%~S" attribute val vvbm)
                  (break))
            (cond ((null val)
                   (loop
                     for vvb in vvbm
                     for nvvb in nvvbm do
                       (setf (gethash i (second vvb)) i)
                       (setf (gethash i (second nvvb)) i)
                     ))
                  ((numberp val)
                   (setf (gethash i (second (nth val vvbm))) i)
                   (loop
                     for nvvb in nvvbm
                     when (not (= val (cdar nvvb))) do
                       (setf (gethash i (second nvvb)) i)))
                  ((listp val)
                   (loop
                     with sva1 = (get-set-valued-block-from-attribute-value val cpd idx) and sva2
                     for nvvb in nvvbm
                     do
                        (setq sva2 (get-set-valued-block-from-attribute-value (list 'not (cdar nvvb)) cpd idx))
                     when (intersection sva1 sva2)
                       do
                          (setf (gethash i (second nvvb)) i))
                   (loop
                     for vvb in vvbm
                     when (not (= (second val) (cdar vvb))) do
                       (setf (gethash i (second vvb)) i))))
            (when nil (and (= cycle* 5) (equal "HAND565" (rule-based-cpd-dependent-id cpd)))
              (format t "~%Updated vvbms:~%~S~%Updated nvvbms:~%~S" vvbm nvvbm)))
       (when nil (and (= cycle* 5) (equal "HAND565" (rule-based-cpd-dependent-id cpd)))
         (break))
       (when (not (gethash (rule-probability rule)
                           (rule-based-cpd-concept-blocks cpd)))
         (setf (gethash (rule-probability rule)
                        (rule-based-cpd-concept-blocks cpd))
               (make-hash-table)))
       (when (not (gethash (rule-count rule)
                           (gethash (rule-probability rule)
                                    (rule-based-cpd-concept-blocks cpd))))
         (setf (gethash (rule-count rule)
                        (gethash (rule-probability rule)
                                 (rule-based-cpd-concept-blocks cpd)))
               (make-hash-table)))
       (setf (gethash i
                      (gethash (rule-count rule)
                               (gethash (rule-probability rule)
                                        (rule-based-cpd-concept-blocks cpd))))
             i))
  (when (= (hash-table-count (rule-based-cpd-concept-blocks cpd)) 0)
    (format t "~%check concept blocks:~%~S" cpd)
    (error))
  ;;(cpd-add-characteristic-sets cpd)
  ;;(cpd-add-lower-approximations (cpd-add-lower-approximations cpd "VVBM")
  ;;"NEGATED")
  cpd)

#| Compute T(G) |#

;; cpd = conditional probability distribution
;; g = goal block to cover
;; concept-block = block of the concept
;; rule = rule in rule-based cpd
;; universe = universe of all cases
(defun get-tog (cpd g concept-block rule universe &key (certain-p nil) &aux (forbidden-attributes))
  (setq forbidden-attributes (make-hash-table :test #'equal))
  (labels ((three-way-hash-intersection (h1 h2 h3)
             (loop
               with result = (make-hash-table :size (ceiling (* 1.3 (hash-table-count h1))) :test #'equal)
               for key being the hash-keys of h1
               when (and (gethash key h2) (gethash key h3))
                 do
                    (setf (gethash key result) key)
               finally (return result)))
           (get-tog-for-vvbms (ident vvbms certain-p new-g att-blocks &key (negate-p))
             (loop
               with intersection and conflicts and condition and att-block and redundancies
               with all-conflicts and all-redundancies and all-partial-coverings
               for value-block in vvbms
               do
                  (setq condition nil)
                  (setq conflicts nil)
                  (setq redundancies nil)
                  (setq all-conflicts nil)
                  (setq all-redundancies nil)
                  (setq all-partial-coverings nil)
                  (setq att-block (second value-block))
                  (setq intersection (hash-intersection att-block new-g :output-hash-p t))
                  (when (and (> (hash-table-count intersection) 0)
                             (not (= (hash-table-count att-block) (hash-table-count universe))))
                    (when (and (= cycle* 2) (equal "HOLDING1182" (rule-based-cpd-dependent-id cpd)))
                          (format t "~%~%value-block: ~S~%att-block: ~S~%rule:~%~S" value-block att-block rule))
                    (cond ((> (hash-table-count (rule-block rule)) 0) ;;(rule-block rule)
                           (cond (certain-p
                                  (setq all-conflicts (hash-intersection (rule-certain-block rule) (rule-avoid-list rule) :output-hash-p t))
                                  ;;(setq all-redundancies (hash-intersection (rule-certain-block rule) (rule-redundancies rule) :output-hash-p t))
                                  ;;(setq all-partial-coverings (make-hash-table))
                                  (setq conflicts (hash-intersection att-block all-conflicts :output-hash-p t))
                                  ;;(setq redundancies (hash-intersection att-block all-redundancies :output-hash-p t))
                                  )
                                 (t
                                  (setq all-conflicts (rule-avoid-list rule))
                                  ;;(setq all-redundancies (rule-redundancies rule))
                                  ;;(setq all-partial-coverings (block-difference (rule-block rule) (rule-certain-block rule) :output-hash-p t))
                                  (setq conflicts (three-way-hash-intersection all-conflicts att-block (rule-block rule)))
                                  ;;(setq redundancies (three-way-hash-intersection all-redundancies att-block (rule-block rule)))
                                  )))
                          (t
                           (setq all-conflicts (block-difference universe concept-block :output-hash-p t))
                           ;;(setq all-redundancies (block-difference concept-block new-g :output-hash-p t))
                           ;;(setq all-partial-coverings (make-hash-table))
                           (setq conflicts (hash-intersection att-block all-conflicts :output-hash-p t))
                           ;;(setq redundancies (hash-intersection att-block all-redundancies :output-hash-p t))
                           (when (and (= cycle* 2) (equal "HOLDING1182" (rule-based-cpd-dependent-id cpd)))
                             (format t "~%all conflicts: ~S~%conflicts: ~S"all-conflicts conflicts))
                           ))
                    (when nil (and (= cycle* 2) (equal "GOAL859" (rule-based-cpd-dependent-id cpd)))
                          (format t "~%g: ~S~%intersection: ~S~%conflicts: ~S" new-g intersection conflicts)
                          (break)))
                  (when (= (hash-table-count att-block) (hash-table-count universe))
                    (setq intersection (make-hash-table)))
                  (if (null negate-p)
                      (setq condition (cons ident (cdar value-block)))
                      (setq condition (cons ident (list 'not (cdar value-block)))))
                  (setq att-blocks (cons (list (list condition att-block) intersection conflicts redundancies new-g all-conflicts all-redundancies all-partial-coverings) att-blocks))
               finally
                  (return att-blocks)))
           (pass-condition-p (ident rule-conditions)
             (let (val)
               (setq val (gethash ident rule-conditions))
               (if val
                   (values nil val)
                   (values t val)))))
    (when nil (and (>= cycle* 2) (equal "HOLDING1182" (rule-based-cpd-dependent-id cpd)))
      (format t "~%identifiers:~%~S~%num idents: ~d" (rule-based-cpd-identifiers cpd) (hash-table-count (rule-based-cpd-identifiers cpd))))
    (loop
      named togger
      with new-g = (if (and certain-p (> (hash-table-count (rule-block rule)) 0))
                       (hash-intersection g (rule-certain-block rule) :output-hash-p t)
                       g)
      with tog = (make-hash-table :test #'equal)
      for ident being the hash-keys of (rule-based-cpd-identifiers cpd)
        using (hash-value idx)
      do
         (multiple-value-bind (pass attribute)
             (pass-condition-p ident (rule-conditions rule))
           (declare (ignore attribute))
           (when pass
             (setf (gethash ident tog)
                   (cond (certain-p
                          (get-tog-for-vvbms ident (gethash idx (rule-based-cpd-lower-approx-var-value-block-map cpd)) t new-g
                                             (get-tog-for-vvbms ident (gethash idx (rule-based-cpd-lower-approx-negated-vvbms cpd)) t new-g nil :negate-p t)))
                         ((not certain-p)
                          (get-tog-for-vvbms ident (gethash idx (rule-based-cpd-var-value-block-map cpd)) nil new-g
                                             (get-tog-for-vvbms ident (gethash idx (rule-based-cpd-negated-vvbms cpd)) nil new-g nil :negate-p t)))))))
      finally
         (return-from togger tog))))

#| Get the block of cases that are possible members of the condition from the ruleset |#

;; rule-set = ruleset in cpd
;; uncertain-attribute-block = cases where attribute was negated in the ruleset
;; condition = condition in rule
(defun get-uncertain-block (rule-set uncertain-attribute-block condition)
  (cond ((listp (cdr condition))
         (loop
           for rule being the elements of rule-set
           for i from 0
           when (null (gethash (car condition) (rule-conditions rule)))
             collect i into uncertain
           else when (and (member i uncertain-attribute-block)
                          (not (= (second (gethash (car condition) (rule-conditions rule)))
                                  (second (cdr condition)))))
                  collect i into uncertain
           finally
              (return uncertain)))
        (t
         (loop
           for rule being the elements of rule-set
           for i from 0
           when (null (gethash (car condition) (rule-conditions rule)))
             collect i into uncertain
           else when (member i uncertain-attribute-block)
                  collect i into uncertain
           finally
              (return uncertain)))))

#| Determine whether a candidate condition satisfies case constraints for a set of cases in goal |#

;; condition = candidate condition for rule
;; goal = intersection of condition with current rule
;; case-constraints = hash table of constraints that a rule must satisfy for each covered case
;; cpd = conditional probability distribution
(defun condition-satisfy-case-constraints-p (condition goal case-constraints cpd)
  (loop
    with att-constraints and constraints
    with condition-sva and condition-pos = (gethash (car condition) (rule-based-cpd-identifiers cpd))
    for g being the hash-keys of goal
    do
       (setq att-constraints (gethash g case-constraints))
       (when att-constraints
         (setq constraints (gethash (car condition) att-constraints))
         (when constraints
           (if (listp (cdr condition))
               (setq condition-sva (nth (second (cdr condition))
                                   (gethash condition-pos (rule-based-cpd-set-valued-negated-attributes cpd))))
               (setq condition-sva (nth (cdr condition)
                                        (gethash condition-pos (rule-based-cpd-set-valued-attributes cpd)))))
           (when (notany #'(lambda (constraint) (intersection condition-sva constraint)) constraints)
             (return-from condition-satisfy-case-constraints-p nil))))
    finally
       (return t)))

#| Get next condition candidtate for new rule |#

;; certain-tog = certain T(G)
;; tog = T(G)
;; junk = conditions to avoid
;; compatibilities = hash table showing which rules are compatible with each other
;; cpd = conditional probability distributions
;; case-constraints = hash table of constraints that a rule must satisfy for each covered case
(defun find-subset-with-max (certain-tog tog junk cpd case-constraints  &key (reject-conditions))
  (loop
    with best-condition and best-block and best-lower-approx and best-conflicts and best-redundancies and best-cert-intersection
    with max-certain-discounted-coverage = most-negative-fixnum and max-discounted-coverage = most-negative-fixnum
    with smallest-certain-card = most-positive-fixnum and smallest-card = most-positive-fixnum
    for certain-ident being the hash-keys of certain-tog
      using (hash-value certain-att-blocks)
    for ident being the hash-keys of tog
      using (hash-value att-blocks)
    do
       (when (and (= cycle* 2) (equal "HOLDING1182" (rule-based-cpd-dependent-id cpd)))
         (format t "~%"))
       (loop
         with condition and att-block and lower-approx
         with certain-discounted-coverage and discounted-coverage
         with goodness-weight and goodness and cert-goodness-weight and cert-goodness
         with penalty-weight and penalty and cert-penalty-weight and cert-penalty
         with redundancy-weight and redundancy and cert-redundancy-weight and cert-redundancy
         with partial-coverings and partial-coverings-weight
         with size-penalty
         for (cert-condition-block cert-intersection cert-conflicts cert-redundancies cert-g cert-all-conflicts cert-all-redundancies cert-all-partial-coverings) in certain-att-blocks
         for (condition-block intersection conflicts redundancies g all-conflicts all-redundancies all-partial-coverings) in att-blocks
         when (and (> (hash-table-count cert-intersection) 0)
                   (condition-satisfy-case-constraints-p (car condition-block) cert-intersection case-constraints cpd)
                   (not (listp (cdar condition-block))))
           do
              (setq condition (car condition-block))
              (setq att-block (second condition-block))
              (setq lower-approx (second cert-condition-block))
              
              (setq cert-goodness 1)
              (setq cert-penalty 1)
              (setq cert-redundancy 1)
              (setq partial-coverings 1)

              (setq cert-goodness-weight (/ (hash-table-count cert-intersection) (hash-table-count att-block)))
              (setq cert-penalty-weight (hash-table-count conflicts))
              ;;(setq cert-redundancy-weight (/ (hash-table-count redundancies) (hash-table-count att-block)))
              ;;(setq partial-coverings-weight (/ (hash-table-count (block-difference intersection cert-intersection :output-hash-p t)) (hash-table-count intersection)))
              
              (setq certain-discounted-coverage (- 0 ;;(* cert-goodness-weight cert-goodness)
                                                   cert-penalty-weight
                                                   ;;(* cert-redundancy-weight cert-redundancy)
                                                   ;;(* partial-coverings-weight partial-coverings)
                                                   ))
              (when (and (= cycle* 2) (equal "HOLDING1182" (rule-based-cpd-dependent-id cpd)))
                (format t "~%~%B_~S = ~S~%~S = ~S~%   conflicts: ~S = ~d~%cert intersection: ~S ~%intersection: ~S~%size: ~d"
                        condition lower-approx
                        condition att-block
                        conflicts
                        (hash-table-count conflicts)
                        (hash-table-count cert-intersection) (hash-table-count intersection) (hash-table-count att-block)))
              (when (or (> certain-discounted-coverage max-certain-discounted-coverage)
                        (and (= certain-discounted-coverage  max-certain-discounted-coverage)
                             (> (hash-table-count cert-intersection) best-cert-intersection))
                        (and (= certain-discounted-coverage  max-certain-discounted-coverage)
                             (= (hash-table-count cert-intersection) best-cert-intersection)
                             (< (hash-table-count att-block) smallest-card)))
                (setq best-cert-intersection (hash-table-count cert-intersection))
                (setq best-condition condition)
                (setq best-block att-block)
                (setq best-lower-approx lower-approx)
                (setq best-conflicts conflicts)
                (setq best-redundancies redundancies)
                (setq max-certain-discounted-coverage certain-discounted-coverage)
                (setq smallest-certain-card (hash-table-count lower-approx))
                ;;(setq max-discounted-coverage discounted-coverage)
                (setq smallest-card (hash-table-count att-block))))
    finally
       (when nil (and (= cycle* 8) (equal "HAND" (rule-based-cpd-dependent-var cpd)))
         (format t "~%~%returning best condition:~%~S~%" best-condition)
         (when (null best-condition)
           (break))
         )
       (return (values best-condition best-block best-lower-approx best-conflicts best-redundancies))))

#| Compute the block of a rule |#

;; cpd = conditional probability distribution
;; rule = rule to compute block for
;; avoid = condition to ignore
(defun get-rule-block (cpd rule &key (avoid nil) (certain-p nil))
  (when nil (and (= cycle* cycle*) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
    (format t "~%getting ~A rule block for cpd:~%rule:~%~S~%avoid:~%~S" (if certain-p "certain" "") rule avoid))
  (loop
    with rule-block = (make-hash-table) and vvbm and nvvbm and att-block and i = 0
    for attribute being the hash-keys of (rule-conditions rule)
      using (hash-value value)
    when (not (equal attribute avoid)) do
      (cond (certain-p
             (setq vvbm (gethash (gethash attribute (rule-based-cpd-identifiers cpd))
                                 (rule-based-cpd-lower-approx-var-value-block-map cpd)))
             (setq nvvbm (gethash (gethash attribute (rule-based-cpd-identifiers cpd))
                                  (rule-based-cpd-lower-approx-negated-vvbms cpd))))
            ((null certain-p)
             (setq vvbm (gethash (gethash attribute (rule-based-cpd-identifiers cpd))
                                 (rule-based-cpd-var-value-block-map cpd)))
             (setq nvvbm (gethash (gethash attribute (rule-based-cpd-identifiers cpd))
                                  (rule-based-cpd-negated-vvbms cpd)))))
      (cond ((not (numberp value))
             (setq att-block (second (nth (second value) nvvbm))))
            (t
             (setq att-block (second (nth value vvbm)))))
      (when nil (and (= cycle* cycle*) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
        (format t "~%~%attribute:~%~S~%value:~%~S~%attribute block:~%~S~%partial rule block:~%~S" attribute value att-block rule-block))
      (if (= i 0)
          (setq rule-block att-block)
          (setq rule-block (hash-intersection rule-block att-block :output-hash-p t)))
      (when nil (and (= cycle* cycle*) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
            (format t "~%new rule block:~%~S" rule-block))
      (when nil (and (= cycle* cycle*) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
            (format t "~%remaining rule attribute: ~S~%block: ~S~%rule-block so far: ~S" attribute att-block rule-block))
      (setq i (+ i 1))
    finally
       (when nil (and (= cycle* cycle*) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
         (format t "~%returning:~%~S" rule-block))
       (return rule-block)))

(defun print-case-constraints (case-constraints)
  (format t "~%~%case constraints")
  (loop
    for case being the hash-keys of case-constraints
      using (hash-value attributes)
    do
       (format t "~%~%~d:" case)
       (loop
         for att being the hash-keys of attributes
           using (hash-value constraints)
         do
            (format t "~%~%   ~S" att)
            (loop
              for constraint in constraints
              do
              (format t "~%      ~S" constraint)))))

#| Print T(G) |#

;; ToG = T(G)
(defun print-ToG (ToG)
  (loop
    for attribute being the hash-keys of ToG
    using (hash-value vvbm)
    do
       (format t "~%~%~S" attribute)
       (loop
         for vvb in vvbm
         do
            (format t "~%~%   ~S~%   Block: ~S~%   T(G): ~S~%   Conflicts: ~S~%   Redundancies: ~S" (caar vvb) (second (car vvb)) (second vvb) (third vvb) (fourth vvb)))))


#| Add a constraint to the case constraints |#

;; case = case to add constraint on
;; att = attribute to place constraint
;; sva = constraint on acceptable attribute values
;; case-constraints = hash table of constraints that a rule must satisfy for each covered case
(defun add-case-constraint (case att sva case-constraints)
  (setf (gethash att (gethash case case-constraints))
        (cons sva (gethash att (gethash case case-constraints))))
  case-constraints)

#| Initialize case constraints using initial rules |#

;; cpd = rule-based cpd
(defun init-case-constraints(cpd)
  (when nil (and (= cycle* 2) (equal "Y548" (rule-based-cpd-dependent-id cpd)))
        (format t "~%~%initializing case constraints"))
  (loop
    with case-constraints = (make-hash-table)
    for rule being the elements of (rule-based-cpd-rules cpd)
    do
       (loop
         for case being the hash-keys of (rule-block rule)
         do
            (when nil (and (= cycle* 2) (equal "Y548" (rule-based-cpd-dependent-id cpd)))
                  (format t "~%~%case ~d:" case))
            (when (null (gethash case case-constraints))
              (setf (gethash case case-constraints) (make-hash-table :test #'equal)))
            (loop
              with idx and rule-sva
              for att being the hash-keys of (rule-conditions rule)
                using (hash-value val)
              do
                 (setq idx (gethash att (rule-based-cpd-identifiers cpd)))
                 (if (listp val)
                     (setq rule-sva (nth (second val) (gethash idx (rule-based-cpd-set-valued-negated-attributes cpd))))
                     (setq rule-sva (nth val (gethash idx (rule-based-cpd-set-valued-attributes cpd)))))
                 (when nil (and (= cycle* 2) (equal "Y548" (rule-based-cpd-dependent-id cpd)))
                       (format t "~%~%attribute in rule: ~S" att)
                       (format t "~%rule sva: ~S" rule-sva))
                 (loop
                   for sva in (gethash idx (rule-based-cpd-set-valued-attributes cpd))
                   for svna in (gethash idx (rule-based-cpd-set-valued-negated-attributes cpd))
                   do
                      (cond ((intersection sva rule-sva)
                             (setq case-constraints (add-case-constraint case att sva case-constraints)))
                            ((intersection svna rule-sva)
                             (setq case-constraints (add-case-constraint case att svna case-constraints)))))))
    finally
       (return case-constraints)))

#| Constrain space of possible conditions per rule |#

;; cpd = conditional probability distribution
;; rule = rule in rule-based-cpd
;; case-constraints = hash table of constraints that a rule must satisfy for each covered case
(defun update-case-constraints (cpd rule case-constraints)
  (labels ((add-case-constraint (case att sva case-constraints)
             (setf (gethash att (gethash case case-constraints))
                   (cons sva (gethash att (gethash case case-constraints))))
             case-constraints)
           (remove-case-constraint (case att sva case-constraints)
             (setf (gethash att (gethash case case-constraints))
                   (remove sva (gethash att (gethash case case-constraints))))
             case-constraints))
    (when nil (and (= cycle* 2) (equal "Y548" (rule-based-cpd-dependent-id cpd)))
          (format t "~%~%Updating case constraints based on rule:~%~S" rule))
    (loop
      for case being the hash-keys of (rule-block rule)
      ;;when (member case (rule-certain-block rule))
      ;;  do
      ;;     (remhash case case-constraints)
      ;;else when (not (member case (rule-certain-block rule)))
        do
           (when nil (and (= cycle* 2) (equal "Y548" (rule-based-cpd-dependent-id cpd)))
                 (format t "~%~%case ~d:" case))
           (when (null (gethash case case-constraints))
             (setf (gethash case case-constraints) (make-hash-table :test #'equal)))
           (loop
             with idx and rule-sva
             for att being the hash-keys of (rule-conditions rule)
               using (hash-value val)
             do
                (setq idx (gethash att (rule-based-cpd-identifiers cpd)))
                (if (listp val)
                    (setq rule-sva (nth (second val) (gethash idx (rule-based-cpd-set-valued-negated-attributes cpd))))
                    (setq rule-sva (nth val (gethash idx (rule-based-cpd-set-valued-attributes cpd)))))
                (when nil (and (= cycle* 2) (equal "Y548" (rule-based-cpd-dependent-id cpd)))
                      (format t "~%~%attribute in rule: ~S" att)
                      (format t "~%rule sva: ~S" rule-sva))
                (loop
                  for value-block in (gethash idx (rule-based-cpd-var-value-block-map cpd))
                  for sva in (gethash idx (rule-based-cpd-set-valued-attributes cpd))
                  for negated-value-block in (gethash idx (rule-based-cpd-negated-vvbms cpd))
                  for svna in (gethash idx (rule-based-cpd-set-valued-negated-attributes cpd))
                  do
                     (when nil (and (= cycle* 2) (equal "Y548" (rule-based-cpd-dependent-id cpd)))
                           (format t "~%   value-block: ~S~%   sva: ~S" value-block sva))
                     (cond ((and (not (intersection sva rule-sva))
                                 (member case (second value-block)))
                            (when nil (and (= cycle* 2) (equal "Y548" (rule-based-cpd-dependent-id cpd)))
                                  (format t "~%   adding ~S constraint to ~A for case ~d" sva att case))
                            (setq case-constraints (add-case-constraint case att sva case-constraints)))
                           ((and (gethash case (rule-certain-block rule))
                                 (intersection sva rule-sva)
                                 (member sva (gethash att (gethash case case-constraints))))
                            (when nil (and (= cycle* 2) (equal "Y548" (rule-based-cpd-dependent-id cpd)))
                                  (format t "~%   removing ~S constraint to ~A for case ~d" sva att case))
                            (setq case-constraints (remove-case-constraint case att sva case-constraints))))
                     (cond ((and (not (intersection svna rule-sva))
                                 (member case (second negated-value-block)))
                            (when nil(and (= cycle* 2) (equal "Y548" (rule-based-cpd-dependent-id cpd)))
                                  (format t "~%   adding ~S constraint to ~A for case ~d" svna att case))
                            (setq case-constraints (add-case-constraint case att svna case-constraints)))
                           ((and (gethash case (rule-certain-block rule))
                                 (intersection svna rule-sva)
                                 (member svna (gethash att (gethash case case-constraints))))
                            (when nil (and (= cycle* 2) (equal "Y548" (rule-based-cpd-dependent-id cpd)))
                                  (format t "~%   removing ~S constraint to ~A for case ~d" svna att case))
                            (setq case-constraints (remove-case-constraint case att svna case-constraints)))))
                (when (null (gethash att (gethash case case-constraints)))
                  (remhash att (gethash case case-constraints))))
           (when (= (hash-table-count (gethash case case-constraints)) 0)
             (remhash case case-constraints)))
    case-constraints))

#| Exploit local structure in CPD to induce minimal rule set |#

;; cpd = conditional probability distribution
(defun get-local-coverings (cpd)
  (labels ((rule-satisfy-case-constraints-p (rule case-constraints &key (avoid nil))
             (loop
               with att-constraints
               for g being the hash-keys of (rule-certain-block rule)
               do
                  (setq att-constraints (gethash g case-constraints))
                  (when att-constraints
                    (loop
                      with rule-condition = nil and rule-sva and condition-pos
                      for att being the hash-keys of att-constraints
                        using (hash-value constraints)
                        do
                         (setq rule-condition (gethash att (rule-conditions rule)))
                         (when (or (null rule-condition)
                                   (and (equal att avoid)
                                        rule-condition))
                           (return-from rule-satisfy-case-constraints-p nil))
                         (setq condition-pos (gethash att (rule-based-cpd-identifiers cpd)))
                         (if (listp rule-condition)
                             (setq rule-sva (nth (second rule-condition)
                                                 (gethash condition-pos (rule-based-cpd-set-valued-negated-attributes cpd))))
                             (setq rule-sva (nth rule-condition
                                                 (gethash condition-pos (rule-based-cpd-set-valued-attributes cpd)))))
                         (when (not (member rule-sva constraints :test #'equal));;(notany #'(lambda (constraint) (intersection rule-sva constraint)) constraints)
                           (return-from rule-satisfy-case-constraints-p nil))))
               finally
                  (return t))))
    (when (and (= cycle* 2) (equal "HOLDING1182" (rule-based-cpd-dependent-id cpd)))
      (format t "~%getting local covering for:~%~S" cpd)
      ;;(break)
      )
    (loop
      with universe =
                    (loop with uni-hash = (make-hash-table :size (ceiling (* 1.3 (array-dimension (rule-based-cpd-rules cpd) 0))))
                          for i from 0 to (- (array-dimension (rule-based-cpd-rules cpd) 0) 1)
                          do
                             (setf (gethash i uni-hash) i)
                          finally (return uni-hash))
      with case-constraints = (make-hash-table) ;;(init-case-constraints cpd)
      with minimal-rules and case = 0 and goal and junk
      for probability-concept being the hash-keys of (rule-based-cpd-concept-blocks cpd)
        using (hash-value counts-hash)
      do
         (when (and (= cycle* 2) (equal "HOLDING1182" (rule-based-cpd-dependent-id cpd)))
           (print-case-constraints case-constraints))
         (loop
           for count being the hash-keys of counts-hash
             using (hash-value concept-block)
           do
              (setq goal (copy-hash-table concept-block))
              (setq junk nil)
              (loop
                with new-rule and tog and certain-tog and rule-set and rule-set-block = (make-hash-table) and certain-rule-blocks and attr-lower-approxs
                with uncertain-block
                while (> (hash-table-count goal) 0)
                do
                   (setq new-rule (make-rule :id (symbol-name (gensym "RULE-"))
                                             :conditions (make-hash-table :test #'equal)
                                             :probability probability-concept
                                             :block (make-hash-table)
                                             :certain-block (make-hash-table)
                                             :avoid-list (make-hash-table)
                                             :redundancies (make-hash-table)
                                             :count count))
                   (setq attr-lower-approxs (make-hash-table :test #'equal))
                   (setq tog (get-tog cpd goal concept-block new-rule universe))
                   (setq certain-tog (get-tog cpd goal concept-block new-rule universe :certain-p t))
                   (when (and (= cycle* 2) (equal "HOLDING1182" (rule-based-cpd-dependent-id cpd)))
                     (format t "~%~%G:~%~S~%Avoid List:~%~S~%certain T(G) for new rule:" goal (block-difference universe concept-block :output-hash-p t))
                     ;;(print-tog certain-tog)
                     ;;(format t "~%~%T(G) for new rule:")
                     ;;(print-tog tog)
                     ;;(break)
                     )
                   (loop
                     with reject-conditions
                     while (and (or (= (hash-table-count (rule-conditions new-rule)) 0)
                                    (not (= (hash-table-count (block-difference (rule-block new-rule) concept-block :output-hash-p t)) 0)) ;;(not (subsetp (rule-block new-rule) goal))
                                    (not (rule-satisfy-case-constraints-p new-rule case-constraints))
                                    ;;(some #'(lambda (rule) (compatible-rule-p new-rule rule cpd cpd)) (union rule-set minimal-rules))
                                    )
                                (not (= (hash-table-count tog) 0)))
                     do
                        (multiple-value-bind (condition condition-block lower-approx conflicts redundancies)
                            (find-subset-with-max certain-tog tog junk cpd case-constraints :reject-conditions reject-conditions)
                          (when (and (= cycle* 2) (equal "HOLDING1182" (rule-based-cpd-dependent-id cpd)))
                            (format t "~%--------------~%condition:~%~S~%condition-block:~%~S~%condition lower-approximation:~%~S~%condition conflicts:~%~S~%condition redundancies:~%~S" condition condition-block lower-approx conflicts redundancies))
                          (setf (gethash (car condition) (rule-conditions new-rule)) (cdr condition))
                          (if (> (hash-table-count (rule-block new-rule)) 0)
                              (setf (rule-block new-rule) (get-rule-block cpd new-rule))
                              (setf (rule-block new-rule) condition-block))
                          (if (> (hash-table-count (rule-certain-block new-rule)) 0) ;;(rule-certain-block new-rule)
                              (setf (rule-certain-block new-rule) (hash-intersection (rule-certain-block new-rule) lower-approx :output-hash-p t))
                              (setf (rule-certain-block new-rule) lower-approx))
                          (setf (rule-avoid-list new-rule) conflicts)
                          ;;(setf (rule-redundancies new-rule) redundancies)
                          (setf (gethash (car condition) attr-lower-approxs) lower-approx)
                          (when (and (= cycle* 2) (equal "HOLDING1182" (rule-based-cpd-dependent-id cpd)))
                            (format t "~%updated rule block:~%~S" (rule-block new-rule))
                            (format t "~%updated rule certain block:~%~S" (rule-certain-block new-rule))
                            (format t "~%updated rule avoid list:~%~S" (rule-avoid-list new-rule))
                            (format t "~%updated rule redundancies list:~%~S" (rule-redundancies new-rule)))
                          (setq goal (hash-intersection goal (rule-block new-rule) :output-hash-p t))
                          (when (and (= cycle* 2) (equal "HOLDING1182" (rule-based-cpd-dependent-id cpd)))
                            (format t "~%updated goal:~%~S" goal))
                          (setq tog (get-tog cpd goal concept-block new-rule universe))
                          (setq certain-tog (get-tog cpd goal concept-block new-rule universe :certain-p t))
                          (when  (and (= cycle* 19) (equal "BLOCK505" (rule-based-cpd-dependent-id cpd)))
                                (format t "~%certain T(G)")
                                ;;(print-tog certain-tog)
                                (format t "~%~%T(G)")
                                ;;(print-tog tog)
                                ))
                        (when nil t nil (and (= cycle* 2) (equal "GOAL859" (rule-based-cpd-dependent-id cpd)))
                          (format t "~%concept block:~%~S~%rule certain block:~%~S~%rule block:~%~S~%certain rule block in goal?: ~S~%rule block in goal?: ~S~%empty certain T(G)?: ~S~%empty T(G)?: ~S"
                                  concept-block
                                  (rule-certain-block new-rule)
                                  (rule-block new-rule)
                                  (= (hash-table-count (block-difference (rule-certain-block new-rule) concept-block :output-hash-p t)) 0)
                                  (= (hash-table-count (block-difference (rule-block new-rule) concept-block :output-hash-p t)) 0)
                                  (= (hash-table-count certain-tog) 0)
                                  (= (hash-table-count tog) 0))
                          ;;(break)
                          ))
                   (cond ((and (= (hash-table-count (block-difference (rule-block new-rule) concept-block :output-hash-p t)) 0) ;;(subsetp (rule-block new-rule) goal)
                               (> (hash-table-count (rule-block new-rule)) 0) ;;(not (null (rule-block new-rule)))
                               (= (hash-table-count (rule-avoid-list new-rule)) 0) ;;(null (rule-avoid-list new-rule))
                               ;;(= (hash-table-count (rule-redundancies new-rule)) 0) ;;(null (rule-redundancies new-rule))
                               )
                          
                          ;; remove extraneous conditions, but make sure that pruned rule isn't compatible with existing rules!!
                          (when nil (and (= cycle* 19) (equal "BLOCK505" (rule-based-cpd-dependent-id cpd)))
                            ;;(break)
                            (format t "~%~%testing for redundant conditions!~%goal: ~S~%rule:~%~S" concept-block new-rule))
                          (loop
                            with rule-partial-coverings = (block-difference (rule-block new-rule) (rule-certain-block new-rule) :output-hash-p t)
                            with new-certain-block and new-rule-block and new-partial-coverings and new-redundancies
                            for attribute being the hash-keys of (rule-conditions new-rule)
                              using (hash-value value)
                            do
                               (setq new-certain-block (get-rule-block cpd new-rule :avoid attribute :certain-p t))
                               (setq new-rule-block (get-rule-block cpd new-rule :avoid attribute))
                               (setq new-redundancies (hash-intersection new-rule-block rule-set-block :output-hash-p t))
                               (setq new-partial-coverings (block-difference new-rule-block new-certain-block :output-hash-p t))
                               (when nil (and (= cycle* 19) (equal "BLOCK505" (rule-based-cpd-dependent-id cpd)))
                                 (format t"~%testing condition: (~S ~S)~%proposed rule block: ~S~%proposed certain block: ~S~%current rule block: ~S~%current certain block: ~S" attribute value new-rule-block new-certain-block (rule-block new-rule) (rule-certain-block new-rule)))
                               (when (and (not (= (hash-table-count new-rule-block) 0)) ;; (not (null new-rule-block))
                                          (not (= (hash-table-count new-certain-block) 0)) ;; (not (null new-certain-block))
                                          (= (hash-table-count (block-difference new-rule-block concept-block :output-hash-p t)) 0) ;;(subsetp new-rule-block concept-block)
                                          ;;(= (hash-table-count (block-difference new-certain-block (block-difference concept-block rule-set-block :output-hash-p t) :output-hash-p t)) 0);; (subsetp new-certain-block (set-difference concept-block rule-set-block))
                                          ;;(= (hash-table-count (block-difference new-partial-coverings rule-partial-coverings :output-hash-p t)) 0)
                                          ;;(= (hash-table-count new-redundancies) 0)
                                          #|
                                          (notany #'(lambda (rule) (compatible-rule-p new-rule rule cpd cpd :avoid attribute))
                                                  (union rule-set minimal-rules))
                                          |#
                                          (rule-satisfy-case-constraints-p new-rule case-constraints :avoid attribute))
                                 (when nil (and (= cycle* 19) (equal "BLOCK505" (rule-based-cpd-dependent-id cpd)))
                                   (format t "~%Success!"))
                                 (remhash attribute (rule-conditions new-rule))
                                 (setf (rule-certain-block new-rule) new-certain-block)
                                 (setf (rule-block new-rule) new-rule-block)
                                 (when nil (and (= cycle* 19) (equal "BLOCK505" (rule-based-cpd-dependent-id cpd)))
                                   (format t "~%updated rule:~%~S" new-rule))))
                          
                          ;;(setq case-constraints (update-case-constraints cpd new-rule case-constraints))
                          (when (and (= cycle* 2) (equal "HOLDING1182" (rule-based-cpd-dependent-id cpd)))
                            (format t "~%final rule:~%~S" new-rule)
                            (print-case-constraints case-constraints)
                            (break)
                            )
                          (setq rule-set (reverse (cons new-rule (reverse rule-set))))
                          ;;(setq certain-rule-blocks (cons (rule-certain-block new-rule) certain-rule-blocks))
                          ;;(setq rule-blocks (cons (rule-block new-rule) rule-blocks))
                          ;; update attribute blocks and everything
                          (setq uncertain-block (block-difference (rule-block new-rule) (rule-certain-block new-rule) :output-hash-p t))
                          (when nil t (and (= cycle* cycle*) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
                            (format t "~%Candidate rule:~%~S~%decision block: ~S~%goal: ~S~%uncertain block: ~S" new-rule concept-block goal uncertain-block))
                          #|
                          (loop
                            with pos and attribute-block and att-lower-approx and remove-cases and new-block
                            for attribute being the hash-keys of (rule-conditions new-rule)
                              using (hash-value value)
                            when (> (hash-table-count uncertain-block) 0) do
                              (setq pos (gethash attribute (rule-based-cpd-identifiers cpd)))
                              (when nil (and (= cycle* 2) (equal "Y548" (rule-based-cpd-dependent-id cpd)))
                                (format t "~%~%updating blocks and lower approximations for ~S across all relevant values" attribute))
                              (cond ((listp value)
                                     (when nil (and (= cycle* 5) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
                                           (format t "~%~%updating positive vvbs"))
                                     (loop
                                       with card = (aref (rule-based-cpd-cardinalities cpd) pos)
                                       with negated-vvb = (nth (second value) (gethash pos (rule-based-cpd-negated-vvbms cpd)))
                                       with blocks-list = nil
                                       for vvb in (gethash pos (rule-based-cpd-var-value-block-map cpd))
                                       for lower-vvb in (gethash pos (rule-based-cpd-lower-approx-var-value-block-map cpd))
                                       when (not (= (second value) (cdar vvb))) do
                                         (when nil (and (= cycle* 5) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
                                               (format t "~%value:~%~S~%attribute block before change:~%~S~%lower-approximation before change:~%~S" (cdar vvb) (second vvb) (second lower-vvb)))
                                         (setq attribute-block (second vvb))
                                         (setq att-lower-approx (second lower-vvb))
                                         (setq remove-cases (hash-intersection (block-difference attribute-block att-lower-approx :output-hash-p t) (rule-block new-rule) :output-hash-p t))
                                         (setq new-block (block-difference attribute-block remove-cases :output-hash-p t))
                                         (setf (second vvb) new-block)
                                         (when nil (and (= cycle* 5) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
                                               (format t "~%attribute block after change:~%~S" (second vvb))))
                                     (when nil (and (= cycle* 5) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
                                           (format t "~%~%updating negated vvbs"))
                                     (loop
                                       with card = (aref (rule-based-cpd-cardinalities cpd) pos)
                                       with negated-attribute-block and blocks-list
                                       for i from 0 to (- card 1)
                                       do
                                          (setq blocks-list (make-hash-table))
                                          (setq negated-attribute-block (second
                                                                         (nth i
                                                                              (gethash pos (rule-based-cpd-negated-vvbms cpd)))))
                                          (when nil (and (= cycle* 5) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
                                                (format t "~%~%attribute:~%~S~%value:~%~S~%attribute block:~%~S" attribute (list 'not i) negated-attribute-block))
                                          (loop
                                            with att-block
                                            for j from 0 to (- card 1)
                                            when (not (= j i))
                                              do
                                                 (setq att-block (second
                                                                  (nth j
                                                                       (gethash pos (rule-based-cpd-var-value-block-map cpd)))))
                                                 (loop
                                                   for c being the hash-keys of att-block
                                                   do
                                                   (setf (gethash c blocks-list) c)))
                                          (setf (second (nth i (gethash pos (rule-based-cpd-negated-vvbms cpd)))) blocks-list)
                                          (when nil (and (= cycle* 5) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
                                                (format t "~%updated attribute block:~%~S" (second
                                                                                            (nth i
                                                                                                 (gethash pos (rule-based-cpd-negated-vvbms cpd))))))))
                                    ((numberp value)
                                     (setq attribute-block (second (nth value (gethash pos (rule-based-cpd-var-value-block-map cpd)))))
                                     (setq att-lower-approx (second (nth value (gethash pos (rule-based-cpd-lower-approx-var-value-block-map cpd)))))
                                     (setq remove-cases (hash-intersection (block-difference attribute-block att-lower-approx :output-hash-p t) (rule-block new-rule) :output-hash-p t))
                                     (setq new-block (block-difference attribute-block remove-cases :output-hash-p t))
                                     (when nil (and (= cycle* 5) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
                                       (format t "~%~%attribute:~%~S~%value:~%~S~%attribute block:~%~S~%lower approximation:~%~S~%cases to remove:~%~S" attribute value attribute-block att-lower-approx remove-cases))
                                     (setf (second (nth value (gethash pos (rule-based-cpd-var-value-block-map cpd)))) new-block)
                                     (when nil (and (= cycle* 5) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
                                       (format t "~%updated attribute block:~%~S" (second (nth value (gethash pos (rule-based-cpd-var-value-block-map cpd))))))
                                     (when nil (and (= cycle* 5) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
                                       (format t "~%Updated negated attribute blocks"))
                                     (loop
                                       with card = (aref (rule-based-cpd-cardinalities cpd) pos)
                                       with negated-attribute-block and blocks-list
                                       for i from 0 to (- card 1)
                                       when (not (= i value))
                                         do
                                            (setq blocks-list (make-hash-table))
                                            (setq negated-attribute-block (second
                                                                           (nth i
                                                                                (gethash pos (rule-based-cpd-negated-vvbms cpd)))))
                                            (when nil (and (= cycle* 3) (equal "BLOCK546" (rule-based-cpd-dependent-id cpd)))
                                              (format t "~%~%attribute:~%~S~%value:~%~S~%attribute block:~%~S" attribute (list 'not i) negated-attribute-block))
                                            (loop
                                              with att-block
                                              for j from 0 to (- card 1)
                                              when (not (= j i))
                                                do
                                                   (setq att-block (second
                                                                    (nth j
                                                                         (gethash pos (rule-based-cpd-var-value-block-map cpd)))))
                                                   (loop
                                                     for c being the hash-keys of att-block
                                                     do
                                                     (setf (gethash c blocks-list) c)))
                                            (setf (second (nth i (gethash pos (rule-based-cpd-negated-vvbms cpd)))) blocks-list)
                                            (when nil (and (= cycle* 3) (equal "BLOCK546" (rule-based-cpd-dependent-id cpd)))
                                              (format t "~%updated attribute block:~%~S" (second
                                                                                          (nth i
                                                                                               (gethash pos (rule-based-cpd-negated-vvbms cpd))))))))))
                          |#
                          (when nil (and (= cycle* 3) (equal "BLOCK546" (rule-based-cpd-dependent-id cpd)))
                            (format t "~%check updated blocks")
                            ;;(break)
                            )
                          ;;(setq cpd (cpd-add-characteristic-sets cpd))
                          (setq cpd (cpd-add-lower-approximations (cpd-add-lower-approximations cpd "VVBM")
                                                                  "NEGATED"))
                          (when nil (and (= cycle* 5) (equal "STATUS569" (rule-based-cpd-dependent-id cpd)))
                            (loop
                              for ident being the hash-keys of (rule-based-cpd-identifiers cpd)
                                using (hash-value idx)
                              do
                                 (format t "~%~%Ident: ~S" ident)
                                 (format t "~%B_positive:~%~S" (gethash idx (rule-based-cpd-lower-approx-var-value-block-map cpd)))
                                 (format t "~%B_negated:~%~S" (gethash idx (rule-based-cpd-lower-approx-negated-vvbms cpd)))))
                          #|
                          (loop
                            with pos and lower-vvbm and vvbm
                            for attribute being the hash-keys of (rule-conditions new-rule)
                              using (hash-value val)
                            do
                               (setq pos (gethash attribute (rule-based-cpd-identifiers cpd)))
                               (cond ((numberp val)
                                      (setq lower-vvbm (gethash pos (rule-based-cpd-lower-approx-var-value-block-map cpd)))
                                      (setq vvbm (gethash pos (rule-based-cpd-var-value-block-map cpd))))
                                     (t
                                      (setq lower-vvbm (gethash pos (rule-based-cpd-lower-approx-negated-vvbms cpd)))
                                      (setq vvbm (gethash pos (rule-based-cpd-negated-vvbms cpd)))))
                               (when (and (= cycle* 15) (equal "BLOCK56750" (rule-based-cpd-dependent-id cpd)))
                                 (format t "~%attribute: ~S~%value: ~S~%attribute block:~S~%new lower approximation: ~S" attribute val vvbm lower-vvbm)))
                          |#
                          #|
                          (loop
                            for rule-block in certain-rule-blocks
                            nconc (copy-list rule-block) into rsb
                            finally
                               (setq rule-set-block rsb))
                          |#
                          (loop
                            for c being the hash-keys of (rule-certain-block new-rule)
                            do
                               (setf (gethash c rule-set-block) c))
                          (setq goal (copy-hash-table concept-block))
                          (when nil t (and (= cycle* 3) (equal "BLOCK546" (rule-based-cpd-dependent-id cpd)))
                                (format t "~%rule-set:~%~S~%rule-set block:~%~S~%goal:~%~S" rule-set rule-set-block goal))
                          #|
                          (loop
                            for b in rule-set-block
                            do
                               (setq goal (remove b goal)))
                          |#
                          (setq goal (block-difference goal rule-set-block :output-hash-p t))
                          (setq junk nil)
                          (when nil t (and (= cycle* 3) (equal "BLOCK546" (rule-based-cpd-dependent-id cpd)))
                                (format t "~%updated goal:~%~S~%****************************~%" goal)))
                         (t
                          (format t "~%cpd:~%~S~%goal:~%~S~%failed rule:~%~S" cpd concept-block new-rule)
                          (break)
                          #|
                          (loop
                          for attribute being the hash-keys of (rule-conditions new-rule)
                          using (hash-value value)
                          do
                          (setq junk (cons (cons attribute value) junk)))
                          |#
                          ))
                   (when nil t (and (= cycle* 3) (equal "BLOCK546" (rule-based-cpd-dependent-id cpd)))
                         (format t "~%~%candidate rule:~%~S~%rule-block:~%~S~%goal:~%~S" new-rule (rule-block new-rule) goal))
                finally
                   (when nil (and (= cycle* 3) (equal "BLOCK546" (rule-based-cpd-dependent-id cpd)))
                         (format t "~%~%candidate rule set:~%~S"rule-set))
                   (loop
                     for rule in rule-set
                     do
                        (setf (rule-block rule) (make-hash-table))
                        (setf (rule-certain-block rule) (make-hash-table))
                        (setf (gethash case (rule-block rule)) case) ;;(setf (rule-block rule) (list case))
                        (setf (gethash case (rule-certain-block rule)) case) ;;(setf (rule-certain-block rule) (list case))
                        (setq minimal-rules (reverse (cons rule (reverse minimal-rules))))
                        (setq case (+ case 1)))
                #|
                   (loop
                with reduced-block
                for rule in rule-set
                do
                   (setq reduced-block nil)
                   (loop
                for rule2 in rule-set
                when (not (equal (rule-id rule) (rule-id rule2)))
                do
                   (setq reduced-block (union reduced-block (rule-block rule2))))
                   (when (not (and (subsetp reduced-block concept-block) (subsetp concept-block reduced-block)))
                   (setf (rule-block rule) (list case))
                   (setq minimal-rules (reverse (cons rule (reverse minimal-rules))))
                   (setq case (+ case 1))))
                |#))
      finally
         (when nil t nil (and (= cycle* 15) (equal "INTENTION37068" (rule-based-cpd-dependent-id cpd)))
           (format t "~%~%final rules:~%~S~%*********************************" minimal-rules)
               (break)
               )
         (setq cpd (update-cpd-rules cpd (make-array case :initial-contents minimal-rules) :check-uniqueness t))
         (return cpd))))

#| Get variable domain while avoiding specific value |#

;; attribute = variable in cpd
;; cpd = rule-based conditional probability distrubution
;; avoid = value to avoid from variable domain
(defun get-cpd-var-domain (attribute cpd &key (avoid nil))
  (loop
    with pos = (gethash attribute (rule-based-cpd-identifiers cpd))
    for vvbm in (gethash pos (rule-based-cpd-var-value-block-map cpd))
    when (not (equal (cdar vvbm) avoid))
      collect (cdar vvbm) into domain
    finally (return domain)))

#| Check if two two conditions (att val) are compatible |#

;; attribute = attribute of the condition
;; cpd1 = conditional probability for rule1
;; cpd2 = conditional probability for rule2
;; value1 = attribute value in condition of first rule
;; value2 = attribute value in condition of second rule
;; present-p = whether attribute existed in second rule
;; exact = flag for forcing equality in negated conditions
(defun compatible-conditions-p (attribute cpd1 cpd2 value1 value2 present-p &key (exact nil))
  (cond ((and present-p (numberp value1) (numberp value2) (not (= value1 value2)))
         (values nil 0))
        ((and present-p (not exact) (not (numberp value1)) (numberp value2) (= (second value1) value2))
         (values nil 0))
        ((and present-p (not exact) (numberp value1) (not (numberp value2)) (= value1 (second value2)))
         (values nil 0))
        ((and present-p (not exact) (not (numberp value1)) (not (numberp value2)) (not (= (second value1) (second value2))))
         ;; check if I need a key argument for when computing characteristic sets
         (let (domain1 domain2)
           (setq domain1 (get-cpd-var-domain attribute cpd1 :avoid (second value1)))
           (setq domain2 (get-cpd-var-domain attribute cpd2 :avoid (second value2)))
           (if (intersection domain1 domain2)
               (values t 1)
               (values nil 0))))
        ((and present-p exact (not (numberp value1)) (numberp value2))
         (values nil 0))
        ((and present-p exact (numberp value1) (not (numberp value2)))
         (values nil 0))
        ((and present-p exact (not (numberp value1)) (not (numberp value2)) (not (= (second value1) (second value2))))
         (values nil 0))
        ((not present-p)
         (values t 1))
        (t
         (values t 1))))

#| Returns true if conditions in the intersection agree |#

;; rule1 = candidate rule to check
;; rule2 = rule to check compatiblity against
;; cpd1 = conditional probability for rule1
;; cpd2 = conditional probability for rule2
;; exact = flag for forcing equality in negated conditions
;; avoid = condition to ignore in rule1
(defun compatible-rule-p (rule1 rule2 cpd1 cpd2 &key (exact nil) (avoid nil) (check-count nil))
  (loop
    with num-compatible = 0
    for attribute being the hash-keys of (rule-conditions rule1)
      using (hash-value value1)
    when (not (equal attribute avoid))
      do
         (multiple-value-bind (value2 present-p)
             (gethash attribute (rule-conditions rule2))
           (multiple-value-bind (match-p count)
               (compatible-conditions-p attribute cpd1 cpd2 value1 value2 present-p :exact exact)
             (setq num-compatible (+ num-compatible count))
             (when (or (not match-p)
                       (and check-count (not (eq (rule-count rule1) (rule-count rule2)))))
               (return-from compatible-rule-p (values nil num-compatible)))))
    finally
       (return (values t num-compatible))))

#| Split a rule on variable. See (Koller and Friedman, 2009) |#
;; rule = rule to split
;; var = variable to split on
;; domain = var domain
;; cpd = conditional probability distribution
;; existing-rules = list of existing rules from which rule came
;; var-dif = new idents in episode not in schema
(defun split-rule-on-variable (rule var domain cpd var-dif &key (avoid-hash (make-hash-table :test #'equal)))
  (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd)))
    (format t "~%~%splitting rule:~%~S~%on variable: ~s~%with domain: ~S~%cpd-idents:~%~S" rule var domain (rule-based-cpd-identifiers cpd)))
  (cond ((gethash var (rule-conditions rule))
         (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd)))
           (format t "~%var in rule. returning"))
         (values (list rule) avoid-hash))
        (t
         (loop
           with dependent-condition
           with new-rule and new-rules
           for value in domain
           when (not (equal (gethash var avoid-hash) value)) do
             (setq new-rule (copy-cpd-rule rule))
             (setf (gethash var (rule-conditions new-rule)) value)
             (setq dependent-condition (gethash (rule-based-cpd-dependent-id cpd) (rule-conditions new-rule)))
             (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd)))
               (format t "~%~%new rule:~%~S~%dependent id: (~S . ~S)~%var present in cpd?:~%~S~%missing:~%~S" new-rule (rule-based-cpd-dependent-id cpd) dependent-condition (null (null (gethash var (rule-based-cpd-identifiers cpd)))) var-dif))
             (cond ((null (gethash var (rule-based-cpd-identifiers cpd)))
                    (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd)))
                      (format t "~%var value:~%~S." value))
                    (cond ((= value 0)
                           (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd)))
                             (format t "~%set rule count to: ~d" (rule-count rule)))
                           (setf (rule-count new-rule) (rule-count rule)))
                          (t
                           (setf (rule-count new-rule) 0)
                           (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd)))
                             (format t "~%set rule count to: 0"))
                           (cond ((and dependent-condition (= dependent-condition 0))
                                  (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd)))
                                    (format t "~%set rule probability to: 1"))
                                  (setf (rule-probability new-rule) 1))
                                 (dependent-condition
                                  (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd)))
                                    (format t "~%set rule probability to: 0"))
                                  (setf (rule-probability new-rule) 0))
                                 (t
                                  (format t "~%WHAT PROBABILITY DO I SET NOW??")
                                  (break))))))
                   (t
                    (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd)))
                          (format t "~%var value:~%~S." value))
                    (let (retrieved-rule new-conditions unseen-new-condition)
                      (setq retrieved-rule (car (get-compatible-rules cpd cpd new-rule :find-all nil)))
                      (loop
                        with new-prob = 0 and missing-val
                        for missing being the hash-keys of var-dif
                        do
                           (setq missing-val (gethash missing (rule-conditions new-rule)))
                           ;; missing-val can be a list in the case of negation. When statement doesn't handle this case
                           (when (and missing-val (> missing-val 0))
                             (setq unseen-new-condition t)))
                      (cond ((and dependent-condition (= dependent-condition 0) (not unseen-new-condition))
                             (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd)))
                                   (format t "~%set rule probability to: ~d" (rule-probability retrieved-rule)))
                             (setf (rule-probability new-rule) (rule-probability retrieved-rule)))
                            ((and dependent-condition (= dependent-condition 0) unseen-new-condition)
                             (setf (rule-probability new-rule) 1))
                            ((and dependent-condition (> dependent-condition 0) (not unseen-new-condition))
                             (setf (rule-probability new-rule) (rule-probability retrieved-rule)))
                            ((and dependent-condition (> dependent-condition 0) unseen-new-condition)
                             (setf (rule-probability new-rule) 0)))
                      (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd)))
                            (format t "~%set rule probability to: ~d" (rule-probability new-rule))))))
             (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd)))
               (format t "~%updated new rule:~%~S" new-rule))
             (setq new-rules (cons new-rule new-rules))
           finally
              (remhash var avoid-hash)
              (return (values new-rules avoid-hash))))))

#| Make the rule of a rule compatible with context. See (Koller and Friedman, 2009) |#

;; rule = rule to split
;; conditions = context on which to split
;; cpd1 = conditional probability distribution
;; cpd2 = conditional probability distribution
;; var-dif = new idents in episode not in schema
(defun rule-split (rule conditions cpd1 cpd2 var-dif &key (enforce-compatible t) (avoid-hash (make-hash-table :test #'equal)))
  (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd1)))
    (format t "~%in rule-split on rule:~%~S~%conditions:~%~S~%avoid:~%~S" rule conditions avoid-hash))
  (cond ((and enforce-compatible (not (compatible-rule-p rule (make-rule :conditions conditions) cpd1 cpd2 :exact nil)))
         (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd1)))
               (format t "~%not compatible. Returning"))
         #|nil|# (list rule))
        ((not (hash-difference avoid-hash #|conditions|# (rule-conditions rule) nil))
         (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd1)))
           (format t "~%conditions are a subset of the current rule context. Returning"))
         (list rule))
        (t
         (let (y new-rules y-domain pos)
           (setq y (car (hash-difference avoid-hash #|conditions|# (rule-conditions rule) nil)))
           (cond (y
                  (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd1)))
                    (format t "~%splitting on ~S" y))
                  (setq pos (gethash y (rule-based-cpd-identifiers cpd1)))
                  ;;(setq y-domain (mapcar #'car (gethash pos (rule-based-cpd-var-value-block-map cpd1))))
                  (setq y-domain (gethash y avoid-hash))
                  (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd1)))
                    (format t "~%attribute domain:~%~S~%from cpd:~%~S" y-domain cpd1))
                  (multiple-value-setq (new-rules avoid-hash)
                    (split-rule-on-variable rule y y-domain cpd1 var-dif :avoid-hash avoid-hash))
                  (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd1)))
                    (format t "~%new rules:~%~S" new-rules)
                    ;;(break)
                    )
                  (mapcan #'(lambda (new-rule)
                              (rule-split new-rule conditions cpd1 cpd2 var-dif :enforce-compatible enforce-compatible :avoid-hash avoid-hash))
                          new-rules))
                 (t
                  (when nil (and (= cycle* 16) (equal "X525" (rule-based-cpd-dependent-id cpd1)))
                    (format t "~%No variable to split on. Returning"))
                  (list rule)))))))

#| Apply operation to rule when both rules share the same context |#

;; r1 = one of the rules
;; r2 = another rule
;; op = operation to apply
;; case = case number for rule
(defun rule-filter (r1 r2 op case)
  (let (norm-const new-prob rest-prob count)
    (cond ((or (eq #'+ op) (eq '+ op))
           (setq new-prob (funcall op (* (rule-probability r1) (rule-count r1))
                                   (* (rule-probability r2) (rule-count r2))))
           (setq norm-const (+ (rule-count r1) (rule-count r2)))
           (setq count norm-const))
          ((or (eq #'* op) (eq '* op))
           (setq rest-prob (funcall op (- 1 (rule-probability r1)) (- 1 (rule-probability r2))))
           (setq new-prob (funcall op (rule-probability r1) (rule-probability r2)))
           (setq norm-const (+ new-prob rest-prob))
           (cond ((rule-count r2)
                  (setq count (rule-count r2)))
                 (t
                  (setq norm-const 0)))))
    (cond (op
           (let (new-rule)
             (setq new-rule (make-rule :id (symbol-name (gensym "RULE-"))
                                       :conditions (copy-hash-table (rule-conditions r1))
                                       :probability (if (> norm-const 0)
                                                        (/ new-prob norm-const)
                                                        new-prob
                                                        ;;(error "norm const is 0.~%r1:~%~S~%r2:~%~S" r1 r2)
                                                        )
                                       :block (make-hash-table) ;;(list case)
                                       :count count))
             (setf (gethash case (rule-block new-rule)) case)
             new-rule))
          (t
           r1))))

#| replace rule in ruleset with its split correlates |#

;; existing-rules = ruleset
;; new-rules = rules to add to ruleset
;; remove-rule = base rule that was split on to remove from ruleset
(defun add-split-rules-to-existing (existing-rules new-rules remove-rule)
  (loop
    for new-rule in new-rules
    when (not (member new-rule existing-rules
                      :test #'(lambda (new existing)
                                (same-rule-p new existing :exact t))))
      do
         (setq existing-rules (cons new-rule existing-rules)))
  (remove remove-rule existing-rules :test #'equal))

#| Expand compatible rules in the rule set so compression works |#

;; r1 = rule from cpd1
;; r2 = rule from cpd2
;; cpd1 = conditional probability distribution
;; cpd2 = conditional probability distribution
(defun prepare-rules-for-split (r1 r2 cpd1 cpd2)
  (labels ((prepare-rule (copy-r1 copy-r2 r1-split-conditions r2-split-conditions cpd1 cpd2)
             (loop
               with cond-pos and domain
               for attribute being the hash-keys of (rule-conditions copy-r2)
                 using (hash-value val)
               do
                  (setq cond-pos (gethash attribute (rule-based-cpd-identifiers cpd2)))
                  (multiple-value-bind (value present-p)
                      (gethash attribute (rule-conditions copy-r1))
                    (cond ((not present-p)
                           (setq domain (mapcar #'cdar (gethash cond-pos (rule-based-cpd-var-value-block-map cpd2))))
                           (when (null domain)
                             (setq cond-pos (gethash attribute (rule-based-cpd-identifiers cpd1)))
                             (setq domain (gethash cond-pos (rule-based-cpd-var-values cpd1))))
                           (setf (gethash attribute r1-split-conditions) domain))
                          ((and (listp val) (numberp value))
                           (setq cond-pos (gethash attribute (rule-based-cpd-identifiers cpd2)))
                           (setq domain (nth (second val) (gethash cond-pos (rule-based-cpd-set-valued-negated-attributes cpd2))))
                           (when (null domain)
                             (setq cond-pos (gethash attribute (rule-based-cpd-identifiers cpd1)))
                             (setq domain (nth (second val) (gethash cond-pos (rule-based-cpd-set-valued-negated-attributes cpd1)))))
                           (setf (gethash attribute r2-split-conditions) domain)
                           (remhash attribute (rule-conditions copy-r2)))
                          ((and (listp value) (numberp val))
                           (setq cond-pos (gethash attribute (rule-based-cpd-identifiers cpd1)))
                           (setq domain (nth (second value) (gethash cond-pos (rule-based-cpd-set-valued-negated-attributes cpd1))))
                           (when (null domain)
                             (setq cond-pos (gethash attribute (rule-based-cpd-identifiers cpd2)))
                             (setq domain (nth (second val) (gethash cond-pos (rule-based-cpd-set-valued-negated-attributes cpd2)))))
                           (setf (gethash attribute r1-split-conditions) domain)
                           (remhash attribute (rule-conditions copy-r1)))
                          ((and (listp val) (listp value))
                           (setq cond-pos (gethash attribute (rule-based-cpd-identifiers cpd2)))
                           (setq domain (nth (second val) (gethash cond-pos (rule-based-cpd-set-valued-negated-attributes cpd2))))
                           (when (null domain)
                             (setq cond-pos (gethash attribute (rule-based-cpd-identifiers cpd1)))
                             (setq domain (nth (second val) (gethash cond-pos (rule-based-cpd-set-valued-negated-attributes cpd1)))))
                           (setf (gethash attribute r2-split-conditions) domain)
                           (remhash attribute (rule-conditions copy-r2))
                           (setq cond-pos (gethash attribute (rule-based-cpd-identifiers cpd1)))
                           (setq domain (nth (second value) (gethash cond-pos (rule-based-cpd-set-valued-negated-attributes cpd1))))
                           (when (null domain)
                             (setq cond-pos (gethash attribute (rule-based-cpd-identifiers cpd2)))
                             (setq domain (nth (second val) (gethash cond-pos (rule-based-cpd-set-valued-negated-attributes cpd2)))))
                           (setf (gethash attribute r1-split-conditions) domain)
                           (remhash attribute (rule-conditions copy-r1))))))
             (values copy-r1 r1-split-conditions copy-r2 r2-split-conditions)))
    (multiple-value-bind (split-r1 r1-split-conditions split-r2 r2-split-conditions)
        (prepare-rule (copy-cpd-rule r1) (copy-cpd-rule r2) (make-hash-table :test #'equal) (make-hash-table :test #'equal) cpd1 cpd2)
      (multiple-value-bind (split-r2 r2-split-conditions split-r1 r1-split-conditions)
          (prepare-rule (copy-cpd-rule split-r2) (copy-cpd-rule split-r1) r2-split-conditions r1-split-conditions cpd2 cpd1)
        (values split-r1 split-r2 r1-split-conditions r2-split-conditions)))))

#| Disambiguate rule conditions based on rule to split with |#

;; r1 = rule to disambiguate
;; r2 = reference rule
;; cpd = conditional probability distribution
(defun disambiguate-rules (r1 r2 cpd &key (allow-negations-p t) (incorporate-reference-conditions t))
  (loop
    for attribute being the hash-keys of (rule-conditions r1)
      using (hash-value value)
    when (not (gethash attribute (rule-conditions r2)))
      do
         (setf (gethash attribute (rule-conditions r2)) value))
  (loop
    with r1-val and r1-avoid-list = (make-hash-table :test #'equal) and r2-avoid-list = (make-hash-table :test #'equal)
    for attribute being the hash-keys of (rule-conditions r2)
      using (hash-value value)
    do
       (setq r1-val (gethash attribute (rule-conditions r1)))
       (when nil (and (= cycle* 3) (equal "GO_HOLD925" (rule-based-cpd-dependent-id cpd)))
         (format t "~%condition from reference rule:~% ~S~%matching condition from existing rule:~%~S" (cons attribute value) r1-val))
       (cond ((not r1-val)
              (cond (incorporate-reference-conditions
                     (cond ((and (listp value) allow-negations-p)
                            (setf (gethash attribute (rule-conditions r1)) value))
                           ((and (listp value) (not allow-negations-p))
                            ;;(setf (gethash attribute r1-avoid-list) (second r1-val))
                            (setf (gethash attribute r1-avoid-list)
                                  (nth (second r1-val)
                                       (gethash (gethash attribute (rule-based-cpd-identifiers cpd))
                                                (rule-based-cpd-set-valued-negated-attributes cpd))))
                            ;;(setf (gethash attribute r2-avoid-list) (second value))
                            (setf (gethash attribute r2-avoid-list)
                                  (nth (second value)
                                       (gethash (gethash attribute (rule-based-cpd-identifiers cpd))
                                                (rule-based-cpd-set-valued-negated-attributes cpd))))
                            (remhash attribute (rule-conditions r2))))
                     (setf (gethash attribute (rule-conditions r1)) value))
                    ((not incorporate-reference-conditions)
                     (cond ((listp value)
                            (setf (gethash attribute (rule-conditions r1)) (second value)))
                           ((numberp value)
                            ;;(setf (gethash attribute r1-avoid-list) value)
                            (loop
                              for i from 0 to (- (aref (rule-based-cpd-cardinalities cpd)
                                                       (gethash attribute (rule-based-cpd-identifiers cpd))) 1)
                              when (not (= i value))
                                collect i into domain
                              finally (setf (gethash attribute r1-avoid-list) domain)))))))
             ((and (numberp value) (not (numberp r1-val)))
              ;;(setf (gethash attribute r1-avoid-list) (second r1-val))
              (setf (gethash attribute r1-avoid-list)
                    (nth (second r1-val)
                         (gethash (gethash attribute (rule-based-cpd-identifiers cpd))
                                  (rule-based-cpd-set-valued-negated-attributes cpd))))
              (remhash attribute (rule-conditions r1)))
             ((and (numberp r1-val) (not (numberp value)))
              ;;(setf (gethash attribute r2-avoid-list) (second value))
              #|
              (setf (gethash attribute r2-avoid-list)
                    (nth (second value)
                         (gethash (gethash attribute (rule-based-cpd-identifiers cpd))
                                  (rule-based-cpd-set-valued-negated-attributes cpd))))
              |#
              (loop
                for i from 0 to (- (aref (rule-based-cpd-cardinalities cpd)
                                         (gethash attribute (rule-based-cpd-identifiers cpd))) 1)
                when (not (= i (second value)))
                  collect i into domain
                finally (setf (gethash attribute r2-avoid-list) domain))
              (remhash attribute (rule-conditions r2)))
             ((and allow-negations-p (not (numberp r1-val)) (not (numberp value)) (not (= (second r1-val) (second value))))
              ;;(setf (gethash attribute r1-avoid-list) (second r1-val))
              ;;(setf (gethash attribute r2-avoid-list) (second value))
              (setf (gethash attribute r1-avoid-list)
                    (nth (second r1-val)
                         (gethash (gethash attribute (rule-based-cpd-identifiers cpd))
                                  (rule-based-cpd-set-valued-negated-attributes cpd))))
              #|
              (setf (gethash attribute r2-avoid-list)
                    (nth (second value)
                         (gethash (gethash attribute (rule-based-cpd-identifiers cpd))
                                  (rule-based-cpd-set-valued-negated-attributes cpd))))
              |#
              (loop
                for i from 0 to (- (aref (rule-based-cpd-cardinalities cpd)
                                         (gethash attribute (rule-based-cpd-identifiers cpd))) 1)
                when (not (= i (second value)))
                  collect i into domain
                finally (setf (gethash attribute r2-avoid-list) domain))
              (remhash attribute (rule-conditions r1))
              (remhash attribute (rule-conditions r2)))
             ((and (not allow-negations-p) (not (numberp r1-val)) (not (numberp value)))
              ;;(setf (gethash attribute r1-avoid-list) (second r1-val))
              ;;(setf (gethash attribute r2-avoid-list) (second value))
              (setf (gethash attribute r1-avoid-list)
                    (nth (second r1-val)
                         (gethash (gethash attribute (rule-based-cpd-identifiers cpd))
                                  (rule-based-cpd-set-valued-negated-attributes cpd))))
              #|
              (setf (gethash attribute r2-avoid-list)
                    (nth (second value)
                         (gethash (gethash attribute (rule-based-cpd-identifiers cpd))
                                  (rule-based-cpd-set-valued-negated-attributes cpd))))
              |#
              (loop
                for i from 0 to (- (aref (rule-based-cpd-cardinalities cpd)
                                         (gethash attribute (rule-based-cpd-identifiers cpd))) 1)
                when (not (= i (second value)))
                  collect i into domain
                finally (setf (gethash attribute r2-avoid-list) domain))
              (remhash attribute (rule-conditions r1))
              (remhash attribute (rule-conditions r2))))
    finally
       (return (values r1 r2 r1-avoid-list r2-avoid-list))))

#|
(defun update-failed-rule-matches (rule counterpart-rules counterpart-cpd phi1 op incompatibles new-rules num-rules)
  ;;(format t "~%incompatible")
  (cond ((gethash (rule-id rule) incompatibles)
         (setf (gethash (rule-id rule) incompatibles) (+ (gethash (rule-id rule) incompatibles) 1)))
        (t
         (setf (gethash (rule-id rule) incompatibles) 1)))
  (when (= (gethash (rule-id rule) incompatibles) (array-dimension counterpart-rules 0))
    (when t (equal "Y553" (rule-based-cpd-dependent-id phi1))
      (format t "~%adding incompatible rule:~%~S" rule))
    (let (new-rule)
      (loop
        named looper
        with filter-rule-p = t and counterpart-rule
        with pos and existing-val and vvbm
        for attribute being the hash-keys of (rule-conditions rule)
          using (hash-value value)
        do
           (setq pos (gethash attribute (rule-based-cpd-identifiers counterpart-cpd)))
           (setq vvbm (gethash pos (rule-based-cpd-var-value-block-map counterpart-cpd)))
           (if (listp value)
               (setq existing-val t)
               (setq existing-val (rassoc value (mapcar #'car vvbm))))
           (when nil (equal "Y553" (rule-based-cpd-dependent-id phi1))
             (format t "~%attribute: ~S~%value: ~S~%position of attribute in counterpart cpd: ~d~%counterpart vvbm: ~S~%existing val: ~S" attribute value pos vvbm existing-val))
           (when (or (and (null pos) (null existing-val))
                     (and pos (> pos 0) (null existing-val)))
             (when nil (equal "Y553" (rule-based-cpd-dependent-id phi1))
               (format t "~%Adding new row to distribution"))
             (setq filter-rule-p nil)
             (setq new-rule (copy-cpd-rule rule :fresh-id t :count (+ (rule-count rule) 1)))
             (setf (rule-block new-rule) (list num-rules))
             (return-from looper nil))
        finally
           (when nil (equal "Y553" (rule-based-cpd-dependent-id phi1))
             (format t "~%filter rule?: ~S" filter-rule-p))
           (when filter-rule-p
             (when nil (equal "Y553" (rule-based-cpd-dependent-id phi1))
               (format t "~%Extending row in distribution"))
             (setq counterpart-rule (copy-cpd-rule rule :fresh-id t :count (rule-based-cpd-count counterpart-cpd)))
             (setf (rule-probability counterpart-rule) 0)
             (setq new-rule (rule-filter rule counterpart-rule op num-rules (not (rule-based-cpd-singleton-p phi1))))))
      (when nil (equal "Y553" (rule-based-cpd-dependent-id phi1))
        (format t "~%new rule:~%~S" new-rule))
      (setq new-rules (cons new-rule new-rules))
      (setq num-rules (+ num-rules 1))))
  (values new-rules num-rules))
|#

#| Remove duplicate rules such that we keep the rule with max count and discard the rest |#

;; rules = list of rules
;; cpd = conditional probability distribution from factor
(defun remove-duplicate-rules-max-count (rules cpd)
  (let ((result nil))
    (dolist (rule rules)
      (let ((existing (car (member rule result
				   :test #'(lambda (r1 r2)
					     (same-rule-p r1 r2 cpd cpd :check-count nil))))))
        (cond (existing
               (if (> (rule-count rule) (rule-count existing))
                   (setf (rule-count existing) (rule-count rule))))
	      (t
               (push rule result)))))
    result))

#| Remove duplicate rules such that we keep the rule with max count and discard the rest

;; rules = list of rules
;; cpd = conditional probability distribution from factor
(defun remove-duplicate-rules-max-count (rules cpd)
  (let ((seen (make-hash-table :test #'equalp :size (ceiling (* 1.3 (length rules)))))
	(result))
    (dolist (rule rules)
      (when nil
	(format t "~%~%rule:~%~S~%seen:~%~S" rule seen))
      (let ((existing (gethash (rule-conditions rule) seen)))
	(when nil
	  (format t "~%exitingp?:~%~S" existing))
        (cond (existing
               (if (> (rule-count rule) (rule-count existing))
                   (setf (rule-count existing) (rule-count rule)))
	       (when nil
		 (format t "~%updated rule:~%~S" rule)))
	      (t
	       (setf (gethash (rule-conditions rule) seen) rule)
	       (when nil
		 (format t "~%updated seen:~%~S" seen))))))
    (maphash #'(lambda (k v) (setq result (cons v result))) seen)
    result))
|#

#| Perform a filter operation over rules

;; rules1 = rules from phi1
;; rules2 = rules from phi2
;; phi1 = schema conditional probability distribution
;; phi2 = episode conditional probability distribution
;; op = operation to apply on rules
(defun operate-filter-rules (rules1 rules2 phi1 phi2 op &key (allow-negations-p t))
  (when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
	(format t "~%~%identifiers1:~%~S~%rules1:~%~S~%identifiers:2:~%~S~%rules2:~%~S" (rule-based-cpd-identifiers phi1) rules1 (rule-based-cpd-identifiers phi2) rules2))
  (loop
    with unmatched-qs = (make-array (length rules2) :initial-element nil)
    with match-p = nil
    with new-rule and new-rules and num-rules = 0
    for r1 being the elements of rules1
    when (or (and (or (eq op '+) (eq op #'+))
                  (> (rule-count r1) 0))
             (and (or (eq op '*) (eq op #'*))))
      do
	 (setq new-rule nil)
	 (setq match-p nil)
	 (loop
           with match-p = nil
           with split1 and split2 and avoid1 and avoid2
           with new-r1-rules and new-r2-rules and same-rule?
           for r2 being the elements of rules2
           for j from 0
           when (or (and (or (eq op '+) (eq op #'+))
			 (> (rule-count r2) 0))
                    (and (or (eq op '*) (eq op #'*)))) ;;(> (rule-count r2) 0)
             do
		(multiple-value-bind (compatible-p num-compatible)
                    (compatible-rule-p r1 r2 phi1 phi2)
		  (when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
			(format t "~%rule1:~%~S~%~S~%rule2:~%~S~%compatible: ~S" r1 op r2 compatible-p))
		  (cond (compatible-p
			 (setq match-p t)
			 (setf (aref unmatched-qs j) t)
			 (cond ((same-rule-p r1 r2 phi1 phi2 :check-count nil :check-probability nil :exact t)
				(when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
				      (format t "~%same-rule~%singleton-p phi1?: ~S" (singleton-cpd? phi1)))
				(setq new-rule (rule-filter r1 r2 op num-rules))
				(when (rule-based-cpd-singleton-p phi2)
				  (setf (rule-count new-rule) nil))
				(when (< (rule-probability new-rule) 0)
				  (format t "~%rule with negative probability:~%~S" new-rule)
				  (break))
				;;(format t "~%new rule:~% ~S" new-rule)
				(setf (rule-block new-rule) (make-hash-table))
				(setf (gethash num-rules (rule-block new-rule)) num-rules) ;;(setf (rule-block candidate-rule) (list num-rules))
				(setq new-rules (cons new-rule new-rules))
				(setq num-rules (+ num-rules 1))
				(when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
                                      ;;(format t "~%op: ~S" op)
                                      (format t "~%updated new rules:~%~S" new-rules)))
                               (t
				(when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
				      (format t "~%not same rule"))
				(when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
                                      (format t "~%splitting:~%~S~%and~%~S" r1 r2))
				(setq new-rule (rule-filter r1 r2 op num-rules))
				(when (rule-based-cpd-singleton-p phi2)
				  (setf (rule-count new-rule) nil))
				(when (< (rule-probability new-rule) 0)
				  (format t "~%rule with negative probability:~%~S" new-rule)
				  (break))
				(loop
				  for att being the hash-keys of (rule-conditions r2)
                                    using (hash-value val)
				  do
                                     (setf (gethash att (rule-conditions new-rule)) val))
				(when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
                                      (format t "~%new rule:~%~S" new-rule)
                                      ;;(break)
                                      )
				(setf (rule-block new-rule) (make-hash-table))
				(setf (gethash num-rules (rule-block new-rule)) num-rules) ;;(setf (rule-block candidate-rule) (list num-rules))
				(setq new-rules (cons new-rule new-rules))
				(setq num-rules (+ num-rules 1))
				(when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
                                      ;;(format t "~%op: ~S" op)
                                      (format t "~%updated new rules:~%~S" new-rules)
                                      ;;(break)
                                      ))))))
           finally
              (when (and (null match-p))
		(when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
                      (format t "~%no match for r1:~%~S~%in phi2" r1))
		(cond ((gethash (rule-based-cpd-dependent-id phi2) (rule-conditions r1))
                       (when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
			     (format t "~%~S~%contains phi2 dependent id: ~S" r1 (rule-based-cpd-dependent-id phi2)))
                       (setq new-rule (make-rule :id (symbol-name (gensym "RULE-"))
						 :conditions (copy-hash-table (rule-conditions r1))
						 :probability (rule-probability r1)
						 :block (make-hash-table)
						 :count (rule-count r1)))
                       (when (and (eq op '*) (rule-count r2))
			 (setf (rule-count new-rule) (rule-count r2)))
                       (when (or (eq op '+) (eq op #'+)
				 (and (or (eq op '*) (eq op #'*)) (= (gethash (rule-based-cpd-dependent-id phi2) (rule-conditions r1)) 0)))
			 (when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
                               (format t "~%adding~%~S~%to new rules" new-rule))
			 (setf (gethash num-rules (rule-block new-rule)) num-rules)
			 (setq new-rules (cons new-rule new-rules))
			 (setq num-rules (+ num-rules 1))))
                      (t
                       (when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
			     (format t "~%~S~%does not contain phi2 dependent id: ~S" r1 (rule-based-cpd-dependent-id phi2)))
                       (loop
			 for val in (gethash 0 (rule-based-cpd-var-values phi2))
			 do
                            (setq new-rule (make-rule :id (symbol-name (gensym "RULE-"))
                                                      :conditions (copy-hash-table (rule-conditions r1))
                                                      :probability (rule-probability r1)
                                                      :block (make-hash-table)
                                                      :count (rule-count r1)))
                            (when (and (eq op '*) (rule-count r2))
                              (setf (rule-count new-rule) (rule-count r2)))
                            (setf (gethash (rule-based-cpd-dependent-id phi2) (rule-conditions new-rule)) val)
                            (when (or (eq op '+) (eq op #'+)
                                      (and (or (eq op '*) (eq op #'*)) (= val 0)))
                              (when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
				    (format t "~%adding~%~S~%to new rules" new-rule))
                              (setf (gethash num-rules (rule-block new-rule)) num-rules)
                              (setq new-rules (cons new-rule new-rules))
                              (setq num-rules (+ num-rules 1))))))
		(when nil (and #|(eq op '*)|# (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
                      (format t "~%updated new rules for incompatible:~%~S" new-rules))))
    finally
       (loop
         with new-r2-rule
         for r2 in rules2
         for j from 0
         when (and (or (and (or (eq op '+) (eq op #'+))
                            (> (rule-count r2) 0))
                       (and (or (eq op '*) (eq op #'*))))
                   (null (aref unmatched-qs j)))
           do
              (setq new-r2-rule (copy-cpd-rule r2))
              (setf (rule-block new-r2-rule) (make-hash-table))
              (cond ((or (eq op '+) (eq op #'+))
                     (setf (gethash num-rules (rule-block new-r2-rule)) num-rules)
                     (setq new-rules (cons new-r2-rule new-rules))
                     (setq num-rules (+ num-rules 1)))
                    ((or (eq op '*) (eq op #'*))
                     (when (null (gethash (rule-based-cpd-dependent-id phi2) (rule-conditions new-r2-rule)))
                       (setf (gethash (rule-based-cpd-dependent-id phi2) (rule-conditions new-r2-rule)) 0))
                     (when (= (gethash (rule-based-cpd-dependent-id phi2) (rule-conditions new-r2-rule)) 0)
                       (setf (gethash num-rules (rule-block new-r2-rule)) num-rules)
                       (setq new-rules (cons new-r2-rule new-rules))
                       (setq num-rules (+ num-rules 1))))))
       (when nil (and (= cycle* cycle*) (equal "ON_HORIZONTAL_AXIS612" (rule-based-cpd-dependent-id phi1)))
             (format t "~%returning new rules:~%~S" new-rules)
             (break))
       (return new-rules)))
|#

#| Perform a filter operation over rules |#

;; rules1 = rules from phi1
;; rules2 = rules from phi2
;; phi1 = schema conditional probability distribution
;; phi2 = episode conditional probability distribution
;; new-idents = identifiers hash table for merged cpd
;; op = operation to apply on rules
(defun operate-filter-rules (rules1 rules2 phi1 phi2 new-idents op)
  (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
	(format t "~%~%identifiers1:~%~S~%rules1:~%~S~%identifiers:2:~%~S~%rules2:~%~S" (rule-based-cpd-identifiers phi1) rules1 (rule-based-cpd-identifiers phi2) rules2))
  (loop
    with seen = (make-hash-table :test #'equal :size (ceiling (* (hash-table-count new-idents) 1.3)))
    with unmatched-qs = (make-array (length rules2) :initial-element nil)
    with match-p = nil
    with new-rule and new-rules and num-rules = 0
    with existing and rule-key
    for r1 being the elements of rules1
    when (or (and (or (eq op '+) (eq op #'+))
                  (> (rule-count r1) 0))
             (and (or (eq op '*) (eq op #'*))
		  (or (rule-based-cpd-singleton-p phi1)
		      (> (rule-count r1) 0))))
      do
	 (setq new-rule nil)
	 (setq match-p nil)
	 (setq rule-key (make-array (+ (hash-table-count new-idents) 1)
				    :initial-element 0))
	 (setf (aref rule-key 0) 1)
	 (when nil (and (= cycle* 38))
	   (format t "~%rule1:~%~S~%cpd identifiers:~%~S" r1 (rule-based-cpd-identifiers phi1)))
	 (loop
	   for cond being the hash-keys of (rule-conditions r1)
	     using (hash-value val)
	   do
	      (when nil (and (= cycle* 38))
		(format t "~%att: ~S val: ~S~%identifiers:~%~S~%rule key:~%~S" cond val new-idents rule-key))
	      (setf (aref rule-key (+ (gethash cond new-idents) 1)) (+ 1 val)))
	 (loop
           with match-p = nil
           with split1 and split2 and avoid1 and avoid2
           with new-r1-rules and new-r2-rules and same-rule?
	   with rk
           for r2 being the elements of rules2
           for j from 0
           when (or (and (or (eq op '+) (eq op #'+))
			 (> (rule-count r2) 0))
                    (and (or (eq op '*) (eq op #'*))
			 (or (rule-based-cpd-singleton-p phi2)
			     (> (rule-count r2) 0)))) ;;(> (rule-count r2) 0)
             do
		(setq rk (copy-array rule-key))
		(multiple-value-bind (compatible-p num-compatible)
                    (compatible-rule-p r1 r2 phi1 phi2)
		  (declare (ignore num-compatible))
		  (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
			(format t "~%rule1:~%~S~%~S~%rule2:~%~S~%compatible: ~S" r1 op r2 compatible-p))
		  (cond (compatible-p
			 (setq match-p t)
			 (setf (aref unmatched-qs j) t)
			 (cond ((same-rule-p r1 r2 phi1 phi2 :check-count nil :check-probability nil :exact t)
				(when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
				      (format t "~%same-rule~%singleton-p phi1?: ~S" (singleton-cpd? phi1)))				
				(setq new-rule (rule-filter r1 r2 op num-rules))
				(when (rule-based-cpd-singleton-p phi2)
				  (setf (rule-count new-rule) nil))
				(when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
				  (format t "~%candidate new rule:~%~S~%rule key:~%~S" new-rule rk))
				(when (< (rule-probability new-rule) 0)
				  (format t "~%rule with negative probability:~%~S" new-rule)
				  (break))
			        (setq existing (gethash rk seen))
				(when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
				  (format t "~%rule exist in seen?:~%~S" existing))
				(cond (existing
				       (when (> (rule-count new-rule) (rule-count existing))
					 (setf (rule-count existing) (rule-count new-rule))
					 (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
					   (format t "~%updated existing rule count:~%~S" existing))))
				      (t
				       ;;(format t "~%new rule:~% ~S" new-rule)
				       (setf (rule-block new-rule) (make-hash-table))
				       (setf (gethash num-rules (rule-block new-rule)) num-rules) ;;(setf (rule-block candidate-rule) (list num-rules))
				       (setf (gethash rk seen) new-rule)
				       ;;(setq new-rules (cons new-rule new-rules))
				       (setq num-rules (+ num-rules 1))
				       (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
					 ;;(format t "~%op: ~S" op)
					 (format t "~%updated new rules:")
					 (maphash #'print-hash-entry seen)))))
                               (t
				(when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
				  (format t "~%not same rule"))
				(when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
                                      (format t "~%splitting:~%~S~%and~%~S" r1 r2))
				(setq new-rule (rule-filter r1 r2 op num-rules))
				(when (rule-based-cpd-singleton-p phi2)
				  (setf (rule-count new-rule) nil))
				(when (< (rule-probability new-rule) 0)
				  (format t "~%rule with negative probability:~%~S" new-rule)
				  (break))
				(loop
				  for att being the hash-keys of (rule-conditions r2)
                                    using (hash-value val)
				  do
                                     (setf (gethash att (rule-conditions new-rule)) val)
				     (setf (aref rk (+ (gethash att new-idents) 1)) (+ 1 val)))
				(when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
                                      (format t "~%candidate new rule:~%~S~%rule key:~%~S" new-rule rk)
                                      ;;(break)
                                      )
				(setq existing (gethash rk seen))
				(when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
				  (format t "~%rule exist in seen?:~%~S" existing))
				(cond (existing
				       (when (> (rule-count new-rule) (rule-count existing))
					 (setf (rule-count existing) (rule-count new-rule))
					 (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
					   (format t "~%updated existing rule count:~%~S" existing))))
				      (t
				       (setf (rule-block new-rule) (make-hash-table))
				       (setf (gethash num-rules (rule-block new-rule)) num-rules) ;;(setf (rule-block candidate-rule) (list num-rules))
				       (setf (gethash rk seen) new-rule)
				       ;;(setq new-rules (cons new-rule new-rules))
				       (setq num-rules (+ num-rules 1))
				       (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
					     ;;(format t "~%op: ~S" op)
					     (format t "~%updated new rules:")
					     (maphash #'print-hash-entry seen)
					     ;;(break)
					     ))))))))
           finally
              (when (and (null match-p))
		(when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
                      (format t "~%no match for r1:~%~S~%in phi2" r1))
		(cond ((gethash (rule-based-cpd-dependent-id phi2) (rule-conditions r1))
                       (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
			     (format t "~%~S~%contains phi2 dependent id: ~S" r1 (rule-based-cpd-dependent-id phi2)))
		       (setq new-rule (make-rule :id (symbol-name (gensym "RULE-"))
						 :conditions (copy-hash-table (rule-conditions r1))
						 :probability (rule-probability r1)
						 :block (make-hash-table)
						 :count (rule-count r1)))
                       (when (and (eq op '*) (rule-count r2))
			 (setf (rule-count new-rule) (rule-count r2)))
                       (when (or (eq op '+) (eq op #'+)
				 (and (or (eq op '*) (eq op #'*)) (= (gethash (rule-based-cpd-dependent-id phi2) (rule-conditions r1)) 0)))
			 (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
                               (format t "~%candidate new rule:~%~S~%rule key:~%~S" new-rule rule-key))
			 (setq existing (gethash rule-key seen))
			 (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
				  (format t "~%rule exist in seen?:~%~S" existing))
			 (cond (existing
				(when (> (rule-count new-rule) (rule-count existing))
				  (setf (rule-count existing) (rule-count new-rule))
				  (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
				    (format t "~%updated existing rule count:~%~S" existing))))
			       (t
				(setf (gethash num-rules (rule-block new-rule)) num-rules)
				(setf (gethash rule-key seen) new-rule)
				;;(setq new-rules (cons new-rule new-rules))
				(setq num-rules (+ num-rules 1))
				(when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
				  (format t "~%updated new rules:")
				  (maphash #'print-hash-entry seen))))))
                      (t
                       (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
			     (format t "~%~S~%does not contain phi2 dependent id: ~S" r1 (rule-based-cpd-dependent-id phi2)))
                       (loop
			 with rk = (copy-array rule-key)
			 for val in (gethash 0 (rule-based-cpd-var-values phi2))
			 do
			    (setq rk (copy-array rule-key))
                            (setq new-rule (make-rule :id (symbol-name (gensym "RULE-"))
                                                      :conditions (copy-hash-table (rule-conditions r1))
                                                      :probability (rule-probability r1)
                                                      :block (make-hash-table)
                                                      :count (rule-count r1)))
                            (when (and (eq op '*) (rule-count r2))
                              (setf (rule-count new-rule) (rule-count r2)))
                            (setf (gethash (rule-based-cpd-dependent-id phi2) (rule-conditions new-rule)) val)
			    (setf (aref rk (+ (gethash (rule-based-cpd-dependent-id phi2)
						       new-idents)
					      1))
				  (+ val 1))
                            (when (or (eq op '+) (eq op #'+)
                                      (and (or (eq op '*) (eq op #'*)) (= val 0)))
                              (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
				    (format t "~%candidate new rule:~%~S~%rule key:~%~S" new-rule rk))
			      (setq existing (gethash rk seen))
			      (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
				(format t "~%rule exist in seen?:~%~S" existing))
			      (cond (existing
				     (when (> (rule-count new-rule) (rule-count existing))
				       (setf (rule-count existing) (rule-count new-rule))
				       (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
					 (format t "~%updated existing rule count:~%~S" existing))))
				    (t
				     (setf (gethash num-rules (rule-block new-rule)) num-rules)
				     (setf (gethash rk seen) new-rule)
				     ;;(setq new-rules (cons new-rule new-rules))
				     (setq num-rules (+ num-rules 1))
				     (when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
				       (format t "~%updated new rules:")
				       (maphash #'print-hash-entry seen))))))))
		(when nil (and (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
                  (format t "~%updated new rules for incompatible:")
		  (maphash #'print-hash-entry seen))))
    finally
       (loop
         with new-r2-rule
         for r2 in rules2
         for j from 0
         when (and (or (and (or (eq op '+) (eq op #'+))
                            (> (rule-count r2) 0))
                       (and (or (eq op '*) (eq op #'*))
			    (or (rule-based-cpd-singleton-p phi2)
				(> (rule-count r2) 0))))
                   (null (aref unmatched-qs j)))
           do
              (setq new-r2-rule (copy-cpd-rule r2))
              (setf (rule-block new-r2-rule) (make-hash-table))
              (cond ((or (eq op '+) (eq op #'+))
                     (setf (gethash num-rules (rule-block new-r2-rule)) num-rules)
		     (setf (gethash j seen) new-r2-rule)
		     ;;(setq new-rules (cons new-r2-rule new-rules))
                     (setq num-rules (+ num-rules 1)))
                    ((or (eq op '*) (eq op #'*))
                     (when (null (gethash (rule-based-cpd-dependent-id phi2) (rule-conditions new-r2-rule)))
                       (setf (gethash (rule-based-cpd-dependent-id phi2) (rule-conditions new-r2-rule)) 0))
                     (when (= (gethash (rule-based-cpd-dependent-id phi2) (rule-conditions new-r2-rule)) 0)
                       (setf (gethash num-rules (rule-block new-r2-rule)) num-rules)
		       (setf (gethash j seen) new-r2-rule)
		       ;;(setq new-rules (cons new-r2-rule new-rules))
                       (setq num-rules (+ num-rules 1))))))
       (maphash #'(lambda (key rule)
		    (setq new-rules (cons rule new-rules)))
		seen)
       (when nil (and (= cycle* cycle*) (eq op '*) (equal "SELF2713766" (rule-based-cpd-dependent-id phi2)))
             (format t "~%returning new rules:~%~S" new-rules)
             ;;(break)
	     )
       (return (nreverse new-rules))))

#| Perform an operation over rules

;; rules1 = rules from phi1
;; rules2 = rules from phi2
;; phi1 = schema conditional probability distribution
;; phi2 = episode conditional probability distribution
;; op = operation to apply on rules
(defun operate-rules-1 (rules1 rules2 phi1 phi2 op var-dif &key (allow-negations-p t))
  (when nil (and (= cycle* 2) (equal "BLOCK505" (rule-based-cpd-dependent-id phi1)))
    (format t "~%~%identifiers1:~%~S~%rules1:~%~S~%identifiers:2:~%~S~%rules2:~%~S" (rule-based-cpd-identifiers phi1) rules1 (rule-based-cpd-identifiers phi2) rules2))
  (loop
    with new-rule and new-rules and num-rules = 0
    for r1 being the elements of rules1
    do
       (setq new-rule nil)
       (loop
         with split1 and split2 and avoid1 and avoid2 and candidate-rules
         with new-r1-rules and new-r2-rules and same-rule?
         for r2 being the elements of rules2
         do
            (multiple-value-bind (compatible-p num-compatible)
                (compatible-rule-p r1 r2 phi1 phi2)
              (when nil (and (= cycle* 2) (equal "BLOCK505" (rule-based-cpd-dependent-id phi1)))
                    (format t "~%rule1:~%~S~%~S~%rule2:~%~S~%compatible: ~S" r1 op r2 compatible-p))
              (cond (compatible-p
                     (cond ((same-rule-p r1 r2 phi1 phi2 :check-count nil :check-probability nil :exact t)
                            (when nil (and (= cycle* 2) (equal "BLOCK505" (rule-based-cpd-dependent-id phi1)))
                              (format t "~%same-rule"))
                            (setq new-rule (rule-filter r1 r2 op num-rules (not (singleton-cpd? phi1))))
                            ;;(format t "~%new rule:~% ~S" new-rule)
                            (setq candidate-rules (list new-rule)))
                           (t
                            (when nil (and (= cycle* 2) (equal "BLOCK505" (rule-based-cpd-dependent-id phi1)))
                              (format t "~%not same rule"))
                            (when nil (and (= cycle* 2) (equal "BLOCK505" (rule-based-cpd-dependent-id phi1)))
                                  (format t "~%splitting:~%~S~%and~%~S" r1 r2))
                            (loop
                              with new-rule = (rule-filter r1 r2 op num-rules (not (singleton-cpd? phi1)))
                              for att being the hash-keys of (rule-conditions r2)
                                using (hash-value val)
                              do
                                 (setf (gethash att (rule-conditions new-rule)) val)
                              finally
                                 (setq candidate-rules (list new-rule)))
                            (when nil (and (= cycle* 2) (equal "BLOCK505" (rule-based-cpd-dependent-id phi1)))
                                  (format t "~%new rule:~%~S" (car candidate-rules))
                                  ;;(break)
                                  )))
                     (loop
                       for candidate-rule in candidate-rules
                       for i from 0
                       do
                          (when t #|(and (or (null op)
                                         (and op
                                              (compatible-rule-p candidate-rule r1 nil nil)
                                              (compatible-rule-p candidate-rule r2 nil nil)))
                                       (not (member candidate-rule new-rules :test #'(lambda (r1 r2) (same-rule-p r1 r2 phi1 phi2)))))
                                |#
                            ;; look up compatibilities
                            ;; if compatible with rule that has diff probability, break compatibilities by adding conditions
                            ;; conditions must be not present in cpd
                            (when nil (and (= cycle* 7) (equal "HAND1679" (rule-based-cpd-dependent-id phi1)))
                                  (format t "~%added new rule"))
                            (setf (rule-block candidate-rule) (make-hash-table))
                            (setf (gethash num-rules (rule-block candidate-rule)) num-rules) ;;(setf (rule-block candidate-rule) (list num-rules))
                            (setq new-rules (cons candidate-rule new-rules))
                            (setq num-rules (+ num-rules 1))
                            (when nil (and (= cycle* 2) (equal "BLOCK505" (rule-based-cpd-dependent-id phi1)))
                                  ;;(format t "~%op: ~S" op)
                                  (format t "~%updated new rules:~%~S" new-rules)
                                  (break))))))))
    finally
       (when nil (and (= cycle* 15) (equal "INTENTION37068" (rule-based-cpd-dependent-id phi1)))
             (format t "~%returning new rules:~%~S" new-rules))
       (return new-rules)))
|#

#| Perform an operation over rules

;; rules1 = rules from phi1
;; rules2 = rules from phi2
;; phi1 = schema conditional probability distribution
;; phi2 = episode conditional probability distribution
;; op = operation to apply on rules
;; var-dif = new idents in episode not in schema
(defun operate-rules-2 (rules1 rules2 phi1 phi2 op var-dif &key (allow-negations-p t))
  (when (and (= cycle* 17) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
        (format t "~%~%identifiers1:~%~S~%rules1:~%~S~%identifiers2:~%~S~%rules2:~%~S" (rule-based-cpd-identifiers phi1) rules1 (rule-based-cpd-identifiers phi2) rules2))
  (loop
    with new-rule and new-rules and num-rules = 0
    with r1 and match-p = nil
    with rules2-copy = rules2
    while rules1
    do
       (setq r1 (car rules1))
       (setq new-rule nil)
       (setq match-p nil)
       (loop
         named loop-rules2
         with split1 and split2 and avoid1 and avoid2 and candidate-rules
         with new-r1-rules and new-r2-rules
         with r2
         with j = 0
         with rules2-copy-indexer = rules2-copy
         while rules2
         do
            (setq r2 (car rules2))
            (multiple-value-bind (compatible-p num-compatible)
                (compatible-rule-p r1 r2 phi1 phi2)
              (declare (ignore num-compatible))
              (when (and (= cycle* 17) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                (format t "~%rule1:~%~S~%~S~%rule2:~%~S~%compatible: ~S" r1 op r2 compatible-p))
              (cond (compatible-p
                     (cond ((same-rule-p r1 r2 phi1 phi2 :check-count nil :check-probability nil :exact t)
                            (when (and (= cycle* 17) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                              (format t "~%same-rule"))
                            (setq new-rule (rule-filter r1 r2 op num-rules (not (singleton-cpd? phi1))))
                            ;;(format t "~%new rule:~% ~S" new-rule)
                            (setf (rule-block new-rule) (make-hash-table))
                            (setf (gethash num-rules (rule-block new-rule)) num-rules)
                            (setq new-rules (cons new-rule new-rules))
                            (setq num-rules (+ num-rules 1))
                            (when (and (= cycle* 17) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                              ;;(format t "~%op: ~S" op)
                              (format t "~%updated new rules:~%~S" new-rules)
                              ;;(break)
                              )
                            (setq rules1 (rest rules1))
                            (setq r1 (car rules1))
                            (cond ((= j 0)
                                   (setq rules2 (rest rules2))
                                   (setq rules2-copy rules2)
                                   (setq rules2-copy-indexer rules2-copy))
                                  (t
                                   (setf (cdr rules2-copy-indexer) (cdr rules2))
                                   (setq rules2 rules2-copy)))
                            (setq match-p t)
                            (return-from loop-rules2 nil))
                           (t
                            (when (and (= cycle* 17) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                              (format t "~%not same rule"))
                            (when (and (= cycle* 17) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                              (format t "~%splitting:~%~S~%and~%~S" r1 r2))
                            (multiple-value-setq (split1 split2 avoid1 avoid2)
                              (prepare-rules-for-split r1 r2 phi1 phi2))
                            (when (and (= cycle* 17) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                              (format t "~%rule1:~%~S~%rule2:~%~S~%avoid1:~%~S~%avoid2:~%~S" split1 split2 avoid1 avoid2))
                            (setq new-r1-rules (rule-split split1 (rule-conditions split2) phi1 phi2 var-dif :enforce-compatible (not (null op)) :avoid-hash avoid1))
                            (setq new-r2-rules (rule-split split2 (rule-conditions split1) phi2 phi1 (make-hash-table) :enforce-compatible (not (null op)) :avoid-hash avoid2))
                            (when (and (= cycle* 17) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                              (format t "~%split r1:~%~S~%split r2:~%~S" new-r1-rules new-r2-rules))
                            (setf (cdr (last new-r1-rules)) (rest rules1))
                            (setf (cdr (last new-r2-rules)) (rest rules2))
                            (setq rules1 new-r1-rules)
                            (setq r1 (car rules1))
                            (setq rules2 new-r2-rules)
                            (cond ((= j 0)
                                   (setq rules2-copy rules2)
                                   (setq rules2-copy-indexer rules2-copy))
                                  (t
                                   (setf (cdr rules2-copy-indexer) new-r2-rules)))
                            (when (and (= cycle* 17) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                              (format t  "~%~%new r1 rules:~%~S~%new r2 rules:~%~S" rules1 rules2-copy)
                              ;;(break)
                              ))))
                    (t
                     (setq j (+ j 1))
                     (setq rules2-copy-indexer rules2)
                     (setq rules2 (rest rules2)))))
         finally
            (when (and (= cycle* 17) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
              (format t "~%~%no match for~%~S~%skipping" r1)
              ;; (break)
              )
            (setq rules2 rules2-copy))
       (when (not match-p)
         (setq rules1 (rest rules1)))
    finally
       (when nil (and (= cycle* 15) (equal "INTENTION37068" (rule-based-cpd-dependent-id phi1)))
             (format t "~%returning new rules:~%~S" new-rules))
       (return new-rules)))
|#

#| Check cpd vvbms |#

;; cpd = conditional probability distribution
(defun check-cpd-vvbms (cpd)
  (when nil
    (loop
      for card being the elements of (rule-based-cpd-cardinalities cpd)
      for idx being the hash-keys of (rule-based-cpd-var-value-block-map cpd)
        using (hash-value vvbm)
      when (not (= card (length vvbm)))
        do
           (format t "~%Malformed cpd cardinalities/vvbms:~%~S" cpd)
           (error "Check vvbms"))))

#| Check if cpd is a valid cpd |#

;; cpd = conditional probability distribution
(defun check-cpd (cpd &key (check-uniqueness t) (check-prob-sum t) (check-counts t) (check-count-prob-agreement t))
  (when t
    (loop
      with row-len = (aref (rule-based-cpd-cardinalities cpd) 0)
      with index-rule = (make-rule :conditions (make-hash-table :test #'equal))
      with row-probs and row-counts = (make-list row-len :initial-element 0) and row-rules and row-assns and row-prob and compatible-rule and reference-count
      with assn = (make-array (hash-table-count (rule-based-cpd-identifiers cpd)) :initial-element 0)
      for i from 0 to (reduce #'* (rule-based-cpd-cardinalities cpd))
      do
         (setq assn (get-cpd-assignment-from-index cpd i))
         (loop
           for ident being the hash-keys of (rule-based-cpd-identifiers cpd)
             using (hash-value pos)
           do
              (setf (gethash ident (rule-conditions index-rule)) (aref assn pos)))
         (setq compatible-rule (get-compatible-rules cpd cpd index-rule))
         (when (and compatible-rule (null reference-count))
           (setq reference-count (rule-count (car compatible-rule))))
         (cond ((null compatible-rule)
                (format t "~%no compatible rule for assignment:~%~S~%cpd:~%~S" index-rule cpd)
                (error "check compatible rules"))
               ((and check-uniqueness (> (length compatible-rule) 1))
                (format t "~%multiple rules fire for assignment:~%~S~%cpd:~%~S~%compatible rules:~%~S" index-rule cpd compatible-rule)
                (error "check compatible rules"))
               ((and (not check-uniqueness) (= -1 (reduce #'(lambda(x y) (if (= x y) x -1))
                                                          (mapcar #'(lambda (rule)
                                                                      (rule-probability rule))
                                                                  compatible-rule))))
                (format t "~%compatible rules have different probabilities.~%Assignment:~%~S~%cpd:~%~S~%compatible rules:~%~S" index-rule cpd compatible-rule)
                (error "check compatible rules"))
               #|
               ((not (= (rule-count (car compatible-rule)) reference-count))
                (format t "~%rule count is different from cpd count~%rule:~%~S~%cpd:~%~S" (car compatible-rule) cpd)
                (error "check rule count"))
               |#
               ((and check-count-prob-agreement (not (integerp (* (rule-count (car compatible-rule)) (rule-probability (car compatible-rule))))))
                (format t "~%probability-count mismatch.~%rule:~%~S~%cpd:~%~S" (car compatible-rule) cpd)
                (error "check rule count and probability"))
               (t
                (setq compatible-rule (car compatible-rule))))
         (setq row-probs (cons (rule-probability compatible-rule) row-probs))
         (setq row-rules (cons compatible-rule row-rules))
         (setq row-assns (cons assn row-assns))
         ;;(setq row-counts (cons (rule-count compatible-rule) row-counts))
         (cond ((gethash (rule-based-cpd-dependent-id cpd) (rule-conditions compatible-rule))
                (setf (nth (gethash (rule-based-cpd-dependent-id cpd) (rule-conditions compatible-rule)) row-counts)
                      (rule-count compatible-rule)))
               (t
                (setq row-counts (make-list row-len :initial-element (rule-count compatible-rule)))))
         (when (= (length row-probs) row-len)
           (setq row-prob (reduce #'+ row-probs))
           (cond ((and check-prob-sum (not (= 1 row-prob)))
                  (format t "~%Malformed cpd:~%~S~%row assignments:~%~S~%row rules:~%~S~%row probs:~%~S~%row probability is ~d, not 1" cpd row-assns row-rules row-probs row-prob)
                  (error "Check row sums"))
                 ((and check-counts (notevery #'= row-counts (rest row-counts)))
                  (format t "~%Malformed cpd:~%~S~%row assignments:~%~S~%row rules:~%~S~%row probs:~%~S~%row counts:~%~S~% row counts are not equal" cpd row-assns row-rules row-probs row-counts)
                  (error "Check row counts")))
           (setq row-probs nil)
           (setq row-rules nil)
           (setq row-assns nil)
           (setq row-counts (make-list row-len :initial-element 0))))))

#| Generate intermediate factor by multiplying two existing ones.
Roughly based on (Koller and Friedman, 2009) |#

;; phi1 = conditional probability density 1
;; phi2 = conditional probability density 2
;; op = operation to apply to factor (* or +)
(defun factor-filter (phi1 phi2 &optional (op '*))
  (labels ((expand-rules (cpd1 cpd2)
             (let (var-dif expanded-rules)
               (setq var-dif (block-difference (rule-based-cpd-identifiers cpd2)
                                               (rule-based-cpd-identifiers cpd1)
                                               :output-hash-p t
                                               :test #'equal))
               (when nil (and (= cycle* 3) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                     (format t "~%idents in episode not in schema:~%~S~%hash table count: ~d" var-dif (hash-table-count var-dif)))
               (cond ((> (hash-table-count var-dif) 0)
                      (loop
                        with missing-idx
                        for missing being the hash-keys of var-dif
                        do
                           (setq missing-idx (gethash missing (rule-based-cpd-identifiers cpd2)))
                           (setf (gethash missing var-dif) (gethash missing-idx (rule-based-cpd-var-values cpd2))))
                      (when nil (and (= cycle* 3) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                            (format t "~%idents and their domains:~%~S" var-dif))
                      (loop
                        with split-rules
                        for rule being the elements of (rule-based-cpd-rules cpd1)
                        do
                           (when nil (and (= cycle* 3) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                                 (format t "~%~%rule:~%~S" rule)
                                 (break))
                        when (> (rule-count rule) 0)
                          do
                             (setq split-rules (rule-split rule (make-hash-table :test #'equal) cpd1 cpd2 var-dif
                                                           :enforce-compatible (not (null op))
                                                           :avoid-hash (copy-hash-table var-dif)))
                             (when nil (and (= cycle* 3) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                                   (format t "~%split rules:~%~S" split-rules))
                             (setf (cdr (last split-rules)) expanded-rules)
                             (setq expanded-rules split-rules)
                             (when nil (and (= cycle* 3) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                                   (format t "~%expansion:~%~S" expanded-rules)
                                   (break))
                        else
                          do
                             (setq expanded-rules (cons rule expanded-rules))
                             (when nil (and (= cycle* 5) (equal "HAND524" (rule-based-cpd-dependent-id phi1)))
                                   (format t "~%expansion:~%~S" expanded-rules)
                                   (break))))
                     (t
                      (setq expanded-rules (reverse (coerce (rule-based-cpd-rules cpd1) 'list)))))
               (values expanded-rules var-dif))))
    (cond ((and (numberp phi1) (rule-based-cpd-p phi2))
         (return-from factor-filter phi2))
        ((and (numberp phi2) (rule-based-cpd-p phi1))
         (return-from factor-filter  phi1))
        ((and (numberp phi1) (numberp phi2))
         (return-from factor-filter phi1))
        ((and (rule-based-cpd-p phi1) (null phi2))
         (return-from factor-filter phi1)))
  (let (var-union types idents concept-ids qvars values cardinalities steps var-value-block-map negated-vvbms sva svna lower-vvbms lower-nvvbms new-phi new-rules)
    (multiple-value-setq (idents var-union types concept-ids qvars var-value-block-map negated-vvbms sva svna lower-vvbms lower-nvvbms values)
      (ordered-union phi1 phi2))
    (when nil (and #|(eq op '*)|# (eq op '+) (equal "GOAL732" (rule-based-cpd-dependent-id phi1)))
      (format t "~%~%phi1:~%~A~%phi2:~%~A~%unioned-ids: ~A~%var union: ~A~%unioned-concept-ids: ~A~%qualified vars: ~A~%var value block map: ~S" phi1 phi2 idents var-union concept-ids qvars var-value-block-map))
    (setq cardinalities (get-var-cardinalities var-value-block-map))
    (setq steps (generate-cpd-step-sizes cardinalities))
    (setq new-phi (make-rule-based-cpd :dependent-id (rule-based-cpd-dependent-id phi1)
                                       :identifiers idents
                                       :dependent-var (rule-based-cpd-dependent-var phi1)
                                       :vars var-union
                                       :types types
                                       :concept-ids concept-ids
                                       :qualified-vars qvars
                                       :var-value-block-map var-value-block-map
                                       :set-valued-attributes sva
                                       :set-valued-negated-attributes svna
                                       :negated-vvbms negated-vvbms
                                       :lower-approx-var-value-block-map lower-vvbms
                                       :lower-approx-negated-vvbms lower-nvvbms
                                       :characteristic-sets (make-hash-table)
                                       :characteristic-sets-values (make-hash-table)
                                       :var-values values
                                       :cardinalities cardinalities
                                       :step-sizes steps
                                       :count (if (or (eq #'+ op) (eq '+ op)) (+ (rule-based-cpd-count phi1) (rule-based-cpd-count phi2)))
                                       :singleton-p (rule-based-cpd-singleton-p phi1)
                                       :lvl (rule-based-cpd-lvl phi1)))
    (when nil (and (eq op '+) (equal "GOAL732" (rule-based-cpd-dependent-id phi1)))
      (format t "~%~%unexpanded schema rules:~%~S" (rule-based-cpd-rules phi1)))
    (multiple-value-bind (expanded-schema-rules var-dif)
        (expand-rules phi1 phi2)
      (declare (ignore var-dif))
      (when nil (and (eq op '+) (equal "GOAL732" (rule-based-cpd-dependent-id phi1)))
        (format t "~%~%expanded schema rules:~%~S" expanded-schema-rules))
      (setq new-rules (operate-filter-rules (coerce (rule-based-cpd-rules phi2) 'list) expanded-schema-rules phi2 phi1 idents op))
      (when (> (length new-rules) (reduce #'* (rule-based-cpd-cardinalities new-phi)))
	 (format t "~%number of rules exceeds cpd parameters.~%new phi:~%~S~%rules:~%~S" new-phi new-rules)
	 (break))
      (when (rule-based-cpd-singleton-p phi1)
        (loop
          with norm-const = (reduce #'+ new-rules :key #'rule-probability)
          for rule in new-rules
          when (> norm-const 0)
	    do
               (setf (rule-probability rule) (/ (rule-probability rule) norm-const))
	  else
	    do
	       (setf (rule-probability rule) (/ 1 (length new-rules)))))
      (when nil (and (eq op '+) (equal "GOAL732" (rule-based-cpd-dependent-id phi1)))
        (format t "~%~%new rules:~%~S" new-rules)
        ;;(break)
        ))
    (setq new-rules (make-array (length new-rules) :initial-contents new-rules))
    (when nil (and (= cycle* 2) (equal "HOLDING1182" (rule-based-cpd-dependent-id phi1)))
      (format t "~%rules before compression:~%~S" new-rules)
      (break))
    #|
    (setq new-phi (get-local-coverings
                   (update-cpd-rules new-phi
                                     (make-array (length new-rules)
                                                 :initial-contents new-rules))))
    |#
    (cond ((eq op '*)
           (setf (rule-based-cpd-rules new-phi)
                 (make-array (length new-rules) :initial-contents new-rules))
           ;;(check-cpd new-phi :check-uniqueness nil :check-prob-sum nil :check-counts nil :check-count-prob-agreement nil)
           )
          (t
           (setq new-phi (update-cpd-rules new-phi (make-array (length new-rules)
                                                               :initial-contents new-rules)))))
    ;;(check-cpd new-phi :check-uniqueness nil)
    (when nil (and (eq op '+) (equal "NO_OP7336" (rule-based-cpd-dependent-id phi1)))
      (format t "~%final rules:~%~S" new-phi)
      ;;(break)
      )
    new-phi)))

#| Merge two matching factors together |#

;; phi1 = conditional probability density from pattern
;; phi2 = conditional probability density from base
;; bindings = variable bindings
;; new-nodes = list of corespondences from p to q so far
;; phi2-count = schema episode count
(defun factor-merge (phi1 phi2 bindings new-nodes phi2-count)
  (cond ((null phi2)
         (loop
           with phi1-copy = (copy-rule-based-cpd phi1)
           with rule and new-rules
           for i from 0 to 1
           do
              (setq rule (make-rule :id (symbol-name (gensym "RULE-"))
                                    :conditions (make-hash-table :test #'equal)
                                    :block (make-hash-table) ;;(list i)
                                    :count (cond ((= (hash-table-count (rule-based-cpd-identifiers phi1)) 1)
                                                  phi2-count)
                                                 (t
                                                  0))
                                    ;;0 #|(rule-based-cpd-count phi1-copy) |#
                                    ))
              (setf (gethash i (rule-block rule)) i)
              (cond ((= i 0)
                     (setf (gethash (rule-based-cpd-dependent-id phi1) (rule-conditions rule)) 0)
                     (setf (rule-probability rule) 1)
                     (setq new-rules (cons rule new-rules)))
                    ((= i 1)
                     ;;(setf (gethash (rule-based-cpd-dependent-id phi1) (rule-conditions rule)) (list 'not 0))
                     (loop
                       with loop-rule
                       for vvb in (gethash 0 (rule-based-cpd-var-value-block-map phi1-copy))
                       for j from 0
                       when (not (= j 0))
                         do
                            (setq loop-rule (copy-cpd-rule rule))
                            (setf (gethash (rule-based-cpd-dependent-id phi1) (rule-conditions loop-rule)) j)
                            (setf (rule-probability loop-rule) 0)
                            (setq new-rules (cons loop-rule new-rules)))
                     ;;(setf (rule-probability rule) 0)
                     ;; add new-rules together with additional rules
                     ))
           finally
              (let (merged)
                (setf (rule-based-cpd-rules phi1-copy) (make-array 2 :initial-contents new-rules))
                (setf (rule-based-cpd-count phi1-copy) phi2-count)
                (when nil (and (= cycle* 3) (equal "INTENTION2751" (rule-based-cpd-dependent-id phi1)))
                  (format t "~%created schema match:~%~S" phi1-copy))
                (setq merged (factor-merge phi1 phi1-copy bindings new-nodes phi2-count))
                (when nil (= cycle* 15)
                      (format t "~%merged:~%~S" merged)
                      ;;(break)
                      )
                (return merged))))
        (t
         (when nil (and (= cycle* cycle*) (equal "GOAL732" (rule-based-cpd-dependent-id phi2)))
           (format t "~%~%episode before update:~%~S~%schema before update:~%~S" phi1 phi2)
           ;;(format t "~%updating episode with schema")
           )
         (setq phi2 (cpd-update-existing-vvms phi2 bindings new-nodes))
         (when nil (and (= cycle* cycle*) (equal "GOAL732" (rule-based-cpd-dependent-id phi2)))
           (format t "~%intermediate schema:~%~S" phi2))
         ;;(check-cpd phi2 :check-uniqueness nil)
	 (setq phi2 (cpd-update-schema-domain phi2 phi1))
         ;;(check-cpd phi2 :check-uniqueness nil)
         (when nil (and (= cycle* cycle*) (equal "GOAL732" (rule-based-cpd-dependent-id phi2)))
           (format t "~%intermediate schema2:~%~S" phi2))
	 (setq phi1 (subst-cpd phi1 phi2 bindings))
	 (when nil (and (= cycle* cycle*) (equal "GOAL732" (rule-based-cpd-dependent-id phi2)))
	   (format t "~%intermediate episode:~%~S" phi1)
	   ;;(break)
	   )
	 (setq phi1 (cpd-transform-episode-domain phi1 phi2))
         (when nil (and (= cycle* cycle*) (equal "GOAL732" (rule-based-cpd-dependent-id phi2)))
           (format t "~%episode after update:~%~S~%schema after update:~%~S" phi1 phi2)
           ;;(break)
	   )
	 (when nil (and (= cycle* cycle*) (gethash "ACTION7357" (rule-based-cpd-identifiers phi2))
		    (= (cdar (assoc "NO_OP7356"
				    (gethash (gethash "ACTION7357" (rule-based-cpd-identifiers phi2))
					     (rule-based-cpd-var-value-block-map phi2))
				    :key #'car
				    :test #'equal))
		       2))
	   (format t "~%malformed schema CPD:~%~S" phi2)
	   (break))
	 ;;(check-cpd-vvbms phi1)
         ;;(check-cpd phi1 :check-uniqueness nil)
         (factor-filter phi2 phi1 '+))))


#| Perform a marginalize operation over rules |#

;; phi = schema conditional probability distribution
;; vars = variable to keep
;; op = operation to apply on rules
;; new-dep-id = dependent variable after marginalization step is complete
(defun operate-marginalize-rules-keep (phi vars op new-dep-id)
  (when nil (or (equal "LEFT_OF608" (rule-based-cpd-dependent-id phi))
	    (equal "SELF573" (rule-based-cpd-dependent-id phi))) nil t
    (format t "~%phi:~%~S~%vars to keep:~%~S" phi vars))
  (loop
    with rules = (rule-based-cpd-rules phi)
    with new-rules
    with marginalized-rule
    with rule-bag
    with global-ignore-idxs
    with intersection1
    with num-rules = 0
    for r1 being the elements of rules
    for i from 0
    do
       (when nil t
	 (format t "~%~%rule at index i = ~d~%~S~%global ignore rule indeces:~%~S" i r1 global-ignore-idxs))
       (setq marginalized-rule nil)
       (setq intersection1 nil)
       (loop
	 named inter
	 with val
	 for var in vars
	 do
	    (setq val (gethash var (rule-conditions r1)))
	 when val
	   do
	      (setq intersection1 (cons (cons var val) intersection1)))
       (cond ((not intersection1)
	      (when nil t
		(format t "~%adding rule to new rules because by default"))
	      (setq marginalized-rule (copy-cpd-rule r1))
	      (setf (rule-block marginalized-rule) (make-hash-table))
	      (setf (gethash num-rules (rule-block marginalized-rule)) num-rules)
	      ;;(setq num-rules (+ num-rules 1))
	      )
	     ((and intersection1 (not (member i global-ignore-idxs)))
	      (setq rule-bag (list r1))
	      (setq marginalized-rule (make-rule :id (gensym "RULE-")
						 :conditions (make-hash-table :test #'equal)
						 :probability (rule-probability r1)
						 :block (make-hash-table)
						 :count (rule-count r1)))
	      (loop
		for inter in intersection1
		do
		   (setf (gethash (car inter) (rule-conditions marginalized-rule))
			 (cdr inter)))
	      (when nil t
		(format t "~%initial marginalized rule:~%~S"marginalized-rule))
	      (setf (gethash num-rules (rule-block marginalized-rule)) num-rules)
	      ;;(setq num-rules (+ num-rules 1))
	      (when nil t
		(format t "~%rule has intersection with keep vars"))
	      (loop
		with intersection2
		with local-ignore = global-ignore-idxs
		with num-conditions
		for r2 being the elements of rules
		for j from 0
		when (not (= j i)) do
		  (when nil t
		    (format t "~%  checking if r2 is compatible with marginalized rule. r2:~%  ~S" r2)
		    (format t "~%  local ignore:~%  ~S" local-ignore))
		  (setq intersection2 nil)
		  (loop
		    named inter
		    with val
		    for var in vars
		    do
		       (setq val (gethash var (rule-conditions r2)))
		    when val
		      do
			 (setq intersection2 (cons (cons var val) intersection2)))
		  (cond ((not intersection2)
			 (setq rule-bag (cons r2 rule-bag))
			 (setf (rule-probability marginalized-rule)
			       (funcall op
					(rule-probability marginalized-rule)
					(rule-probability r2)))
			 (when (not (rule-based-cpd-singleton-p phi))
			   (setf (rule-count marginalized-rule)
				 (funcall op 1
					  #|
					  (if (rule-count marginalized-rule)
					      (rule-count marginalized-rule)
					      1)
					  (if (rule-count r2)
					      (rule-count r2)
					      1)
					  |#)))
			 (when nil t
			   (format t "~%  compatible by default.~%  updated marginalized rule:~%  ~S" marginalized-rule)))
			(t
			 (when (compatible-rule-p r2 marginalized-rule phi phi)
			   (when nil t
			     (format t "~%  compatible: T"))
			   (setq rule-bag (cons r2 rule-bag))
			   (setf (rule-probability marginalized-rule)
				 (funcall op
					  (rule-probability marginalized-rule)
					  (rule-probability r2)))
			   (when (not (rule-based-cpd-singleton-p phi))
			     (setf (rule-count marginalized-rule)
				   (funcall op 1
					    #|
					    (if (rule-count marginalized-rule)
						(rule-count marginalized-rule)
						1)
					    (if (rule-count r2)
						(rule-count r2)
						1)
					    |#)))
			   (setq num-conditions (hash-table-count (rule-conditions marginalized-rule)))
			   (loop
			     for inter in intersection2
			     do
				(setf (gethash (car inter) (rule-conditions marginalized-rule))
				      (cdr inter)))
			   (if (> (hash-table-count (rule-conditions marginalized-rule)) num-conditions)
			       (setq local-ignore (cons j global-ignore-idxs))
			       (setq local-ignore (cons j local-ignore)))
			   (when nil t
			     (format t "~%  updated marginalized rule:~%  ~S" marginalized-rule)))))
		finally
		   (setq new-rules (cons marginalized-rule new-rules))
		   (setq num-rules (+ num-rules 1))
		   (setq global-ignore-idxs local-ignore)
		   (when nil t
		     (format t "~%updated new rules:~%~S~%updated global ignore rule indexes:~%~S" new-rules global-ignore-idxs)))))
    finally
       #|
       (when (not (rule-based-cpd-singleton-p phi))
	 (loop
           with new-rule
	   with dep-val
           for rule in new-rules
           do
	      (setq dep-val (gethash new-dep-id (rule-conditions rule)))
	      #|
	      (when (and dep-val (= 0 dep-val) (not (= (rule-probability rule) 0)))
		(format t "~%observed case with ~S = 0 with probability greater than 0.~%marginalized rule:~%~S~%phi:~%~S" new-dep-id rule phi)
		(break "problem?"))
	      |#
	      (when (and dep-val (not (= 0 dep-val)))
		(setq new-rule (copy-cpd-rule rule))
		(setf (gethash new-dep-id (rule-conditions new-rule)) 0)
		(setf (rule-probability new-rule) 0)
		(setf (rule-count new-rule) 1)
		(setf (rule-block new-rule) (make-hash-table))
		(setf (gethash num-rules (rule-block new-rule)) num-rules)
		(setq num-rules (+ num-rules 1))
		(when nil (and (equal "TOWER545" (rule-based-cpd-dependent-id phi)))
                      (format t "~%new rule:~%~S" new-rule))
		(setq new-rules (cons new-rule new-rules)))))
    |#
       (when (null new-rules)
	 (error "No marginalized rules left."))
       (when nil
	 (format t "~%returning:~%~S" new-rules))
       (return (make-array num-rules :initial-contents (reverse new-rules)))))


#| Perform a marginalize operation over rules |#

;; phi = schema conditional probability distribution
;; var = variable to marginalize out
;; op = operation to apply on rules
;; new-dep-id = dependent variable after marginalization step is complete
(defun operate-marginalize-rules (phi var op new-dep-id)
  (loop
    with rules = (rule-based-cpd-rules phi)
    with var-idx = (gethash var (rule-based-cpd-identifiers phi))
    with new-rules
    with var-val1 and num-var-vals = (make-hash-table)
    with marginalized-rule
    with rule-bag
    for r1 being the elements of rules
    do
       (setq var-val1 (gethash var (rule-conditions r1)))
       (setq rule-bag (list r1))
       (when nil (and (equal "TOWER545" (rule-based-cpd-dependent-id phi)))
         (format t "~%~%rule1:~%~S~%var: ~S val: ~S" r1 var var-val1))
       (setq marginalized-rule (copy-cpd-rule r1))
       (cond ((null var-val1)
              (loop
                for i in (gethash var-idx (rule-based-cpd-var-values phi))
                do
                   (setf (gethash i num-var-vals) t)))
             (t
              (setf (gethash var-val1 num-var-vals) t)
              (remhash var (rule-conditions marginalized-rule))))
      (when nil (and (equal "TOWER545" (rule-based-cpd-dependent-id phi)))
        (format t "~%~%candidate marginalized rule:~%~S" marginalized-rule))
      (loop
        with var-val2
        for r2 being the elements of rules
        for idx-j from 0
        do
           (setq var-val2 (gethash var (rule-conditions r2)))
           (when nil (and (equal "TOWER545" (rule-based-cpd-dependent-id phi)))
             (format t "~%r2:~%~S~%var-val2:~%~S" r2 var-val2))
        when (and (or (and var-val2
                           (not (eq var-val1 var-val2)))
                      (null var-val2))
                  (compatible-rule-p marginalized-rule r2 phi phi :avoid var))
          do
             (when nil (and (equal "TOWER545" (rule-based-cpd-dependent-id phi)))
               (format t "~%compatiblep: T"))
             (setq rule-bag (reverse (cons r2 (reverse rule-bag))))
             (cond ((null var-val2)
                    (loop
                      for i in (gethash var-idx (rule-based-cpd-var-values phi))
                      do
                         (setf (gethash i num-var-vals) t)))
                   (t
                    (setf (gethash var-val2 num-var-vals) t)))
             (setf (rule-probability marginalized-rule) (funcall op (rule-probability marginalized-rule) (rule-probability r2)))
             (when (< (rule-probability marginalized-rule) 0)
               (format t "~%rule with negative probability:~%~S" marginalized-rule)
               (break))
             (when (not (rule-based-cpd-singleton-p phi))
               (setf (rule-count marginalized-rule)
                     (funcall op
                              (if (rule-count marginalized-rule)
                                  (rule-count marginalized-rule)
                                  1)
                              (if (rule-count r2)
                                  (rule-count r2)
                                  1))))
             (loop
               for att being the hash-keys of (rule-conditions r2)
                 using (hash-value value)
               when (and (not (equal att var)) (null (gethash att (rule-conditions marginalized-rule))))
                 do
                    (setf (gethash att (rule-conditions marginalized-rule)) value))
             (when nil (and (equal "TOWER545" (rule-based-cpd-dependent-id phi)))
               (format t "~%updated marginalized rule:~%~S" marginalized-rule))
        finally
          (let ((val-dif (- (length (gethash var-idx (rule-based-cpd-var-values phi))) (hash-table-count num-var-vals)))
                (dep-val nil))
            (when nil (and (equal "TOWER545" (rule-based-cpd-dependent-id phi)))
                  (format t "~%rule:~%~S~%rule-bag:~%~S~%~S domain: ~S~%observed values for ~S: ~d~%num missing observations: ~d" r1 rule-bag var (gethash var-idx (rule-based-cpd-var-values phi)) var num-var-vals val-dif))
            (when (not (=  val-dif 0))
              (setq dep-val (gethash (rule-based-cpd-dependent-id phi) (rule-conditions r1)))
              (cond ((null dep-val)
                     (error "Rule has no assignment for ~S which is the dependent variable." (rule-based-cpd-dependent-id phi)))
                    ((= dep-val 0)
                     (setf (rule-probability marginalized-rule) (+ (rule-probability marginalized-rule) (* val-dif 1)))
                     (when (< (rule-probability marginalized-rule) 0)
                       (format t "~%rule with negative probability:~%~S" marginalized-rule)
                       (break))
                     (when (not (rule-based-cpd-singleton-p phi))
                       (setf (rule-count marginalized-rule) (+ (if (rule-count marginalized-rule)
                                                                   (rule-count marginalized-rule)
                                                                   1)
                                                               0))))))
            (setq new-rules (cons marginalized-rule new-rules))
            (when nil (and (equal "TOWER545" (rule-based-cpd-dependent-id phi)))
                  (format t "~%updated new rules:~%~S" new-rules))))
    finally
       (when (equal var (rule-based-cpd-dependent-id phi))
         (when nil (and (equal "TOWER545" (rule-based-cpd-dependent-id phi)))
           (format t "~%adding rules for unobserved cases with ~S = 0" new-dep-id))
         (loop
           with new-rule
           for rule in new-rules
           do
              (when (gethash new-dep-id (rule-conditions rule))
                (setq new-rule (copy-cpd-rule rule))
                (setf (gethash new-dep-id (rule-conditions new-rule)) 0)
                (setf (rule-probability new-rule) 0)
                (setf (rule-count new-rule) 1)
                (when nil (and (equal "TOWER545" (rule-based-cpd-dependent-id phi)))
                  (format t "~%new rule:~%~S" new-rule))
                (setq new-rules (cons new-rule new-rules)))))
       (return (remove-duplicates new-rules
                                  :test #'(lambda (r1 r2)
                                            (same-rule-p r1 r2 phi phi :check-probability nil :check-count nil))))))

(defun reduce-cpd-meta-data (phi var)
  (let* ((var-pos (gethash var (rule-based-cpd-identifiers phi)))
         new-dep-id new-dep-var new-identifiers
         (var-difference (reduce-ordinal-hash (rule-based-cpd-vars phi) var-pos))
         (type-difference (reduce-ordinal-hash (rule-based-cpd-types phi) var-pos))
         (new-concept-ids (reduce-ordinal-hash (rule-based-cpd-concept-ids phi) var-pos))
         (new-qvars (reduce-ordinal-hash (rule-based-cpd-qualified-vars phi) var-pos))
         (new-vvbm (reduce-ordinal-hash (rule-based-cpd-var-value-block-map phi) var-pos))
         (new-sva (reduce-ordinal-hash (rule-based-cpd-set-valued-attributes phi) var-pos))
         (new-svna (reduce-ordinal-hash (rule-based-cpd-set-valued-negated-attributes phi) var-pos))
         (new-negated-vvbm (reduce-ordinal-hash (rule-based-cpd-negated-vvbms phi) var-pos))
         (new-lower-vvbm (reduce-ordinal-hash (rule-based-cpd-lower-approx-var-value-block-map phi) var-pos))
         (new-lower-nvvbm (reduce-ordinal-hash (rule-based-cpd-lower-approx-negated-vvbms phi) var-pos))
         (new-values (reduce-ordinal-hash (rule-based-cpd-var-values phi) var-pos))
         (cardinalities (get-var-cardinalities new-vvbm))
         (steps (generate-cpd-step-sizes cardinalities))
         ;;(var-card (aref (rule-based-cpd-cardinalities phi) var-pos))
         (marginalized nil))
    (multiple-value-setq (new-identifiers new-dep-id)
      (reduce-categorical-hash (rule-based-cpd-identifiers phi) var var-pos))
    (when (null new-dep-id)
      (setq new-dep-id (rule-based-cpd-dependent-id phi)))
    (setq new-dep-var (gethash 0 var-difference))
    (setq marginalized (make-rule-based-cpd :dependent-id new-dep-id
                                            :identifiers new-identifiers
                                            :dependent-var new-dep-var
                                            :vars var-difference
                                            :types type-difference
                                            :concept-ids new-concept-ids
                                            :qualified-vars new-qvars
                                            :var-value-block-map new-vvbm
                                            :negated-vvbms new-negated-vvbm
                                            :set-valued-attributes new-sva
                                            :set-valued-negated-attributes new-svna
                                            :lower-approx-var-value-block-map new-lower-vvbm
                                            :lower-approx-negated-vvbms new-lower-nvvbm
                                            :characteristic-sets (make-hash-table)
                                            :characteristic-sets-values (make-hash-table)
                                            :var-values new-values
                                            :cardinalities cardinalities
                                            :step-sizes steps
					    :rules (rule-based-cpd-rules phi)
					    :count (rule-based-cpd-count phi)
                                            :singleton-p (rule-based-cpd-singleton-p phi)
                                            :lvl (rule-based-cpd-lvl phi)))
    marginalized))

#| Marginalize out one variable from factor |#

;; phi = conditional probability density
;; vars = variables to keep
;; op = operation to apply to factor (max or +)
(defun operate-factor (phi vars op)
  (setf (rule-based-cpd-rules phi)
	(operate-marginalize-rules-keep phi vars op (rule-based-cpd-dependent-id phi)))
  phi)

#| Generate intermediate factor by marginalization or max |#

;; phi = conditional probability density
;; keep = vars to keep
;; remove = variables to op out
;; op = operation to apply to factor (max or +)
(defun factor-operation (phi keep remove  op)
  (cond ((null remove)
	 (operate-factor phi keep op))
	(t
	 (factor-operation (reduce-cpd-meta-data phi (car remove)) keep (rest remove) op))))

#| Prepare factor graph for message passing |#

;; edges = array of edges in factor graph
;; evidence = self-messages from conditional probability densities
(defun initialize-graph (edges evidence)
  ;;See (Koller & Friedman, 2009) if this is not a factor graph
  (when nil
    (format t "~%size of evidence:~%~A~%size of edges:~%~A" (hash-table-count evidence) (array-dimension edges 0)))
  (loop
    for edge being the elements of edges
    when (not (= (car edge) (cdr edge)))
      do
         (when (null (gethash (car edge) evidence))
           (setf (gethash (car edge) evidence) (make-hash-table)))
         (setf (gethash (cdr edge) (gethash (car edge) evidence)) 1))
  evidence)

#| Send a message from one factor to the other |#

;; i = index of conditional probability density message sender
;; j = index of conditional probability density message receiver
;; factors = array of conditional probability densities
;; op = operation to apply to factor (max or +)
;; edges = array of edges in factor graph
;; messages = messages from factor to factor
;; sepset = separating set preserving the Running Intersection Property
(defun send-message (i j factors op edges messages sepset)
  ;;(format t "~%edges:~%~A" edges)
  ;;(print-messages messages)
  (when nil t
    (format t "~%~%sending message from ~d to ~d~%~d: ~S~%~d: ~S" i j i (rule-based-cpd-identifiers (aref factors i)) j (rule-based-cpd-identifiers (aref factors j))))
  (let (nbrs-minus-j reduced)
    (loop
       for k from 0 to (- (array-dimension edges 0) 1)
       for edge being the elements of edges
       when (and (= (cdr edge) i) (not (= (car edge) j)))
       collect (gethash i (gethash (car edge) messages)) into neighbors
       finally (setq nbrs-minus-j neighbors))
    (when nil t
      (format t "~%neighbors minus j:~%~S~%i:~%~S" nbrs-minus-j (aref factors i)))
    (setq reduced (reduce 'factor-filter (cons (aref factors i) nbrs-minus-j)))
    (when nil t
      (format t "~%evidence-collected:~%~S~%sepset: ~S~%variables to eliminate: ~S" reduced sepset
              (set-difference (hash-keys-to-list (rule-based-cpd-identifiers reduced)) sepset :test #'equal)))
    (factor-operation reduced sepset (set-difference (hash-keys-to-list (rule-based-cpd-identifiers reduced)) sepset :test #'equal) op)))

#| Compute final belief of a factor |#

;; i = index of conditional probability density
;; factors = array of conditional probability densities
;; edges = array of edges in factor graph
;; messages = messages from factor to factor
(defun compute-belief (i factors edges messages)
  (loop
    with factor
    for k from 0 to (- (array-dimension edges 0) 1)
    for edge being the elements of edges
    when (= (cdr edge) i)
      collect (gethash i (gethash (car edge) messages)) into nbrs
    finally
       (when nil
         (format t "~%computing belief for factor:~%~A~%on nbrs:~%~A" (aref factors i) nbrs))
       (setq factor (reduce 'factor-filter (cons (aref factors i) nbrs)))
       (return factor)))

#| Dampen message signals to avoid oscilations

;; message = conditional probability density
;; messages = messages from factor to factor
;; j = index into messages
;; k = index into messages
;; lr = learning rate to dampen updates, and help convergence
(defun smooth (factor j k messages lr)
  (let (factor-rules old-cpd old-rules new-rules)
    (setq factor-rules (coerce (rule-based-cpd-rules factor) 'list))
    (setq old-cpd (gethash k (gethash j messages)))
    (when (rule-based-cpd-p old-cpd)
      (setq old-rules (coerce (rule-based-cpd-rules old-cpd) 'list))
      (setq new-rules (mapcar #'(lambda (r1 r2)
                                  (make-rule
                                   :id (gensym "RULE-")
                                   :conditions (rule-conditions r1)
                                   :probability (rationalize (+ (* lr r1) (* (- 1 lr) r2)))
                                   :block (rule-block r1)
                                   :certain-block (rule-certain-block r1)
                                   :avoid-list (rule-avoid-list r1)
                                   :redundancies (rule-redundancies r1)
                                   :count (rule-count r1)))
                              factor-rules old-rules))
      (setf (rule-based-cpd-rules factor) (make-array (length new-rules) :initial-contents new-rules :fill-pointer t))
      ;;(break "new-message: ~A~%old-message: ~A~%res: ~A~%updated-factor: ~A" factor-assns old-assns new-assns factor)
      )
    factor))
|#

(defun smooth (factor j k messages lr)
  (let (old-cpd new-assns)
    (cond ((= lr 1)
	   factor)
	  (t
	   (setq old-cpd (gethash k (gethash j messages)))
     (loop
       with new-val and new-rule and r2
       for r1 being the elements of (rule-based-cpd-rules factor)
       ;;for r2 being the elements of (rule-based-cpd-rules old-cpd)
       do
          (cond ((rule-based-cpd-p old-cpd)
                 (setq r2 (car (get-compatible-rules old-cpd factor r1 :find-all nil))))
                (t
                 (setq r2 (make-rule :probability old-cpd))))
          (setq new-val (rationalize (+ (* lr (rule-probability r1)) (* (- 1 lr) (rule-probability r2)))))
          (setq new-rule (copy-cpd-rule r1))
          (setf (rule-probability new-rule) new-val)
       collect new-rule into new-rules
       finally
          (setf (rule-based-cpd-rules factor) (make-array (array-dimension (rule-based-cpd-rules factor) 0) :initial-contents new-rules)))
	   (normalize-rule-probabilities factor (rule-based-cpd-dependent-id factor))))))

#| Check if factor message has changed from one iteration to the next |#

;; m1 = current message
;; m2 = updated message
(defun same-message-p (m1 m2 &key round)
  (cond ((and (numberp m1) (numberp m2))
         (= m1 m2))
        ((and (numberp m1) (rule-based-cpd-p m2))
         nil)
        ((and (rule-based-cpd-p m1) (numberp m2))
         nil)
        ((and (rule-based-cpd-p m1) (rule-based-cpd-p m2))
         (cond ((not (= (array-dimension (rule-based-cpd-rules m1) 0)
                        (array-dimension (rule-based-cpd-rules m2) 0)))
                nil)
               (t
                (loop
                  for rule being the elements of (rule-based-cpd-rules m1)
                  when (notany #'(lambda (r) (same-rule-p rule r m1 m2 :check-count nil :round round)) (rule-based-cpd-rules m2))
                    do
                       (return-from same-message-p nil))
                (loop
                  for rule being the elements of (rule-based-cpd-rules m2)
                  when (notany #'(lambda (r) (same-rule-p rule r m1 m2 :check-count nil :round round)) (rule-based-cpd-rules m1))
                    do
                       (return-from same-message-p nil))
                t)))))

#| Perform loopy belief propagation over a factor graph
   See (Koller & Friedman, 2009) |#

;; factors = array of conditional probability densities
;; op = operation to apply to factor (max or +)
;; edges = array of edges in factor graph
;; evidence = hashtable of self-messages from conditional probability densities
;; lr = learning rate to dampen updates, and help convergence
(defun calibrate-factor-graph (factors op edges evidence lr)
  (loop
    with round = t
    with j and k and sepset and messages = (initialize-graph edges evidence)
    with calibrated and conflicts and max-iter = 200 and deltas
    for count from 0
    do
       (when nil t
         (format t "~%~%Iteration: ~d." count))
       (setq calibrated t)
       (setq conflicts nil)
       (setq deltas nil)
       (loop
         with current-message and new-message
         for i from 0 to (- (array-dimension edges 0) 1)
         do
            (setq j (car (aref edges i)))
            (setq k (cdr (aref edges i)))
            (when (not (= j k))
              (setq sepset (hash-intersection (rule-based-cpd-identifiers (aref factors j))
                                              (rule-based-cpd-identifiers (aref factors k))
                                              :test #'equal))
              (when nil t
                    (format t "~%~%factor j = ~d:~%~A~%factor k = ~d:~%~A~%sepset: ~A" j (rule-based-cpd-identifiers (aref factors j)) k (rule-based-cpd-identifiers (aref factors k)) sepset))
              (setq current-message (gethash k (gethash j messages)))
              ;;(setq new-message (smooth (send-message j k factors op edges messages sepset) j k messages lr))
              (setq new-message (send-message j k factors op edges messages sepset))
	      ;; when doing sum-product message passing, normalize after getting the message
	      (when (or (eq #'+ op) (eq '+ op))
		(setq new-message (normalize-rule-probabilities new-message (rule-based-cpd-dependent-id new-message)))
		;;(check-cpd new-message :check-uniqueness nil :check-prob-sum nil #|(when (not (rule-based-cpd-singleton-p marginalized)) t)|# :check-counts nil :check-count-prob-agreement nil)
		)
	      (setq new-message (smooth new-message j k messages lr))
	      (when nil t
                (format t "~%current message from ~d:" j)
                (print-hash-entry k current-message)
                (format t "~%new message from ~d:" j)
                (print-hash-entry k new-message))
              (loop
                for new-rule being the elements of (rule-based-cpd-rules new-message)
                do
                   (cond (round
                          (setq deltas (cons (abs (- (read-from-string (format nil "~$" (rule-probability new-rule)))
                                                     (read-from-string (format nil "~$"
                                                                               (if (rule-based-cpd-p current-message)
                                                                                   (rule-probability (car (get-compatible-rules
                                                                                                           current-message
                                                                                                           new-message
                                                                                                           new-rule
                                                                                                           :find-all nil)))
                                                                                   current-message)))))
                                             deltas)))
                         (t
                          (setq deltas (cons (abs (- (rule-probability new-rule)
                                                     (if (rule-based-cpd-p current-message)
                                                         (rule-probability (car (get-compatible-rules
                                                                                 current-message
                                                                                 new-message
                                                                                 new-rule
                                                                                 :find-all nil)))
                                                         current-message)))
                                             deltas)))))
              (setf (gethash k (gethash j messages)) new-message))
         when (not (same-message-p current-message new-message :round t))
           do
              (setq conflicts (cons (cons current-message new-message) conflicts))
              (setq calibrated nil))
       ;;(break "~%end of iteration")
       (when nil t
	 (format t "~%~%num conflicts: ~d" (length conflicts))
	 (format t "~%delta_mean: ~d~%delta_std: ~d" (float (mean deltas)) (float (stdev deltas))))
       (log-message (list "~d,~d,~d,~d,~d~%" lr count (length conflicts) (float (mean deltas)) (float (stdev deltas))) "learning-curves.csv")
    until (or calibrated (= (+ count 1) max-iter))
    finally
       (when nil t
         (cond (calibrated
                (format t "~%Reached convergence after ~d iterations." (+ count 1)))
               (t
                (format t "~%Reached inference limit at iteration ~d." (+ count 1)))))
       (return
         (cond ((eq op '+)
                (loop
                  for i from 0 to (- (array-dimension factors 0) 1)
                  collect (compute-belief i factors edges messages)))
               ((eq op 'max)
                (when nil
                  (format t "~%Computing most likely state."))
                ;;(break)
                (let (constraints vars unassigned csp copy-factors-list)
                  (when nil
                    (format t "~%factors:~%~A" factors))
                  (loop
                    for factor being the elements of factors
                    do
                       (setq copy-factors-list (cons (copy-rule-based-cpd factor) copy-factors-list))
                    finally
                       (setq copy-factors-list (reverse copy-factors-list)))
                  (when nil
                    (format t "~%Max marginals:~%~A" copy-factors-list))
                  (loop
                    for i from 0 to (- (array-dimension factors 0) 1)
                    for copy-factor in copy-factors-list
                    with constraint and max-val and new-assns and num-assignments
                    do
                       (setq max-val 0)
                       (setq constraint (compute-belief i factors edges messages))
                       (when nil
                         (format t "~%constraint:~%~A" constraint)
                         (break))
                       (setq num-assignments (reduce #'* (coerce (cpd-cardinalities constraint) 'list)))
                       (setf (cpd-counts constraint) (cpd-counts copy-factor))
                       (setq new-assns (make-hash-table))
                       (loop
                         for i from 0 to (- num-assignments 1)
                         with hashed-val and max = 0
                         do
                            (cond ((cpd-counts constraint)
                                   (when (> (access-counts (cpd-counts constraint) i (aref (cpd-cardinalities constraint) 0)) 0)
                                     (setq hashed-val (hash-access (cpd-assignments constraint) 0 constraint (list i)))
                                     (if (> hashed-val max)
                                         (setq max hashed-val))
                                     (if (> hashed-val 0)
                                         (setf (gethash i new-assns) hashed-val))))
                                  (t
                                   (setq hashed-val (hash-access (cpd-assignments constraint) 0 nil (list i)))
                                   (if (> hashed-val max)
                                       (setq max hashed-val))))
                         finally (setq max-val max))
                       (when (cpd-counts constraint)
                         (setf (cpd-assignments constraint) new-assns))
                       (loop
                         for j from 0 to (- num-assignments 1)
                         do
                            (cond ((and (> max-val 0) (= (hash-access (cpd-assignments constraint) 0 constraint (list j)) max-val))
                                   (setf (gethash j (cpd-assignments constraint)) 1))
                                  (t
                                   (remhash j (cpd-assignments constraint)))))
                    collect constraint into cnstrts
                    finally
                       ;;(break)
                       (setq constraints cnstrts))
                  (setq vars (remove-duplicates
                              (mapcan #'(lambda (cpd)
                                          (hash-keys-to-list (cpd-identifiers cpd)))
                                      constraints)
                              :test #'equal))
                  (loop
                    for constraint in constraints
                    do
                    #|
                       (when (= (length (cpd-identifiers constraint)) 1)
                       (format t "~%singleton constraint:~%~A~%assignments" constraint)
                       (maphash #'print-hash-entry (cpd-assignments constraint)))|#
                       (loop
                         with bindings and values
                         for var being the hash-keys of (cpd-identifiers constraint)
                           using (hash-value pos)
                         do
                            (setq bindings (gethash pos (cpd-var-value-map constraint)))
                            (setq values (mapcar 'car bindings))
                            (when (not (member (cons var values)
                                               unassigned :test #'equal))
                              (setq unassigned (cons (cons var values)
                                                     unassigned)))))
                  (setq unassigned (node-consistency unassigned constraints))
                  (setq csp (list ':constraints (make-array (length constraints) :initial-contents constraints :fill-pointer t) ':vars vars))
                  (when nil
                    (format t "~%constraints:~%~S" (getf csp :constraints))
                    ;;(break)
                    )
                  ;;(backtracking-search csp unassigned)
                  (min-conflicts csp)))))))

(defun ins (item lst &optional (key #'<))
  (if (null lst)
      (list item)
      (if (funcall key (cdr item) (cdr (car lst)))
          (cons item lst)
          (cons (car lst) (ins item (cdr lst) key)))))

(defun iter-ins (item lst &optional (key #'<))
  (cond ((null lst)
	 (list item))
	(t
	 (loop
	    for val in lst
	    for i from 0
	    when (funcall key (second item) (second val))
	    do
	      (return (append (subseq lst 0 i) (cons item (subseq lst i))))
	    finally
	      (return (reverse (cons item (reverse lst))))))))

#| Make a ruleset for describing the conditional probability distribution

;; cpd = conditional probability distribution
;; assn = assignment with probability 1
(defun make-initial-rules (cpd assn)
  (labels ((invert-assignment-at-index (assn idx invert-map)
             (setf (aref assn idx)
                   (cdr (assoc (aref assn idx)
                               invert-map)))
             assn)
           (make-initial-rule (asn prob case count)
             (let (rule)
               (setq rule (make-rule :id (symbol-name (gensym "RULE-"))
                                     :conditions (make-hash-table :test #'equal)
                                     :probability prob
                                     :block (make-hash-table) ;;(list case)
                                     :certain-block (make-hash-table) ;;(list case)
                                     :count count))
               (setf (gethash case (rule-block rule)) case)
               (setf (gethash case (rule-certain-block rule)) case)
               (loop
                 for attribute being the hash-keys of (rule-based-cpd-identifiers cpd)
                   using (hash-value pos)
                 when (< pos (array-dimension asn 0))
                   do
                      (setf (gethash attribute (rule-conditions rule)) (aref asn pos)))
               rule)))
    (let (assn-0)
      (setq assn-0 (copy-array assn))
      (if (= (aref assn 0) 0)
          (setf (aref assn-0 0) 1)
          (setf (aref assn-0 0) 0))
      (loop
        with var-value-prob-map = '((0 . 1) (1 . 0))
        with rules and case = 0 and rule
        for assignment in (list assn assn-0)
        for prob in (list 1 0)
        do
           (setq rule (make-initial-rule assignment prob case 1))
           (setq rules (cons rule rules))
           (setq case (+ case 1))
           (when (> (array-dimension assignment 0) 1)
             (setq assignment (invert-assignment-at-index (copy-array assignment)
                                                          (- (array-dimension assignment 0) 1)
                                                          var-value-prob-map))
             (when (or (and (= prob 0) (not (= (aref assignment 0) 1)))
                       (and (= prob 1) (not (= (aref assignment 0) 0))))
               (setq assignment (invert-assignment-at-index assignment 0 var-value-prob-map)))
             (setq rule (make-initial-rule assignment prob case 0))
             (setq rules (cons rule rules))
             (setq case (+ case 1)))
           (loop
             for i from (- (array-dimension assignment 0) 1) downto 2
             do
                (setq assignment (invert-assignment-at-index (subseq assignment 0 i) (- i 1) var-value-prob-map))
                (setq rule (make-initial-rule assignment prob case 0))
                (setq rules (cons rule rules))
                (setq case (+ case 1)))
        finally
           (return (make-array case :initial-contents (nreverse rules)))))))
|#

#| Make a ruleset for describing the conditional probability distribution |#

;; cpd = conditional probability distribution
;; assn = assignment with probability 1
(defun make-initial-rules (cpd assn)
  (labels ((invert-assignment-at-index (assn idx invert-map)
             (setf (aref assn idx)
                   (cdr (assoc (aref assn idx)
                               invert-map)))
             assn)
           (make-initial-rule (asn prob case count)
             (let (rule)
               (setq rule (make-rule :id (symbol-name (gensym "RULE-"))
                                     :conditions (make-hash-table :test #'equal)
                                     :probability prob
                                     :block (make-hash-table) ;;(list case)
                                     :certain-block (make-hash-table) ;;(list case)
                                     :count count))
               (setf (gethash case (rule-block rule)) case)
               (setf (gethash case (rule-certain-block rule)) case)
               (loop
                 for attribute being the hash-keys of (rule-based-cpd-identifiers cpd)
                   using (hash-value pos)
                 when (< pos (array-dimension asn 0))
                   do
                      (setf (gethash attribute (rule-conditions rule)) (aref asn pos)))
               rule)))
    (loop
      with assn-0s and assn-0 and 0s
      for card from 0 to (- (aref (rule-based-cpd-cardinalities cpd) 0) 1)
      when (not (= card (aref assn 0)))
	do
	   (setq assn-0 (copy-array assn))
	   (setf (aref assn-0 0) card)
	   (setq assn-0s (cons assn-0 assn-0s))
	   (setq 0s (cons 0 0s))
      finally 
	 (return
	   (loop
	     with var-value-prob-map = '((0 . 1) (1 . 0))
	     with rules and case = 0 and rule
	     for assignment in (cons assn assn-0s)
	     for prob in (cons 1 0s)
	     do
		(setq rule (make-initial-rule assignment prob case 1))
		(setq rules (cons rule rules))
		(setq case (+ case 1))
	     finally
		(return (make-array case :initial-contents (nreverse rules))))))))

#| Make proper attribute blocs, concept blocks, and rules for each factor |#

;; factors-list = list of factors
(defun finalize-factors (factors-list)
  (loop
    with factor-rules and assn
    for factor in factors-list
    do
       (setq assn (make-array (hash-table-count (rule-based-cpd-vars factor)) :initial-element 1))
       (setq factor-rules (make-initial-rules factor assn))
    collect (update-cpd-rules factor factor-rules :check-uniqueness t)
      into final
    finally
       (return final)))

#| Make a list of counts for assignments in a cpd |#

;; rows = number of entrys in assignemnts array
;; row-length = length of rows
(defun make-counts (rows row-length)
  (let ((counts (make-hash-table)))
    (setf (gethash (* (- rows 1) row-length) counts) 1)
    counts))

#| Return the corresponding concept of a belief |#

;; belief = cinstance
(defun get-concept (belief)
  (car (member (cinstance-id belief) cltm*
               :key 'concept-id
               :test #'(lambda (id1 id2) (equal (symbol-name id1) (symbol-name id2))))))

#| For each belief, generate a set of conditional probability densities.
   One for the name, each argument, and each attribute. |#

;; belief = belief in state
(defun get-cpds-for-belief (belief)
  (let (listform cpd1 cpd2 vars types-hash id dependent-ident vvbm nvvbm sva svna vals cards steps rules lvl identifiers cid cids type)
    (setq listform (instantiate-listform (cdr belief)))
    (setq type (symbol-name (car listform)))
    (setq vars (make-hash-table))
    (setf (gethash 0 vars) type)
    (setq types-hash (make-hash-table))
    (setf (gethash 0 types-hash) "BELIEF")
    (setq id (symbol-name (gensym type)))
    (setq identifiers (make-hash-table :test #'equal))
    (setf (gethash id identifiers) 0)
    #|
    (loop
      for idx being the hash-keys of vars
        using (hash-value ele)
      do
         (if (= idx 0) (setq dependent-ident (symbol-name (gensym ele))))
         (setf (gethash (if (= idx 0) dependent-ident (symbol-name (gensym ele))) identifiers) idx))
    |#
    (setq vvbm (make-hash-table))
    ;;(setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table)) (list (cons id 1) (make-hash-table))))
    (setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table)) (list (cons "T" 1) (make-hash-table))))
    (setq sva (make-hash-table))
    (setf (gethash 0 sva) (list (list 0) (list 1))) ;; ((0) (1) (2))
    (setq svna (make-hash-table))
    (setf (gethash 0 svna) (list (list 1) (list 0))) ;; ((1) (0)) -> ((1 2) (0 2) (0 1))
    (setq vals (make-hash-table))
    (setf (gethash 0 vals) (list 0 1))
    (setq lvl (car belief))
    (setq cards (generate-cpd-cardinalities vvbm))
    (setq steps (generate-cpd-step-sizes cards))
    (setq cids (make-hash-table))
    (setf (gethash 0 cids) "NIL")
    (setq cpd1 (make-rule-based-cpd :dependent-id id ;;dependent-ident
                                    :identifiers identifiers
                                    :dependent-var type
                                    :vars vars
                                    :types types-hash
                                    :concept-ids cids
                                    :qualified-vars (generate-cpd-vars identifiers vars cids)
                                    :var-value-block-map vvbm
                                    :negated-vvbms (copy-hash-table vvbm)
                                    :set-valued-attributes sva
                                    :set-valued-negated-attributes svna
                                    :lower-approx-var-value-block-map (copy-hash-table vvbm)
                                    :lower-approx-negated-vvbms (copy-hash-table vvbm)
                                    :characteristic-sets (make-hash-table)
                                    :characteristic-sets-values (make-hash-table)
                                    :var-values vals
                                    :cardinalities cards
                                    :step-sizes steps
                                    :rules rules
                                    :concept-blocks (make-hash-table)
                                    :count 1
                                    :lvl lvl))
    #|
    (setq vars (make-hash-table))
    (setf (gethash 0 vars) type)
    (setf (gethash 1 vars) type)
    (setq types-hash (make-hash-table))
    (setf (gethash 0 types-hash) "BELIEF")
    (setf (gethash 1 types-hash) "BELIEF")
    (setq identifiers (make-hash-table :test #'equal))
    (setf (gethash id identifiers) 0)
    (setf (gethash (rule-based-cpd-dependent-id cpd1) identifiers) 1)
    (setq cid (symbol-name (concept-id (get-concept (cdr belief)))))
    (setq vvbm (make-hash-table))
    (setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table)) (list (cons "T" 1) (make-hash-table))))
    (setf (gethash 1 vvbm) (list (list (cons "NA" 0) (make-hash-table)) (list (cons id 1) (make-hash-table))))
    (setq sva (make-hash-table))
    (setf (gethash 0 sva) (list (list 0) (list 1)))
    (setf (gethash 1 sva) (list (list 0) (list 1)))
    (setq svna (make-hash-table))
    (setf (gethash 0 svna) (list (list 1) (list 0)))
    (setf (gethash 1 svna) (list (list 1) (list 0)))
    (setq vals (make-hash-table))
    (setf (gethash 0 vals) (list 0 1))
    (setf (gethash 1 vals) (list 0 1))
    (setq lvl (car belief))
    (setq cards (generate-cpd-cardinalities vvbm))
    (setq steps (generate-cpd-step-sizes cards))
    (setq cids (make-hash-table))
    (setf (gethash 0 cids) cid)
    (setf (gethash 1 cids) (gethash 0 (rule-based-cpd-concept-ids cpd1)))
    (setq cpd2 (make-rule-based-cpd :dependent-id id
                                    :identifiers identifiers
                                    :dependent-var type
                                    :vars vars
                                    :types types-hash
                                    :concept-ids cids
                                    :qualified-vars (generate-cpd-vars identifiers vars cids)
                                    :var-value-block-map vvbm
                                    :negated-vvbms (copy-hash-table vvbm)
                                    :set-valued-attributes sva
                                    :set-valued-negated-attributes svna
                                    :lower-approx-var-value-block-map (copy-hash-table vvbm)
                                    :lower-approx-negated-vvbms (copy-hash-table vvbm)
                                    :characteristic-sets (make-hash-table)
                                    :characteristic-sets-values (make-hash-table)
                                    :var-values vals
                                    :cardinalities cards
                                    :step-sizes steps
                                    :rules rules
                                    :concept-blocks (make-hash-table)
                                    :count 1
                                    :lvl lvl))
    (list cpd1 cpd2)
    |#
    (list cpd1)))

#| For each percept, generate a set of conditional probability densities.
   One for the name and each attribute. |#

;; percept = percept in state
(defun get-cpds-for-percept (percept)
  (loop
     for (att val) on (cdr percept) by #'cddr
     for i from 0
     with cpd and vars and types-hash and cids and vvbm and sva and svna and vals and cards and steps and rules and identifiers
     with name and type and type-identifier and att-identifier
     when (= i 0)
       do
          (setq type att)
          (setq name val)
          (setq vars (make-hash-table))
          (setf (gethash 0 vars) att)
          (setq types-hash (make-hash-table))
          (setf (gethash 0 types-hash) "PERCEPT")
          (setq identifiers (make-hash-table :test #'equal))
          (setq type-identifier (symbol-name (gensym type)))
          (setf (gethash type-identifier identifiers) 0)
          (setq vvbm (make-hash-table))
          (setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table)) (list (cons name 1) (make-hash-table))))
          (setq sva (make-hash-table))
          (setf (gethash 0 sva) (list (list 0) (list 1)))
          (setq svna (make-hash-table))
          (setf (gethash 0 svna) (list (list 1) (list 0)))
          (setq vals (make-hash-table))
          (setf (gethash 0 vals) (list 0 1))
          (setq cards (generate-cpd-cardinalities vvbm))
          (setq steps (generate-cpd-step-sizes cards))
          (setq cids (make-hash-table))
          (setf (gethash 0 cids) "NIL")
          (setq cpd (make-rule-based-cpd :dependent-id type-identifier
                                         :identifiers identifiers
                                         :dependent-var att
                                         :vars vars
                                         :types types-hash
                                         :concept-ids cids
                                         :qualified-vars (generate-cpd-vars identifiers vars cids)
                                         :var-value-block-map vvbm
                                         :negated-vvbms (copy-hash-table vvbm)
                                         :set-valued-attributes sva
                                         :set-valued-negated-attributes svna
                                         :lower-approx-var-value-block-map (copy-hash-table vvbm)
                                         :lower-approx-negated-vvbms (copy-hash-table vvbm)
                                         :characteristic-sets (make-hash-table)
                                         :characteristic-sets-values (make-hash-table)
                                         :var-values vals
                                         :cardinalities cards
                                         :step-sizes steps
                                         :rules rules
                                         :concept-blocks (make-hash-table)
                                         :count 1
                                         :lvl (car percept)))
       and collect cpd into cpds
     if (> i 0 )
       do
          (setq att-identifier (symbol-name (gensym att)))
          (setq vars (make-hash-table))
          (setf (gethash 0 vars) att)
          (setf (gethash 1 vars) type)
          (setq types-hash (make-hash-table))
          (setf (gethash 0 types-hash) "PERCEPT")
          (setf (gethash 1 types-hash) "PERCEPT")
          (setq identifiers (make-hash-table :test #'equal))
          (setf (gethash att-identifier identifiers) 0)
          (setf (gethash type-identifier identifiers) 1)
          (setq vvbm (make-hash-table))
          (setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table)) (list (cons val 1) (make-hash-table))))
          (setf (gethash 1 vvbm) (list (list (cons "NA" 0) (make-hash-table)) (list (cons name 1) (make-hash-table))))
          (setq sva (make-hash-table))
          (setf (gethash 0 sva) (list (list 0) (list 1)))
          (setf (gethash 1 sva) (list (list 0) (list 1)))
          (setq svna (make-hash-table))
          (setf (gethash 0 svna) (list (list 1) (list 0)))
          (setf (gethash 1 svna) (list (list 1) (list 0)))
          (setq vals (make-hash-table))
          (setf (gethash 0 vals) (list 0 1))
          (setf (gethash 1 vals) (list 0 1))
          (setq cards (generate-cpd-cardinalities vvbm))
          (setq steps (generate-cpd-step-sizes cards))
          (setq cids (make-hash-table))
          (setf (gethash 0 cids) "NIL")
          (setf (gethash 1 cids) "NIL")
          (setq cpd (make-rule-based-cpd :dependent-id att-identifier
                                         :identifiers identifiers
                                         :dependent-var att
                                         :vars vars
                                         :types types-hash
                                         :concept-ids cids
                                         :qualified-vars (generate-cpd-vars identifiers vars cids)
                                         :var-value-block-map vvbm
                                         :negated-vvbms (copy-hash-table vvbm)
                                         :set-valued-attributes sva
                                         :set-valued-negated-attributes svna
                                         :lower-approx-var-value-block-map (copy-hash-table vvbm)
                                         :lower-approx-negated-vvbms (copy-hash-table vvbm)
                                         :characteristic-sets (make-hash-table)
                                         :characteristic-sets-values (make-hash-table)
                                         :var-values vals
                                         :cardinalities cards
                                         :step-sizes steps
                                         :rules rules
                                         :concept-blocks (make-hash-table)
                                         :count 1
                                         :lvl (car percept)))
       and collect cpd into cpds
     finally
       (return cpds)))

#| For each action, generate a set of conditional probability densities.
   One for the generic action and the action type. |#

;; action = action completed in state
(defun get-cpds-for-action (action)
  (let (listform cpd cpds vars types-hash id dependent-ident vvbm sva svna vals cards steps rules lvl identifiers cid cids type)
    (setq listform (cdr action))
    (setq type (second listform))
    (setq vars (make-hash-table))
    (setf (gethash 0 vars) type)
    (setq types-hash (make-hash-table))
    (setf (gethash 0 types-hash) "ACTION")
    (setq id (symbol-name (gensym (car listform))))
    (setq identifiers (make-hash-table :test #'equal))
    (loop
      for idx being the hash-keys of vars
        using (hash-value ele)
      do
         (if (= idx 0) (setq dependent-ident (symbol-name (gensym ele))))
         (setf (gethash (if (= idx 0) dependent-ident (symbol-name (gensym ele))) identifiers) idx))
    (setq vvbm (make-hash-table))
    (setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table)) (list (cons id 1) (make-hash-table))))
    (setq sva (make-hash-table))
    (setf (gethash 0 sva) (list (list 0) (list 1)))
    (setq svna (make-hash-table))
    (setf (gethash 0 svna) (list (list 1) (list 0)))
    (setq vals (make-hash-table))
    (setf (gethash 0 vals) (list 0 1))
    (setq lvl (car action))
    (setq cards (generate-cpd-cardinalities vvbm))
    (setq steps (generate-cpd-step-sizes cards))
    (setq cids (make-hash-table))
    (setf (gethash 0 cids) "NIL")
    (setq cpd (make-rule-based-cpd :dependent-id dependent-ident
                                   :identifiers identifiers
                                   :dependent-var type
                                   :vars vars
                                   :types types-hash
                                   :concept-ids cids
                                   :qualified-vars (generate-cpd-vars identifiers vars cids)
                                   :var-value-block-map vvbm
                                   :negated-vvbms (copy-hash-table vvbm)
                                   :set-valued-attributes sva
                                   :set-valued-negated-attributes svna
                                   :lower-approx-var-value-block-map (copy-hash-table vvbm)
                                   :lower-approx-negated-vvbms (copy-hash-table vvbm)
                                   :characteristic-sets (make-hash-table)
                                   :characteristic-sets-values (make-hash-table)
                                   :var-values vals
                                   :cardinalities cards
                                   :step-sizes steps
                                   :rules rules
                                   :concept-blocks (make-hash-table)
                                   :count 1
                                   :lvl lvl))
    (setq cpds (cons cpd cpds))
    (setq vars (make-hash-table))
    (setf (gethash 0 vars) (car listform))
    (setf (gethash 1 vars) type)
    (setq types-hash (make-hash-table))
    (setf (gethash 0 types-hash) "ACTION")
    (setf (gethash 1 types-hash) "ACTION")
    (setq type id)
    (setq identifiers (make-hash-table :test #'equal))
    (setf (gethash id identifiers) 0)
    (setf (gethash (rule-based-cpd-dependent-id cpd) identifiers) 1)
    (setq cid "NIL")
    (setq vvbm (make-hash-table))
    (setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table)) (list (cons (list-to-string (list (car listform))) 1) (make-hash-table))))
    (setf (gethash 1 vvbm) (list (list (cons "NA" 0) (make-hash-table)) (list (cons id 1) (make-hash-table))))
    (setq sva (make-hash-table))
    (setf (gethash 0 sva) (list (list 0) (list 1)))
    (setf (gethash 1 sva) (list (list 0) (list 1)))
    (setq svna (make-hash-table))
    (setf (gethash 0 svna) (list (list 1) (list 0)))
    (setf (gethash 1 svna) (list (list 1) (list 0)))
    (setq vals (make-hash-table))
    (setf (gethash 0 vals) (list 0 1))
    (setf (gethash 1 vals) (list 0 1))
    (setq lvl (car action))
    (setq cards (generate-cpd-cardinalities vvbm))
    (setq steps (generate-cpd-step-sizes cards))
    (setq cids (make-hash-table))
    (setf (gethash 0 cids) cid)
    (setf (gethash 1 cids) (gethash 0 (rule-based-cpd-concept-ids cpd)))
    (setq cpd (make-rule-based-cpd :dependent-id id
                                   :identifiers identifiers
                                   :dependent-var (car listform)
                                   :vars vars
                                   :types types-hash
                                   :concept-ids cids
                                   :qualified-vars (generate-cpd-vars identifiers vars cids)
                                   :var-value-block-map vvbm
                                   :negated-vvbms (copy-hash-table vvbm)
                                   :set-valued-attributes sva
                                   :set-valued-negated-attributes svna
                                   :lower-approx-var-value-block-map (copy-hash-table vvbm)
                                   :lower-approx-negated-vvbms (copy-hash-table vvbm)
                                   :characteristic-sets (make-hash-table)
                                   :characteristic-sets-values (make-hash-table)
                                   :var-values vals
                                   :cardinalities cards
                                   :step-sizes steps
                                   :rules rules
                                   :concept-blocks (make-hash-table)
                                   :count 1
                                   :lvl lvl))
    (setq cpds (cons cpd cpds))
    #|
    (loop
       for (att val) on (nthcdr 2 listform) by #'cddr do
         (setq id (symbol-name (gensym att)))
         (setq vars (make-hash-table))
         (setf (gethash 0 vars) att)
         (setf (gethash 1 vars) (car listform))
         (setq types-hash (make-hash-table))
         (setf (gethash 0 types-hash) "ACTION")
         (setf (gethash 1 types-hash) "ACTION")
         (setq identifiers (make-hash-table :test #'equal))
         (setf (gethash id identifiers) 0)
         (setf (gethash type identifiers) 1)
         (setq vvbm (make-hash-table))
         (setf (gethash 0 vvbm) (list (list (cons "NA" 0) nil) (list (cons val 1) nil)))
         (setf (gethash 1 vvbm) (list (list (cons "NA" 0) nil) (list (cons "T" 1) nil)))
    (setq sva (make-hash-table))
    (setf (gethash 0 sva) (list (list 0) (list 1)))
    (setf (gethash 1 sva) (list (list 0) (list 1)))
    (setq svna (make-hash-table))
    (setf (gethash 0 svna) (list (list 1) (list 0)))
    (setf (gethash 1 svna) (list (list 1) (list 0)))
    (setq vals (make-hash-table))
    (setf (gethash 0 vals) (list 0 1))
    (setf (gethash 1 vals) (list 0 1))
         (setq cards (generate-cpd-cardinalities vvbm))
         (setq steps (generate-cpd-step-sizes cards))
         (setq cids (make-hash-table))
         (setf (gethash 0 cids) "NIL")
         (setf (gethash 1 cids) "NIL")
         (setq cpd (make-rule-based-cpd :dependent-id id
                             :identifiers identifiers
                             :dependent-var att
                             :vars vars
                             :types types-hash
                             :concept-ids cids
                             :qualified-vars (generate-cpd-vars identifiers vars cids)
                             :var-value-block-map vvbm
                             :negated-vvbms (copy-hash-table vvbm)
    :set-valued-attributes: sva
    :set-valued-negated-attributes: svna
    :lower-approx-var-value-block-map (copy-hash-table vvbm)
    :lower-approx-negated-vvbms (copy-hash-table vvbm)
    :characteristic-sets (make-hash-table)
    :characteristic-sets-values (make-hash-table)
    :var-values vals
                             :cardinalities cards
                             :step-sizes steps
                             :rules rules
                             :concept-blocks (make-hash-table)
                             :count 1
                             :lvl (car action)))
         (setq cpd (get-local-coverings cpd))
         (setq cpds (cons cpd cpds)))
    |#
    (reverse cpds)))

#| Check if variable matches value |#

;; var = variable
;; val = value
;; bindings = variable bindings
(defun var-eq (var val &optional bindings)
  (cond ((numberp var)
         (equal var val))
         ((and (variable-p (make-symbol var))
               (not (rassoc val bindings :test #'(lambda (v b)
                                                   (equal v (if (numberp b) b (symbol-name b)))))))
         t)
        (t
         (equal var val))))

#| Find CPDs that corresponds to belief's elements and conditions |#

;; instantiated = instantiated belief element
;; concept = concept definition for instantiated
;; elements-cpds = conditional probabilities for each state element
;; bindings = variable bindings
;; start = subset of elements-cpds starting from index
;; stop = subset of elements-cpds ending at index
(defun find-matching-cpd (instantiated concept elements-cpds bindings start &optional stop)
  (loop
    named matcher
    for element-cpds in (subseq elements-cpds start stop)
    with copy-element
     do
        (setq copy-element (car element-cpds))
    when (every #'(lambda (item)
                    (cond ((equal copy-element (remove item copy-element
                                                       :count 1
                                                       :test #'(lambda (c i)
                                                                 (var-eq i c bindings))))
                           nil)
                          (t
                           (setq copy-element (remove item copy-element
                                                      :count 1
                                                      :test #'(lambda (c i)
                                                                (var-eq i c bindings))))
                           t)))
                instantiated)
      do
         (return-from matcher element-cpds)))

#| Find CPDs that corresponds to skill actions |#

;; instantiated = instantiated action
;; elements-cpds = conditional probabilities for each state element
;; start = subset of elements-cpds starting from index
;; stop = subset of elements-cpds ending at index
(defun find-matching-cpd-for-action (instantiated elements-cpds start &optional stop)
  (loop
    named matcher
    for element-cpds in (subseq elements-cpds start stop)
    with copy-element and action-name
    do
       (setq action-name (subseq (car instantiated) 1))
       (setq copy-element (car element-cpds))
    when (equal action-name (car copy-element))
      do
         (return-from matcher element-cpds)))

#| Get variable name without number modifiers at the end |#

;; var-string = variable name in string form
(defun get-var-stem (var-string)
  (loop
     for i from (- (length var-string) 1) downto 0
     when (and (not (digit-char-p (aref var-string i)))
	       (not (equal #\. (aref var-string i))))
     do
       (return-from get-var-stem (+ i 1))))

#| Flatten a nested list |#

;; x = list
(defun flatten (x &optional stack out)
  (cond ((consp x) (flatten (rest x) (cons (first x) stack) out))
        (x         (flatten (first stack) (rest stack) (cons x out)))
        (stack     (flatten (first stack) (rest stack) out))
        (t out)))

#| Modify the conents of original CPD with another cpd |#

;; cpd = cpd to modify
;; modifier-cpd = cpd that updates values in cpd
(defun modify-cpd (cpd modifier-cpd)
  (let (new-cards new-steps)
    (setf (gethash (rule-based-cpd-dependent-id modifier-cpd)
                   (rule-based-cpd-identifiers cpd))
          (hash-table-count (rule-based-cpd-identifiers cpd)))
    (setf (gethash (hash-table-count (rule-based-cpd-vars cpd))
                   (rule-based-cpd-vars cpd))
          (gethash 0 (rule-based-cpd-vars modifier-cpd)))
    (setf (gethash (hash-table-count (rule-based-cpd-types cpd))
                   (rule-based-cpd-types cpd))
          (gethash 0 (rule-based-cpd-types modifier-cpd)))
    (setf (gethash (hash-table-count (rule-based-cpd-concept-ids cpd))
                   (rule-based-cpd-concept-ids cpd))
          (gethash 0 (rule-based-cpd-concept-ids modifier-cpd)))
    (setf (gethash (hash-table-count (rule-based-cpd-qualified-vars cpd))
                   (rule-based-cpd-qualified-vars cpd))
          (gethash 0 (rule-based-cpd-qualified-vars modifier-cpd)))
    (setf (gethash (hash-table-count (rule-based-cpd-var-value-block-map cpd))
                   (rule-based-cpd-var-value-block-map cpd))
          (deep-copy-list (gethash 0 (rule-based-cpd-var-value-block-map modifier-cpd))))
    (setf (gethash (hash-table-count (rule-based-cpd-negated-vvbms cpd))
                   (rule-based-cpd-negated-vvbms cpd))
          (deep-copy-list (gethash 0 (rule-based-cpd-negated-vvbms modifier-cpd))))
    (setf (gethash (hash-table-count (rule-based-cpd-set-valued-attributes cpd))
                   (rule-based-cpd-set-valued-attributes cpd))
          (copy-tree (gethash 0 (rule-based-cpd-set-valued-attributes modifier-cpd))))
    (setf (gethash (hash-table-count (rule-based-cpd-set-valued-negated-attributes cpd))
                   (rule-based-cpd-set-valued-negated-attributes cpd))
          (copy-tree (gethash 0 (rule-based-cpd-set-valued-negated-attributes modifier-cpd))))
    (setf (gethash (hash-table-count (rule-based-cpd-lower-approx-var-value-block-map cpd))
                   (rule-based-cpd-lower-approx-var-value-block-map cpd))
          (deep-copy-list (gethash 0 (rule-based-cpd-lower-approx-var-value-block-map modifier-cpd))))
    (setf (gethash (hash-table-count (rule-based-cpd-lower-approx-negated-vvbms cpd))
                   (rule-based-cpd-lower-approx-negated-vvbms cpd))
          (deep-copy-list (gethash 0 (rule-based-cpd-lower-approx-negated-vvbms modifier-cpd))))
    (setf (gethash (hash-table-count (rule-based-cpd-var-values cpd))
                   (rule-based-cpd-var-values cpd))
          (copy-list (gethash 0 (rule-based-cpd-var-values modifier-cpd))))
    (setq new-cards (generate-cpd-cardinalities (rule-based-cpd-var-value-block-map cpd)))
    (setq new-steps (generate-cpd-step-sizes new-cards))
    (setf (rule-based-cpd-cardinalities cpd) new-cards)
    (setf (rule-based-cpd-step-sizes cpd) new-steps)))

#| Update the conditional probability densities with new variables |#

;; cpds = conditional probability densities
;; modifier-cpd = conditional probability to modify cpds
;; concept = concept definition for modifier-cpd
;; element = item in concept elements or conditions
(defun update-cpds (cpds modifier-cpd concept element &optional (test-accessor #'concept-tests))
  (loop
    for cpd in cpds
    for i from 0
    with match and copy-element = (mapcar #'(lambda (l) (if (numberp l) l (symbol-name l))) element) and ele and variable and stripped-not
    with flat-tests = (mapcar #'(lambda (l) (if (numberp l) l (symbol-name l))) (flatten (funcall test-accessor concept)))
    do
       (when nil (eq "BELIEF" (gethash 0 (cpd-types modifier-cpd)))
         (format t "~%copy-element: ~A~%cpd:~%~A" copy-element cpd))
       (setq match nil)
       (cond ((equal "PERCEPT" (gethash 0 (rule-based-cpd-types cpd)))
              (setq ele (remove (gethash 0 (rule-based-cpd-vars cpd)) copy-element :count 1 :test #'equal))
	      (when nil t
		(format t "~%ele: ~A" ele))
              (cond ((and (= i 0) (not (equal ele copy-element)))
                     (setq match t))
                    ((not (equal ele copy-element))
                     (setq variable (cadr (member (gethash 0 (rule-based-cpd-vars cpd)) copy-element :test #'equal)))
                     (when (member variable flat-tests :test #'equal)
                       (setq match t)))))
             ((equal "ACTION" (gethash 0 (rule-based-cpd-types cpd)))
              (if (char=  #\* (char (car copy-element) 0))
                  (setq copy-element (cons (subseq (car copy-element) 1) (rest copy-element))))
              (setq ele (remove (gethash 0 (rule-based-cpd-vars cpd)) copy-element :count 1 :test #'equal))
              ;;(format t "~%ele: ~A~%copy-element: ~A" ele copy-element)
              (when (not (equal ele copy-element))
                (setq match t)))
             (t
              (when (eq 'not (car copy-element))
                (setq copy-element (second copy-element))
                (setq stripped-not t))
              (setq ele (remove (gethash 0 (rule-based-cpd-vars cpd)) copy-element :count 1 :test #'equal))
              (if stripped-not
                  (setq ele (cons 'not ele)))
              (cond ((and (= i 0)
                          (not (equal ele copy-element)))
                     (setq match t))
                    ((not (equal ele copy-element))
                     (setq variable (cadr (member (gethash 0 (rule-based-cpd-vars cpd)) copy-element :test #'equal)))
                     (when (member variable flat-tests :test #'equal)
                       (setq match t))))))
       ;;(when (= cycle* 11)
       ;;  (format t "~%match: ~A" match))
       (when match
         (modify-cpd cpd modifier-cpd)))
  cpds)

#| Update the conditional probability densities with new variables |#

;; cpds = conditional probability densities
;; modifier-cpds = conditional probability distributions to modify cpds
(defun update-cpds-given-action (cpds modifier-cpds)
  ;;(format t "~%~%concept:~%~A~%modifier-cpd:~%~A" concept modifier-cpd)
  (loop
    for cpd in cpds
    for i from 0
    with match and modifier-cpd
    do
       ;;(format t "~%copy-element: ~A~%cpd:~%~A" copy-element cpd)
       (setq match nil)
       (setq modifier-cpd nil)
       (loop
         named matcher
         for m-cpd in modifier-cpds do
           (when (and (equal "PERCEPT" (gethash 0 (rule-based-cpd-types cpd)))
                      (equal (rule-based-cpd-dependent-var cpd) (rule-based-cpd-dependent-var m-cpd)))
             (setq match t)
             (setq modifier-cpd m-cpd)
             (return-from matcher)))
       ;;(when (= cycle* 11)
       ;;(format t "~%~%modifier-cpd:~%~A~%cpd:~%~A" modifier-cpd cpd))
       (when match
         (modify-cpd cpd modifier-cpd)
         ;;(when (= cycle* 11)
         ;;  (format t "~%modified:~%~A" cpd))
         ))
  cpds)

(defun add-temporal-link-for-action (elements-cpds prev-state)
  (when prev-state
    (let (action-cpds cpd)
      (setq action-cpds (car (member 'action elements-cpds
                                     :key #'(lambda(ele) (second (car ele))))))
      (setq cpd (car (member 'action (rest action-cpds) :key #'rule-based-cpd-dependent-var)))
      (loop
        for modifier-cpd being the elements of (car prev-state)
        when (equal (rule-based-cpd-dependent-var cpd) (rule-based-cpd-dependent-var modifier-cpd)) do
          (modify-cpd cpd modifier-cpd)))))

(defun list-to-string (l)
  (format nil "(~{~A~^ ~})" l))

#| Make CPDs for intentions on execution stack. |#

;; int = intention structure
;; lvl = level of current intention
;; intentions = list of intention cpds so far
(defun get-cpds-for-executing-intention (int lvl intentions elements-cpds pstm)
  (cond ((null int)
         (let (top-lvl-int top-lvl-int-cpd)
           (setq top-lvl-int (car intentions))
           (setq top-lvl-int-cpd (second intentions))
           (setq intentions (rest intentions))
           (loop
             with cpd1 and cpd2 and type and id and dep-id and idents and vars and types-hash and cid and cids and vvbm
             with sva and svna and vals and cards and steps and rules
             for goal in (if top-lvl-int (intention-targets top-lvl-int) nil)
             do
                (setq type (symbol-name (car goal)))
                (setq vars (make-hash-table))
                (setf (gethash 0 vars) "GOAL")
                (setq types-hash (make-hash-table))
                (setf (gethash 0 types-hash) "GOAL")
                (setq id (symbol-name (gensym type)))
                (setq idents (make-hash-table :test #'equal))
                (loop
                  for idx being the hash-keys of vars
                    using (hash-value ele)
                  do
                     (if (= idx 0) (setq dep-id (symbol-name (gensym ele))))
                     (setf (gethash (if (= idx 0) dep-id (symbol-name (gensym ele))) idents) idx))
                (setq vvbm (make-hash-table))
                (setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table)) (list (cons id 1) (make-hash-table))))
                (setq sva (make-hash-table))
                (setf (gethash 0 sva) (list (list 0) (list 1)))
                (setq svna (make-hash-table))
                (setf (gethash 0 svna) (list (list 1) (list 0)))
                (setq vals (make-hash-table))
                (setf (gethash 0 vals) (list 0 1))
                (setq cards (generate-cpd-cardinalities vvbm))
                (setq steps (generate-cpd-step-sizes cards))
                (setq cids (make-hash-table))
                (setf (gethash 0 cids) "NIL")
                (setq cpd1 (make-rule-based-cpd :dependent-id dep-id
                                                :identifiers idents
                                                :dependent-var (gethash 0 vars)
                                                :vars vars
                                                :types types-hash
                                                :concept-ids cids
                                                :qualified-vars (generate-cpd-vars idents vars cids)
                                                :var-value-block-map vvbm
                                                :negated-vvbms (copy-hash-table vvbm)
                                                :set-valued-attributes sva
                                                :set-valued-negated-attributes svna
                                                :lower-approx-var-value-block-map (copy-hash-table vvbm)
                                                :lower-approx-negated-vvbms (copy-hash-table vvbm)
                                                :characteristic-sets (make-hash-table)
                                                :characteristic-sets-values (make-hash-table)
                                                :var-values vals
                                                :cardinalities cards
                                                :step-sizes steps
                                                :rules rules
                                                :concept-blocks (make-hash-table)
                                                :count 1
                                                :lvl lvl))
                (setq vars (make-hash-table))
                (setf (gethash 0 vars) type)
                (setf (gethash 1 vars) "GOAL")
                (setq types-hash (make-hash-table))
                (setf (gethash 0 types-hash) "GOAL")
                (setf (gethash 1 types-hash) "GOAL")
                (setq idents (make-hash-table :test #'equal))
                (setf (gethash id idents) 0)
                (setf (gethash (rule-based-cpd-dependent-id cpd1) idents) 1)
                (setq cid "NIL")
                (setq vvbm (make-hash-table))
                (setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table))
                                             (list (cons (list-to-string (sublis (intention-bindings top-lvl-int) goal)) 1) (make-hash-table))))
                (setf (gethash 1 vvbm) (list (list (cons "NA" 0) (make-hash-table)) (list (cons id 1) (make-hash-table))))
                (setq sva (make-hash-table))
                (setf (gethash 0 sva) (list (list 0) (list 1)))
                (setf (gethash 1 sva) (list (list 0) (list 1)))
                (setq svna (make-hash-table))
                (setf (gethash 0 svna) (list (list 1) (list 0)))
                (setf (gethash 1 svna) (list (list 1) (list 0)))
                (setq vals (make-hash-table))
                (setf (gethash 0 vals) (list 0 1))
                (setf (gethash 1 vals) (list 0 1))
                (setq cards (generate-cpd-cardinalities vvbm))
                (setq steps (generate-cpd-step-sizes cards))
                (setq cids (make-hash-table))
                (setf (gethash 0 cids) cid)
                (setf (gethash 1 cids) (gethash 0 (rule-based-cpd-concept-ids cpd1)))
                (setq cpd2 (make-rule-based-cpd :dependent-id id
                                                :identifiers idents
                                                :dependent-var type
                                                :vars vars
                                                :types types-hash
                                                :concept-ids cids
                                                :qualified-vars (generate-cpd-vars idents vars cids)
                                                :var-value-block-map vvbm
                                                :negated-vvbms (copy-hash-table vvbm)
                                                :set-valued-attributes sva
                                                :set-valued-negated-attributes svna
                                                :lower-approx-var-value-block-map (copy-hash-table vvbm)
                                                :lower-approx-negated-vvbms (copy-hash-table vvbm)
                                                :characteristic-sets (make-hash-table)
                                                :characteristic-sets-values (make-hash-table)
                                                :var-values vals
                                                :cardinalities cards
                                                :step-sizes steps
                                                :rules rules
                                                :concept-blocks (make-hash-table)
                                                :count 1
                                                :lvl lvl))
                (modify-cpd top-lvl-int-cpd cpd2)
                (setq intentions (cons cpd2 intentions))
                (setq intentions (cons cpd1 intentions))))
         intentions)
        (t
         (let (cpd cpd1 dep-id dependent-id identifiers vars type types-hash cids var-value-block-map
               sva svna vals cards steps rules prev-cpd sclause)
           (setq type (symbol-name (car (intention-head int))))
           (setq vars (make-hash-table))
           (setf (gethash 0 vars) "INTENTION")
           (setq types-hash (make-hash-table))
           (setf (gethash 0 types-hash) "INTENTION")
           (setq dependent-id (symbol-name (gensym type)))
           (setq identifiers (make-hash-table :test #'equal))
           (loop
             for idx being the hash-keys of vars
               using (hash-value ele)
             do
                (if (= idx 0) (setq dep-id (symbol-name (gensym ele))))
                (setf (gethash (if (= idx 0) dep-id (symbol-name (gensym ele))) identifiers) idx))
           (setq var-value-block-map (make-hash-table))
           ;;(setf (gethash 0 var-value-map) (list (cons "NA" 0) (cons dependent-id 1)))
           (setf (gethash 0 var-value-block-map) (list (list (cons "NA" 0) (make-hash-table))
                                                       (list (cons (list-to-string (sublis (intention-bindings int) (intention-head int))) 1) (make-hash-table))))
           (setq sva (make-hash-table))
           (setf (gethash 0 sva) (list (list 0) (list 1)))
           (setq svna (make-hash-table))
           (setf (gethash 0 svna) (list (list 1) (list 0)))
           (setq vals (make-hash-table))
           (setf (gethash 0 vals) (list 0 1))
           (setq cards (generate-cpd-cardinalities var-value-block-map))
           (setq steps (generate-cpd-step-sizes cards))
           (setq cids (make-hash-table))
           (setf (gethash 0 cids) "NIL")
           (setq cpd1 (make-rule-based-cpd :dependent-id dep-id
                                           :identifiers identifiers
                                           :dependent-var (gethash 0 vars)
                                           :vars vars
                                           :types types-hash
                                           :concept-ids cids
                                           :qualified-vars (generate-cpd-vars identifiers vars cids)
                                           :var-value-block-map var-value-block-map
                                           :negated-vvbms (copy-hash-table var-value-block-map)
                                           :set-valued-attributes sva
                                           :set-valued-negated-attributes svna
                                           :lower-approx-var-value-block-map (copy-hash-table var-value-block-map)
                                           :lower-approx-negated-vvbms (copy-hash-table var-value-block-map)
                                           :characteristic-sets (make-hash-table)
                                           :characteristic-sets-values (make-hash-table)
                                           :var-values vals
                                           :cardinalities cards
                                           :step-sizes steps
                                           :rules rules
                                           :concept-blocks (make-hash-table)
                                           :count 1
                                           :lvl lvl))
           (setq identifiers (make-hash-table :test #'equal))
           (setf (gethash dependent-id identifiers) 0)
           (setf (gethash (rule-based-cpd-dependent-id cpd1) identifiers) 1)
           (setq vars (make-hash-table))
           (setf (gethash 0 vars) type)
           (setf (gethash 1 vars) type)
           (setq types-hash (make-hash-table))
           (setf (gethash 0 types-hash) "INTENTION")
           (setf (gethash 1 types-hash) "INTENTION")
           (setq cids (make-hash-table))
           (setf (gethash 0 cids) (write-to-string (intention-id int)))
           (setf (gethash 1 cids) (gethash 0 (rule-based-cpd-concept-ids cpd1)))
           (setq var-value-block-map (make-hash-table))
           (setf (gethash 0 var-value-block-map) (list (list (cons "NA" 0) (make-hash-table)) (list (cons (if (car intentions) (rule-based-cpd-dependent-id (car intentions)) "T") 1) (make-hash-table))))
           (setf (gethash 1 var-value-block-map) (list (list (cons "NA" 0) (make-hash-table)) (list (cons (list-to-string (sublis (intention-bindings int) (intention-head int))) 1) (make-hash-table))))
           (setq sva (make-hash-table))
           (setf (gethash 0 sva) (list (list 0) (list 1)))
           (setf (gethash 1 sva) (list (list 0) (list 1)))
           (setq svna (make-hash-table))
           (setf (gethash 0 svna) (list (list 1) (list 0)))
           (setf (gethash 1 svna) (list (list 1) (list 0)))
           (setq vals (make-hash-table))
           (setf (gethash 0 vals) (list 0 1))
           (setf (gethash 1 vals) (list 0 1))
           (setq cards (generate-cpd-cardinalities var-value-block-map))
           (setq steps (generate-cpd-step-sizes cards))
           (setq cpd (make-rule-based-cpd :dependent-id dependent-id
                                          :identifiers identifiers
                                          :dependent-var type
                                          :vars vars
                                          :types types-hash
                                          :concept-ids cids
                                          :qualified-vars (generate-cpd-vars identifiers vars cids)
                                          :var-value-block-map var-value-block-map
                                          :negated-vvbms (copy-hash-table var-value-block-map)
                                          :set-valued-attributes sva
                                          :set-valued-negated-attributes svna
                                          :lower-approx-var-value-block-map (copy-hash-table var-value-block-map)
                                          :lower-approx-negated-vvbms (copy-hash-table var-value-block-map)
                                          :characteristic-sets (make-hash-table)
                                          :characteristic-sets-values (make-hash-table)
                                          :var-values vals
                                          :cardinalities cards
                                          :step-sizes steps
                                          :rules rules
                                          :concept-blocks (make-hash-table)
                                          :count 1
                                          :lvl lvl))
           (setq prev-cpd (car intentions))
           (when prev-cpd
             (modify-cpd prev-cpd cpd))
           (setq sclause (intention-operator int))
	   #|
	   (loop
             for element in (sclause-elements sclause)
             with instantiated and match
             do
                (setq instantiated (mapcar #'(lambda (p) (if (numberp p) p (symbol-name p))) (sublis (intention-bindings int) element)))
                (setq match (find-matching-cpd instantiated sclause elements-cpds (intention-bindings int) 0 (length pstm)))
             when match
               do
                  (update-cpds (rest match) cpd sclause element #'sclause-tests))
           (loop
             for condition in (sclause-conditions sclause)
             with instantiated and match
             do
                (when (eq 'not (car condition))
                  (setq condition (second condition)))
                (setq instantiated (mapcar #'(lambda (p) (if (numberp p) p (symbol-name p))) (sublis (intention-bindings int) condition)))
                (setq match (find-matching-cpd instantiated sclause elements-cpds (intention-bindings int) (length pstm)))
             when match
               do
                  (if (>= (rule-based-cpd-lvl (car (rest match))) (rule-based-cpd-lvl cpd))
                      (setq lvl (+ (rule-based-cpd-lvl (car (rest match))) 1))
                      (setf (rule-based-cpd-lvl cpd) lvl))
                  (update-cpds (rest match) cpd sclause condition #'sclause-tests))
	   |#
           (loop
             for act in (sclause-actions sclause)
             with instantiated and match
             do
                ;;(format t "~%action:~%~S" act)
                (setq instantiated (mapcar #'(lambda (p) (if (or (stringp p) (numberp p)) p (symbol-name p))) (sublis (intention-bindings int) act)))
                ;;(format  t "~%instantiated action:~%~A~%elements-cpds:~%~A" instantiated elements-cpds)
                (setq match (find-matching-cpd-for-action instantiated elements-cpds 0 (length pstm)))
                ;;(format t "~%match: ~A" match)
             when match
               do
                  (update-cpds (rest match) cpd sclause act #'sclause-tests))
           (get-cpds-for-executing-intention (intention-execution-i-parent int) (+ lvl 1) (if (intention-execution-i-parent int) (cons cpd1 (cons cpd intentions)) (cons int (cons cpd1 (cons cpd intentions)))) elements-cpds pstm)))))

#| Convert decompositions to a Bayesion Network |#

;; dcmps = list of references to lower-level decompositions
;; cpds = list of cpds representing state transitions
(defun get-cpds-for-decompositions (dcmps cpds)
  (cond ((null dcmps)
         (let (factors edges)
           (setq factors (finalize-factors (reverse cpds)))
           (setq factors (make-array (length cpds) :initial-contents factors :fill-pointer t))
           (setq edges (make-graph-edges factors))
           (values factors edges)))
        (t
         (let (cpd vars types-hash id dependent-ident vvbm sva svna vals cards steps rules lvl identifiers cids type)
           (setq type "STATE")
           (setq vars (make-hash-table))
           (setf (gethash 0 vars) type)
           (if cpds
               (setf (gethash 1 vars) (rule-based-cpd-dependent-var (car cpds))))
           (setq types-hash (make-hash-table))
           (setf (gethash 0 types-hash) "DECOMPOSITION")
           (if cpds
               (setf (gethash 1 types-hash) (gethash 0 (rule-based-cpd-types (car cpds)))))
           (setq id (episode-id (caar dcmps)))
           (setq identifiers (make-hash-table :test #'equal))
           (setq dependent-ident (symbol-name (gensym (gethash 0 vars))))
           (setf (gethash dependent-ident identifiers) 0)
           (if cpds
               (setf (gethash (rule-based-cpd-dependent-id (car cpds)) identifiers) 1))
           (setq vvbm (make-hash-table))
           (setf (gethash 0 vvbm) (list (list (cons "NA" 0) (make-hash-table)) (list (cons id 1) (make-hash-table))))
           (setq sva (make-hash-table))
           (setf (gethash 0 sva) (list (list 0) (list 1)))
           (setq svna (make-hash-table))
           (setf (gethash 0 svna) (list (list 1) (list 0)))
           (setq vals (make-hash-table))
           (setf (gethash 0 vals) (list 0 1))
           (when cpds
             (setf (gethash 1 vvbm) (gethash 0 (rule-based-cpd-var-value-block-map (car cpds))))
             (setf (gethash 1 sva) (list (list 0) (list 1)))
             (setf (gethash 1 svna) (list (list 1) (list 0)))
             (setf (gethash 1 vals) (list 0 1)))
           (setq lvl 1)
           (setq cards (generate-cpd-cardinalities vvbm))
           (setq steps (generate-cpd-step-sizes cards))
           (setq cids (make-hash-table))
           (setf (gethash 0 cids) "NIL")
           (if cpds
               (setf (gethash 1 cids) "NIL"))
           (setq cpd (make-rule-based-cpd :dependent-id dependent-ident
                                          :identifiers identifiers
                                          :dependent-var type
                                          :vars vars
                                          :types types-hash
                                          :concept-ids cids
                                          :qualified-vars (generate-cpd-vars identifiers vars cids)
                                          :var-value-block-map vvbm
                                          :negated-vvbms (copy-hash-table vvbm)
                                          :set-valued-attributes sva
                                          :set-valued-negated-attributes svna
                                          :lower-approx-var-value-block-map (copy-hash-table vvbm)
                                          :lower-approx-negated-vvbms (copy-hash-table vvbm)
                                          :characteristic-sets (make-hash-table)
                                          :characteristic-sets-values (make-hash-table)
                                          :var-values vals
                                          :cardinalities cards
                                          :step-sizes steps
                                          :rules rules
                                          :concept-blocks (make-hash-table)
                                          :count 1
                                          :lvl lvl))
           (get-cpds-for-decompositions (rest dcmps) (cons cpd cpds))))))

#| Update cpd assignments to reflect the state

;; elements-cpds
(defun update-cpd-assignments (cpds)
  (loop
    for cpd in cpds
    with num-assignments
    do
       (setq num-assignments (reduce #'* (cpd-cardinalities cpd)))
       (setf (gethash (- num-assignments 1) (cpd-assignments cpd)) 1))
  cpds)
|#

#| Construct a Bethe Cluster graph |#

;; factors = array of factors
;; singleton-factors = array of factors with one variable
;; evidence = potentially empty hash-table of variable value pairs
(defun make-bethe-graph-edges (factors singleton-factors evidence)
  (loop
    for i from 0 to (- (array-dimension factors 0) 1)
    nconc (loop
            for j from 0 to (- (array-dimension singleton-factors 0) 1)
            when (gethash (rule-based-cpd-dependent-id (aref singleton-factors j)) (rule-based-cpd-identifiers (aref factors i)))
              collect (cons i (+ j (array-dimension factors 0)))
              and collect (cons (+ j (array-dimension factors 0)) i)) into i-edges
    finally
       (loop ;; make place holders for evidence edges
         for i from 1 to (hash-table-count evidence)
             do (setq i-edges (reverse (cons 0 (reverse i-edges)))))
       (return (make-array (length i-edges) :initial-contents i-edges :fill-pointer t))))

#| Construct a Bayesian Network |#

;; factors = array of factors
(defun make-graph-edges (factors)
  (loop
    with edges = (make-hash-table :test #'equal)
    for i from 0 to (- (array-dimension factors 0) 1)
    do
       (loop
         for j from 0 to (- (array-dimension factors 0) 1)
         when (and (not (= i j)) (cpd-child-p (aref factors j) (aref factors i)))
           do
              ;;(format t "~%adding (~d ~d) and (~d ~d) edges." i j j i)
              (multiple-value-bind (hash bool)
                  (gethash i edges)
                (cond (bool
                       (multiple-value-bind (val bool2)
                           (gethash j hash)
                         (declare (ignore val))
                         (cond (bool2
                                ;;(error "(i: ~d, val: ~d), and (val: ~d, i: ~d) already exists. Can't overrwite assignment with (i: ~d, j: ~d) and (j: ~d, i: ~d).~%i: ~A~%val: ~A~%j: ~A" i val val i i j j i (aref factors i) (aref factors val) (aref factors j))
                                (warn "edge (~d, ~d) already set:~%parent factor (i = ~d):~%~S~%child factor (j = ~d):~%~S" i j i (aref factors i) j (aref factors j))
                                (break))
                               (t
                                (setf (gethash j hash) 1)))))
                      (t
                       (let (row)
                         (setq row (make-hash-table))
                         (setf (gethash j row) 1)
                         (setf (gethash i edges) row)))))
              (multiple-value-bind (hash bool)
                  (gethash j edges)
                (cond (bool
                       (multiple-value-bind (val bool2)
                           (gethash i hash)
                         (declare (ignore val))
                         (cond (bool2
                                ;;(error "can't overrwite values")
                                (warn "edge (~d, ~d) already set" j i))
                               (t
                                (setf (gethash i hash) 1)))))
                      (t
                       (let (row)
                         (setq row (make-hash-table))
                         (setf (gethash i row) 1)
                         (setf (gethash j edges) row))))))
    finally
       ;;(print-messages edges)
       ;;(return (make-array (length i-edges) :initial-contents i-edges))
       (return edges)))

#| See if cpd1 is higher level than cpd2 |#

;; cpd1 = conditional probability distribution
;; cpd2 = conditional probability distribution
(defun higher-lvl-cpd (cpd1 cpd2)
  (cond ((> (rule-based-cpd-lvl cpd1) (rule-based-cpd-lvl cpd2)) t)
        ((and (= (rule-based-cpd-lvl cpd1) (rule-based-cpd-lvl cpd2))
              (cpd-child-p cpd2 cpd1)) t)
        (t nil)))

#| Converts a state to a directed asyclic graph where the nodes are conditional probability densities |#

;; pstm = percepts
;; cstm = beliefs
;; previous-state = most recent state in episodic buffer
;; executing-intention = current executing intention
(defun state-to-graph (pstm cstm &key (previous-state nil) (executing-intention nil) &aux percepts action-buff actions beliefs intentions state-elements)
  (let (take-rest result)
    (setq result
          (mapcar #'(lambda (percept)
                      (cond ((eq 'action (second percept))
                             (setq take-rest t)
                             (setq action-buff (cons percept action-buff)))
                            (t
                             (cons 1 (mapcar #'(lambda (p)
                                                 (if (numberp p) p (symbol-name p)))
                                             percept)))))
                  pstm))
    (setq percepts (if take-rest (rest result) result)))
  (when nil (and (= cycle* 4))
    (format t "~%pstm:~%~S~%percepts:~%~S" pstm percepts)
    (break))
  (setq actions (mapcar #'(lambda (action)
                            (cons 2 (mapcar #'(lambda (a)
                                                (if (or (numberp a) (stringp a)) a (symbol-name a)))
                                            action)))
                        action-buff))
  (setq beliefs (mapcar #'(lambda (belief)
                            (let (concept)
                              (setq concept (get-concept belief))
                              (cons (+ (depth concept cltm* cltm*) 2) belief)))
                        cstm))
  (when nil (and (= cycle* 4))
    (format t "~%cstm:~%~S~%beliefs:~%~S" cstm beliefs)
    (break))
  (setq state-elements (append percepts actions (sort beliefs #'< :key 'car)))
  (loop
    for st-ele in state-elements
    for i from 0
    with elements-cpds
    with concept do
      (cond ((and (listp (cdr st-ele))
                  (equal "ACTION" (second (cdr st-ele))))
             (setq elements-cpds (reverse (cons (cons (cdr st-ele) (get-cpds-for-action st-ele)) (reverse elements-cpds))))
             (add-temporal-link-for-action elements-cpds previous-state)
             ;;(format t "~%(cdr st-ele): ~A~%(nthcdr 2 (cdr st-ele)): ~A" (cdr st-ele) (nthcdr 2 (cdr st-ele)))
             (loop
               with relevant-cpds
               for (att val) on (nthcdr 2 (cdr st-ele)) by #'cddr do
                 ;;(format t "~%~%att: ~A val: ~A" att val)
                 (setq relevant-cpds nil)
                 (loop
                   named cpd-finder
                   for element in elements-cpds do
                     ;;(format t "~%element:~%~A" element)
                     (when (and (equal (caar element) att)
                                (equal (second (car element)) val))
                       (setq relevant-cpds element)
                       (return-from cpd-finder)))
                 ;;(format t "~%relevant-cpds:~%~A~%modifier-cpds:~%~A" relevant-cpds (nthcdr 2 (car (last elements-cpds))))
                 (when relevant-cpds
                   (update-cpds-given-action (rest relevant-cpds) (nthcdr 2 (car (last elements-cpds)))))))
            ((and (listp (cdr st-ele))
                  (not (equal "ACTION" (second (cdr st-ele)))))
             (setq elements-cpds (reverse (cons (cons (cdr st-ele) (get-cpds-for-percept st-ele)) (reverse elements-cpds))))
             ;;(when (= cycle* 11)
             ;;  (format t "~%~%elements-cpds:~%~A" elements-cpds))
             )
            ((cinstance-p (cdr st-ele))
             (setq elements-cpds (reverse (cons (cons (mapcar #'(lambda (p)
								  (if (numberp p) p (symbol-name p)))
							      (instantiate-listform (cdr st-ele)))
						      (get-cpds-for-belief st-ele))
						(reverse elements-cpds))))
             (setq concept (get-concept (cdr st-ele)))
             (loop
               for element in (concept-elements concept)
               with instantiated and match
               do
                  (setq instantiated (mapcar #'(lambda (p)
						 (if (numberp p) p (symbol-name p)))
					     (sublis (cinstance-bindings (cdr st-ele)) element)))
		  ;;(format t "~%~%element:~%~S~%instantiated:~%~S~%concept:~%~S" element instantiated concept)
		  (setq match (find-matching-cpd instantiated concept elements-cpds (cinstance-bindings (cdr st-ele)) 0 (length pstm)))
		  ;;(format t "~%match:~%~S~%elements-cpds:~%~S" match elements-cpds)
               when match
                 do
                    (update-cpds (rest match) (second (car (last elements-cpds))) concept element))
             (loop
               for condition in (concept-conditions concept)
               with instantiated and match
               do
                  (when (eq 'not (car condition))
                    (setq condition (second condition)))
                  (setq instantiated (mapcar #'(lambda (p) (if (numberp p) p (symbol-name p))) (sublis (cinstance-bindings (cdr st-ele)) condition)))
                  (setq match (find-matching-cpd instantiated concept elements-cpds (cinstance-bindings (cdr st-ele)) (length pstm)))
               when match
                 do
                    (update-cpds (rest match) (second (car (last elements-cpds))) concept condition))))
    finally
       (setq intentions (get-cpds-for-executing-intention executing-intention 3 nil elements-cpds pstm))
       (let (factors-list factors edges)
         (setq factors-list (mapcan #'cdr elements-cpds))
         (setq factors-list (sort (append intentions factors-list) #'higher-lvl-cpd))
         (setf factors-list (finalize-factors factors-list))
         (setq factors (make-array (length factors-list) :initial-contents factors-list :fill-pointer t))
         (setq edges (make-graph-edges factors))
         (return (values factors edges)))))

(defun make-empty-graph ()
  (multiple-value-bind (factors edges)
      (state-to-graph nil nil)
    (cons factors edges)))

(defun add-hash-key-value-pair (hash-table key val)
  (setf (gethash key hash-table) val)
  hash-table)

#| Make singleton CPD |#

;; cpd = conditional probability distribution
(defun make-singleton-cpd (cpd &key probability-distribution-p)
  (labels ((make-singleton-prob-distribution (values rules size)
	     (if (null values)
		 (make-array size :initial-contents rules)
		 (make-singleton-prob-distribution (rest values)
						   (cons (make-rule :id (gensym "RULE-")
								    :conditions (add-hash-key-value-pair (make-hash-table :test #'equal)
													 (rule-based-cpd-dependent-id cpd)
													 (car values))
								    :probability (float (if (= (car values) probability-distribution-p) 1 0))
								    :count 1.0)
							 rules)
						   (+ size 1)))))
    (make-rule-based-cpd :dependent-id (rule-based-cpd-dependent-id cpd)
			 :identifiers (add-hash-key-value-pair (make-hash-table :test #'equal) (rule-based-cpd-dependent-id cpd) 0)
			 :dependent-var (rule-based-cpd-dependent-var cpd)
			 :vars (add-hash-key-value-pair (make-hash-table) 0 (rule-based-cpd-dependent-var cpd))
			 :types (add-hash-key-value-pair (make-hash-table) 0 (gethash 0 (rule-based-cpd-types cpd)))
			 :concept-ids (add-hash-key-value-pair (make-hash-table) 0 (gethash 0 (rule-based-cpd-concept-ids cpd)))
			 :qualified-vars (add-hash-key-value-pair (make-hash-table) 0 (gethash 0 (rule-based-cpd-qualified-vars cpd)))
			 :var-value-block-map (add-hash-key-value-pair (make-hash-table) 0 (gethash 0 (rule-based-cpd-var-value-block-map cpd)))
			 :negated-vvbms (add-hash-key-value-pair (make-hash-table) 0 (gethash 0 (rule-based-cpd-negated-vvbms cpd)))
			 :set-valued-attributes (add-hash-key-value-pair (make-hash-table) 0 (gethash 0 (rule-based-cpd-set-valued-attributes cpd)))
			 :set-valued-negated-attributes (add-hash-key-value-pair (make-hash-table) 0 (gethash 0 (rule-based-cpd-set-valued-negated-attributes cpd)))
			 :lower-approx-var-value-block-map (add-hash-key-value-pair (make-hash-table) 0 (gethash 0 (rule-based-cpd-lower-approx-var-value-block-map cpd)))
			 :lower-approx-negated-vvbms (add-hash-key-value-pair (make-hash-table) 0 (gethash 0 (rule-based-cpd-lower-approx-negated-vvbms cpd)))
			 :characteristic-sets (make-hash-table)
			 :characteristic-sets-values (make-hash-table)
			 :var-values (add-hash-key-value-pair (make-hash-table) 0 (gethash 0 (rule-based-cpd-var-values cpd)))
			 :cardinalities (make-array 1 :initial-contents (list (aref (rule-based-cpd-cardinalities cpd) 0)) :fill-pointer t)
			 :step-sizes (make-array 1 :initial-element 1 :fill-pointer t)
			 :rules (if probability-distribution-p
				    (make-singleton-prob-distribution (gethash 0 (rule-based-cpd-var-values cpd)) nil 0)
				    (initialize-rule-potentials factor 1))
			 :singleton-p t
			 :lvl (rule-based-cpd-lvl cpd))))

#| Perfrom probabilistic inference over bayesian network |#

;; state = bayesian network of the state represented as a graph of cpds
;; evidence = hash-table of variable value pairs
;; op = operation to apply to factor (max or +)
;; lr = learning rate
(defun loopy-belief-propagation (state evidence op lr)
  (when nil
    (format t "~%evidence listing:~%")
    (maphash #'print-hash-entry evidence))
  (let (factors-list factors singleton-factors-list singleton-factors all-factors-list all-factors edges initial-messages estimates)
    ;;(setq factors-list (coerce (car state) 'list))
    (loop
      for factor being the elements of (car state)
      do
	 (loop
	   for rule being the elements of (rule-based-cpd-rules factor)
	   do
	      (setf (rule-probability rule) (float (rule-probability rule)))
	      (setf (rule-count rule) (float (rule-count rule))))
	 (setq factors-list (cons factor factors-list))
      finally
	 (setq factors-list (reverse factors-list)))
    (when nil 
      (format t "~%explicit factors:~%~A~%num elements: ~d" factors-list (array-dimension (car state) 0)))
    (loop
      with singleton
      with dep-var and vars and types-hash and id and dep-id and cid and qvars and vvbm and nvvbm and sva and svna and lower-vvbm and lower-nvvbm and var-values and cards and steps and rules and lvl
      for factor in factors-list
      for i from (length factors-list)
      do
         (setq dep-id (rule-based-cpd-dependent-id factor))
         (setq id (make-hash-table :test #'equal))
         (setf (gethash dep-id id) 0)
         (setq dep-var (rule-based-cpd-dependent-var factor))
         (setq vars (make-hash-table))
         (setf (gethash 0 vars) dep-var)
         (setq types-hash (make-hash-table))
         (setf (gethash 0 types-hash) (gethash 0 (rule-based-cpd-types factor)))
         (setq cid (make-hash-table))
         (setf (gethash 0 cid) (gethash 0 (rule-based-cpd-concept-ids factor)))
         (setq qvars (make-hash-table))
         (setf (gethash 0 qvars) (gethash 0 (rule-based-cpd-qualified-vars factor)))
         (setq vvbm (make-hash-table))
         (setf (gethash 0 vvbm) (gethash 0 (rule-based-cpd-var-value-block-map factor)))
         (setq nvvbm (make-hash-table))
         (setf (gethash 0 nvvbm) (gethash 0 (rule-based-cpd-negated-vvbms factor)))
         (setq sva (make-hash-table))
         (setf (gethash 0 sva) (gethash 0 (rule-based-cpd-set-valued-attributes factor)))
         (setq svna (make-hash-table))
         (setf (gethash 0 svna) (gethash 0 (rule-based-cpd-set-valued-negated-attributes factor)))
         (setq lower-vvbm (make-hash-table))
         (setf (gethash 0 lower-vvbm) (gethash 0 (rule-based-cpd-lower-approx-negated-vvbms factor)))
         (setq lower-nvvbm (make-hash-table))
         (setf (gethash 0 lower-nvvbm) (gethash 0 (rule-based-cpd-lower-approx-negated-vvbms factor)))
         (setq var-values (make-hash-table))
         (setf (gethash 0 var-values) (gethash 0 (rule-based-cpd-var-values factor)))
         (setq cards (make-array 1 :initial-contents (list (aref (rule-based-cpd-cardinalities factor) 0)) :fill-pointer t))
         (setq steps (make-array 1 :initial-element 1 :fill-pointer t))
         (setq rules (initialize-rule-potentials factor 1))
         (setq lvl (rule-based-cpd-lvl factor))
         (setq singleton (make-rule-based-cpd :dependent-id dep-id
                                              :identifiers id
                                              :dependent-var dep-var
                                              :vars vars
                                              :types types-hash
                                              :concept-ids cid
                                              :qualified-vars qvars
                                              :var-value-block-map vvbm
                                              :negated-vvbms nvvbm
                                              :set-valued-attributes sva
                                              :set-valued-negated-attributes svna
                                              :lower-approx-var-value-block-map lower-vvbm
                                              :lower-approx-negated-vvbms lower-nvvbm
                                              :characteristic-sets (make-hash-table)
                                              :characteristic-sets-values (make-hash-table)
                                              :var-values var-values
                                              :cardinalities cards
                                              :step-sizes steps
                                              :rules rules
                                              :singleton-p t
                                              :lvl lvl))
	 (when (equal (rule-based-cpd-dependent-id factor) "NO_OP7336")
	   (format t "~%factor:~%~S~%singleton:~%~S" factor singleton)
	   (break))
      collect singleton into singletons
      finally (setq singleton-factors-list singletons))
    (when nil
      (format t "~%singleton factors:~%~S:~%num elements: ~d" singleton-factors-list (length singleton-factors-list)))
    (setq all-factors-list (append factors-list singleton-factors-list))
    (setq factors (make-array (length factors-list) :initial-contents factors-list :fill-pointer t))
    (setq singleton-factors (make-array (length singleton-factors-list)
                                        :initial-contents singleton-factors-list
                                        :fill-pointer t))
    (setq all-factors (make-array (length all-factors-list)
                                  :initial-contents all-factors-list
                                  :fill-pointer t))
    (setq edges (make-bethe-graph-edges factors singleton-factors evidence))
    (loop
      for i from 0 to (- (array-dimension singleton-factors 0) 1)
      with factor and offset = (- (array-dimension edges 0) (hash-table-count evidence))
      with value and index
      with messages = (make-hash-table) and msg
      with rules
      do
         (setq factor (aref singleton-factors i))
         (setq value (gethash (rule-based-cpd-dependent-id factor) evidence))
         (when nil (equal (rule-based-cpd-dependent-id factor) "ACTION7337")
           (format t "~%~%singleton factor:~%~A~%id in evidence?: ~A" factor value))
         (when value
           (when nil (equal (rule-based-cpd-dependent-id factor) "ACTION7337")
             (format t "~%observed variable: ~A~%observed variable value: ~A" (rule-based-cpd-dependent-id factor) value))
           (setq value (cdar (assoc value
				    (gethash 0 (rule-based-cpd-var-value-block-map factor))
				    :test #'equal :key #'car)))
	   (when nil (equal (rule-based-cpd-dependent-id factor) "ACTION7337")
	     (format t "~%value index: ~d" value))
	   (setq index (+ i (array-dimension factors 0)))
           (setf (aref edges offset) (cons index index))
           (setq offset (+ offset 1))
           (cond (value
                  ;;(format t "~%index: ~d~%offset: ~d~%value: ~d" index offset value)
                  (setq rules (make-array (length (gethash 0 (rule-based-cpd-var-values factor)))))
                  (loop
                    with rule
                    for val in (gethash 0 (rule-based-cpd-var-values factor))
                    for i from 0
                    when (= val value)
                      do
                         (setq rule (make-rule :id (gensym "RULE-")
                                               :conditions (make-hash-table :test #'equal)
                                               :probability 1.0
                                               :count 1.0))
                         (setf (gethash  (rule-based-cpd-dependent-id factor)
					 (rule-conditions rule))
			       val)
                         (setf (aref rules i) rule)
                    else
                      do
                         (setq rule (make-rule :id (gensym "RULE-")
                                               :conditions (make-hash-table :test #'equal)
                                               :probability 0.0
                                               :count 1.0))
                         (setf (gethash (rule-based-cpd-dependent-id factor)
					(rule-conditions rule))
			       val)
                         (setf (aref rules i) rule))
                  (setq msg (make-rule-based-cpd :dependent-id (rule-based-cpd-dependent-id factor)
                                                 :identifiers (rule-based-cpd-identifiers factor)
                                                 :dependent-var (rule-based-cpd-dependent-var factor)
                                                 :vars (rule-based-cpd-vars factor)
                                                 :types (rule-based-cpd-types factor)
                                                 :concept-ids (rule-based-cpd-concept-ids factor)
                                                 :qualified-vars (rule-based-cpd-qualified-vars factor)
                                                 :cardinalities (rule-based-cpd-cardinalities factor)
                                                 :var-value-block-map (rule-based-cpd-var-value-block-map factor)
                                                 :step-sizes (rule-based-cpd-step-sizes factor)
                                                 :rules rules
                                                 :singleton-p t
                                                 :lvl (rule-based-cpd-lvl factor)))
		  (when nil (equal (rule-based-cpd-dependent-id factor) "ACTION7337")
		   (format t "~%message:~%~S" msg)
		   (break)))
		 (t
                  (setq msg factor)))
           (when (null (gethash index messages))
             (setf (gethash index messages) (make-hash-table)))
           (setf (gethash index (gethash index messages)) msg))
      finally
         (setq initial-messages messages))
    (when nil
      (format t "~%~%Factors:~%~A~%Edges:~%~A" all-factors edges)
      (format t "~%~%initial messages:~%~A" initial-messages)
      (break))
    (setq estimates (calibrate-factor-graph all-factors op edges initial-messages lr))))

#| Move assignment by 1 |#

;; assignment = current assignment array
;; cardnialities = cardinalities of variable in assignment
;; step-sizes = step sizes for variables
;; j = ...not sure..
(defun generate-next-assignment (assignment cardinalities step-sizes j)
  (loop
    for l from 0 to (- (array-dimension assignment 0) 1)
    do
       (setf (aref assignment l) (+ (aref assignment l) 1))
       (cond ((= (aref assignment l) (aref cardinalities l))
              (setf (aref assignment l) 0)
              (setq j (- j (* (- (aref cardinalities l) 1)
                              (aref step-sizes l)))))
             (t
              (setq j (+ j (aref step-sizes l)))
              (return))))
  (cons assignment j))

(defun get-adjacent-nodes (node graph)
  (loop
     for i from 0 to (- (array-dimension (cdr graph) 0) 1)
     when (= 1 (aref (cdr graph) node i))
     collect i into adjacent
     finally (return adjacent)))

;; node = node in graph
;; visited = array of visited nodes
;; graph = relation or object node in graph
;; objects = list of object indecies
(defun graph-dfs-util (node visited graph objects)
  (setf (aref visited node) t)
  (when (<= (cpd-lvl (aref (car graph) node)) 2)
    (setq objects (reverse (cons (aref (car graph) node) (reverse objects)))))
  (loop
     for i in (get-adjacent-nodes node graph)
     when (not (aref visited i))
     do (setq objects (graph-dfs-util i visited graph objects)))
  objects)

#| Do depth first traverse to the object nodes |#

;; source = node in graph
;; graph = relation node in graph
(defun graph-dfs-from-source (source graph &aux visited)
  (setq visited (make-array (array-dimension (car graph) 0) :initial-element nil :fill-pointer t))
  (graph-dfs-util source visited graph nil))

(defun my-count (a l)
  (cond
   ((null l) 0)
   ((equal a (car l)) (+ 1 (my-count a (cdr l))))
   (t (my-count a (cdr l)))))

#| Obtain subgraph of original graph starting at node source |#

;; source = node in graph
;; graph = relation node in graph
(defun get-objects (source graph)
  (let (factors-list factors edges)
    (setq factors-list (graph-dfs-from-source source graph))
    (setq factors (make-array (length factors-list) :initial-contents factors-list :fill-pointer t))
    (setq edges (make-graph-edges factors))
    (cons factors edges)))

(defun list-difference (list1 list2)
  (cond ((null list2) list1)
        (t (list-difference (remove (car list2) list1 :count 1 :test #'equal) (rest list2)))))

(defun attribute-cpd-p (cpd)
  (or (and (> (hash-table-count (rule-based-cpd-types cpd)) 1) (equal "PERCEPT" (gethash 0 (rule-based-cpd-types cpd))) (equal "PERCEPT" (gethash 1 (rule-based-cpd-types cpd))))
      (and (> (hash-table-count (rule-based-cpd-types cpd)) 1) (equal "ACTION" (gethash 0 (rule-based-cpd-types cpd))) (equal "ACTION" (gethash 1 (rule-based-cpd-types cpd))))))

(defun percept-cpd-p (cpd)
  (and (not (attribute-cpd-p cpd))
       (equal "PERCEPT" (gethash 0 (rule-based-cpd-types cpd)))))

(defun action-cpd-p (cpd)
  (and (equal "ACTION" (gethash 0 (rule-based-cpd-types cpd)))))

;; cpd1 = cpd in p
;; cpd2 = cpd in q
;;bindings from p to q
(defun same-parent-percept (cpd1 cpd2 bindings &aux parent index)
  (loop
    named parent-finder
    for p being the hash-keys of (rule-based-cpd-identifiers cpd1)
      using (hash-value i)
    when (= i 1)
      do (setq parent p)
         (setq index i)
         (return-from parent-finder))
  (equal index (gethash (gethash parent bindings) (rule-based-cpd-identifiers cpd2))))

#| Return true if two cpds have the same set of matched parents 

;; p-cpd = conditional probability distribution in pattern graph
;; q-cpd = conditional probability distribution in base graph
;; p-copy = p-cpd with substitutions for q-cpd
;; p = pattern graph
;; q = base graph
;; assignments = matches so far
(defun same-matched-parents (p-cpd q-cpd p-copy p q assignment heuristic)
  (cond ((and (attribute-cpd-p cpd1) (attribute-cpd-p cpd2))
	 (same-parent-percept p-copy q-cpd))
	(t
	 (loop
	    with p-node and q-node
	    with p-val and q-val
	    for (n1 . n2) in assignment
	    do
	      (setq p-node (aref (car p) n1))
	      (setq p-val (cpd-child-p p-cpd p-node))
	      (setq q-node nil)
	      (setq q-val nil)
	      (when n2
          (setq q-node (aref (car q) n2))
          (setq q-val (cpd-child-p q-cpd q-node)))
	      (cond ((or (and q-node (not p-val) q-val))
               (return-from same-matched-parents nil))
              ((and (eq 'percept (gethash 0 (cpd-types p-cpd)))
                    (eq 'percept (gethash 0 (cpd-types p-node))) p-val (null q-node))
		     (return-from same-matched-parents nil)))
	    finally
	      (return t)))))
|#

#| Return true if two cpds have the same set of matched parents |#

;; p-cpd = conditional probability distribution in pattern graph
;; q-cpd = conditional probability distribution in base graph
;; bindings = variable bindings for matches
;; q-first-bindings = bindings hash table where elements of q are the keys
(defun same-matched-parents (p-cpd q-cpd bindings q-first-bindings heuristic)
  (cond ((and (attribute-cpd-p p-cpd) (attribute-cpd-p q-cpd))
         (same-parent-percept p-cpd q-cpd bindings))
	;; hotbar test is domain-specific heuristic -- I could change the domain language to make sure hotbar percepts are encoded differently, but I don't have time for that right now. I need unique hotbar percepts, so that all of them don't get mashed together in the schema.
	((and (percept-cpd-p p-cpd) (percept-cpd-p q-cpd) (not (equal "HOTBAR" (gethash 0 (rule-based-cpd-vars p-cpd)))))
	 t)
        (t
         (when nil (and heuristic (= cycle* 21) (equal (rule-based-cpd-dependent-var p-cpd) "RESOURCE"))
           (format t "~%~%p-cpd: ~S~%q-cpd: ~S" (rule-based-cpd-identifiers p-cpd) (rule-based-cpd-identifiers q-cpd)))
         (loop
           with p-val and p-match
           for q-val being the hash-keys of (rule-based-cpd-identifiers q-cpd)
             using (hash-value q-idx)
           when (> q-idx 0) do
             (setq p-val nil)
             (setq p-match (gethash q-val q-first-bindings))
             (when p-match
               (if (gethash p-match (rule-based-cpd-identifiers p-cpd))
                   (setq p-val t)))
             (when nil (and heuristic (= cycle* 21) (equal (rule-based-cpd-dependent-var p-cpd) "RESOURCE"))
               (format t "~%~%p bindings:~%~S~%q first bindings:~%~S" bindings q-first-bindings)
               (format t "~%q-parent: ~S~%p-parent match: ~S~%p-val: ~S" q-val p-match p-val))
             (cond ((and p-match (not p-val) q-val)
                    (when nil (and heuristic (= cycle* 21) (equal (rule-based-cpd-dependent-var p-cpd) "RESOURCE"))
                      (format t "~%fail. Returning.")
                      (break))
                    (return-from same-matched-parents nil))))
         (loop
           with q-val and q-match and no-matched-parents = t
           for p-val being the hash-keys of (rule-based-cpd-identifiers p-cpd)
             using (hash-value p-idx)
           when (> p-idx 0) do
             (setq q-val nil)
             (setq q-match (gethash p-val bindings))
             (when q-match
               (setq no-matched-parents nil)
               (if (gethash q-match (rule-based-cpd-identifiers q-cpd))
                   (setq q-val t)))
             (when nil (and heuristic (= cycle* 21) (equal (rule-based-cpd-dependent-var p-cpd) "RESOURCE"))
               (format t "~%~%p bindings:~%~S~%q first bindings:~%~S" bindings q-first-bindings)
               (format t "~%p-parent: ~S~%q-parent match: ~S~%q-val: ~S" p-val q-match q-val))
             (cond ((and q-match p-val (not q-val))
                    (when nil (and heuristic (= cycle* 21) (equal (rule-based-cpd-dependent-var p-cpd) "RESOURCE"))
                      (format t "~%fail. Returning.")
                      (break))
                    (return-from same-matched-parents nil)))
           finally
              (if (and nil no-matched-parents (> (hash-table-count (rule-based-cpd-identifiers q-cpd)) 1))
                  (return-from same-matched-parents nil)))
         (when nil (and heuristic (= cycle* 21) (equal (rule-based-cpd-dependent-var p-cpd) "RESOURCE"))
           (format t "~%success.")
           ;;(break)
	   )
         t)))

#| A set of candidate matches for pnum |#

;; pnum = index of cpd in pattern graph
;; p = pattern graph
;; q = base graph
;; possible-q-candidates = list of potential candidate nodes in base graph
;; bindings = variable bindings for matches
;; q-first-bindings = bindings hash table where elements of q are the keys
(defun candidate-nodes (pnum p q possible-q-candidates bindings q-first-bindings &optional (heuristic nil) &aux p-cpd)
  (setq p-cpd (aref (car p) pnum))
  (when nil (and (= cycle* 21) (equal (rule-based-cpd-dependent-var p-cpd) "RESOURCE"))
        (format t "~%~%p-cpd:~%~S" (rule-based-cpd-identifiers p-cpd)))
  (loop
    with q-cpd
    for i in possible-q-candidates
    do
       (setq q-cpd (aref (car q) i))
       (when nil (and (= cycle* 21) (equal (rule-based-cpd-dependent-var p-cpd) "RESOURCE"))
        (format t "~%~%p-cpd:~%~S" (rule-based-cpd-identifiers p-cpd)))
    when (and (not (gethash (rule-based-cpd-dependent-id q-cpd) q-first-bindings))
              (same-matched-parents p-cpd q-cpd bindings q-first-bindings heuristic))
      collect i into candidates
     finally
       (cond ((and (= (hash-table-count (rule-based-cpd-identifiers p-cpd)) 1)
		   candidates)
	      (return candidates)
	      ;;(return (cons nil candidates))
	      )
	     ((and (> (hash-table-count (rule-based-cpd-identifiers p-cpd)) 1)
		   candidates)
	      (return candidates))
	     ((null candidates)
	      (return (list nil))))))

#| Find nodes in q that have the same role as pnum in p |#

;; pnum = index of cpd in pattern graph
;; p = pattern graph
;; q = base graph
;; possible-q-candidates = list of potential candidate nodes in base graph
;; bindings = variable bindings for matches
;; q-first-bindings = bindings hash table where elements of q are the keys
(defun analog-nodes (pnum p q possible-q-candidates bindings q-first-bindings &aux p-cpd)
  (setq p-cpd (aref (car p) pnum))
  (when nil (and (= cycle* 11) (equal (rule-based-cpd-dependent-var p-cpd) "BLOCK"))
        (format t "~%~%p-cpd:~%~S" (rule-based-cpd-identifiers p-cpd)))
  (loop
    with q-cpd
    for i in possible-q-candidates
    do
       (setq q-cpd (aref (car q) i))
       (when nil (and (= cycle* 11) (equal (rule-based-cpd-dependent-var p-cpd) "BLOCK"))
             (format t "~%q-cpd:~%~S" (rule-based-cpd-identifiers q-cpd)))
    when (same-matched-parents p-cpd q-cpd bindings q-first-bindings t)
     collect i into candidates
     and counting i into size
     finally
	(cond (nil (and (= (hash-table-count (rule-based-cpd-identifiers p-cpd)) 1)
		    candidates)
	       (setq candidates (cons nil candidates))
	       (setq size (+ 1 size)))
	      ((null candidates)
	       (setq candidates (list nil))
	       (setq size 1)))
	(return (make-array size :initial-contents candidates :fill-pointer t))))

(defun unzip (lol)
  (loop
     for ele in lol
     collect (car ele) into list1
     collect (cdr ele) into list2
     finally
       (return (values list1 list2))))



#| Find rules whose conditions in the intersection agree

;; schema-cpd = conditional probability distribution
;; event-cpd = conditional probability distribution
;; rule = rule to reference when finding compatible rules
(defun get-compatible-rules (schema-cpd event-cpd rule &key (find-all t))
  (loop
    for schema-rule being the elements of (rule-based-cpd-rules schema-cpd)
    when (and find-all (compatible-rule-p schema-rule rule schema-cpd event-cpd))
      collect schema-rule into compatible-rules
    else when (and (not find-all) (compatible-rule-p schema-rule rule schema-cpd event-cpd))
      collect schema-rule into compatible-rules and
    do
       (return-from get-compatible-rules compatible-rules)
    finally
       (return compatible-rules)))
|#

#| Find rules whose conditions in the intersection agree |#

;; schema-cpd = conditional probability distribution
;; event-cpd = conditional probability distribution
;; rule = rule to reference (from event-cpd) when finding compatible rules
(defun get-compatible-rules (schema-cpd event-cpd rule &key (find-all t) (check-count nil))
  (loop
    with event-dependent-id-val = (gethash (rule-based-cpd-dependent-id event-cpd) (rule-conditions rule))
    with match-p = nil
    for schema-rule being the elements of (rule-based-cpd-rules schema-cpd)
    when (and find-all (compatible-rule-p schema-rule rule schema-cpd event-cpd :check-count check-count))
      do
         (setq match-p t)
      and collect schema-rule into compatible-rules
    else when (and (not find-all) (compatible-rule-p schema-rule rule schema-cpd event-cpd :check-count check-count))
      collect schema-rule into compatible-rules and
    do
       (setq match-p t)
       (return-from get-compatible-rules compatible-rules)
    finally
       (when (null match-p)
         (let (zero-count-rule)
           (cond ((null event-dependent-id-val)
                  (loop
                    for i from 0 to (- (aref (rule-based-cpd-cardinalities event-cpd) 0) 1)
                    do
                       (setq zero-count-rule (make-rule :id (symbol-name (gensym "RULE-"))
                                                        :conditions (copy-hash-table (rule-conditions rule))
                                                        :count (if (rule-based-cpd-singleton-p schema-cpd) nil 0)))
                       (setf (gethash (rule-based-cpd-dependent-id event-cpd) (rule-conditions zero-count-rule)) i)
                    when (= i 0)
                      do
                         (setf (rule-probability zero-count-rule) 1)
                    else
                      do
                         (setf (rule-probability zero-count-rule) 0)
                    collect zero-count-rule into rules
                    finally
                       (if find-all
                           (setq compatible-rules rules)
                           (setq compatible-rules (list (car rules))))))
                 ((= event-dependent-id-val 0)
                  (setq zero-count-rule (make-rule :id (symbol-name (gensym "RULE-"))
                                                   :conditions (copy-hash-table (rule-conditions rule))
                                                   :probability 1
                                                   :count (if (rule-based-cpd-singleton-p schema-cpd) nil 0)))
                  (setq compatible-rules (cons zero-count-rule compatible-rules)))
                 (t
                  (setq zero-count-rule (make-rule :id (symbol-name (gensym "RULE-"))
                                                   :conditions (copy-hash-table (rule-conditions rule))
                                                   :probability 0
                                                   :count (if (rule-based-cpd-singleton-p schema-cpd) nil 0)))
                  (setq compatible-rules (cons zero-count-rule compatible-rules))))))
       (return compatible-rules)))

#| Compute the likelihood that schema generates epsidode |#

;; event = episode cpd
;; cpd = schema cpd
;; forbidden-likelihood-p = flag for whether or not we should calculate likelihood or just return 1
(defun local-likelihood (event cpd &optional forbidden-likelihood-p)
  (cond (cpd
         (cond (forbidden-likelihood-p
                1)
               ((> (hash-table-count (rule-based-cpd-vars event)) (hash-table-count (rule-based-cpd-vars cpd)))
                0)
               (t
                (let (local-likelihood extra-schema-vars)
                  (declare (ignore extra-schema-vars))
                  (setq local-likelihood 1)
                  ;; if the arg has no match, you shouldn't match it..
                  (cond ((not (= (hash-difference-p (rule-based-cpd-identifiers event) (rule-based-cpd-identifiers cpd) event) 0))
                         (setq local-likelihood 0))
                        (t
                         (cond ((> (hash-table-count (rule-based-cpd-vars event)) (hash-table-count (rule-based-cpd-vars cpd)))
                                (setq local-likelihood 0))
                               (t
                                (when nil t
                                  (format t "~%episode cpd:~%~S~%schema cpd:~%~S" event cpd))
                                (loop
                                  with rule and compatible-rules
                                  with m and theta-mle and res
                                  for rule-idx from 0 to (- (array-dimension (rule-based-cpd-rules event) 0) 1)
                                  ;;for count-idx being the hash-keys of (cpd-counts event)
                                  do
                                     (setq rule (aref (rule-based-cpd-rules event) rule-idx))
				     (when (> (rule-probability rule) 0)
				       ;;(setq compatible-rules (list (car (get-compatible-rules cpd event rule))))
                                       (setq compatible-rules (get-compatible-rules cpd event rule :find-all t))
                                       ;; find compatible rules in schema
                                       (cond (compatible-rules
                                              (when nil t
                                                    (format t "~%episode rule:~%~S" rule))
                                              (loop
					        for compatible-rule in compatible-rules
						for denom from 1
						summing (rule-probability compatible-rule) into prob
						finally
						   (setq theta-mle (/ prob denom))
						   (setq m (rule-count rule))
						   (setq res (rationalize (expt theta-mle m)))
						   (setq local-likelihood (* local-likelihood res))
						   (when (= local-likelihood 0)
						     (return-from local-likelihood 0))
						   (when nil t
							 (format t "~%new likelihood: ~d" local-likelihood))
						   #|
						do
						   
                                                   (setq theta-mle (rule-probability compatible-rule))
					           (setq m (rule-count rule))
						   (setq res (rationalize (expt theta-mle m)))
                                                   (when nil t
							 (format t "~%compatible rule:~%~S~%theta-mle: ~d~%m: ~d~%res: ~d" compatible-rule theta-mle m res))
                                                   (when (> res 1)
                                                     (error "probability is greater than 1!~%rules:~%~A~%theta-mle: ~d~%m: ~d~%probability: ~d " (rule-based-cpd-rules cpd) theta-mle m (expt theta-mle m)))
                                                   (setq local-likelihood (* local-likelihood res))
						   (when (= local-likelihood 0)
						     (return-from local-likelihood 0))
						   (when nil t
							 (format t "~%new likelihood: ~d" local-likelihood))
						|#))
                                             (t
                                              (return-from local-likelihood 0)))))))))
                  local-likelihood))))
        (t
         0)))

#| Prevent system from penalyzing on cpds that it shouldn't |#

;; var-dif = list of identifiers in schema that are not in episode
;; cpd = schema cpd
;; forbidden-types = cpd types to remove from var-dif
(defun remove-forbidden-identifiers (var-dif cpd forbidden-types)
  (loop
    with new-vars
    for type in forbidden-types
    do
       (loop
         with var-pos and var-type
         for var in var-dif do
           (setq var-pos (gethash var (cpd-identifiers cpd)))
           (setq var-type (gethash var-pos (cpd-types cpd)))
           (when (not (equal type var-type))
             (setq new-vars (cons var-type new-vars))))
    finally
       (if forbidden-types
           (return new-vars)
           (return var-dif))))

#| Add na condition for each variable not in cpd |#

;; cpd = conditional probability distribution
;; var-dif = variables missing from cpd
(defun add-na-conditions-to-rules (cpd var-dif)
  (loop
    for var in var-dif
    do
       (loop
	 for rule being the elements of (rule-based-cpd-rules cpd)
	 do
	    (setf (gethash var (rule-conditions rule)) 0)))
  cpd)

#| Cost of matching two cpds |#

;; x = episode cpd
;; y = schema cpd
;; bindings = bindings between episode and schema
;; q-first-bindings = bindings hash table where elements of q are the keys
;; cost-of-nil = episode count for matching to nil
;; bic-p = whether to compute bic or likelihood
(defun cost (x y bindings q-first-bindings &key (cost-of-nil 2) (bic-p t) (forbidden-types nil))
  (cond ((and (numberp x) (numberp y))
         (abs (- x y)))
        (t ;; we are matching cpds
         (incf calls-to-cost*)
         (let (likelihood var-x-name var-y-name sum-count)
           (setq var-x-name (rule-based-cpd-dependent-var x))
           (when (rule-based-cpd-p y)
             (setq var-y-name (rule-based-cpd-dependent-var y))
             (if (= (rule-based-cpd-count y) 1)
                 (setq sum-count 2)
                 (setq sum-count (rule-based-cpd-count y))))
           (cond ((and (not (null y))
                       (or (not (equal var-x-name var-y-name))
                           #|(not (equal (gethash 0 (rule-based-cpd-concept-ids x))
                                       (gethash 0 (rule-based-cpd-concept-ids y))))|#
			   ))
                  0)
                 ((member (gethash 0 (rule-based-cpd-types x)) forbidden-types :test #'equal)
                  1)
                 (t
                  (let (x-copy discount var-dif kost bic)
                    (cond ((rule-based-cpd-p y)
			   (when nil (and (= cycle* 4) y (equal "BLOCK581" (rule-based-cpd-dependent-id y)))
			     (format t "~%~%p-cpd before subst:~%~S~%q-match:~%~S" x y))
                           (setq x-copy (subst-cpd-2 x y bindings))
			   (when nil (and (= cycle* 4) y (equal "BLOCK581" (rule-based-cpd-dependent-id y)))
			     (format t "~%p-cpd after subst:~%~S" x-copy))
                           (cond ((hash-intersection-p (rule-based-cpd-identifiers x-copy) (rule-based-cpd-identifiers y))
				  (multiple-value-bind (dif forbidden-likelihood)
                                      (hash-difference-p (rule-based-cpd-identifiers y)
                                                       (rule-based-cpd-identifiers x-copy) y forbidden-types)
                                    (setq var-dif dif)
                                    ;;remove identifiers whose cpd types are in the ignore list
                                    ;;(setq var-dif (remove-forbidden-identifiers var-dif y forbidden-types))
				    ;;(setq x-copy (add-na-conditions-to-rules x-copy var-dif))
                                    (setq likelihood (local-likelihood x-copy y forbidden-likelihood))
                                    ;;(when (< bic 0) (setq bic 0))
                                    (when nil
                                      (format t "~%var-dif: ~A~%discount: ~d~%likelihood: ~d~%forbidden likelihood?: ~A~%total penalty: ~d" var-dif discount likelihood forbidden-likelihood (- likelihood discount))))
                                  (cond (bic-p
                                         ;;(setq kost (- 1 (/ (* (rule-based-cpd-lvl x) bic) (rule-based-cpd-lvl x))))
                                         ;;(setq kost (* (rule-based-cpd-lvl x) (- 1 (- likelihood discount))))
					 ;;(setq kost (* (- 1 (/ 1 (rule-based-cpd-lvl x))) likelihood))
					 (setq kost likelihood)
					 (when (< kost 0)
                                           (error "~%negative cost ~d for cpds:~%~A~%and~%~A.~%(cpd-lvl x): ~d~%likelihood: ~d~%discount: ~d~%(log sum-count): ~d~%sum-count: ~d~%length var-dif: ~d" kost x-copy y (rule-based-cpd-lvl x) likelihood discount (log sum-count) sum-count var-dif))
					 (when nil (and (= cycle* 4) y (equal "BLOCK581" (rule-based-cpd-dependent-id y)))
					   (format t "~%likelihood: ~d" kost)
					   (break))
					 kost)
                                        (t
                                         ;;(setq kost (- 1 (/ (* (rule-based-cpd-lvl x) likelihood) (rule-based-cpd-lvl x))))
                                         ;;(setq kost (* (rule-based-cpd-lvl x) (- 1 likelihood)))
					 ;;(setq kost (* (- 1 (/ 1 (rule-based-cpd-lvl x))) likelihood))
					 (setq kost likelihood)
					 (when nil (and (= cycle* 4) y (equal "BLOCK581" (rule-based-cpd-dependent-id y)))
					   (format t "~%likelihood: ~d" kost)
					   (break))
					 kost)))
                                 (t
				  ;;(break "~%here2~%episode cpd:~%~A~%substituted episode:~%~A~%schema cpd:~%~A~%bindings:~%~A~%q-first-bindings:~%~A" x x-copy y bindings q-first-bindings)
                                  ;;most-positive-fixnum
				  (when nil (and (= cycle* 4) y (equal "BLOCK581" (rule-based-cpd-dependent-id y)))
				    (format t "~%likelihood: 0")
				    (break))
				  0)))
                          (t
			   ;;(* (cpd-lvl x) 1)
                           (if (= cost-of-nil 1) (setq cost-of-nil 2))
                           (setq var-dif (hash-table-count (rule-based-cpd-identifiers x)))
                           (setq discount (* (/ (log cost-of-nil) 2) var-dif))
                           (setq likelihood 0)
                           (setq bic 0)
			   0
			   #|
			   (cond (bic-p
                                  ;;(- 1 (/ (* (rule-based-cpd-lvl x) bic) (rule-based-cpd-lvl x)))
                                  (* (rule-based-cpd-lvl x) (- 1 (- likelihood discount))))
                                 (t
                                  ;;(- 1 (/ (* (rule-based-cpd-lvl x) likelihood) (rule-based-cpd-lvl x)))
                                  (* (rule-based-cpd-lvl x) (- 1 likelihood))))
			   |#)))))))))

(defun c (a b bindings p q)
  (cond (b
         #|
         (format t "~%~%edge1: ~A~%edge2: ~A~%cost: ~d" a b (cost (hash-access (cdr p) most-positive-fixnum (list (car a) (cdr a)))
         (hash-access (cdr q) most-positive-fixnum (list (car b) (cdr b)))
         bindings))
         |#
         (cost (hash-access (cdr p) most-positive-fixnum nil (list (car a) (cdr a)))
               (hash-access (cdr q) most-positive-fixnum nil (list (car b) (cdr b)))
               bindings))
        (t
         #|
         (format t "~%~%edge1: ~A~%no b.~%cost: ~d" a (cost (hash-access (cdr p) most-positive-fixnum (list (car a) (cdr a)))
                                                            ;; add the depth of p[a, 0] as penalty
                                                            (+ (hash-access (cdr p) most-positive-fixnum (list (car a) (cdr a))) (cpd-lvl (aref (car p) (car a))))
                                                            bindings)) |#
         (cost (hash-access (cdr p) most-positive-fixnum nil (list (car a) (cdr a)))
               ;; add the depth of p[a, 0] as penalty
               (+ (hash-access (cdr p) most-positive-fixnum nil (list (car a) (cdr a))) (cpd-lvl (aref (car p) (car a))))
               bindings))))

(defun c-prime (e1 e2 bindings p q)
  (* (c e1 (when (and (car e2) (cdr e2)) e2) bindings p q)
     2))

;; cost-of-nil = episode count for matching to nil
;; bic-p = whether to compute bic or likelihood
(defun k (n bindings p q cost-of-nil bic-p forbidden-types)
  (let (p-node qp)
    (multiple-value-bind (n1 n2)
        (unzip n)
      (setq p-node (car n1))
      (setq qp (car n2))
      (cost (aref (car p) p-node)
            (if qp (aref (car q) qp) nil)
            bindings
            :cost-of-nil cost-of-nil
            :bic-p bic-p
            :forbidden-types forbidden-types))))

#| Determine total cost for current assignment |#

;; n = assignment so far
;; bindings = variable bindings hash table
;; q-first-bindings = bindings hash table where elements of q are the keys
;; p = pattern graph
;; q = base graph
;; q-dif = difference between number of free variables (nodes with no parents) in q that p doesn't have
;; cost-of-nil = episode count for matching to nil
(defun g (n bindings q-first-bindings p q q-dif q-m &key (cost-of-nil 2) (bic-p t) (forbidden-types nil))
  (loop
     with q-likelihood = 1
     for (p-node . qp) in n
     do
       (setq q-likelihood (* q-likelihood
			     (cost (aref (car p) p-node)
				   (if qp (aref (car q) qp) nil)
				   bindings
				   q-first-bindings
				   :cost-of-nil cost-of-nil
				   :bic-p bic-p
				   :forbidden-types forbidden-types)))
     finally
	(if bic-p
	    (return (abs (- 1 (- q-likelihood
				 (* (/ (log q-m) 2)
				    q-dif)))))
	    (return (abs (- 1 q-likelihood))))))
  
;; cost-of-nil = episode count for matching to nil
;; bic-p = whether to compute bic or likelihood
;; q-first-bindings = bindings hash table where elements of q are the keys
(defun k-prime (p-node qp n1 n2 bindings q-first-bindings p q cost-of-nil bic-p forbidden-types)
  (cost (aref (car p) p-node)
        (if qp (aref (car q) qp) nil)
        bindings
        q-first-bindings
        :cost-of-nil cost-of-nil
        :bic-p bic-p
        :forbidden-types forbidden-types))

(defun my-min (vals)
  (loop
     with min-ele = (first vals)
     for val in (rest vals)
     do
       (when (< (fourth val) (fourth min-ele))
	 (setq min-ele val))
     finally
       (return min-ele)))

#| Estimate future cost of matches |#

;; m1 = unassigned nodes in pattern
;; n1 = assigned nodes in pattern
;; n2 = base nodes that match to n1 nodes
;; assignment = assignment so far
;; possible-candidates = list of potential candidate nodes in base graph
;; bindings = variable bindings hash table
;; q-first-bindings = bindings hash table where elements of q are the keys
;; cost-of-nil = episode count for matching to nil
;; bic-p = whether to compute bic or likelihood
(defun a (m1 n1 n2 assignment possible-candidates bindings q-first-bindings p q cost-of-nil bic-p forbidden-types)
  (when (and nil (= cycle* 2) (= (car n1) 8))
    (format t "~%~%estimating future cost"))
  (setq assignment (cons (cons (car n1) (car n2)) assignment))
  (reduce '+ (mapcar #'(lambda (i)
                         (when (and nil (= cycle* 2) (= (car n1) 8))
                           (format t "~%~%unmatched p: ~d~%~A" i (aref (car p) i)))
                         (let (min-ele)
                           (setq min-ele (my-min (mapcar #'(lambda (qmatch)
                                                             (when (and nil (= cycle* 2) (= (car n1) 8))
                                                               (format t "~%qp: ~d~%~A" qmatch (if (null qmatch) nil (aref (car q) qmatch))))
                                                             (let (new-bindings new-q-first-bindings new-assn kost)
                                                               (setq new-bindings (copy-hash-table bindings))
                                                               (setq new-q-first-bindings (copy-hash-table q-first-bindings))
                                                               (when qmatch
                                                                 (setf (gethash (rule-based-cpd-dependent-id (aref (car p) i)) new-bindings) (rule-based-cpd-dependent-id (aref (car q) qmatch)))
                                                                 (setf (gethash (rule-based-cpd-dependent-id (aref (car q) qmatch)) new-q-first-bindings) (rule-based-cpd-dependent-id (aref (car p) i))))
                                                               (setq new-assn (cons (cons i qmatch) assignment))
                                                               (setq kost (k-prime i qmatch n1 n2 new-bindings new-q-first-bindings p q cost-of-nil bic-p forbidden-types))
                                                               (when (and nil (= cycle* 2) (= (car n1) 8))
                                                                 (format t "~%cost of match: ~d" kost))
                                                               (list new-bindings new-q-first-bindings new-assn kost)))
                                                         (candidate-nodes i p q (gethash i possible-candidates) bindings q-first-bindings))))
                           (when (and nil (= cycle* 2) (= (car n1) 8))
                             (format t "~%future assignment:~%~A~%bindings: ~A~%cost: ~d" (second min-ele) (first min-ele) (third min-ele)))
                           (setq bindings (first min-ele))
			   (setq q-first-bindings (second min-ele))
                           (setq assignment (third min-ele))
                           (fourth min-ele)))
                     m1)))

;; cost-of-nil = episode count for matching to nil
;; possible-candidates = list of potential candidate nodes in base graph
(defun h (m1 n1 n2 assignment possible-candidates bindings q-first-bindings p q cost-of-nil bic-p forbidden-types)
  (a m1 n1 n2 assignment possible-candidates bindings q-first-bindings p q cost-of-nil bic-p forbidden-types))

#| Estimate total cost of match from partial match |#

;; n = assignment so far
;; possible-candidates = list of potential candidate nodes in base graph
;; bindings = variable bindings hash table
;; q-first-bindings = bindings hash table where elements of q are the keys
;; m1 = unassigned nodes from pattern
;; p = pattern graph
;; q = base graph
;; cost-of-nil = episode count for matching to nil
;; bic-p = whether to compute bic or likelihood
(defun f (n possible-candidates bindings q-first-bindings m1 p q cost-of-nil bic-p forbidden-types)
  (let (optimal-cost-to-p)
    (setq optimal-cost-to-p (g n bindings q-first-bindings p q :cost-of-nil cost-of-nil :bic-p bic-p :forbidden-types forbidden-types))
    (when nil t
      (format t "~%optimal cost to p: ~d" optimal-cost-to-p))
    (+ optimal-cost-to-p (h m1 (mapcar 'car n) (mapcar 'cdr n) (rest n) possible-candidates bindings q-first-bindings p q cost-of-nil bic-p forbidden-types))))

#| Map each node in p to a set of possible matches in q |#

;; p = pattern graph
;; q = base graph
(defun get-possible-candidates (p q)
  (loop
    with possible-candidates = (make-hash-table)
    with p-cpd
    for i from 0 to (- (if p (array-dimension (car p) 0) 0) 1)
    do
       (setq p-cpd (aref (car p) i))
       (when nil (and (= cycle* 6) (equal (rule-based-cpd-dependent-var p-cpd) "BLOCK"))
             (format t "~%~%p-cpd:~%~S" (rule-based-cpd-identifiers p-cpd)))
       (loop
         with q-cpd
         for j from 0 to (- (if q (array-dimension (car q) 0) 0) 1)
         do
            (setq q-cpd (aref (car q) j))
            (when (and
		   ;;(equal (gethash 0 (rule-based-cpd-vars p-cpd)) (gethash 0 (rule-based-cpd-vars q-cpd)))
		   (equal (gethash 0 (rule-based-cpd-qualified-vars p-cpd)) (gethash 0 (rule-based-cpd-qualified-vars q-cpd)))
                   (equal (gethash 0 (rule-based-cpd-types p-cpd)) (gethash 0 (rule-based-cpd-types q-cpd))))
              (when nil (and (= cycle* 6) (equal (rule-based-cpd-dependent-var p-cpd) "BLOCK"))
                    (format t "~%possible candidate::~%~S" (rule-based-cpd-identifiers q-cpd))
                    (break))
              (setf (gethash i possible-candidates)
                    (cons j (gethash i possible-candidates)))))
    finally
       (return possible-candidates)))

#| Find largest common subgraph between two graphs |#


;; p = pattern graph
;; q = base graph
;; bic-p = whether to compute bic or likelihood
(defun subgraph-optimal-monomorphism (p q &optional (bic-p t) &aux p-nodes q-nodes)
  ;;(setq p (cons (car p) (make-edge-matrix (cdr p) (array-dimension (car p) 0))))
  ;;(setq q (cons (car q) (make-edge-matrix (cdr q) (array-dimension (car q) 0))))
  (loop
     for i from 0 to (- (array-dimension (car p) 0) 1)
     collect i into nds
     finally (setq p-nodes (make-array (length nds)
				       :initial-contents nds
				       :fill-pointer t)))
  (loop
     for i from 0 to (- (array-dimension (car q) 0) 1)
     collect i into nds
     finally (setq q-nodes (make-array (length nds)
				       :initial-contents nds
				       :fill-pointer t)))
  (let* ((tentative nil)
	 (tentative-bindings nil)
	 (n2 nil)
	 (active-node-list (list (list n2 0 nil)))
	 (p-weight)
	 (q-weight)
	 (intersection-weight)
	 (intersection)
	 (weighted-jaccard-index)
	 (upper most-positive-fixnum))
    (setq p-weight (reduce #'+ (mapcar #'(lambda (node)
					   (cpd-lvl (aref (car p) node)))
				       (coerce p-nodes 'list))))
    (setq q-weight (reduce #'+ (mapcar #'(lambda (node)
					   (cpd-lvl (aref (car q) node)))
				       (coerce q-nodes 'list))))
    ;;(format t "~%~%p:~%~A~%q:~%~A" (car p) (car q))
    (loop
       while active-node-list
       with cur-node and n2-prime and m1 and m2-prime and pnum
       with plist and qlist and p-candidates and bindings
       do
	 ;;cur-node = (list matches cost bindings)
	 (setq cur-node (car active-node-list))
	 (setq active-node-list (rest active-node-list))
	 (setq n2-prime (car cur-node))
	 (setq bindings (third cur-node))
	 (setq plist (coerce p-nodes 'list))
	 (setq qlist (coerce q-nodes 'list))
	 (loop
	    with p-copy = plist
	    for node in (mapcar 'car n2-prime)
	    do
	      (setq p-copy (remove node p-copy))
	    finally (setq pnum (car p-copy)))
	 (loop
	    with p-copy = plist
	    for node in (mapcar 'car n2-prime)
	    do
	      (setq p-copy (remove node p-copy))
	    finally (setq m1 (cdr p-copy)))
	 (loop
	    with q-copy = qlist
	    for node in (mapcar 'cdr n2-prime)
	    do
	      (setq q-copy (remove node q-copy))
	    finally (setq m2-prime q-copy))
	 (setq p-candidates (candidate-nodes pnum p q n2-prime bindings))
	 ;;(format t "~%~%matching node ~d:~%~A~%qps: ~A~%current bindings: ~A" pnum (aref (car p) pnum) p-candidates bindings)
	 (loop
	    for s in p-candidates
	    with qp and res and m2
	    with kost and binding and new-bindings
	    do
	      (setq qp s)
	      ;;(format t "~%candiate match:~%~A" qp)
	      (cond (qp
		     (setq binding (cons (car (cpd-identifiers (aref (car p) pnum)))
					 (car (cpd-identifiers (aref (car q) qp)))))
		     (setq new-bindings (cons binding bindings)))
		    (t
		     (setq new-bindings (cons nil bindings))))
	      (setq n2 (cons (cons pnum qp) n2-prime))
	      (setq m2 (remove qp m2-prime))
	      (setq kost (f n2 new-bindings m1 n2-prime p q))
	      ;; res needs to add the bindings
	      (setq res (list n2 kost new-bindings))
	      (cond (m1
               (setq active-node-list (iter-ins res active-node-list)))
              ((< (second res) upper)
               (setq upper (second res))
               (setq tentative (car res))
               (setq tentative-bindings (third res))))
	      ;;(format t "~%upper: ~d ~%active node list: ~A" upper active-node-list)
      )
	 (setq active-node-list (mapcan #'(lambda (node)
					    (when (< (second node) upper)
					      (list node)))
					active-node-list))
       ;;(format t "~%Active-node-list after pruning: ~A" active-node-list)
	 )
    ;; values solution and optimal cost / jaccard index
    (loop
       for ele in tentative
       collect (cons (aref (car p) (car ele))
		     (if (cdr ele)
			 (aref (car q) (cdr ele))
			 nil)) into tent
       finally
	 (setq tentative tent))
    (setq tentative (sort tentative #'higher-lvl-cpd :key 'car))
    (setq intersection (mapcan #'(lambda (pair)
				   (when (cdr pair)
				     (list pair)))
			       tentative))
    (setq intersection-weight (reduce #'+ (mapcar #'(lambda (pair)
						      (cpd-lvl (car pair)))
						  intersection)))
    (setq weighted-jaccard-index (/ (* intersection-weight (length intersection))
				    (- (+ (* p-weight (array-dimension p-nodes 0))
					  (* q-weight (array-dimension q-nodes 0)))
				       (* intersection-weight (length intersection)))))
    (if (> weighted-jaccard-index 0)
	(values tentative (/ upper weighted-jaccard-index) tentative-bindings)
	(values tentative most-positive-fixnum tentative-bindings))))

#| Find largest common subgraph between two graphs |#

;; p = pattern graph
;; q = base graph
;; bic-p = whether to compute bic or likelihood
(defun subgraph-greedy-monomorphism (p q &key (cost-of-nil 2) (bic-p t) (forbidden-types nil) &aux p-nodes q-nodes)
  (let* ((partial-sol (list nil 0 (make-hash-table) (make-hash-table)))
         (n2 nil)
         (p-weight 0)
         (q-weight 0)
         (possible-candidates (get-possible-candidates p q))
         total-weight
         plist
         qlist)
    (setq p-nodes (make-hash-table))
    (setq q-nodes (make-hash-table))
    (loop
      for i from 0 to (- (if p (array-dimension (car p) 0) 0) 1)
      do
         (setf (gethash (rule-based-cpd-dependent-id (aref (car p) i)) p-nodes) i)
      collect i into p-list
      summing (rule-based-cpd-lvl (aref (car p) i)) into weight
      finally
         (setq plist p-list)
         (setq p-weight weight))
    (loop
      for i from 0 to (- (if q (array-dimension (car q) 0) 0) 1)
      do
         (setf (gethash (rule-based-cpd-dependent-id (aref (car q) i)) q-nodes) i)
      collect i into q-list
      summing (rule-based-cpd-lvl (aref (car q) i)) into weight
      finally
         (setq qlist q-list)
         (setq q-weight weight))
    (setq total-weight (+ p-weight q-weight))
    (when nil t
      (format t "~%~%p:~%~A~%q:~%~A" (car p) (car q)))
    (loop
      with n2-prime and m2-prime
      with m1 = plist and p-candidates and bindings = (make-hash-table) and q-first-bindings = (make-hash-table)
      for pnum from 0 to (- (length plist) 1)
      do
         ;;partial-sol = (list matches cost bindings q-first-bindings)
         (setq n2-prime (car partial-sol))
         (if (third partial-sol)
             (setq bindings (third partial-sol)))
         (if (fourth partial-sol)
             (setq q-first-bindings (fourth partial-sol)))
         (when nil t
               (format t "~%~%partial assignments: ~A~%partial bindings: ~A" n2-prime bindings))
         (setq m1 (remove pnum m1))
         (loop
           with q-copy = qlist
           for node in (mapcar 'cdr n2-prime)
           do
              (setq q-copy (remove node q-copy))
           finally (setq m2-prime q-copy))
         (setq p-candidates (candidate-nodes pnum p q (gethash pnum possible-candidates) bindings q-first-bindings))
         (when nil t
           (format t "~%matching node ~d:~%~A~%qps: ~A" pnum (aref (car p) pnum) p-candidates))
         (loop
           for s in p-candidates
           with qp and res and m2
           with kost and new-bindings and new-q-first-bindings
           do
              (setq new-bindings (copy-hash-table bindings))
              (setq new-q-first-bindings (copy-hash-table q-first-bindings))
              (setq qp s)
              (cond (qp
                     (when nil t
                       (format t "~%~%candidate match: ~A~%~A" qp (aref (car q) qp)))
                     (setf (gethash (rule-based-cpd-dependent-id (aref (car p) pnum)) new-bindings)
                           (rule-based-cpd-dependent-id (aref (car q) qp)))
                     (setf (gethash (rule-based-cpd-dependent-id (aref (car q) qp)) new-q-first-bindings) (rule-based-cpd-dependent-id (aref (car p) pnum))))
                    (t
                     (when nil t
                           (format t "~%candidate match: ~A" qp))))
              (when nil t
                    (format t "~%current bindings: ~A~%current q-first-bindings: ~A" new-bindings new-q-first-bindings))
              (setq n2 (cons (cons pnum qp) n2-prime))
              (setq m2 (remove qp m2-prime))
              (setq kost (f n2 possible-candidates new-bindings new-q-first-bindings m1 p q cost-of-nil bic-p forbidden-types))
              ;; res needs to add the bindings
              (setq res (list n2 kost new-bindings new-q-first-bindings))
           collect res into pool
           finally
              (loop
                for assn in pool
                with min = (car pool)
                do
                   (cond ((< (second assn) (second min))
                          (setq min assn))
                         ((and (= (second assn) (second min))
                               (eq nil (cdar (first min))))
                          (setq min assn)))
                finally
                   (setq partial-sol min)))
         (when nil t
               (format t "~%partial solution: ~A~%cost so far: ~d" (first partial-sol) (second partial-sol))))
    (multiple-value-bind (sol weighted-cost bindings q-first-bindings cost)
        (if bic-p 
            (get-cost (make-array (if p (array-dimension (car p) 0) 0) :initial-contents (reverse (first partial-sol)) :fill-pointer t)
                      (if (third partial-sol) (third partial-sol) (make-hash-table))
                      (if (fourth partial-sol) (fourth partial-sol) (make-hash-table))
                      p q #|get-cost needs q-dif and q-m arguments|# p-nodes q-nodes cost-of-nil bic-p forbidden-types)
            (values (first partial-sol) (second partial-sol) (third partial-sol) (fourth partial-sol) (second partial-sol)))
      (loop
        for ele being the elements of sol
        collect (cons (aref (car p) (car ele))
                      (if (cdr ele)
                          (aref (car q) (cdr ele))
                          nil)) into solution
        finally
           (setq sol solution))
      (values sol (make-na-matches-for-unmatched-cpds p q sol  bindings q-first-bindings p-nodes) weighted-cost bindings q-first-bindings cost))))

#| For each unmatched cpd in base graph, add a corresponding CPD that takes on NA with probability 1 |#

;; p = pattern-graph
;; q = base graph
;; matches = array mapping from pattern to base
;; bindings = variable bindings for matches
;; q-first-bindings = bindings hash table where elements of q are the keys
;; p-nodes = nodes in p in list form
(defun make-na-matches-for-unmatched-cpds (p q matches bindings q-first-bindings p-nodes)
  (loop
    with matched-qs = (map 'list #'cdr matches)
    for i from 0 to (- (if q (array-dimension (car q) 0) 0) 1)
    when (not (member i matched-qs :test #'equalp))
      collect i into unmatched
    finally
       (return
         (loop
           for unmatched-q in unmatched
           with q-cpd
           with idents and vars and types and vvbm and nvvbm and sva and svna and vals
           with cards and steps and cids and qvars and assn and dummy-match and rules
           do
              (setq q-cpd (aref (car q) unmatched-q))
              (setq idents (make-hash-table :test #'equal))
              (setq vars (make-hash-table))
              (setq types (make-hash-table))
              (setq vvbm (make-hash-table))
              (setq nvvbm (make-hash-table))
              (setq sva (make-hash-table))
              (setq svna (make-hash-table))
              (setq vals (make-hash-table))
              (setq cids (make-hash-table))
              (setq qvars (make-hash-table))
              (loop
                with i = 1 and p-match and zeros = (list 0)
                for parent being the hash-keys of (rule-based-cpd-identifiers q-cpd)
                  using (hash-value idx)
                do
                   (cond ((= idx 0)
                          (setf (gethash (rule-based-cpd-dependent-id q-cpd) idents) idx)
                          (setf (gethash idx vars) (gethash idx (rule-based-cpd-vars q-cpd)))
                          (setf (gethash idx types) (gethash idx (rule-based-cpd-types q-cpd)))
                          (setf (gethash idx vvbm) (gethash idx (rule-based-cpd-var-value-block-map q-cpd)))
                          (setf (gethash idx nvvbm) (gethash idx (rule-based-cpd-negated-vvbms q-cpd)))
                          (setf (gethash idx sva) (gethash idx (rule-based-cpd-set-valued-attributes q-cpd)))
                          (setf (gethash idx svna) (gethash idx (rule-based-cpd-set-valued-negated-attributes q-cpd)))
                          (setf (gethash idx vals) (gethash idx (rule-based-cpd-var-values q-cpd)))
                          (setf (gethash idx cids) (gethash idx (rule-based-cpd-concept-ids q-cpd)))
                          (setf (gethash idx qvars) (gethash idx (rule-based-cpd-qualified-vars q-cpd))))
                         (t
                          (setq p-match nil)
                          (when (gethash parent q-first-bindings)
                            (setq p-match (aref (car p) (gethash (gethash parent q-first-bindings) p-nodes))))
                          (setf (gethash parent idents) i)
                          (setf (gethash i vars) (gethash idx (rule-based-cpd-vars q-cpd)))
                          (setf (gethash i types) (gethash idx (rule-based-cpd-types q-cpd)))
                          (setf (gethash i cids) (gethash idx (rule-based-cpd-concept-ids q-cpd)))
                          (setf (gethash i qvars) (gethash idx (rule-based-cpd-qualified-vars q-cpd)))
                          (cond (p-match
                                 (setf (gethash i vvbm) (gethash 0 (rule-based-cpd-var-value-block-map p-match)))
                                 (setf (gethash i nvvbm) (gethash 0 (rule-based-cpd-negated-vvbms p-match)))
                                 (setf (gethash i sva) (gethash 0 (rule-based-cpd-set-valued-attributes p-match)))
                                 (setf (gethash i svna) (gethash 0 (rule-based-cpd-set-valued-negated-attributes p-match)))
                                 (setf (gethash i vals) (gethash 0 (rule-based-cpd-var-values p-match))))
                                (t
                                 (setf (gethash i vvbm) (gethash idx (rule-based-cpd-var-value-block-map q-cpd)))
                                 (setf (gethash i nvvbm) (gethash idx (rule-based-cpd-negated-vvbms q-cpd)))
                                 (setf (gethash i sva) (gethash idx (rule-based-cpd-set-valued-attributes q-cpd)))
                                 (setf (gethash i svna) (gethash idx (rule-based-cpd-set-valued-negated-attributes q-cpd)))
                                 (setf (gethash i vals) (gethash idx (rule-based-cpd-var-values q-cpd)))
                                 (setq zeros (cons i zeros))))
                          (setq i (+ i 1))))
                finally
                   (setq assn (make-array (hash-table-count idents) :initial-element 1))
                   (loop
                     for zero in zeros
                     do
                        (setf (aref assn zero) 0))
                   ;; make hash table for assignment
                   ;;(setq cards (make-array (hash-table-count idents) :initial-element 2))
		   (setq cards (get-var-cardinalities vvbm))
		   (setq steps (generate-cpd-step-sizes cards)))
              (setq dummy-match
                    (make-rule-based-cpd
                     :dependent-id (rule-based-cpd-dependent-id q-cpd)
                     :identifiers idents
                     :dependent-var (rule-based-cpd-dependent-var q-cpd)
                     :vars vars
                     :types types
                     :concept-ids cids
                     :qualified-vars qvars
                     :var-value-block-map vvbm
                     :negated-vvbms nvvbm
                     :set-valued-attributes sva
                     :set-valued-negated-attributes svna
                     :lower-approx-var-value-block-map (copy-hash-table vvbm)
                     :lower-approx-negated-vvbms (copy-hash-table vvbm)
                     :characteristic-sets (make-hash-table)
                     :characteristic-sets-values (make-hash-table)
                     :var-values vals
                     :cardinalities cards
                     :step-sizes steps
                     :count 1
                     :lvl (rule-based-cpd-lvl q-cpd)))
              (setq rules (make-initial-rules dummy-match assn))
              (when nil (and (= cycle* 6) (equal "CLOSE_HAND2405" (rule-based-cpd-dependent-id dummy-match)))
                (format t "~%unmatched q-cpd:~%~S~%dummy match:~%~S~%assignment with probability 1:~%~S~%initial rules:~%~S" q-cpd dummy-match assn rules)
                (break))
              (setq dummy-match (update-cpd-rules dummy-match rules :check-uniqueness t))
           collect (cons (subst-cpd dummy-match q-cpd bindings) unmatched-q) into no-matches
           finally
              (return no-matches)))))

#| Add cost of not matching schema nodes to total cost |#

;; cost = cost of solution
;; p = pattern graph
;; q = base graph
;; bindings = variable bindings for matches
;; q-first-bindings = bindings hash table where elements of q are the keys
;; cost-of-nil = episode count for matching to nil
;; bic-p = whether to compute bic or likelihood
(defun compute-weighted-cost (cost p q bindings q-first-bindings cost-of-nil bic-p forbidden-types)
  (loop
    with rule = (make-rule :conditions (make-hash-table :test #'equal)) and compatible-rule
    for i from 0 to (- (if q (array-dimension (car q) 0) 0) 1)
    when (and (not (gethash (rule-based-cpd-dependent-id (aref (car q) i)) q-first-bindings))
              (not (member (gethash 0 (rule-based-cpd-types (aref (car q) i))) forbidden-types :test #'equal)))
      do
         (setf (rule-conditions rule) (make-hash-table :test #'equal))
         (loop
           for ident being the hash-keys of (rule-based-cpd-identifiers (aref (car q) i))
           do
              (setf (gethash ident (rule-conditions rule)) 0))
         (setq compatible-rule (car (get-compatible-rules (aref (car q) i) (aref (car q) i) rule :find-all nil)))
         (when (null compatible-rule)
           (format t "~%rule to match:~%~S~%cpd:~%~S" rule (aref (car q) i))
           (break))
      and summing (- 1 (/ (* (rule-based-cpd-lvl (aref (car q) i))
                             (rule-probability compatible-rule))
                          (rule-based-cpd-lvl (aref (car q) i))))  into add-cost
      and counting i into n
    finally
       (return (/ (+ cost (if (> n 0) (/ add-cost n) 0)) 2))))

#| Generate hash key for candidate solution |#

;; matches = current structure mapping cast as array 
(defun key-from-matches (matches)
  (let ((key-str ""))
    (map nil #'(lambda (match)
                 (setq key-str (concatenate 'string key-str (write-to-string (car match))))
                 (when (cdr match)
                   (setq key-str (concatenate 'string key-str (write-to-string (cdr match))))))
         matches)
    key-str))

#| Compute the cost of a solution |#

;; solution = structure mapping from pattern to base graph
;; bindings = array of variable bindings
;; q-first-bindings = bindings hash table where elements of q are the keys
;; p-nodes = nodes in p in list form
;; q-nodes = nodes in q in list form
;; q-dif = difference between number of free variables (nodes with no parents) in q that p doesn't have
;; q-m = number of summarized in q
;; cost-of-nil = episode count for matching to nil
;; bic-p = whether to compute bic or likelihood
(defun get-cost (solution bindings q-first-bindings p q q-dif q-m p-nodes q-nodes cost-of-nil bic-p forbidden-types &key (sol-cost-map))
  (let (cost weighted-cost key previous-cost)
    (setq key (key-from-matches solution))
    (when sol-cost-map
      (setq previous-cost (gethash key sol-cost-map)))
    (cond (previous-cost
           (values solution previous-cost bindings q-first-bindings cost))
          (t
           (setq cost (g (coerce solution 'list)
                         bindings
                         q-first-bindings
                         p q q-dif q-m
                         :cost-of-nil cost-of-nil
                         :bic-p bic-p
                         :forbidden-types forbidden-types))
	   (setq weighted-cost cost)
           (when sol-cost-map
             (setf (gethash key sol-cost-map) weighted-cost))
           (values solution weighted-cost bindings q-first-bindings cost)))))

#| Find largest common subgraph between two graphs |#

;; p = pattern graph
;; q = base graph
;; p-nodes = nodes in p in list form
;; p-nodes = nodes in q in list form
;; cost-of-nil = episode count for matching to nil
;; bic-p = whether to compute bic or likelihood
(defun greedy-assignment (p q &key (cost-of-nil 2) (bic-p t) (forbidden-types nil))
  (let* ((partial-sol (list nil 0 (make-hash-table) (make-hash-table)))
         (n2 nil)
         (possible-candidates nil))
    (when nil t
      (format t "~%~%p:~%~A~%q:~%~A" (car p) (car q)))
    (setq possible-candidates (get-possible-candidates p q))
    (loop
      with n2-prime
      with p-candidates and bindings = (make-hash-table :test #'equal) and q-first-bindings = (make-hash-table :test #'equal)
      for pnum from 0 to (- (if p (array-dimension (car p) 0) 0) 1)
      do
         ;;partial-sol = (list matches cost bindings q-first-bindings)
         (setq n2-prime (car partial-sol))
         (if (third partial-sol)
             (setq bindings (third partial-sol)))
         (if (fourth partial-sol)
             (setq q-first-bindings (fourth partial-sol)))
         (when nil (and (= cycle* 6))
           (format t "~%~%pnum: ~d~%p-identifiers: ~S" pnum (rule-based-cpd-identifiers (aref (car p) pnum))))
         (setq p-candidates (candidate-nodes pnum p q (gethash pnum possible-candidates) bindings q-first-bindings))
         (when nil (and (= cycle* 6))
           (format t "~%candidate matches:~%~S~%partial assignments: ~A~%partial bindings: ~A"  p-candidates n2-prime bindings)
           (break))
         (when nil t
               (format t "~%matching node ~d:~%~A~%qps: ~A" pnum (aref (car p) pnum) p-candidates))
         (loop
           for s in p-candidates
           with qp and res and best-res = (list nil most-positive-fixnum)
           with kost and new-bindings and new-q-first-bindings
           do
              (setq new-bindings (copy-hash-table bindings))
              (setq new-q-first-bindings (copy-hash-table q-first-bindings))
              (setq qp s)
              (cond (qp
                     (when nil (and (= cycle* 6))
                           (format t "~%   candidate match: ~A~%   ~A" qp (rule-based-cpd-identifiers (aref (car q) qp))))
                     (setf (gethash (rule-based-cpd-dependent-id (aref (car p) pnum)) new-bindings)
                           (rule-based-cpd-dependent-id (aref (car q) qp)))
                     (setf (gethash (rule-based-cpd-dependent-id (aref (car q) qp)) new-q-first-bindings)
                           (rule-based-cpd-dependent-id (aref (car p) pnum))))
                    (t
                     (when nil (and (= cycle* 6))
                           (format t "~%   candidate match: ~A" qp))))
              (when nil t
                    (format t "~%current bindings: ~A~%current q-first-bindings: ~A" new-bindings new-q-first-bindings))
              (setq n2 (cons (cons pnum qp) n2-prime))
              (setq kost (cost (aref (car p) pnum) (if qp (aref (car q) qp)) new-bindings new-q-first-bindings :cost-of-nil cost-of-nil :bic-p bic-p :forbidden-types forbidden-types))
              (when nil t
                    (format t "~%cost of match: ~d" kost))
              ;; res needs to add the bindings
              (setq res (list n2 (+ kost (second partial-sol)) new-bindings new-q-first-bindings))
           if (< (second res) (second best-res)) do
             (setq best-res res)
           else if (and (= (second res) (second best-res)) (eq nil (cdar (first best-res)))) do
             (setq best-res res)
           finally
              (setq partial-sol best-res))
         (when nil (and (= cycle* 6))
           (format t "~%   partial solution: ~A~%   cost so far: ~d" (first partial-sol) (second partial-sol))
           (break))
      finally
         (return (values (make-array (length (first partial-sol)) :initial-contents (reverse (first partial-sol)))
			 (g (first partial-sol)
			    (third partial-sol)
			    (fourth partial-sol)
			    p q
			    :cost-of-nil cost-of-nil
			    :bic-p bic-p
			    :forbidden-types forbidden-types)
			 #|(compute-weighted-cost (if p (/ (second partial-sol) (array-dimension (car p) 0)) (second partial-sol)) p q (third partial-sol) (fourth partial-sol) cost-of-nil bic-p forbidden-types)|#
                         (third partial-sol) (fourth partial-sol)
                         possible-candidates)))))

;; node = node in pattern with parents to check
;; visited-hash = hash table of nodes we have visted
;; p-nodes = hash table of p node identifiers and their associated indeces
(defun all-parents-visited? (node visited-hash p p-nodes)
  (let (cpd)
    (setq cpd (aref (car p) node))
    (loop
      for ident being the hash-keys of (rule-based-cpd-identifiers cpd)
      using (hash-value pos)
      when (> pos 0) do
        (if (not (gethash (gethash ident p-nodes) visited-hash))
            (return-from all-parents-visited? nil))))
  t)

#| Randomly swap two assignments in matches |#

;; pnum = index of cpd in pattern graph
;; q-match-parent = index of q-match parent in base graph
;; matches = array mapping from pattern to base
;; bindings = array bindings from pattern to base
;; q-first-bindings = bindings hash table where elements of q are the keys
;; possible-candidates = list of potential candidate nodes in base graph
;; p = pattern graph
;; q = base graph
;; p-nodes = nodes in p in list form
;; p-nodes = nodes in q in list form
(defun random-swap (top-lvl-nodes matches bindings q-first-bindings possible-candidates p q p-nodes q-nodes)
  (loop
    with neighbors and match and q-match and new-qnum and pnum-prime and match-prime and candidates
    with pnum and queue = (alexandria:shuffle (copy-list top-lvl-nodes)) and visited-hash = (make-hash-table)
    while (and p queue) do
      (setq pnum (car queue))
      (setq queue (rest queue))
      (setq new-qnum nil)
      (when (not (gethash pnum visited-hash))
        (setq match (aref matches pnum))
        (setq q-match (cdr match))
        (setq pnum-prime nil)
        (when nil (and (= cycle* 4) (or (= pnum 27)
				    (= pnum 17)
				    (= pnum 22))) nil t
          (format t "~%~%new iteration~%pnum: ~d~%~A~%qp: ~d~%~A~%"
                  pnum
                  (aref (car p) pnum)
                  q-match
                  (if q-match (aref (car q) q-match) nil)))
        ;; remove pnum binding from bindings and matches
        (remhash (rule-based-cpd-dependent-id (aref (car p) (car match))) bindings)
        (when q-match
          ;;(setq q-first-bindings (fset:less q-first-bindings (cpd-dependent-id (aref (car q) q-match))))
          (remhash (rule-based-cpd-dependent-id (aref (car q) q-match)) q-first-bindings))
        (when nil (and (= cycle* 4) (or (= pnum 27)
				    (= pnum 17)
				    (= pnum 22))) nil t
          (format t "~%reduced bindings: ~A~%reduced q-first-bindings: ~A" bindings q-first-bindings))
        ;;(setq candidates (analog-nodes pnum p q (gethash pnum possible-candidates) bindings q-first-bindings))
	(setq candidates (candidate-nodes pnum p q (gethash pnum possible-candidates) bindings q-first-bindings t))
	(setq candidates (make-array (length candidates) :initial-contents candidates))
        (when nil (and (= cycle* 4) (or (= pnum 27)
				    (= pnum 17)
				    (= pnum 22))) nil t
          (format t "~%pnum: ~d~%candidates: ~A" pnum candidates))
        ;; swap pnode with random candidate
        (setq new-qnum (aref candidates (random (array-dimension candidates 0))))
        (when nil (and (= cycle* 4) (or (= pnum 27)
				    (= pnum 17)
				    (= pnum 22))) nil t
          (format t "~%matching ~A~%~A~%with ~A~%~A~%current matches:~%~A~%bindings:~%~A" (car match) (aref (car p) (car match)) new-qnum (if new-qnum (aref (car q) new-qnum) nil) matches bindings))
        (setq match (cons (car match) new-qnum))
        (when new-qnum
          (setq pnum-prime (gethash (gethash (rule-based-cpd-dependent-id (aref (car q) new-qnum)) q-first-bindings) p-nodes))
          (when pnum-prime
            (remhash (rule-based-cpd-dependent-id (aref (car p) pnum-prime)) bindings))
          ;; change bindings
          (setf (gethash (rule-based-cpd-dependent-id (aref (car p) (car match))) bindings) (rule-based-cpd-dependent-id (aref (car q) new-qnum)))
          (setf (gethash (rule-based-cpd-dependent-id (aref (car q) new-qnum)) q-first-bindings) (rule-based-cpd-dependent-id (aref (car p) (car match)))))
        (setf (aref matches pnum) match)
        (when pnum-prime
          (when nil (and (= cycle* 4) (or (= pnum 27)
				    (= pnum 17)
				    (= pnum 22))) nil t
            (format t "~%matching ~A~%~A~%with ~A~%~A~%current matches:~%~A~%bindings:~%~A" pnum-prime (aref (car p) pnum-prime) q-match (if q-match (aref (car q) q-match) nil) matches bindings))
          (setq match-prime (cons pnum-prime q-match))
          (setf (aref matches pnum-prime) match-prime)
	  (setf (gethash pnum-prime visited-hash) t)
	  (when q-match
            (setf (gethash (rule-based-cpd-dependent-id (aref (car p) pnum-prime)) bindings) (rule-based-cpd-dependent-id (aref (car q) q-match)))
            (setf (gethash (rule-based-cpd-dependent-id (aref (car q) q-match)) q-first-bindings) (rule-based-cpd-dependent-id (aref (car p) pnum-prime)))))
        (setf (gethash pnum visited-hash) t))
      (multiple-value-bind (val bool)
          (gethash pnum (cdr p))
        (if bool
            (setq neighbors val)
            (setq neighbors (make-hash-table))))
      (loop
        for nbr being the hash-keys of neighbors
        when (higher-lvl-cpd (aref (car p) pnum) (aref (car p) nbr))
          do
             (when (and (not (gethash nbr visited-hash))
                        (all-parents-visited? nbr visited-hash p p-nodes))
               (setq queue (reverse (cons nbr (reverse queue))))))
    finally
       (return (values matches bindings q-first-bindings))))

#| Randomly select to make uphill moves according to probability |#

;; default = best current move
;; alternate = known uphill move
;; probability = probability of making uphill move
(defun prob-select (default alternate probability)
  (let (number sample)
    (when (null probability)
      (setq probability 0.0))
    (setq number (* probability 100))
    (setq sample (random 100.0))
    (when nil (and (= cycle* 6))
      (format t "~%probability of uphill move: ~d~%sample: ~d" number sample))
    (cond ((<= sample number)
           (when nil (and (= cycle* 6))
             (format t "~%taking uphill move...")
             ;;(break)
             )
           alternate)
          (t
           (when nil t
             (format t "~%keeping old value."))
           default))))

#| Randomly choose neighboring state |#

;; matches = current structure mapping cast as array
;; bindings = bindings for structure mapping
;; q-first-bindings = bindings hash table where elements of q are the keys
;; p = pattern graph
;; q = base graph
;; p-nodes = nodes in p in list form
;; p-nodes = nodes in q in list form
(defun random-neighbor (matches bindings q-first-bindings p q p-nodes q-nodes &aux pnum)
  (setq pnum (random (array-dimension (car p) 0)))
  (when nil
    (format t "~%swapping node ~d~%~A" pnum (aref (car p) pnum)))
  (random-swap pnum matches bindings q-first-bindings p q p-nodes q-nodes))

#| Randomly choose neighboring state |#

;; matches = current structure mapping cast as array
;; bindings = bindings for structure mapping
;; q-first-bindings = bindings hash table where elements of q are the keys
;; possible-candidates = list of potential candidate nodes in base graph
;; p = pattern graph
;; q = base graph
;; p-nodes = nodes in p in list form
;; p-nodes = nodes in q in list form
;; pnum = index to swap
(defun linear-neighbor (matches bindings q-first-bindings possible-candidates p q p-nodes q-nodes top-lvl-nodes)
  (random-swap top-lvl-nodes matches bindings q-first-bindings possible-candidates p q p-nodes q-nodes))

#| Determine how fast we bring the system to a low energy state |#

;; time = time of cooling schedule
;; big-t = initial temperature of the system
(defun cooling-schedule (time big-t &optional (alpha .80))
  (rationalize (* big-t (expt alpha time))))

#| Determines if new match is current best |#

;; next = new match
;; best-solution = current best solution so far
(defun better-random-match? (next best-solution)
  (cond ((< (second next) (second best-solution))
         t)
        ((and (= (second next) (second best-solution))
              (> (hash-table-count (third next)) (hash-table-count (third best-solution))))
         t)))
#| Initialize empty set of mappings |#

;; p = pattern graph
(defun make-nil-mappings (p)
  (make-array (array-dimension (car p) 0)
	      :initial-contents (loop
				  for i from 0
				  for cpd being the elements of (car p)
				  collect (cons i nil) into res
				  finally
				     (return res))))
#| Optimize structure mapping between two graphs |#

;; p = pattern graph
;; q = base graph
;; current = best current match
;; possible-candidates = list of potential candidate nodes in base graph
;; sol-cost-map = map of previous solutions
;; start-temp = initial temperature
;; end-temp = stopping temperature
;; alpha = discount for cooling schedule
;; p-nodes = list of nodes in p
;; q-nodes = list of nodes in q
;; q-dif = difference between number of free variables (nodes with no parents) in q that p doesn't have
;; q-m = number of summarized in q
;; cost-of-nil = episode count for matching to nil
;; bic-p = whether to compute bic or likelihood
(defun simulated-annealing (p q current possible-candidates sol-cost-map start-temp end-temp alpha top-lvl-nodes p-nodes q-nodes q-dif q-m cost-of-nil bic-p forbidden-types)
  (loop
    named looper
    with big-t = start-temp and smallest-float = end-temp and next and delta-e and delta-m
    with best-solution = current
    for time from 0 do
      (setq big-t (cooling-schedule time start-temp alpha))
    when (<= big-t smallest-float) do
      ;;(break)
      (when nil (and (= cycle* 4))
        (format t "~%time: ~d ~%total cycles: ~d~%stop temperature: ~d~%bindings:~%~S~%cost: ~d" time (/ time (length top-lvl-nodes)) big-t (third best-solution) (second best-solution))
        (break)
	)
      (return-from looper (values (first best-solution) (second best-solution) (third best-solution) (fourth best-solution) (fifth best-solution)))
    else do
      (when nil (and (= cycle* 4))
        (format t "~%~%iteration: ~d~%temperature: ~d~%current mapping:~%~A~%current bindings: ~A~%current q-first-bindings: ~A~%cost of current solution: ~d" time big-t (first current) (third current) (fourth current) (second current)))
      (multiple-value-bind (solution bindings q-first-bindings)
	  (linear-neighbor (make-nil-mappings p) (make-hash-table :test #'equal) (make-hash-table :test #'equal)  possible-candidates p q p-nodes q-nodes top-lvl-nodes)
	  ;;(linear-neighbor (copy-array (first current)) (copy-hash-table (third current)) (copy-hash-table (fourth current)) possible-candidates p q p-nodes q-nodes top-lvl-nodes)
        (multiple-value-bind (new-matches new-weighted-cost new-bindings new-q-first-bindings new-cost)
            (get-cost solution bindings q-first-bindings p q q-dif q-m p-nodes q-nodes cost-of-nil bic-p forbidden-types :sol-cost-map sol-cost-map)
          (setq next (list new-matches new-weighted-cost new-bindings new-q-first-bindings new-cost)))
        (setq delta-e (- (second next) (second current)))
        (when nil (and (= cycle* 4))
          (format t "~%new mapping:~%~A~%new bindings: ~A~%new q-first-bindings: ~A~%cost of new solution: ~d~%delta cost: ~d" (first next) (third next) (fourth next) (second next) delta-e)
	  ;;(break)
	  #|
	  (loop
            with counter = 0
            for bind being the hash-keys of bindings
            when (search "BLOCK" bind)
              do
                 (setq counter (+ counter 1))
                 (when (>= counter 1)
                   (format t "~%cost of nil: ~d" cost-of-nil)
                   (format t "~%bound blocks: ~d" counter)
                   (format t "~%better random match: ~S" (better-random-match? next best-solution))
                   (break)
                   ))
	  |#)
        (when (better-random-match? next best-solution)
          (setq best-solution (list (copy-array (first next)) (second next) (copy-hash-table (third next)) (copy-hash-table (fourth next)) (fifth next)))
          (when nil (and (= cycle* 4))
            (format t "~%new mapping is new best solution!~%~%Best score: ~d~%Found at iteration: ~d~%temperature: ~d" (second best-solution) time big-t)))
        (cond ((< delta-e 0)
               (when nil (and (= cycle* 4))
                 (format t "~%found better match!"))
               (setq current best-solution))
              (nil t
               (setq current (prob-select current next (ignore-errors (exp (/ (- delta-e) big-t))))))))))

#| Get the expected number of trials needed to observe all outcomes of the variable |#

;; values = domain for variable assignment
(defun get-expected-iterations (values)
  (loop
     for observed from 0 to (- (array-dimension values 0) 1)
     sum (/ 1 (/ (- (array-dimension values 0) observed) (array-dimension values 0))) into iterations
     finally
       (return (ceiling iterations))))

#| Predicate function for determining if a list only contains one item |#

;; lst = list
(defun single-p (lst)
  (and (consp lst) (not (cdr lst))))

#| Find largest common subgraph between two graphs |#

;; p = pattern graph
;; q = base graph
;; cost-of-nil = episode count for matching to nil
;; bic-p = flag to compute BIC
(defun maximum-common-subgraph (p q &key (cost-of-nil 2) (bic-p t) (forbidden-types nil)  &aux p-nodes q-nodes top-lvl-nodes (required-swaps 1))
  (when nil (and (= cycle* 4))
	(format t "~%~%p:~%~S~%|p|: ~d~%q:~%~S~%|q|: ~d" (map 'list #'rule-based-cpd-identifiers (car p))
		(array-dimension (car p) 0)
		(map 'list #'rule-based-cpd-identifiers (car q))
		(array-dimension (car q) 0))
	(break)
	)
  (let (matches cost bindings q-first-bindings possible-candidates current temperature stop-temp alpha almost-zero sol-cost-map key no-matches p-dim p-m q-dim q-m q-dif)
    (setq sol-cost-map (make-hash-table :test #'equal))
    (setq matches (make-nil-mappings p))
    (setq cost most-positive-fixnum)
    (setq bindings (make-hash-table :test #'equal))
    (setq q-first-bindings (make-hash-table :test #'equal))
    (setq possible-candidates (get-possible-candidates p q))
    (setq p-dim 0)
    (setq p-m 0)
    (setq q-dim 0)
    (setq q-m 0)
    (setq q-dif 0)
    (when nil (and (= cycle* 21))
      (format t "~%~%possible candidates:~%~S" possible-candidates)
      )
    (setq key (key-from-matches matches))
    (when (null (gethash key sol-cost-map))
      (setf (gethash key sol-cost-map) cost))
    (setq current (list matches cost bindings q-first-bindings most-positive-fixnum))
    (setq stop-temp (expt 10 (- 1)))
    (setq almost-zero 1.e-39)
    (setq alpha .999)
    (setq p-nodes (make-hash-table :test #'equal))
    (setq q-nodes (make-hash-table :test #'equal))
    (loop
       with i-options and swaps-hash = (make-hash-table :test #'equal)
      for i from 0 to (- (if p (array-dimension (car p) 0) 0) 1)
      do
         (setf (gethash (rule-based-cpd-dependent-id (aref (car p) i)) p-nodes) i)
	 (setq p-m (max p-m (rule-based-cpd-count (aref (car p) i))))
      if (= (hash-table-count (rule-based-cpd-identifiers (aref (car p) i))) 1)
       do
	 (setq p-dim (+ p-dim 1))
         (setq i-options (analog-nodes i p q (gethash i possible-candidates) (make-hash-table) (make-hash-table)))
         (when (> (array-dimension i-options 0) 1)
           (setf (gethash (rule-based-cpd-dependent-var (aref (car p) i)) swaps-hash)
                 (cons (get-expected-iterations i-options) (gethash (rule-based-cpd-dependent-var (aref (car p) i)) swaps-hash))))
         (setq top-lvl-nodes (nreverse (cons i (nreverse top-lvl-nodes))))
       finally
       (setq required-swaps (* (reduce #'+ (loop for swaps being the hash-values of swaps-hash collect (reduce #'* swaps))) 2)))
    (loop
       for i from 0 to (- (if q (array-dimension (car q) 0) 0) 1)
       do
	 (setq q-m (max q-m (rule-based-cpd-count (aref (car q) i))))
	 (setf (gethash (rule-based-cpd-dependent-id (aref (car q) i)) q-nodes) i)
	 (when (= (hash-table-count (rule-based-cpd-identifiers (aref (car q) i))) 1)
	   (setq q-dim (+ q-dim 1))))
    (when (> q-dim p-dim)
      (setq q-dim (- q-dim p-dim)))
    (setq temperature (handler-case (/ stop-temp (expt alpha required-swaps))
                        (error (c)
                          ;;(break "Going to set almost zero")
                          (/ stop-temp almost-zero))))
    (when nil (and (= cycle* 2))
          (format t "~%~%initial temperature: ~d~%alpha: ~d~%num top-lvl-nodes:~%~A~%expected number of cycles: ~d" temperature alpha (length top-lvl-nodes) required-swaps)
          ;;(break)
	  )
    (multiple-value-bind (matches weighted-cost bindings q-first-bindings cost)
        (simulated-annealing p q current possible-candidates sol-cost-map temperature stop-temp alpha top-lvl-nodes p-nodes q-nodes q-dif q-m cost-of-nil bic-p forbidden-types)
      (setq no-matches (make-na-matches-for-unmatched-cpds p q matches bindings q-first-bindings p-nodes))
      (setq current (list matches no-matches weighted-cost bindings q-first-bindings cost)))
    (values (first current) (second current) (third current) (fourth current) (fifth current))))

#| TESTS
1) Structure mapping tests
(load "newicarus")
(initialize-world)
(let (p q)
  (run 1)
  (setq p (car (episode-states (car eltm*))))
  (setq eltm* nil)
  (run 1)
  (eltm-to-pdf)
  (setq q (car (episode-states (car eltm*))))
  (setq eltm* nil)
  (greedy-assignment p q))

(let (rule1 rule2)
  (setq rule1 #S(RULE
                 :ID "RULE-15575"
                 :CONDITIONS #H(("HAND_OPEN570" . 1) ("HAND565" . 0) ("HAND_CLOSED6124" . 0) ("LEFT_OF584" . 1) ("LEFT_OF582" . 1) ("ON574" . 1) ("HANDS_FREE592" . 1) ("LEFT_OF580" . 1))
                 :PROBABILITY 5/8
                 :BLOCK (25)
                 :CERTAIN-BLOCK (25)
                 :AVOID-LIST NIL
                 :REDUNDANCIES NIL
                 :COUNT 8))
  (setq rule2 #S(RULE
                 :ID "RULE-15566"
                 :CONDITIONS #H(("HAND565" . 0) ("HAND_CLOSED6124" . 1) ("GRIPPER586" . 0) ("HANDS_FREE592" . 1) ("ON574" . 1) ("LEFT_OF580" . 1) ("LEFT_OF582" . 1))
                 :PROBABILITY 1
                 :BLOCK (14 13 4)
                 :CERTAIN-BLOCK (14)
                 :AVOID-LIST NIL
                 :REDUNDANCIES NIL
                 :COUNT 8))
  (compatible-rule-p rule1 rule2 nil nil))

(load "newicarus")(initialize-world)(require :sb-sprof)(sb-sprof:with-profiling (:max-samples 100000
:mode :cpu
:report :flat)
(grun))

(load "newicarus")(initialize-world)(grun)(remember eltm* (list (list (make-array 0))) '+ 1 t)

(let (percepts cue)
(load "newicarus")
(initialize-world)
(grun)
(setq percepts (list '(hand h1 y 8)))
(multiple-value-bind (factors edges)
(state-to-graph percepts nil)
(setq cue (cons factors edges))
(remember eltm* (list cue) '+ 1 t)))

(load "newicarus")(initialize-world)(grun)(require :sb-sprof)(sb-sprof:with-profiling (
:mode :cpu
:report :flat)
(remember eltm* (list (list (make-array 0))) '+ 1 t))

(equal #(1 2 2 2 2 2 2 2 2 2) #(1 1 2 2 2 2 2 2 2 2))
|#
