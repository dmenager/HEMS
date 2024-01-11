(in-package :hems)
#| Write a message to the log file |#

;; message = list of format arguments
(defun log-message (message &optional (log-path log-path*) &key (if-exists :append))
  (when (not (equal log-path* log-path))
    (setq log-path (merge-pathnames log-path)))
  (with-open-file (log log-path
		       :direction :output
		       :if-exists if-exists
		       :if-does-not-exist :create)
    (apply #'format (cons log message))))

#| Turn on logging flags |#

(defun enable-logging (&key name log-insert log-generalize log-exp-equal log-explain log-rules log-intentions log-rules-final efficiency-exp)
  (when name
    (setq log-path* (merge-pathnames name)))
  (delete-log)
  (cond (log-insert
         (setq log-insert* t))
        (log-generalize
         (setq log-generalize* t))
        (log-exp-equal
         (setq log-exp-equal* t))
        (log-explain
         (setq log-explain* t))
        (log-rules
         (setq log-rules* t))
        (log-intentions
         (setq log-intentions* t))
        (log-rules-final
         (setq log-rules-final* t))
        (efficiency-exp
         (setq efficiency-exp* t))))

(defun disable-logging ()
  (setq log-insert* nil)
  (setq log-generalize* nil)
  (setq log-exp-equal* nil)
  (setq log-explain* nil))

(defun get-log-variables ()
  (format t "Log Insert: ~S~%Log Generalize: ~S~%Log Explanations: ~A~%Log Explanation Equality: ~A~%"
	  log-insert* log-generalize* log-explain* log-exp-equal*))

#| Delete the log file |#

(defun delete-log ()
  (sb-ext:run-program "rm" (list (namestring log-path*)) :search t :wait t))
