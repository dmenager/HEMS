(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;; CI starts from a fresh checkout, so ASDF may recompile the full system before
;; these regression cases run. Existing non-fatal compile warnings should not
;; prevent the runtime regression suite from executing.
(setf asdf:*compile-file-warnings-behaviour* :warn)
(setf asdf:*compile-file-failure-behaviour* :warn)

(ql:quickload :hems)

(defpackage :hems-rule-compression-regression
  (:use :cl))

(in-package :hems-rule-compression-regression)

(defparameter *example-dir*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defun example-path (name)
  (merge-pathnames name *example-dir*))

(defun slurp-file-contents (path)
  (with-open-file (stream path :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun commented-test-block (path)
  (let* ((contents (slurp-file-contents path))
         (start (search "#| TESTS" contents))
         (end (and start (search "|#" contents :start2 start))))
    (unless (and start end)
      (error "Could not find #| TESTS ... |# block in ~A" path))
    (subseq contents (+ start (length "#| TESTS")) end)))

(defun read-forms-from-string (text &key (package :hems))
  (let ((*package* (find-package package)))
    (with-input-from-string (stream text)
      (loop
        for form = (read stream nil :eof)
        until (eq form :eof)
        collect form))))

(defun read-forms-from-file (path &key (package :hems))
  (read-forms-from-string (slurp-file-contents path) :package package))

(defun elapsed-seconds (start)
  (/ (- (get-internal-real-time) start)
     internal-time-units-per-second))

(defun run-regression (name thunk)
  (format t "~&~A ... " name)
  (finish-output)
  (let ((start (get-internal-real-time))
        (out *standard-output*))
    (handler-case
        (progn
          (let ((*standard-output* (make-broadcast-stream))
                (*trace-output* (make-broadcast-stream)))
            (funcall thunk))
          (format out "PASS (~,2Fs)~%" (elapsed-seconds start)))
      (error (condition)
        (format out "FAIL (~,2Fs)~%" (elapsed-seconds start))
        (format *error-output* "~&~A failed:~%~A~%" name condition)
        (uiop:quit 1)))))

(defun run-test-net2-ex1 ()
  (load (example-path "test-net2.lisp"))
  (funcall (intern "EX1" :hems)))

(defun run-test-net2-ex2 ()
  (load (example-path "test-net2.lisp"))
  (funcall (intern "EX2" :hems)))

(defun run-load-model-commented-tests ()
  (load (example-path "load-model-test.lisp"))
  (loop
    for form in (read-forms-from-string
                 (commented-test-block (example-path "load-model-test.lisp")))
    for i from 1
    do
       (run-regression (format nil "load-model-test commented example ~D" i)
         (lambda () (eval form)))))

(defun run-input-example ()
  (loop
    for form in (read-forms-from-file (example-path "input.txt"))
    for i from 1
    do
       (run-regression (format nil "input.txt example ~D" i)
         (lambda () (eval form)))))

(defun run-all-regressions ()
  (run-regression "test-net2.lisp ex1" #'run-test-net2-ex1)
  (run-regression "test-net2.lisp ex2" #'run-test-net2-ex2)
  (run-load-model-commented-tests)
  (run-input-example)
  (format t "~&All rule compression regression examples passed.~%"))

(run-all-regressions)
(uiop:quit 0)
