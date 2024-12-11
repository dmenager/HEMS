(in-package :hems)

#| Access row in data frame |#

;; df = string name of lisp-stat df
;; n = row index to return
(defun get-row (df n)
  (sqldf:sqldf (format nil "select * from ~A limit 1 offset ~d" df n)))

#| Retrieve a slice of rows from a data frame |#

;; df = lisp-stat df
;; from = index of first row
;; to = index of last row
(defun get-rows (df from to)
  (sqldf:sqldf (format nil "select * from ~A limit ~d offset ~d" df (- to from) from)))

#| Remove white spaces and replace hyphens with underscores |#

;; df = lisp-stat df
(defun n-format-df-column-names (df)
  (loop
    with new-col
    for col being the elements of (ls-user:keys df)
    do
       (setq new-col (intern (cl-ppcre:regex-replace-all "-" (symbol-name col) "_")))
       (ls-user:rename-column! df new-col col)))
