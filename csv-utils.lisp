(in-package :hems)

#| Open and read a csv. Returns a teddy dataframe.
   Teddy repo: https://github.com/40ants/teddy/tree/master
   Teddy doc: https://40ants.com/teddy/ |#

;; csv = path to csv file to open. Assumes first row is the colum header, while subsequent rows contain the data
(defun read-csv (csv)
  (let (data features)
    (uiop:with-safe-io-syntax ()
	(setq data (uiop:read-file-lines csv)))
    (setq features (cons "IDX" (mapcar #'(lambda (feat)
					   (cl-ppcre:regex-replace-all " " (string-upcase feat) "_"))
				       (split-sequence:split-sequence #\, (car data)))))
    (setq data (rest data))
    (teddy/data-frame::make-data-frame
     features
     :rows
     (loop
       for i from 0
       for row in data
       collect (mapcar #'(lambda (ele)
			   (let ((val (read-from-string ele)))
			     (cond ((or (integerp val)
					(floatp val))
				    val)
				   (t
				    ele)))) 
			  (cons (write-to-string i) (split-sequence:split-sequence #\, row)))))))

#| tests 
(ql:quickload :teddy)
(ql:quickload :split-sequence)

(let (df)
  (setq df (read-csv "/home/david/Code/HARLEM/ep_data_1/ppo_CliffWalking-v0_data.csv"))
  (loop
    with it = (teddy/data-frame::make-iterator df)
    for row = (funcall it)
    while row
    do 
       (format t "Row: ~S~%" row))
(teddy/data-frame::get-column "Action"))
|#
