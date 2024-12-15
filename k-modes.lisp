(defparameter *data* '((A B C)
                       (B C A)
                       (C A B)
                       (A C B)
                       (A A B)))

(defparameter *k* 2)

(defparameter *modes* '((A B C)
                        (C B A)))

(defun hamming-distance (object mode)
  "Calculate the Hamming distance between a data object and a mode."
  (reduce #'+
          (mapcar (lambda (x y) (if (equal x y) 0 1))
                  object mode)))

(defun assign-clusters (data modes)
  "Assign each object in data to the closest cluster based on Hamming distance."
  (mapcar (lambda (object)
            (let ((distances (mapcar (lambda (mode)
                                       (hamming-distance object mode))
                                     modes)))
              (position (reduce #'min distances) distances)))
          data))

(defun mode-update (data clusters k)
  "Update the modes for each cluster."
  (mapcar (lambda (cluster-index)
            (let ((cluster-data (remove-if-not (lambda (index)
                                                 (= cluster-index (nth index clusters)))
                                               (loop for i from 0 below (length data) collect i))))
              (mapcar (lambda (col)
                        (let ((column-values (mapcar (lambda (row) (nth col row))
                                                     cluster-data)))
                          (car (reduce (lambda (x y)
                                         (if (> (cdr x) (cdr y)) x y))
                                       (mapcar (lambda (val)
                                                 (cons val (count val column-values)))
                                               column-values)))))
                      (loop for col from 0 below (length (first data)) collect col))))
          (loop for i from 0 below k collect i)))

(defun cluster-data (data k modes iterations)
  "Perform clustering on data with k clusters and initial modes."
  (let ((clusters nil)
        (clusters-prev nil))
    (loop for i from 1 to iterations
          do (setf clusters (assign-clusters data modes))
          (when (equal clusters clusters-prev)
            (return))
          (setf clusters-prev clusters)
          (setf modes (mode-update data clusters k)))
    (values clusters modes)))

(let ((result (cluster-data *data* *k* *modes* 10)))
  (format t "Cluster assignments: ~a~%" (first result))
  (format t "Modes: ~a~%" (second result)))
