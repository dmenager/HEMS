(defpackage :hems
  (:use #:cl)
  (:import-from #:alexandria
		#:shuffle)
  (:export #:episode-buffer* #:eltm* #:push-to-ep-buffer #:eltm-to-pdf #:test-fun))

(in-package :hems)
