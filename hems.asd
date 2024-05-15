(defsystem "hems"
  :description "An implementation of the Hybrid Event Memory System."
  :version "0.0.1"
  :author "David H. Menager <dhmenager@gmail.com>"
  :license "MIT"
  :depends-on ("alexandria" "split-sequence")
  :components((:file "package")
	      (:file "ep-log" :depends-on ("package"))
	      (:file "episodic" :depends-on ("graph-representation"))
	      (:file "hems-program-compiler" :depends-on ("graph-representation" "performance-stats"))
	      (:file "performance-stats" :depends-on ("graph-representation"))
	      (:file "metrics" :depends-on ("graph-representation"))
	      (:file "graph-representation" :depends-on ("package"))
	      (:file "segmentation" :depends-on ("episodic"))
	      (:file "sampler" :depends-on ("graph-representation"))
	      (:file "serializer" :depends-on ("episodic"))))
