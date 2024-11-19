(defsystem "hems"
  :description "An implementation of the Hybrid Event Memory System."
  :version "0.0.1"
  :author "David H. Menager <dhmenager@gmail.com>"
  :license "MIT"
  :depends-on ("alexandria" "split-sequence" "teddy" "uiop")
  :components((:file "package")
	      (:file "ep-log" :depends-on ("package"))
	      (:file "csv-utils" :depends-on ("package"))
	      (:file "graph-representation" :depends-on ("package"))
	      (:file "stat-independence" :depends-on ("csv-utils"))
	      (:file "episodic" :depends-on ("graph-representation"))
	      (:file "fast-causal-inference" :depends-on ("graph-representation"))
	      (:file "performance-stats" :depends-on ("graph-representation"))
	      (:file "metrics" :depends-on ("graph-representation"))
	      (:file "sampler" :depends-on ("graph-representation"))
	      (:file "hems-program-compiler" :depends-on ("graph-representation" "performance-stats"))
	      (:file "segmentation" :depends-on ("episodic"))
	      (:file "serializer" :depends-on ("episodic"))
	      
	      ;;(:file "fast-greedy-equivalence-search" :depends-on ("episodic"))
	      ;;(:file "fges-utils" :depends-on ("fast-greedy-equivalence-search"))
	      ))
