(defsystem "hems"
  :description "An implementation of the Hybrid Event Memory System."
  :version "0.0.1"
  :author "David H. Menager <dhmenager@gmail.com>"
  :license "MIT"
  ;;:depends-on ("alexandria" "split-sequence" "uiop" "cl-ppcre" "lhstats" "lisp-stat" "sqldf")
  :depends-on ("alexandria" "split-sequence" "uiop" "cl-ppcre")
  :components((:file "package")
	      (:file "ep-log" :depends-on ("package"))
	      ;;(:file "df-utils" :depends-on ("package"))
	      (:file "graph-representation" :depends-on ("package"))
	      ;;(:file "stat-independence" :depends-on ("df-utils"))
	      ;;(:file "k-means" :depends-on ("df-utils"))
	      ;;(:file "episodic" :depends-on ("graph-representation"))
	      (:file "episodic" :depends-on ("hems-program-compiler"))
              ;;(:file "causal-discovery" :depends-on ("fast-causal-inference"))	      
        ;;(:file "fast-causal-inference" :depends-on ("graph-representation"))
	      (:file "performance-stats" :depends-on ("graph-representation"))
	      (:file "metrics" :depends-on ("graph-representation"))
	      (:file "sampler" :depends-on ("graph-representation"))
	      (:file "hems-program-compiler" :depends-on ("graph-representation" "performance-stats"))
	      (:file "segmentation" :depends-on ("episodic"))
	      (:file "serializer" :depends-on ("episodic"))
	      ))
