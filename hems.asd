(defsystem "hems"
    :description "An implementation of the Hybrid Event Memory System."
    :version "0.0.1"
    :author "David H. Menager <dhmenager@gmail.com>"
    :license "MIT"
    :depends-on ("alexandria")
    :components((:file "package")
		(:file "ep-log" :depends-on ("package"))
		(:file "episodic" :depends-on ("graph-representation"))
		(:file "graph-representation" :depends-on ("package"))))
