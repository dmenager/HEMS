(ql:quickload :hems)
(in-package :hems)

(let (observations)
    (setq observations
(list (compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (hems:push-to-ep-buffer :state bn :insertp t))
 observations))


;; ----------------------------

;; CPDs
;; P(zeus_angry=1) = 0.2333
;; P(rain=1 | zeus_angry=0) = 0.3522
;; P(rain=1 | zeus_angry=1) = 0.8857
;; P(sprinkler=1 | rain=0) = 0.6943
;; P(sprinkler=1 | rain=1) = 0.2098
;; P(wet=1 | rain=0, sprinkler=0) = 0.0417
;; P(wet=1 | rain=0, sprinkler=1) = 0.7615
;; P(wet=1 | rain=1, sprinkler=0) = 0.8142
;; P(wet=1 | rain=1, sprinkler=1) = 0.9667
