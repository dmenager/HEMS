(ql:quickload :hems)
(in-package :hems)

(defun example ()
(let (observations)
    (setq observations
(list (compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
  c3 -> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil :bic-p nil))
 observations)))


;; ----------------------------

;; CPDs
;; P(zeus_angry=1) = 0.2233
;; P(rain=1 | zeus_angry=0) = 0.3090
;; P(rain=1 | zeus_angry=1) = 0.9552
;; P(sprinkler=1 | rain=0) = 0.6707
;; P(sprinkler=1 | rain=1) = 0.1912
;; P(wet=1 | rain=0, sprinkler=0) = 0.0370
;; P(wet=1 | rain=0, sprinkler=1) = 0.7909
;; P(wet=1 | rain=1, sprinkler=0) = 0.8636
;; P(wet=1 | rain=1, sprinkler=1) = 1.0000

#| TESTS

;; no observations
(load "sprinkler-example.lisp")
(hems::example)
(hems::H[bn] (car (hems::get-eltm)) (make-hash-table))

;; certain observations
(let ((observations (make-hash-table :test #'equal)))
(setf (gethash "ZEUS_ANGRY_243" observations) (list (cons "T" 1)))
(hems::H[bn] (car (hems::get-eltm)) observations))
(let (evidence-bn)
(setq evidence-bn (compile-program nil c = (percept-node zeus_angry :value "T")))
(hems::H[bn] (car (hems::get-eltm)) evidence-bn))

;; uncertain observations
(let ((observations (make-hash-table :test #'equal)))
(setf (gethash "SPRINKLER_245" observations) (list (cons "T" .3) (cons "NIL" .7)))
(hems::H[bn] (car (hems::get-eltm)) observations))
|#
