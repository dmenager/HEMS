(ql:quickload :hems)
(in-package :hems)

(defun build-pop-1 ()
(let (observations)
    (setq observations
(list (compile-program
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
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename eltm-pop-1)
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population distribution:
;; P(zeus_angry=1) = 0.189988
;; P(rain=1 | zeus_angry=0) = 0.123322
;; P(rain=1 | zeus_angry=1) = 0.702233
;; P(sprinkler=1 | rain=0) = 0.521837
;; P(sprinkler=1 | rain=1) = 0.082869
;; P(wet=1 | rain=0, sprinkler=0) = 0.0818996
;; P(wet=1 | rain=0, sprinkler=1) = 0.79689
;; P(wet=1 | rain=1, sprinkler=0) = 0.854837
;; P(wet=1 | rain=1, sprinkler=1) = 0.972931

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.2000
;; P(rain=1 | zeus_angry=0) = 0.1250
;; P(rain=1 | zeus_angry=1) = 0.6667
;; P(sprinkler=1 | rain=0) = 0.5043
;; P(sprinkler=1 | rain=1) = 0.1143
;; P(wet=1 | rain=0, sprinkler=0) = 0.1316

;; P(wet=1 | rain=0, sprinkler=1) = 0.8103

;; P(wet=1 | rain=1, sprinkler=0) = 0.8548

;; P(wet=1 | rain=1, sprinkler=1) = 1.0000

(defun build-pop-2 ()
(let (observations)
    (setq observations
(list (compile-program
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
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename eltm-pop-2)
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population distribution:
;; P(zeus_angry=1) = 0.189988
;; P(rain=1 | zeus_angry=0) = 0.123322
;; P(rain=1 | zeus_angry=1) = 0.702233
;; P(sprinkler=1 | rain=0) = 0.521837
;; P(sprinkler=1 | rain=1) = 0.082869
;; P(wet=1 | rain=0, sprinkler=0) = 0.0818996
;; P(wet=1 | rain=0, sprinkler=1) = 0.79689
;; P(wet=1 | rain=1, sprinkler=0) = 0.854837
;; P(wet=1 | rain=1, sprinkler=1) = 0.972931

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.1950
;; P(rain=1 | zeus_angry=0) = 0.1242
;; P(rain=1 | zeus_angry=1) = 0.6667
;; P(sprinkler=1 | rain=0) = 0.5087
;; P(sprinkler=1 | rain=1) = 0.1087
;; P(wet=1 | rain=0, sprinkler=0) = 0.1057

;; P(wet=1 | rain=0, sprinkler=1) = 0.7872

;; P(wet=1 | rain=1, sprinkler=0) = 0.8862

;; P(wet=1 | rain=1, sprinkler=1) = 1.0000

(defun build-pop-3 ()
(let (observations)
    (setq observations
(list (compile-program
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
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename eltm-pop-3)
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population distribution:
;; P(zeus_angry=1) = 0.189988
;; P(rain=1 | zeus_angry=0) = 0.123322
;; P(rain=1 | zeus_angry=1) = 0.702233
;; P(sprinkler=1 | rain=0) = 0.521837
;; P(sprinkler=1 | rain=1) = 0.082869
;; P(wet=1 | rain=0, sprinkler=0) = 0.0818996
;; P(wet=1 | rain=0, sprinkler=1) = 0.79689
;; P(wet=1 | rain=1, sprinkler=0) = 0.854837
;; P(wet=1 | rain=1, sprinkler=1) = 0.972931

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.1933
;; P(rain=1 | zeus_angry=0) = 0.1212
;; P(rain=1 | zeus_angry=1) = 0.6954
;; P(sprinkler=1 | rain=0) = 0.5224
;; P(sprinkler=1 | rain=1) = 0.0813
;; P(wet=1 | rain=0, sprinkler=0) = 0.1000

;; P(wet=1 | rain=0, sprinkler=1) = 0.7867

;; P(wet=1 | rain=1, sprinkler=0) = 0.8958

;; P(wet=1 | rain=1, sprinkler=1) = 1.0000

(defun build-pop-4 ()
(let (observations)
    (setq observations
(list (compile-program
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
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename eltm-pop-4)
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population distribution:
;; P(zeus_angry=1) = 0.189988
;; P(rain=1 | zeus_angry=0) = 0.123322
;; P(rain=1 | zeus_angry=1) = 0.702233
;; P(sprinkler=1 | rain=0) = 0.521837
;; P(sprinkler=1 | rain=1) = 0.082869
;; P(wet=1 | rain=0, sprinkler=0) = 0.0818996
;; P(wet=1 | rain=0, sprinkler=1) = 0.79689
;; P(wet=1 | rain=1, sprinkler=0) = 0.854837
;; P(wet=1 | rain=1, sprinkler=1) = 0.972931

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.1900
;; P(rain=1 | zeus_angry=0) = 0.1193
;; P(rain=1 | zeus_angry=1) = 0.7149
;; P(sprinkler=1 | rain=0) = 0.5364
;; P(sprinkler=1 | rain=1) = 0.0645
;; P(wet=1 | rain=0, sprinkler=0) = 0.0913

;; P(wet=1 | rain=0, sprinkler=1) = 0.7935

;; P(wet=1 | rain=1, sprinkler=0) = 0.8851

;; P(wet=1 | rain=1, sprinkler=1) = 1.0000

(defun build-pop-5 ()
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
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 -> c2
	c2 -> c3
	c2 -> c4
	c3 -> c4)
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename eltm-pop-5)
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population distribution:
;; P(zeus_angry=1) = 0.189988
;; P(rain=1 | zeus_angry=0) = 0.123322
;; P(rain=1 | zeus_angry=1) = 0.702233
;; P(sprinkler=1 | rain=0) = 0.521837
;; P(sprinkler=1 | rain=1) = 0.082869
;; P(wet=1 | rain=0, sprinkler=0) = 0.0818996
;; P(wet=1 | rain=0, sprinkler=1) = 0.79689
;; P(wet=1 | rain=1, sprinkler=0) = 0.854837
;; P(wet=1 | rain=1, sprinkler=1) = 0.972931

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.1873
;; P(rain=1 | zeus_angry=0) = 0.1206
;; P(rain=1 | zeus_angry=1) = 0.7046
;; P(sprinkler=1 | rain=0) = 0.5195
;; P(sprinkler=1 | rain=1) = 0.0725
;; P(wet=1 | rain=0, sprinkler=0) = 0.0937

;; P(wet=1 | rain=0, sprinkler=1) = 0.7983

;; P(wet=1 | rain=1, sprinkler=0) = 0.8719

;; P(wet=1 | rain=1, sprinkler=1) = 1.0000


#| TESTS
(load "sprinkler-example.lisp")
(let ((obs1 (build-pop-1))
      (obs2 (build-pop-2))
      (obs3 (build-pop-3))
      (obs4 (build-pop-4))
      (obs5 (build-pop-4)))
  (load-eltm-from-file "eltm-pop-1.txt")
  (loop
    for obs in obs1
    collect
    (hems:bn-score obs (car hems:eltm*) (make-hash-table) (make-hash-table) :bic-p nil) into scores
    finally
       (format t "~%~%model id: ~A~%population id: ~d~%average score: ~d (~d)" "eltm-pop-1" 1 (hems:mean scores) (hems:stdev scores))))
  
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
