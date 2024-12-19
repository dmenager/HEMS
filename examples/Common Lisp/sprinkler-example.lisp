(ql:quickload :hems)
(in-package :hems)

(defun build-pop-1 ()
(let (observations)
    (setq observations
(list (compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename "eltm-pop-1.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population ~s distribution:
;; P(zeus_angry=1) = 0.703244
;; P(rain=1 | zeus_angry=0) = 0.112182
;; P(rain=1 | zeus_angry=1) = 0.856423
;; P(sprinkler=1 | rain=0) = 0.52871
;; P(sprinkler=1 | rain=1) = 0.351633
;; P(wet=1 | rain=0, sprinkler=0) = 0.00636434
;; P(wet=1 | rain=0, sprinkler=1) = 0.624908
;; P(wet=1 | rain=1, sprinkler=0) = 0.60242
;; P(wet=1 | rain=1, sprinkler=1) = 0.85182

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.7300
;; P(rain=1 | zeus_angry=0) = 0.1235
;; P(rain=1 | zeus_angry=1) = 0.8584
;; P(sprinkler=1 | rain=0) = 0.5588
;; P(sprinkler=1 | rain=1) = 0.3384
;; P(wet=1 | rain=0, sprinkler=0) = 0.0000

;; P(wet=1 | rain=0, sprinkler=1) = 0.7018

;; P(wet=1 | rain=1, sprinkler=0) = 0.6260

;; P(wet=1 | rain=1, sprinkler=1) = 0.8955

(defun build-pop-2 ()
(let (observations)
    (setq observations
(list (compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename "eltm-pop-2.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population ~s distribution:
;; P(zeus_angry=1) = 0.214987
;; P(rain=1 | zeus_angry=0) = 0.158573
;; P(rain=1 | zeus_angry=1) = 0.863904
;; P(sprinkler=1 | rain=0) = 0.297215
;; P(sprinkler=1 | rain=1) = 0.070717
;; P(wet=1 | rain=0, sprinkler=0) = 0.0138324
;; P(wet=1 | rain=0, sprinkler=1) = 0.727671
;; P(wet=1 | rain=1, sprinkler=0) = 0.795995
;; P(wet=1 | rain=1, sprinkler=1) = 0.945212

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.2567
;; P(rain=1 | zeus_angry=0) = 0.1480
;; P(rain=1 | zeus_angry=1) = 0.9091
;; P(sprinkler=1 | rain=0) = 0.3147
;; P(sprinkler=1 | rain=1) = 0.0583
;; P(wet=1 | rain=0, sprinkler=0) = 0.0074

;; P(wet=1 | rain=0, sprinkler=1) = 0.6290

;; P(wet=1 | rain=1, sprinkler=0) = 0.8351

;; P(wet=1 | rain=1, sprinkler=1) = 1.0000

(defun build-pop-3 ()
(let (observations)
    (setq observations
(list (compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename "eltm-pop-3.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population ~s distribution:
;; P(zeus_angry=1) = 0.652344
;; P(rain=1 | zeus_angry=0) = 0.505027
;; P(rain=1 | zeus_angry=1) = 0.87603
;; P(sprinkler=1 | rain=0) = 0.398571
;; P(sprinkler=1 | rain=1) = 0.155926
;; P(wet=1 | rain=0, sprinkler=0) = 0.0492086
;; P(wet=1 | rain=0, sprinkler=1) = 0.717074
;; P(wet=1 | rain=1, sprinkler=0) = 0.807661
;; P(wet=1 | rain=1, sprinkler=1) = 0.94826

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.6567
;; P(rain=1 | zeus_angry=0) = 0.4466
;; P(rain=1 | zeus_angry=1) = 0.8934
;; P(sprinkler=1 | rain=0) = 0.4231
;; P(sprinkler=1 | rain=1) = 0.1306
;; P(wet=1 | rain=0, sprinkler=0) = 0.0444

;; P(wet=1 | rain=0, sprinkler=1) = 0.7879

;; P(wet=1 | rain=1, sprinkler=0) = 0.8031

;; P(wet=1 | rain=1, sprinkler=1) = 0.8966

(defun build-pop-4 ()
(let (observations)
    (setq observations
(list (compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename "eltm-pop-4.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population ~s distribution:
;; P(zeus_angry=1) = 0.144281
;; P(rain=1 | zeus_angry=0) = 0.113955
;; P(rain=1 | zeus_angry=1) = 0.71136
;; P(sprinkler=1 | rain=0) = 0.232429
;; P(sprinkler=1 | rain=1) = 0.11086
;; P(wet=1 | rain=0, sprinkler=0) = 0.038265
;; P(wet=1 | rain=0, sprinkler=1) = 0.771612
;; P(wet=1 | rain=1, sprinkler=0) = 0.672119
;; P(wet=1 | rain=1, sprinkler=1) = 0.927981

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.1567
;; P(rain=1 | zeus_angry=0) = 0.1225
;; P(rain=1 | zeus_angry=1) = 0.6596
;; P(sprinkler=1 | rain=0) = 0.2395
;; P(sprinkler=1 | rain=1) = 0.1774
;; P(wet=1 | rain=0, sprinkler=0) = 0.0497

;; P(wet=1 | rain=0, sprinkler=1) = 0.8421

;; P(wet=1 | rain=1, sprinkler=0) = 0.6667

;; P(wet=1 | rain=1, sprinkler=1) = 0.9091

(defun build-pop-5 ()
(let (observations)
    (setq observations
(list (compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename "eltm-pop-5.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population ~s distribution:
;; P(zeus_angry=1) = 0.724079
;; P(rain=1 | zeus_angry=0) = 0.121475
;; P(rain=1 | zeus_angry=1) = 0.844983
;; P(sprinkler=1 | rain=0) = 0.858067
;; P(sprinkler=1 | rain=1) = 0.127298
;; P(wet=1 | rain=0, sprinkler=0) = 0.0750849
;; P(wet=1 | rain=0, sprinkler=1) = 0.553515
;; P(wet=1 | rain=1, sprinkler=0) = 0.897279
;; P(wet=1 | rain=1, sprinkler=1) = 0.95758

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.7367
;; P(rain=1 | zeus_angry=0) = 0.0886
;; P(rain=1 | zeus_angry=1) = 0.8371
;; P(sprinkler=1 | rain=0) = 0.8241
;; P(sprinkler=1 | rain=1) = 0.1302
;; P(wet=1 | rain=0, sprinkler=0) = 0.1053

;; P(wet=1 | rain=0, sprinkler=1) = 0.6180

;; P(wet=1 | rain=1, sprinkler=0) = 0.8802

;; P(wet=1 | rain=1, sprinkler=1) = 0.9600


#| TESTS
(let ((obs1 (hems::build-pop-1))
(obs2 (hems::build-pop-2))
(obs3 (hems::build-pop-3))
(obs4 (hems::build-pop-4))
(obs5 (hems::build-pop-5))
)
(hems:load-eltm-from-file "eltm-pop-1.txt")
(hems:log-message (list "MODEL_ID,POPULATION_ID,OBSERVATION_ID,SCORE~%") "model-scores.csv")
(loop
with score
for obs in obs1
for i from 0
do
(setq score (float (hems:bn-score obs (hems:episode-observation (car hems:eltm*)) (make-hash-table) (make-hash-table) :bic-p nil)))
(hems:log-message (list "~A,~d,~d,~d~%" "eltm-pop-1" 1 i score) "model-scores.csv"))

(loop
with score
for obs in obs2
for i from 0
do
(setq score (float (hems:bn-score obs (hems:episode-observation (car hems:eltm*)) (make-hash-table) (make-hash-table) :bic-p nil)))
(hems:log-message (list "~A,~d,~d,~d~%" "eltm-pop-1" 2 i score) "model-scores.csv"))

(loop
with score
for obs in obs3
for i from 0
do
(setq score (float (hems:bn-score obs (hems:episode-observation (car hems:eltm*)) (make-hash-table) (make-hash-table) :bic-p nil)))
(hems:log-message (list "~A,~d,~d,~d~%" "eltm-pop-1" 3 i score) "model-scores.csv"))

(loop
with score
for obs in obs4
for i from 0
do
(setq score (float (hems:bn-score obs (hems:episode-observation (car hems:eltm*)) (make-hash-table) (make-hash-table) :bic-p nil)))
(hems:log-message (list "~A,~d,~d,~d~%" "eltm-pop-1" 4 i score) "model-scores.csv"))

(loop
with score
for obs in obs5
for i from 0
do
(setq score (float (hems:bn-score obs (hems:episode-observation (car hems:eltm*)) (make-hash-table) (make-hash-table) :bic-p nil)))
(hems:log-message (list "~A,~d,~d,~d~%" "eltm-pop-1" 5 i score) "model-scores.csv"))

)
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