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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
 (save-eltm-to-file eltm* :filename "eltm-pop-1.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population distribution:
;; P(zeus_angry=1) = 0.309455
;; P(rain=1 | zeus_angry=0) = 0.221809
;; P(rain=1 | zeus_angry=1) = 0.710539
;; P(sprinkler=1 | rain=0) = 0.343705
;; P(sprinkler=1 | rain=1) = 0.0513488
;; P(wet=1 | rain=0, sprinkler=0) = 0.0325613
;; P(wet=1 | rain=0, sprinkler=1) = 0.549546
;; P(wet=1 | rain=1, sprinkler=0) = 0.829479
;; P(wet=1 | rain=1, sprinkler=1) = 0.925689

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.2900
;; P(rain=1 | zeus_angry=0) = 0.2207
;; P(rain=1 | zeus_angry=1) = 0.7471
;; P(sprinkler=1 | rain=0) = 0.4202
;; P(sprinkler=1 | rain=1) = 0.0536
;; P(wet=1 | rain=0, sprinkler=0) = 0.0367

;; P(wet=1 | rain=0, sprinkler=1) = 0.5443

;; P(wet=1 | rain=1, sprinkler=0) = 0.8113

;; P(wet=1 | rain=1, sprinkler=1) = 1.0000

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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c4 = (percept-node wet :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename "eltm-pop-2.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population distribution:
;; P(zeus_angry=1) = 0.219973
;; P(rain=1 | zeus_angry=0) = 0.384584
;; P(rain=1 | zeus_angry=1) = 0.797824
;; P(sprinkler=1 | rain=0) = 0.134817
;; P(sprinkler=1 | rain=1) = 0.0504042
;; P(wet=1 | rain=0, sprinkler=0) = 0.0959596
;; P(wet=1 | rain=0, sprinkler=1) = 0.683963
;; P(wet=1 | rain=1, sprinkler=0) = 0.664734
;; P(wet=1 | rain=1, sprinkler=1) = 0.904211

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.2600
;; P(rain=1 | zeus_angry=0) = 0.3176
;; P(rain=1 | zeus_angry=1) = 0.7885
;; P(sprinkler=1 | rain=0) = 0.2946
;; P(sprinkler=1 | rain=1) = 0.0455
;; P(wet=1 | rain=0, sprinkler=0) = 0.0844

;; P(wet=1 | rain=0, sprinkler=1) = 0.5859

;; P(wet=1 | rain=1, sprinkler=0) = 0.7619

;; P(wet=1 | rain=1, sprinkler=1) = 0.9167

(defun build-pop-3 ()
(let (observations)
    (setq observations
(list (compile-program
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
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c4 = (percept-node wet :value "T")
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
	c4 = (percept-node wet :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename "eltm-pop-3.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population distribution:
;; P(zeus_angry=1) = 0.379217
;; P(rain=1 | zeus_angry=0) = 0.547968
;; P(rain=1 | zeus_angry=1) = 0.892897
;; P(sprinkler=1 | rain=0) = 0.527314
;; P(sprinkler=1 | rain=1) = 0.322926
;; P(wet=1 | rain=0, sprinkler=0) = 0.072741
;; P(wet=1 | rain=0, sprinkler=1) = 0.791725
;; P(wet=1 | rain=1, sprinkler=0) = 0.623289
;; P(wet=1 | rain=1, sprinkler=1) = 0.927248

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.3100
;; P(rain=1 | zeus_angry=0) = 0.3961
;; P(rain=1 | zeus_angry=1) = 0.8566
;; P(sprinkler=1 | rain=0) = 0.3398
;; P(sprinkler=1 | rain=1) = 0.1753
;; P(wet=1 | rain=0, sprinkler=0) = 0.0912

;; P(wet=1 | rain=0, sprinkler=1) = 0.6525

;; P(wet=1 | rain=1, sprinkler=0) = 0.7025

;; P(wet=1 | rain=1, sprinkler=1) = 0.9176

(defun build-pop-4 ()
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c4 = (percept-node wet :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename "eltm-pop-4.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population distribution:
;; P(zeus_angry=1) = 0.604952
;; P(rain=1 | zeus_angry=0) = 0.115409
;; P(rain=1 | zeus_angry=1) = 0.890384
;; P(sprinkler=1 | rain=0) = 0.592135
;; P(sprinkler=1 | rain=1) = 0.179077
;; P(wet=1 | rain=0, sprinkler=0) = 0.0513136
;; P(wet=1 | rain=0, sprinkler=1) = 0.793335
;; P(wet=1 | rain=1, sprinkler=0) = 0.881695
;; P(wet=1 | rain=1, sprinkler=1) = 0.976805

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.3800
;; P(rain=1 | zeus_angry=0) = 0.3454
;; P(rain=1 | zeus_angry=1) = 0.8618
;; P(sprinkler=1 | rain=0) = 0.3818
;; P(sprinkler=1 | rain=1) = 0.1954
;; P(wet=1 | rain=0, sprinkler=0) = 0.0824

;; P(wet=1 | rain=0, sprinkler=1) = 0.7048

;; P(wet=1 | rain=1, sprinkler=0) = 0.7514

;; P(wet=1 | rain=1, sprinkler=1) = 0.9370

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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
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
	c2 = (percept-node rain :value "NIL")
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
 (save-eltm-to-file eltm* :filename "eltm-pop-5.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population distribution:
;; P(zeus_angry=1) = 0.199052
;; P(rain=1 | zeus_angry=0) = 0.194965
;; P(rain=1 | zeus_angry=1) = 0.890207
;; P(sprinkler=1 | rain=0) = 0.427915
;; P(sprinkler=1 | rain=1) = 0.195935
;; P(wet=1 | rain=0, sprinkler=0) = 0.0427743
;; P(wet=1 | rain=0, sprinkler=1) = 0.762536
;; P(wet=1 | rain=1, sprinkler=0) = 0.779777
;; P(wet=1 | rain=1, sprinkler=1) = 0.949942

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.3453
;; P(rain=1 | zeus_angry=0) = 0.3086
;; P(rain=1 | zeus_angry=1) = 0.8629
;; P(sprinkler=1 | rain=0) = 0.3987
;; P(sprinkler=1 | rain=1) = 0.1933
;; P(wet=1 | rain=0, sprinkler=0) = 0.0865

;; P(wet=1 | rain=0, sprinkler=1) = 0.7391

;; P(wet=1 | rain=1, sprinkler=0) = 0.7653

;; P(wet=1 | rain=1, sprinkler=1) = 0.9379


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