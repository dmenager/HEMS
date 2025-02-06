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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename "eltm-pop-1.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population 1 distribution:
;; P(zeus_angry=1) = 0.186952
;; P(rain=1 | zeus_angry=0) = 0.116693
;; P(rain=1 | zeus_angry=1) = 0.707587
;; P(sprinkler=1 | rain=0) = 0.512376
;; P(sprinkler=1 | rain=1) = 0.236934
;; P(wet=1 | rain=0, sprinkler=0) = 0.0370178
;; P(wet=1 | rain=0, sprinkler=1) = 0.70058
;; P(wet=1 | rain=1, sprinkler=0) = 0.826176
;; P(wet=1 | rain=1, sprinkler=1) = 0.94988

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.1567
;; P(rain=1 | zeus_angry=0) = 0.0791
;; P(rain=1 | zeus_angry=1) = 0.7872
;; P(sprinkler=1 | rain=0) = 0.4979
;; P(sprinkler=1 | rain=1) = 0.2632
;; P(wet=1 | rain=0, sprinkler=0) = 0.0328

;; P(wet=1 | rain=0, sprinkler=1) = 0.7190

;; P(wet=1 | rain=1, sprinkler=0) = 0.8333

;; P(wet=1 | rain=1, sprinkler=1) = 1.0000

(defun build-pop-2 ()
(let (observations)
    (setq observations
(list (compile-program
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c4 = (percept-node wet :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
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
	c4 = (percept-node wet :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename "eltm-pop-2.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population 2 distribution:
;; P(zeus_angry=1) = 0.617698
;; P(rain=1 | zeus_angry=0) = 0.218151
;; P(rain=1 | zeus_angry=1) = 0.832396
;; P(sprinkler=1 | rain=0) = 0.189823
;; P(sprinkler=1 | rain=1) = 0.102937
;; P(wet=1 | rain=0, sprinkler=0) = 0.0869238
;; P(wet=1 | rain=0, sprinkler=1) = 0.529806
;; P(wet=1 | rain=1, sprinkler=0) = 0.784135
;; P(wet=1 | rain=1, sprinkler=1) = 0.907324

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.6500
;; P(rain=1 | zeus_angry=0) = 0.2190
;; P(rain=1 | zeus_angry=1) = 0.8615
;; P(sprinkler=1 | rain=0) = 0.1560
;; P(sprinkler=1 | rain=1) = 0.0942
;; P(wet=1 | rain=0, sprinkler=0) = 0.0435

;; P(wet=1 | rain=0, sprinkler=1) = 0.6471

;; P(wet=1 | rain=1, sprinkler=0) = 0.7861

;; P(wet=1 | rain=1, sprinkler=1) = 0.7778

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
	c4 = (percept-node wet :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
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
	c4 = (percept-node wet :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
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
	c2 = (percept-node rain :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
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
	c4 = (percept-node wet :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c4 = (percept-node wet :value "T")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
))
;; insert into event memory
(map nil #'(lambda (bn)
	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))
 observations)
 (save-eltm-to-file eltm* :filename "eltm-pop-3.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population 3 distribution:
;; P(zeus_angry=1) = 0.728683
;; P(rain=1 | zeus_angry=0) = 0.238785
;; P(rain=1 | zeus_angry=1) = 0.865112
;; P(sprinkler=1 | rain=0) = 0.746143
;; P(sprinkler=1 | rain=1) = 0.392951
;; P(wet=1 | rain=0, sprinkler=0) = 0.044221
;; P(wet=1 | rain=0, sprinkler=1) = 0.566553
;; P(wet=1 | rain=1, sprinkler=0) = 0.639499
;; P(wet=1 | rain=1, sprinkler=1) = 0.850652

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.7433
;; P(rain=1 | zeus_angry=0) = 0.2078
;; P(rain=1 | zeus_angry=1) = 0.8206
;; P(sprinkler=1 | rain=0) = 0.7723
;; P(sprinkler=1 | rain=1) = 0.3618
;; P(wet=1 | rain=0, sprinkler=0) = 0.0435

;; P(wet=1 | rain=0, sprinkler=1) = 0.5000

;; P(wet=1 | rain=1, sprinkler=0) = 0.6142

;; P(wet=1 | rain=1, sprinkler=1) = 0.8194

(defun build-pop-4 ()
(let (observations)
    (setq observations
(list (compile-program
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
	c4 = (percept-node wet :value "T")
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
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c2 = (percept-node rain :value "T")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "T")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
 (save-eltm-to-file eltm* :filename "eltm-pop-4.txt")
 (setq eltm* nil)
 observations))


;; ----------------------------

;; Population 4 distribution:
;; P(zeus_angry=1) = 0.141931
;; P(rain=1 | zeus_angry=0) = 0.452982
;; P(rain=1 | zeus_angry=1) = 0.890251
;; P(sprinkler=1 | rain=0) = 0.81027
;; P(sprinkler=1 | rain=1) = 0.247171
;; P(wet=1 | rain=0, sprinkler=0) = 0.0587679
;; P(wet=1 | rain=0, sprinkler=1) = 0.539402
;; P(wet=1 | rain=1, sprinkler=0) = 0.873255
;; P(wet=1 | rain=1, sprinkler=1) = 0.945052

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.1100
;; P(rain=1 | zeus_angry=0) = 0.4944
;; P(rain=1 | zeus_angry=1) = 0.8788
;; P(sprinkler=1 | rain=0) = 0.7482
;; P(sprinkler=1 | rain=1) = 0.2422
;; P(wet=1 | rain=0, sprinkler=0) = 0.0571

;; P(wet=1 | rain=0, sprinkler=1) = 0.4327

;; P(wet=1 | rain=1, sprinkler=0) = 0.8770

;; P(wet=1 | rain=1, sprinkler=1) = 0.8974

(defun build-pop-5 ()
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c3 = (percept-node sprinkler :value "T")
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
	c4 = (percept-node wet :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
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
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
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
	c3 = (percept-node sprinkler :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
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
	c4 = (percept-node wet :value "NIL")
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
	c1 = (percept-node zeus_angry :value "NIL")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "NIL")
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
	c3 = (percept-node sprinkler :value "T")
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
	c2 = (percept-node rain :value "T")
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
	c2 = (percept-node rain :value "NIL")
	c3 = (percept-node sprinkler :value "NIL")
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
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
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
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "NIL")
	c1 --> c2
	c2 --> c3
	c2 --> c4
	c3 --> c4)
(compile-program
       nil
	c1 = (percept-node zeus_angry :value "T")
	c2 = (percept-node rain :value "T")
	c3 = (percept-node sprinkler :value "NIL")
	c4 = (percept-node wet :value "T")
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

;; Population 5 distribution:
;; P(zeus_angry=1) = 0.740413
;; P(rain=1 | zeus_angry=0) = 0.382903
;; P(rain=1 | zeus_angry=1) = 0.757414
;; P(sprinkler=1 | rain=0) = 0.565996
;; P(sprinkler=1 | rain=1) = 0.454099
;; P(wet=1 | rain=0, sprinkler=0) = 0.036848
;; P(wet=1 | rain=0, sprinkler=1) = 0.538586
;; P(wet=1 | rain=1, sprinkler=0) = 0.604073
;; P(wet=1 | rain=1, sprinkler=1) = 0.824046

;; CPDs (sample distribution)
;; P(zeus_angry=1) = 0.7567
;; P(rain=1 | zeus_angry=0) = 0.2877
;; P(rain=1 | zeus_angry=1) = 0.7401
;; P(sprinkler=1 | rain=0) = 0.6577
;; P(sprinkler=1 | rain=1) = 0.5185
;; P(wet=1 | rain=0, sprinkler=0) = 0.0263

;; P(wet=1 | rain=0, sprinkler=1) = 0.3836

;; P(wet=1 | rain=1, sprinkler=0) = 0.6374

;; P(wet=1 | rain=1, sprinkler=1) = 0.8571


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