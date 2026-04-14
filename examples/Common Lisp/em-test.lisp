(ql:quickload :hems)
(in-package :hems)
(defun ex ()
  (new-push-to-ep-buffer :observation (compile-program nil
	latent = (percept-node test :latent-p t :values ((:value "1") (:value "2") (:value "3")))
	observed = (percept-node observed :value "A")
	latent --> observed)
			     :insertp t
			     :temporal-p nil
			 :hidden-state-p nil)
  (new-push-to-ep-buffer :observation (compile-program nil
	latent = (percept-node test :latent-p t :values ((:value "1") (:value "2") (:value "3")))
	observed = (percept-node observed :value "A")
	latent --> observed)
			     :insertp t
			     :temporal-p nil
			 :hidden-state-p nil)
  (new-push-to-ep-buffer :observation (compile-program nil
	latent = (percept-node test :latent-p t :values ((:value "1") (:value "2") (:value "3")))
	observed = (percept-node observed :value "B")
	latent --> observed)
			     :insertp t
			     :temporal-p nil
			 :hidden-state-p nil)
  (new-push-to-ep-buffer :observation (compile-program nil
	latent = (percept-node test :latent-p t :values ((:value "1") (:value "2") (:value "3")))
	observed = (percept-node observed :value "C")
	latent --> observed)
			     :insertp t
			     :temporal-p nil
			 :hidden-state-p nil)
  (new-push-to-ep-buffer :observation (compile-program nil
	latent = (percept-node test :latent-p t :values ((:value "1") (:value "2") (:value "3")))
	observed = (percept-node observed :value "A")
	latent --> observed)
			     :insertp t
			     :temporal-p nil
			 :hidden-state-p nil)
  (new-push-to-ep-buffer :observation (compile-program nil
	latent = (percept-node test :latent-p t :values ((:value "1") (:value "2") (:value "3")))
	observed = (percept-node observed :value "B")
	latent --> observed)
			     :insertp t
			     :temporal-p nil
			     :hidden-state-p nil))

(defun ex-chain ()
  "Online EM example for A --> B --> C where B is the only latent variable."
  (new-push-to-ep-buffer :observation (compile-program nil
	a = (percept-node a :value "LOW")
	b = (percept-node b :latent-p t :values ((:value "LEFT") (:value "RIGHT")))
	c = (percept-node c :value "BLUE")
	a --> b
	b --> c)
			     :insertp t
			     :temporal-p nil
			     :hidden-state-p nil)
  (new-push-to-ep-buffer :observation (compile-program nil
	a = (percept-node a :value "LOW")
	b = (percept-node b :latent-p t :values ((:value "LEFT") (:value "RIGHT")))
	c = (percept-node c :value "BLUE")
	a --> b
	b --> c)
			     :insertp t
			     :temporal-p nil
			     :hidden-state-p nil)
  (new-push-to-ep-buffer :observation (compile-program nil
	a = (percept-node a :value "LOW")
	b = (percept-node b :latent-p t :values ((:value "LEFT") (:value "RIGHT")))
	c = (percept-node c :value "GREEN")
	a --> b
	b --> c)
			     :insertp t
			     :temporal-p nil
			     :hidden-state-p nil)
  (new-push-to-ep-buffer :observation (compile-program nil
	a = (percept-node a :value "HIGH")
	b = (percept-node b :latent-p t :values ((:value "LEFT") (:value "RIGHT")))
	c = (percept-node c :value "GREEN")
	a --> b
	b --> c)
			     :insertp t
			     :temporal-p nil
			     :hidden-state-p nil)
  (new-push-to-ep-buffer :observation (compile-program nil
	a = (percept-node a :value "HIGH")
	b = (percept-node b :latent-p t :values ((:value "LEFT") (:value "RIGHT")))
	c = (percept-node c :value "GREEN")
	a --> b
	b --> c)
			     :insertp t
			     :temporal-p nil
			     :hidden-state-p nil)
  (new-push-to-ep-buffer :observation (compile-program nil
	a = (percept-node a :value "HIGH")
	b = (percept-node b :latent-p t :values ((:value "LEFT") (:value "RIGHT")))
	c = (percept-node c :value "BLUE")
	a --> b
	b --> c)
			     :insertp t
			     :temporal-p nil
			     :hidden-state-p nil))
