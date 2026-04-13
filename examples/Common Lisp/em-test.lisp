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
