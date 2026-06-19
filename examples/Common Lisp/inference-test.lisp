(ql:quickload :hems)
(in-package :hems)

(defun functional-check ()
  (let (bn)
    (setq bn (compile-program nil
	       v1 = (functional-node threat_curr :arguments NIL :generator (list (cons "NA" 0) (cons "threat" 45/100) (cons "benign" 55/100)) :type "percept")
	       v2 = (functional-node threat_next :arguments (v1) :generator (cond ((string-equal "na" v1) (list (cons "NA" 0) (cons "threat" 0) (cons "benign" 0))) ((string-equal "threat" v1) (list (cons "NA" 0) (cons "threat" 9/10) (cons "benign" 1/10))) ((string-equal "benign" v1) (list (cons "NA" 0) (cons "threat" 1/10) (cons "benign" 9/10))) (t (list (cons "NA" 0) (cons "threat" 0) (cons "benign" 0)))) :type "percept")
	       v3 = (functional-node road_curr :arguments (v1) :generator (let (road-domain connectivity-matrix checkpoint-node-indices class checkpoint-road-prob checkpoint-roads other-roads n-roads n-checkpoint n-other remaining-prob other-prob uniform-prob) (setq road-domain '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26")) (setq connectivity-matrix '((0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 -1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 -1 0 0 0 1 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 -1 0 0 0 1 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0) (0 0 0 0 0 0 0 0 0 1 0 0 0 -1 0 0 0 0) (0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0) (0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 1 0 0 0 -1 0 0 0 0 0 0 0 0 0) (1 0 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0) (-1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 -1 0 0 0 1 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 -1 0 0 0 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 -1 0 0 0 1 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 -1 0 0 0 1 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0) (0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0) (0 -1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 -1 0 0 0 1 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 1))) (setq checkpoint-node-indices '(6 11)) (setq class v1) (setq checkpoint-road-prob 1/10000) (setq n-roads (length road-domain)) (cond ((string-equal class "na") (loop for road in road-domain collect (cons road 0))) ((string-equal class "benign") (setq uniform-prob (if (> n-roads 0) (/ 1 n-roads) 0)) (loop for road in road-domain collect (cons road uniform-prob))) ((string-equal class "threat") (setq checkpoint-roads (loop for row in connectivity-matrix for road in road-domain when (some #'(lambda (node-index) (/= (nth (- node-index 1) row) 0)) checkpoint-node-indices) collect road)) (setq other-roads (loop for road in road-domain unless (member road checkpoint-roads :test #'string-equal) collect road)) (setq n-checkpoint (length checkpoint-roads)) (setq n-other (length other-roads)) (setq remaining-prob (- 1 (* checkpoint-road-prob n-checkpoint))) (setq other-prob (if (> n-other 0) (/ remaining-prob n-other) 0)) (loop for road in road-domain collect (if (member road checkpoint-roads :test #'string-equal) (cons road checkpoint-road-prob) (cons road other-prob)))) (t (loop for road in road-domain collect (cons road 0))))) :type "percept")
	       v4 = (functional-node dist_to_intersection_curr :arguments (v3) :generator (let (dist-domain road-lengths road-index road-length valid-dists n-valid d-num) (setq dist-domain '("20" "40" "60" "80" "100" "120" "140" "160" "180" "200")) (setq road-lengths '(180 180 180 180 180 180 180 180 180 180 180 180 180 180 180 180 180 180 180 180 200 180 180 180 180 200)) (cond ((string-equal "na" v3) (loop for d in dist-domain collect (cons d 0))) (t (setq road-index (parse-integer v3)) (setq road-length (nth (- road-index 1) road-lengths)) (setq valid-dists (loop for d in dist-domain do (setq d-num (parse-integer d)) when (<= d-num road-length) collect d)) (setq n-valid (length valid-dists)) (loop for d in dist-domain collect (cons d (if (member d valid-dists :test #'string-equal) (/ 1 n-valid) 0)))))) :type "percept")
	       v5 = (functional-node heading_curr :arguments NIL :generator (list (cons "NA" 0) (cons "-1" 49/100) (cons "0" 1/50) (cons "1" 49/100)) :type "percept")
	       v6 = (functional-node transition_flag :arguments (v4) :generator (let (flag-domain dist-num dist-next) (setq flag-domain '("0" "1")) (cond ((string-equal "na" v4) (loop for f in flag-domain collect (cons f 0))) (t (setq dist-num (parse-integer v4)) (setq dist-next (- dist-num (* 20 1))) (loop for f in flag-domain collect (cons f (cond ((and (string-equal f "0") (> dist-next 0)) 1) ((and (string-equal f "1") (= dist-next 0)) 1) (t 0))))))) :type "percept")
	       v7 = (functional-node road_next :arguments (v2 v3 v5 v6) :generator (let (road-domain connectivity-matrix checkpoint-node-indices checkpoint-prob current-road heading transition row pos-node-index neg-node-index target-node-indices connected-roads checkpoint-roads regular-roads n-connected n-checkpoint n-regular remaining-prob regular-prob) (setq road-domain '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26")) (setq connectivity-matrix '((0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 -1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 -1 0 0 0 1 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 -1 0 0 0 1 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0) (0 0 0 0 0 0 0 0 0 1 0 0 0 -1 0 0 0 0) (0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0) (0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 1 0 0 0 -1 0 0 0 0 0 0 0 0 0) (1 0 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0) (-1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 -1 0 0 0 1 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 -1 0 0 0 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 -1 0 0 0 1 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 -1 0 0 0 1 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0) (0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0) (0 -1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 -1 0 0 0 1 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 1))) (setq checkpoint-node-indices '(6 11)) (setq checkpoint-prob 1/10000) (cond ((some #'(lambda (arg) (string-equal "na" arg)) (list v2 v3 v5 v6)) (loop for r in road-domain collect (cons r 0))) (t (setq current-road (parse-integer v3)) (setq heading (parse-integer v5)) (setq transition (parse-integer v6)) (setq row (nth (- current-road 1) connectivity-matrix)) (setq pos-node-index (position 1 row)) (setq neg-node-index (position -1 row)) (setq target-node-indices (cond ((= heading 1) (list pos-node-index)) ((= heading -1) (list neg-node-index)) ((= heading 0) (list pos-node-index neg-node-index)) (t nil))) (setq connected-roads (cond ((= transition 0) (list v3)) ((= transition 1) (loop for r-row in connectivity-matrix for road in road-domain for road-index from 1 when (and (/= road-index current-road) (some #'(lambda (node-index) (/= (nth node-index r-row) 0)) target-node-indices)) collect road)) (t nil))) (setq n-connected (length connected-roads)) (cond ((string-equal v2 "benign") (loop for road in road-domain collect (if (and (> n-connected 0) (member road connected-roads :test #'string-equal)) (cons road (/ 1 n-connected)) (cons road 0)))) ((string-equal v2 "threat") (setq checkpoint-roads (loop for r-row in connectivity-matrix for road in road-domain when (and (member road connected-roads :test #'string-equal) (some #'(lambda (node-index) (/= (nth (- node-index 1) r-row) 0)) checkpoint-node-indices)) collect road)) (setq regular-roads (loop for road in connected-roads unless (member road checkpoint-roads :test #'string-equal) collect road)) (setq n-checkpoint (length checkpoint-roads)) (setq n-regular (length regular-roads)) (if (= n-regular 0) (loop for road in road-domain collect (if (and (> n-connected 0) (member road connected-roads :test #'string-equal)) (cons road (/ 1 n-connected)) (cons road 0))) (progn (setq remaining-prob (- 1 (* checkpoint-prob n-checkpoint))) (setq regular-prob (/ remaining-prob n-regular)) (loop for road in road-domain collect (cond ((member road checkpoint-roads :test #'string-equal) (cons road checkpoint-prob)) ((member road regular-roads :test #'string-equal) (cons road regular-prob)) (t (cons road 0))))))) (t (loop for road in road-domain collect (cons road 0))))))) :type "percept")
	       v8 = (functional-node dist_to_intersection_next :arguments (v4 v6 v7) :generator (let (dist-domain road-lengths dist-num dist-next transition road-index road-length) (setq dist-domain '("20" "40" "60" "80" "100" "120" "140" "160" "180" "200")) (setq road-lengths '(180 180 180 180 180 180 180 180 180 180 180 180 180 180 180 180 180 180 180 180 200 180 180 180 180 200)) (cond ((some #'(lambda (arg) (string-equal "na" arg)) (list v4 v6 v7)) (loop for d in dist-domain collect (cons d 0))) (t (setq dist-num (parse-integer v4)) (setq transition (parse-integer v6)) (setq road-index (parse-integer v7)) (setq road-length (nth (- road-index 1) road-lengths)) (setq dist-next (cond ((= transition 0) (- dist-num (* 20 1))) ((= transition 1) road-length) (t -1))) (loop for d in dist-domain collect (cons d (if (= (parse-integer d) dist-next) 1 0)))))) :type "percept")
	       v9 = (functional-node heading_next :arguments (v3 v5 v7) :generator (let (heading-domain connectivity-matrix current-road heading next-road curr-row next-row pos-node-index neg-node-index target-node-indices valid-heading-values n-valid next-entry) (setq heading-domain '("-1" "0" "1")) (setq connectivity-matrix '((0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 -1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 -1 0 0 0 1 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 -1 0 0 0 1 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0) (0 0 0 0 0 0 0 0 0 1 0 0 0 -1 0 0 0 0) (0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0) (0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 1 0 0 0 -1 0 0 0 0 0 0 0 0 0) (1 0 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0) (-1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 -1 0 0 0 1 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 -1 1 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 -1 0 0 0 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 -1 0 0 0 1 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 -1 0 0 0 1 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0) (0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0) (0 -1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 -1 0 0 0 1 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 -1 0 0 0 0 0 1))) (cond ((some #'(lambda (arg) (string-equal "na" arg)) (list v3 v5 v7)) (loop for h in heading-domain collect (cons h 0))) (t (setq current-road (parse-integer v3)) (setq heading (parse-integer v5)) (setq next-road (parse-integer v7)) (setq curr-row (nth (- current-road 1) connectivity-matrix)) (setq next-row (nth (- next-road 1) connectivity-matrix)) (cond ((= current-road next-road) (loop for h in heading-domain collect (cons h (if (= (parse-integer h) heading) 1 0)))) (t (setq pos-node-index (position 1 curr-row)) (setq neg-node-index (position -1 curr-row)) (setq target-node-indices (cond ((= heading 1) (list pos-node-index)) ((= heading -1) (list neg-node-index)) ((= heading 0) (list pos-node-index neg-node-index)) (t nil))) (setq valid-heading-values (loop for node-index in target-node-indices do (setq next-entry (nth node-index next-row)) when (/= next-entry 0) collect (if (= next-entry 1) "-1" "1"))) (setq n-valid (length valid-heading-values)) (loop for h in heading-domain collect (cons h (if (and (> n-valid 0) (member h valid-heading-values :test #'string-equal)) (/ 1 n-valid) 0)))))))) :type "percept")
	       ))
    (print-bn bn)))


(defun run8 ()
  (let (obs)
    (load-eltm-from-file "eltm_debug_repeated_domain.txt")
    ;;(print-episode (car eltm*))
    (setq obs (compile-program nil
			       v5 = (percept-node threat_next :values ((:value "NA" :probability 0.000755 :count 1) (:value "threat" :probability 0.542608 :count 1) (:value "benign" :probability 0.456637 :count 1)))
			       v6 = (percept-node road_next :values ((:value "NA" :probability 0.027534 :count 1) (:value "26" :probability 0.264159 :count 1) (:value "2" :probability 0.028511 :count 1) (:value "3" :probability 0.052976 :count 1) (:value "4" :probability 0.025892 :count 1) (:value "5" :probability 0.041475 :count 1) (:value "6" :probability 0.032005 :count 1) (:value "7" :probability 0.015720 :count 1) (:value "8" :probability 0.020725 :count 1) (:value "9" :probability 0.018676 :count 1) (:value "10" :probability 0.016957 :count 1) (:value "11" :probability 0.033400 :count 1) (:value "12" :probability 0.023090 :count 1) (:value "14" :probability 0.019739 :count 1) (:value "15" :probability 0.022475 :count 1) (:value "16" :probability 0.052367 :count 1) (:value "17" :probability 0.014186 :count 1) (:value "18" :probability 0.036977 :count 1) (:value "19" :probability 0.016055 :count 1) (:value "20" :probability 0.021623 :count 1) (:value "23" :probability 0.049608 :count 1) (:value "24" :probability 0.020306 :count 1) (:value "25" :probability 0.014229 :count 1) (:value "1" :probability 0.043015 :count 1) (:value "22" :probability 0.018961 :count 1) (:value "13" :probability 0.011469 :count 1) (:value "21" :probability 0.057870 :count 1)))
			       v7 = (percept-node dist_to_intersection_next :values ((:value "NA" :probability 0.002594 :count 1) (:value "200" :probability 0.002734 :count 1) (:value "40" :probability 0.001394 :count 1) (:value "60" :probability 0.001394 :count 1) (:value "80" :probability 0.001394 :count 1) (:value "100" :probability 0.001394 :count 1) (:value "120" :probability 0.001394 :count 1) (:value "140" :probability 0.001394 :count 1) (:value "160" :probability 0.001394 :count 1) (:value "180" :probability 0.982456 :count 1) (:value "20" :probability 0.002457 :count 1)))
			       v8 = (percept-node heading_next :values ((:value "NA" :probability 0.001326 :count 1) (:value "0" :probability 0.002654 :count 1) (:value "-1" :probability 0.570082 :count 1) (:value "1" :probability 0.425938 :count 1)))
			       
			       ))
    ;;(print-bn (episode-observation (car eltm*)))
    (remember eltm* obs '+ 1 t :type "OBSERVATION" :soft-likelihoods t)))

(defun run7 ()
  (let (obs
	cpd
	compressed-cpd)
    (load-eltm-from-file "the_real_eltm_zero_check.txt")
    (setq cpd (aref (car (episode-observation (car eltm*))) 7))
    (check-cpd cpd :check-uniqueness nil)
    (print-cpd cpd)
    
    (setq compressed-cpd (get-local-coverings
			  (update-cpd-rules cpd (rule-based-cpd-rules cpd)) :patch nil))
    (print-cpd compressed-cpd)))

(defun run6 ()
  (let (obs)
    (load-eltm-from-file "the_real_eltm_zero_check.txt")
    ;;(print-episode (car eltm*))
    (setq obs (compile-program nil
		v1 = (percept-node road_curr :values ((:value "12" :probability 1.0 :count 1))) 
		v2 = (percept-node dist_to_intersection_curr :values ((:value "180" :probability 1.0 :count 1))) 
		v3 = (percept-node heading_curr :values ((:value "-1" :probability 1.0 :count 1))) 
		v4 = (percept-node transition_flag :values ((:value "0" :probability 1.0 :count 1))) 
		v5 = (percept-node road_next :values ((:value "12" :probability 1.0 :count 1))) 
		v6 = (percept-node dist_to_intersection_next :values ((:value "180" :probability 1.0 :count 1))) 
		v7 = (percept-node heading_next :values ((:value "-1" :probability 1.0 :count 1)))
		))
    ;;(print-bn (episode-observation (car eltm*)))
    (remember eltm* obs '+ 1 t :type "OBSERVATION" :soft-likelihoods nil)))

(defun run5 ()
  (let (obs)
    (load-eltm-from-file "eltm_insurgent_scenario.txt")
    (setq obs
	  (compile-program nil
	    v1 = (percept-node road_curr :values ((:value "1" :probability 1.0 :count 1)))
	    v2 = (percept-node dist_to_intersection_curr :values ((:value "160" :probability 1.0 :count 1)))
	    v3 = (percept-node heading_curr :values ((:value "1" :probability 1.0 :count 1)))
	    v4 = (percept-node transition_flag :values ((:value "0" :probability 1.0 :count 1)))
	    v5 = (percept-node threat_next :values ((:value "NA" :probability 0.000312 :count 1) (:value "threat" :probability 0.485826 :count 1) (:value "benign" :probability 0.513862 :count 1)))
	    v6 = (percept-node road_next :values ((:value "1" :probability 1.000000 :count 1) (:value "NA" :probability 0.000000 :count 1) (:value "2" :probability 0.000000 :count 1) (:value "3" :probability 0.000000 :count 1) (:value "4" :probability 0.000000 :count 1) (:value "5" :probability 0.000000 :count 1) (:value "6" :probability 0.000000 :count 1) (:value "7" :probability 0.000000 :count 1) (:value "8" :probability 0.000000 :count 1) (:value "9" :probability 0.000000 :count 1) (:value "10" :probability 0.000000 :count 1) (:value "11" :probability 0.000000 :count 1) (:value "12" :probability 0.000000 :count 1) (:value "13" :probability 0.000000 :count 1) (:value "14" :probability 0.000000 :count 1) (:value "15" :probability 0.000000 :count 1) (:value "16" :probability 0.000000 :count 1) (:value "17" :probability 0.000000 :count 1) (:value "18" :probability 0.000000 :count 1) (:value "19" :probability 0.000000 :count 1) (:value "20" :probability 0.000000 :count 1) (:value "21" :probability 0.000000 :count 1) (:value "22" :probability 0.000000 :count 1) (:value "23" :probability 0.000000 :count 1) (:value "24" :probability 0.000000 :count 1) (:value "25" :probability 0.000000 :count 1) (:value "26" :probability 0.000000 :count 1)))
	    v7 = (percept-node dist_to_intersection_next :values ((:value "160" :probability 1.000000 :count 1) (:value "NA" :probability 0.000000 :count 1) (:value "20" :probability 0.000000 :count 1) (:value "40" :probability 0.000000 :count 1) (:value "60" :probability 0.000000 :count 1) (:value "80" :probability 0.000000 :count 1) (:value "100" :probability 0.000000 :count 1) (:value "120" :probability 0.000000 :count 1) (:value "140" :probability 0.000000 :count 1) (:value "180" :probability 0.000000 :count 1) (:value "200" :probability 0.000000 :count 1)))
	    v8 = (percept-node heading_next :values ((:value "1" :probability 1.000000 :count 1) (:value "NA" :probability 0.000000 :count 1) (:value "-1" :probability 0.000000 :count 1) (:value "0" :probability 0.000000 :count 1)))
	    ))
    (print-bn (episode-observation (car eltm*)))
    (remember eltm* obs '+ 1 t :type "OBSERVATION" :soft-likelihoods t)))

(defun run4 ()
  (let ((evidence-hash (make-hash-table))
	obs
	st
	slice
	messages)
    (load-eltm-from-file "eltm_insurgent_scenario.txt")
    
    (setq obs
	  (compile-program nil
	    v1 = (percept-node road_curr :values ((:value "16" :probability 1.0 :count 1)))
	    v2 = (percept-node dist_to_intersection_curr :values ((:value "20" :probability 1.0 :count 1)))
	    v3 = (percept-node heading_curr :values ((:value "-1" :probability 1.0 :count 1)))
	    v4 = (percept-node transition_flag :values ((:value "0" :probability 1.0 :count 1)))
	    v5 = (percept-node road_next :values ((:value "16" :probability 1.0 :count 1)))
	    v6 = (percept-node dist_to_intersection_next :values ((:value "20" :probability 1.0 :count 1)))
	    v7 = (percept-node heading_next :values ((:value "-1" :probability 1.0 :count 1)))
	    ))
    
    (setq st (compile-program nil))
    
    (setq slice (make-hash-table :test #'equal))
    (setf (gethash "STATE" slice) st)
    (setf (gethash "OBSERVATION" slice) obs)
    (setf (gethash 0 evidence-hash) slice)
    (setq messages (make-messages evidence-hash 0 0 t))
    (multiple-value-bind (temporal-bn backlinks)
	(make-temporal-episode-retrieval-cue eltm* messages t)
      (remember-temporal eltm* temporal-bn backlinks messages :hidden-state-p t :soft-likelihoods t))))
