(ns pw
	(:require [clojure.string :refer [split join]]
						[clojure.math.numeric-tower :refer [abs ceil floor sqrt]]
						[clojure.core.memoize :as mem]
						[gamalyzer.cmp.tt.cced :refer [with-warp-window distance distances]]
						[gamalyzer.data.input :refer [make-traces make-domains expand-domain**]]
						[k-medoid :refer [k-medoids]]
						[error :refer [mean-error mean-abs-error ms-error rms-error]]
						[pw-read :refer [dissimilarity uniqueness outlierness]]))

(defn standardize-id [trace-id]
	(if (= (subs trace-id 0 5) "game-")
		(join "-" (rest (rest (split trace-id #"-"))))
		trace-id))

(defn get-trace [tid traces]
	(first (filter #(= (standardize-id (:id %)) tid) (:traces traces))))

(defn do-trials [tfn trials]
	(doall (map tfn trials)))

;; dissimilarities

(defn dissimilarities-trial [trial]
	;return ratings and error
	(let [r (:reference trial)
				traces-and-ratings (:ratings trial)
				doms (:domains trial)]
		(doall (map (fn [[trace rating]]
									(let [d (min 1 (distance r trace doms))]
										{:metric d :human rating :error (- d rating)
										 :ref (standardize-id (:id r)) :trace (standardize-id (:id trace))}))
								traces-and-ratings))))

(defonce d-measure (map (fn [d]
												 (let [rid (:reference d)
															 traces (:traces d)
															 r (get-trace rid traces)
															 ratings (map (fn [[tid rating]] [(get-trace tid traces) rating])
																						(:ratings d))]
													 {:trial (:trial d)
														:reference r :ratings ratings :domains (:domains traces)
														:longest-trace-length (apply max (map #(count (:inputs %)) (conj (map first ratings) r)))}))
												(dissimilarity)))

(defn analyze-dissimilarity []
	(let [trials (doall (do-trials dissimilarities-trial d-measure))
				rms-err (rms-error trials)]
		(println "err" rms-err)
		{:metric :glz
		 :rms-err rms-err
		 :ms-err (ms-error trials)
		 :mean-err (mean-error trials)
		 :mean-abs-err (mean-abs-error trials)
		 :trials trials}))

(defn analyze-dissimilarity-find-w []
	(let [longest-trace-length (apply max (map :longest-trace-length d-measure))
				ws (doall
						(map (fn [w]
									 (println "dissimilarity try w =" w)
									 (assoc (with-warp-window w (analyze-dissimilarity)) :w w))
								 (range 1 (inc longest-trace-length))))]
		(println "done with parameter exploration")
		(apply min-key :rms-err ws)))

;; outlierness

(defn outlierness-trial [t k]
	(let [ratings (:ratings t)
				traces (keys ratings)
				domains (:domains t)
				clustering (k-medoids k traces (fn [a b] (min 1 (distance a b domains))))]
		(doall (mapcat (fn [c]
										 (let [_ c
													 samples (:samples c)]
											 (doall (map (fn [s]
																		 (let [rating (get ratings (:sample s))
																					 d (:distance s)]
																			 {:metric d :human rating :error (- d rating)
																				:trace (standardize-id (:id (:sample s)))
																				:medoid (standardize-id (:id (:medoid c)))}))
																	 samples))))
									 (:clusters clustering)))))

(defonce os-measure (map (fn [o]
													 (let [traces (:traces o)
																 ratings (into {} (map (fn [[tid rating]] [(get-trace tid traces) rating])
																											 (:ratings o)))]
														 {:trial (:trial o)
															:ratings ratings :domains (:domains traces)
															:longest-trace-length (apply max (map #(count (:inputs %)) (map first ratings)))}))
												 (outlierness)))

(defn analyze-outlierness [k]
	(let [trials (do-trials #(outlierness-trial % k) os-measure)
				rms-err (rms-error trials)]
		(println "err" rms-err)
		{:k k
		 :metric :glz
		 :rms-err rms-err
		 :ms-err (ms-error trials)
		 :mean-err (mean-error trials)
		 :mean-abs-err (mean-abs-error trials)
		 :trials trials}))

(defn analyze-outlierness-find-k []
	(let [largest-trace-count (apply max (map #(count (:ratings %)) os-measure))
				param-sets
				(doall (map (fn [k]
											(println "outlierness try k =" k)
											(analyze-outlierness k))
										(range 1 (max 3 (int (/ largest-trace-count 5))))))]
		(println "done with parameter exploration")
		(apply min-key :rms-err param-sets)))

;; uniqueness

(defn uniqueness-trial [t k]
	(let [rating (:rating t)
				traces (:traces t)
				domains (:domains t)
				clustering (k-medoids k traces (fn [a b] (min 1 (distance a b domains))))
				cluster-weights (:weight clustering)
				sample-count (count traces)
				d (/ cluster-weights sample-count)]
		;; overall uniqueness is average-distance-of-each-trace-from-its-medoid.
		;; the sum of such distances is already the clustering's weight, so we can just
		;; divide it by the number of samples.
		{:metric d :human rating :error (- d rating)
		 :traces (map #(standardize-id (:id %)) traces)
		 :medoids (map #(:id (:medoid %)) (:clusters clustering))}))

(defonce us-measure (map (fn [o]
													 (let [traces (:traces o)]
														 {:trial (:trial o)
															:rating (:rating o) :domains (:domains traces)
															:traces (:traces traces)
															:longest-trace-length (apply max (map #(count (:inputs %)) (:traces traces)))}))
												 (uniqueness)))

(defn analyze-uniqueness [k]
	(let [trials (do-trials #(uniqueness-trial % k) us-measure)
				rms-err (rms-error trials)]
		{:k k
		 :metric :glz
		 :rms-err rms-err
		 :ms-err (ms-error trials)
		 :mean-err (mean-error trials)
		 :mean-abs-err (mean-abs-error trials)
		 :trials trials}))

(defn analyze-uniqueness-find-k []
	(let [longest-trace-length (apply max (map :longest-trace-length us-measure))
				largest-trace-count (apply max (map #(count (:traces %)) us-measure))
				param-sets
				(doall (mapcat (fn [k]
												 (println "uniqueness try k =" k)
												 (analyze-uniqueness k))
											 (range 1 (max 3 (int (/ largest-trace-count 5))))))]
		(println "done with parameter exploration")
		(apply min-key :rms-err param-sets)))

;; overall

(let [ad (analyze-dissimilarity-find-w)
			w (:w ad)
			ao (with-warp-window w (analyze-outlierness-find-k))
			k (:k ao)
			au (with-warp-window w (analyze-uniqueness k))]
	[{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
		:ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
		:mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
		:mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
	 {:ad ad :ao ao :au au}])
