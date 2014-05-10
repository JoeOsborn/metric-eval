(ns pw
	(:require [clojure.java.io :refer [file reader]]
						[clojure.string :refer [split join]]
						[clojure.math.numeric-tower :refer [abs ceil floor sqrt]]
						[clojure.core.memoize :as mem]
						[gamalyzer.cmp.tt.cced :refer [with-warp-window distance distances]]
						[gamalyzer.data.input :refer [make-traces make-domains expand-domain**]]
						[clojure-csv.core :refer [parse-csv]]
						[k-medoid :refer [k-medoids]]
						[error :refer [mean-error mean-abs-error ms-error rms-error]]
						[pw-read :refer [dissimilarity uniqueness outlierness]])
	(:import [edu.ucsc.eis Baseline])
	)

(defn standardize-id [trace-id]
	(if (= (subs trace-id 0 5) "game-")
		(join "-" (rest (rest (split trace-id #"-"))))
		trace-id))

(defn get-trace [tid traces]
	(first (filter #(= (standardize-id (:id %)) tid) (:traces traces))))

(defn do-trials [tfn trials]
	(doall (map tfn trials)))

;; dissimilarities

(defn dissimilarities-trial [trial metfn metname]
	;return ratings and error
	(let [r (:reference trial)
				traces-and-ratings (:ratings trial)
				doms (:domains trial)]
		(doall (map (fn [[trace rating]]
									(let [d (min 1 (metfn r trace doms))]
										{:metric metname
										 :prediction d :human rating :error (- d rating)
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

(defn analyze-dissimilarity [metfn metname]
	(let [trials (doall (do-trials #(dissimilarities-trial % metfn metname) d-measure))
				rms-err (rms-error trials)]
		(println "err" rms-err)
		{:metric metname
		 :rms-err rms-err
		 :ms-err (ms-error trials)
		 :mean-err (mean-error trials)
		 :mean-abs-err (mean-abs-error trials)
		 :trials trials}))

(defn analyze-dissimilarity-find-w [metfn metname]
	(let [longest-trace-length (apply max (map :longest-trace-length d-measure))
				ws (doall
						(map (fn [w]
									 (println "dissimilarity try w =" w)
									 (assoc (with-warp-window w (analyze-dissimilarity metfn metname)) :w w))
								 (range 1 (inc longest-trace-length))))]
		(println "done with parameter exploration")
		(apply min-key :rms-err ws)))

;; n-gram-specific hack for parameter search
(defn analyze-dissimilarity-find-n [metfn metname]
	(let [ns (doall
						(map (fn [n]
									 (println "dissimilarity try n =" n)
									 (assoc (analyze-dissimilarity (partial metfn n) metname) :n n))
								 (range 1 10)))]
		(println "done with parameter exploration")
		(apply min-key :rms-err ns)))


;; outlierness

(defn outlierness-trial [t k metfn metname]
	(let [ratings (:ratings t)
				traces (keys ratings)
				domains (:domains t)
				clustering (k-medoids k traces (fn [a b] (min 1 (metfn a b domains))))]
		(doall (mapcat (fn [c]
										 (let [_ c
													 samples (:samples c)]
											 (doall (map (fn [s]
																		 (let [rating (get ratings (:sample s))
																					 d (:distance s)]
																			 {:metric metname
																				:prediction d :human rating :error (- d rating)
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

(defn analyze-outlierness [k metfn metname]
	(let [trials (do-trials #(outlierness-trial % k metfn metname) os-measure)
				rms-err (rms-error trials)]
		(println "err" rms-err)
		{:k k
		 :metric metname
		 :rms-err rms-err
		 :ms-err (ms-error trials)
		 :mean-err (mean-error trials)
		 :mean-abs-err (mean-abs-error trials)
		 :trials trials}))

(defn analyze-outlierness-find-k [metfn metname]
	(let [largest-trace-count (apply max (map #(count (:ratings %)) os-measure))
				param-sets
				(doall (map (fn [k]
											(println "outlierness try k =" k)
											(analyze-outlierness k metfn metname))
										(range 1 (max 3 (int (/ largest-trace-count 5))))))]
		(println "done with parameter exploration")
		(apply min-key :rms-err param-sets)))

;; uniqueness

(defn uniqueness-trial [t k metfn metname]
	(let [rating (:rating t)
				traces (:traces t)
				domains (:domains t)
				clustering (k-medoids k traces (fn [a b] (min 1 (metfn a b domains))))
				cluster-weights (:weight clustering)
				sample-count (count traces)
				d (/ cluster-weights sample-count)]
		;; overall uniqueness is average-distance-of-each-trace-from-its-medoid.
		;; the sum of such distances is already the clustering's weight, so we can just
		;; divide it by the number of samples.
		{:metric metname
		 :prediction d :human rating :error (- d rating)
		 :traces (map #(standardize-id (:id %)) traces)
		 :medoids (map #(:id (:medoid %)) (:clusters clustering))}))

(defonce us-measure (map (fn [o]
													 (let [traces (:traces o)]
														 {:trial (:trial o)
															:rating (:rating o) :domains (:domains traces)
															:traces (:traces traces)
															:longest-trace-length (apply max (map #(count (:inputs %)) (:traces traces)))}))
												 (uniqueness)))

(defn analyze-uniqueness [k metfn metname]
	(let [trials (do-trials #(uniqueness-trial % k metfn metname) us-measure)
				rms-err (rms-error trials)]
		{:k k
		 :metric metname
		 :rms-err rms-err
		 :ms-err (ms-error trials)
		 :mean-err (mean-error trials)
		 :mean-abs-err (mean-abs-error trials)
		 :trials trials}))

(defn analyze-uniqueness-find-k [metfn metname]
	(let [longest-trace-length (apply max (map :longest-trace-length us-measure))
				largest-trace-count (apply max (map #(count (:traces %)) us-measure))
				param-sets
				(doall (mapcat (fn [k]
												 (println "uniqueness try k =" k)
												 (analyze-uniqueness k metfn metname))
											 (range 1 (max 3 (int (/ largest-trace-count 5))))))]
		(println "done with parameter exploration")
		(apply min-key :rms-err param-sets)))

(defn game-names [trace] (map #(str (second (second (first (:vals %)))))
															(:inputs trace)))

(defn int-frequencies [l]
	(into {} (map (fn [[k v]] [k (int v)])
								(frequencies l))))

(def ngram-count
	(mem/memo
	 (fn [n t] (int-frequencies (map #(join "__" %)
																	 (partition n 1 (game-names t)))))))

(defn ngram-distance [n a b] (Baseline/ngramDistance (ngram-count n a) (ngram-count n b)))

(defn intent-names [trace] (map #(str
																	(first (first (:vals %))) "_"
																	(second (second (first (:vals %)))) "_"
																	(second (:vals %)))
																(:inputs trace)))

(def intent-count
	(mem/memo
	 (fn [t] (int-frequencies (intent-names t)))))

(defn intent-distance [a b]
	(Baseline/intentCosineDissimilarity (intent-count a) (intent-count b)))

(defn find-file [root nom]
	(first (filter #(= (.getName %) nom) (file-seq (file root)))))

(def game-state
	(mem/memo
	 (fn [t]
		 (let [id (standardize-id (:id t))
					 fl (find-file "../data/FinalStates" (str id ".csv"))
					 csv (parse-csv (reader fl))
					 values (map #(case %
													"true" 100
													"false" 0
													(read-string %))
											 (filter #(not= % "") (flatten (rest (map rest csv)))))]
			 (int-array values)))))

(defn state-distance [a b]
	(Baseline/stateCosineDissimilarity (game-state a) (game-state b)))

;(ngram-count 1 (first (:traces (first us-measure))))
;(intent-count (first (:traces (first us-measure))))
;(into [] (game-state (first (:traces (first us-measure)))))

;; overall

;;; GLZ
#_(let [ad (analyze-dissimilarity-find-w distance :glz)
			w (:w ad)
			ao (with-warp-window w (analyze-outlierness-find-k distance :glz))
			k (:k ao)
			au (with-warp-window w (analyze-uniqueness k distance :glz))]
	[{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
		:ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
		:mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
		:mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
	 {:ad ad :ao ao :au au}])

;;; n-gram
(let [ad (analyze-dissimilarity-find-n (fn [n a b _doms] (ngram-distance n a b)) :n-gram)
			n (:n ad)
			ao (analyze-outlierness-find-k (fn [a b _doms] (ngram-distance n a b)) :n-gram)
			k (:k ao)
			au (analyze-uniqueness k (fn [a b _doms] (ngram-distance n a b)) :n-gram)]
	[{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
		:ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
		:mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
		:mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
	 {:ad ad :ao ao :au au}])

;;; intent cosine dissimilarity
(let [ad (analyze-dissimilarity (fn [a b _doms] (intent-distance a b)) :intent)
			n (:n ad)
			ao (analyze-outlierness-find-k (fn [a b _doms] (intent-distance a b)) :intent)
			k (:k ao)
			au (analyze-uniqueness k (fn [a b _doms] (intent-distance a b)) :intent)]
	[{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
		:ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
		:mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
		:mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
	 {:ad ad :ao ao :au au}])
;;; intent cosine dissimilarity
(let [ad (analyze-dissimilarity (fn [a b _doms] (state-distance a b)) :state)
			n (:n ad)
			ao (analyze-outlierness-find-k (fn [a b _doms] (state-distance a b)) :state)
			k (:k ao)
			au (analyze-uniqueness k (fn [a b _doms] (state-distance a b)) :state)]
	[{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
		:ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
		:mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
		:mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
	 {:ad ad :ao ao :au au}])
