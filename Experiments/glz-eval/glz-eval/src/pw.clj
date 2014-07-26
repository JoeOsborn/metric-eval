(ns pw
	(:require [clojure.java.io :refer [file reader writer]]
						[clojure.string :refer [split join]]
						[clojure.math.numeric-tower :refer [abs ceil floor sqrt]]
						[clojure.core.memoize :as mem]
						[gamalyzer.cmp.tt.cced :refer [with-warp-window distance distances]]
						[gamalyzer.data.input :refer [make-traces make-domains expand-domain** make-trace]]
						[clojure-csv.core :refer [parse-csv write-csv]]
						[clojure.data :refer [diff]]
						[k-medoid :refer [k-medoids]]
						[error :refer [mean-error mean-abs-error ms-error rms-error sum]]
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

;; measures


(defn alter-trace [alter-input t]
	(assoc t :inputs (mapv alter-input (:inputs t))))

(defn alter-traces [alter-input in-traces]
	(let [new-traces (mapv #(alter-trace alter-input %) (:traces in-traces))
				new-domains (expand-domain** new-traces (make-domains))]
		(make-traces new-traces new-domains)))

(defn alternate-d-measure [alter-input]
	(doall (map (fn [d]
								(let [rid (:reference d)
											traces (alter-traces alter-input (:traces d))
											r (get-trace rid traces)
											ratings (doall (map (fn [[tid rating]] [(get-trace tid traces) rating])
																					(:ratings d)))]
									{:trial (:trial d)
									 :reference r :ratings ratings :domains (:domains traces)
									 :longest-trace-length (apply max (doall (map #(count (:inputs %)) (conj (map first ratings) r))))}))
							(dissimilarity))))

(defn alternate-os-measure [alter-input]
	(doall (map (fn [o]
				 (let [traces (alter-traces alter-input (:traces o))
							 ratings (into {} (doall (map (fn [[tid rating]] [(get-trace tid traces) rating])
																						(:ratings o))))]
					 {:trial (:trial o)
						:ratings ratings :domains (:domains traces)
						:longest-trace-length (apply max (doall (map #(count (:inputs %)) (map first ratings))))}))
			 (outlierness))))

(defn alternate-us-measure [alter-input]
	(doall (map (fn [o]
				 (let [traces (alter-traces alter-input (:traces o))]
					 {:trial (:trial o)
						:rating (:rating o) :domains (:domains traces)
						:traces (:traces traces)
						:longest-trace-length (apply max (doall (map #(count (:inputs %)) (:traces traces))))}))
			 (uniqueness))))

(def d-measure-regular (alternate-d-measure identity))
(def os-measure-regular (alternate-os-measure identity))
(def us-measure-regular (alternate-us-measure identity))

;; alternate gamalyzer encodings

(defn intent-majorize-input [i]
	(if-let [[Proc Selection Choice] (:det i)]
		(if (= Choice :socialize)
			(let [[[Initiator [Intent Game _Realization]] Target _Player] (:vals i)
						out
						(assoc i
							:det (list Proc Selection Intent)
							:vals (list [Initiator Target Game]))]
				out)
			i)
		i))

(def d-measure-intent-major (alternate-d-measure intent-majorize-input))
(def os-measure-intent-major (alternate-os-measure intent-majorize-input))
(def us-measure-intent-major (alternate-us-measure intent-majorize-input))

(defn game-majorize-input [i]
	(if-let [[Proc Selection Choice] (:det i)]
		(if (= Choice :socialize)
			(let [[[Initiator [Intent Game _Realization]] Target _Player] (:vals i)
						out
						(assoc i
							:det (list Proc Selection Game)
							:vals (list [Initiator Target Intent]))]
				out)
			i)
		i))

(def d-measure-game-major (alternate-d-measure game-majorize-input))
(def os-measure-game-major (alternate-os-measure game-majorize-input))
(def us-measure-game-major (alternate-us-measure game-majorize-input))

(defn i>g>t-input [i]
	(if-let [[Proc Selection Choice] (:det i)]
		(if (= Choice :socialize)
			(let [[[Initiator [Intent Game _Realization]] Target _Player] (:vals i)
						]
				(assoc i
					:vals (list Initiator (list Intent Game) Target)))
			i)
		i))

(def d-measure-i>g>t (alternate-d-measure i>g>t-input))
(def os-measure-i>g>t (alternate-os-measure i>g>t-input))
(def us-measure-i>g>t (alternate-us-measure i>g>t-input))

(defn i=g=t-input [i]
	(if-let [[Proc Selection Choice] (:det i)]
		(if (= Choice :socialize)
			(let [[[Initiator [Intent Game _Realization]] Target _Player] (:vals i)
						]
				(assoc i
					:vals (list [Initiator (list Game Intent) Target])))
			i)
		i))


(def d-measure-i=g=t (alternate-d-measure i=g=t-input))
(def os-measure-i=g=t (alternate-os-measure i=g=t-input))
(def us-measure-i=g=t (alternate-us-measure i=g=t-input))

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

(defn analyze-dissimilarity [d-measure metfn metname]
	(let [trials (doall (do-trials #(dissimilarities-trial % metfn metname) d-measure))
				rms-err (rms-error trials)]
		(println "err" rms-err)
		{:metric metname
		 :rms-err rms-err
		 :ms-err (ms-error trials)
		 :mean-err (mean-error trials)
		 :mean-abs-err (mean-abs-error trials)
		 :trials trials}))

(defn analyze-dissimilarity-find-w [d-measure metfn metname]
	(let [longest-trace-length (apply max (map :longest-trace-length d-measure))
				ws (doall
						(map (fn [w]
									 (println "dissimilarity try w =" w)
									 (assoc (with-warp-window w (analyze-dissimilarity d-measure metfn metname)) :w w))
								 (range 1 (inc longest-trace-length))))]
		(println "done with parameter exploration")
		(apply min-key :rms-err ws)))

;; n-gram-specific hack for parameter search
(defn analyze-dissimilarity-find-n [d-measure metfn metname]
	(let [ns (doall
						(map (fn [n]
									 (println "dissimilarity try n =" n)
									 (assoc (analyze-dissimilarity d-measure (partial metfn n) metname) :n n))
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

#_(defn kth-neighbor-distances [k traces metfn]
	(into {} (doall (map (fn [t1]
												 ; get distances to every other trace
												 ; sort descending
												 ; pick #k
												 (let [distances (doall (map (fn [t2] (metfn t1 t2)) traces))
															 sorted-distances (reverse (sort distances))]
													 [t1 (nth sorted-distances (dec k))]))
											 traces))))

#_(defn outlierness-trial [t k metfn metname]
	(let [ratings (:ratings t)
				traces (keys ratings)
				domains (:domains t)
				distances (kth-neighbor-distances k traces (fn [a b] (min 1 (metfn a b domains))))]
		(doall (map (fn [[trace d]]
										 (let [rating (get ratings trace)]
											 {:metric metname
												:prediction d :human rating :error (- d rating)
												:trace (standardize-id (:id trace))}))
								distances))))

(defn analyze-outlierness [os-measure k metfn metname]
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

(defn analyze-outlierness-find-k [os-measure metfn metname]
	(let [largest-trace-count (apply max (map #(count (:ratings %)) os-measure))
				param-sets
				(doall (map (fn [k]
											(println "outlierness try k =" k)
											(analyze-outlierness os-measure k metfn metname))
										(range 1 5)))]
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


#_(defn uniqueness-trial [t k metfn metname]
	(let [rating (:rating t)
				traces (:traces t)
				domains (:domains t)
				distances (kth-neighbor-distances k traces (fn [a b] (min 1 (metfn a b domains))))
				sum-distance (sum (map second distances))
				sample-count (count traces)
				d (/ sum-distance sample-count)]
		{:metric metname
		 :prediction d :human rating :error (- d rating)
		 :traces (map #(standardize-id (:id %)) traces)}))

(defn analyze-uniqueness [us-measure k metfn metname]
	(let [trials (do-trials #(uniqueness-trial % k metfn metname) us-measure)
				rms-err (rms-error trials)]
		{:k k
		 :metric metname
		 :rms-err rms-err
		 :ms-err (ms-error trials)
		 :mean-err (mean-error trials)
		 :mean-abs-err (mean-abs-error trials)
		 :trials trials}))

(defn analyze-uniqueness-find-k [us-measure metfn metname]
	(let [longest-trace-length (apply max (map :longest-trace-length us-measure))
				largest-trace-count (apply max (map #(count (:traces %)) us-measure))
				param-sets
				(doall (mapcat (fn [k]
												 (println "uniqueness try k =" k)
												 (analyze-uniqueness us-measure k metfn metname))
											 (range 1 (max 3 (int (/ largest-trace-count 5))))))]
		(println "done with parameter exploration")
		(apply min-key :rms-err param-sets)))

;; baseline support. Only use with non-altered trace sets.

(defn just-action-names [name-type trace]
	(map #(str (case name-type
							 :game (second (second (first (:vals %))))
							 :intent (first (second (first (:vals %))))))
			 (:inputs trace)))

(defn int-frequencies [l]
	(into {} (map (fn [[k v]] [k (int v)])
								(frequencies l))))

(def ngram-count
	(mem/memo
	 (fn [name-type n t]
		 (int-frequencies (map #(join "__" %)
													 (partition n 1 (just-action-names name-type t)))))))

(defn ngram-distance [name-type n a b]
	(Baseline/ngramDistance (ngram-count name-type n a)
													(ngram-count name-type n b)))

(defn action-names [name-type trace]
	(map #(str
				 (first (first (:vals %))) "_"
				 (case name-type
					 :intent (first (second (first (:vals %))))
					 :game (second (second (first (:vals %))))) "_"
				 (second (:vals %)))
			 (:inputs trace)))

(def action-count
	(mem/memo
	 (fn [name-type t] (int-frequencies (action-names name-type t)))))

(defn action-distance [name-type metr a b]
	(case metr
		:angular
		(Baseline/intentCosineDissimilarity (action-count name-type a) (action-count name-type b))
		:manhattan
		(Baseline/ngramDistance (action-count name-type a) (action-count name-type b))))

(defn find-file [root nom]
	(first (filter #(= (.getName %) nom) (file-seq (file root)))))

(def game-state
	(mem/memo
	 (fn [t]
		 (let [id (standardize-id (:id t))
					 fl (find-file "../data/FinalStates" (str id ".csv"))]
			 (with-open [r (reader fl)]
					 (let [csv (parse-csv (reader fl))
								 values (map #(case %
																"true" 100
																"false" 0
																(read-string %))
														 (filter #(not= % "") (flatten (rest (map rest csv)))))]
						 (int-array values)))))))

(defn square [a] (* a a))

(defn state-distance [metr a b]
	(case metr
		:angular
		(Baseline/stateCosineDissimilarity (game-state a) (game-state b))
		:manhattan
		(let [sa (game-state a)
					sb (game-state b)
					deltas (map (fn [ai bi] (abs (- ai bi))) sa sb)
					maximum (* 100 (count sa))]
			(/ (sum deltas) maximum))
		:euclidean
		(let [sa (game-state a)
					sb (game-state b)
					squares (map (fn [ai bi] (square (- ai bi))) sa sb)
					maximum (sqrt (* (count sa) (square 100)))]
			(/ (sqrt (sum squares)) maximum))
		))

;(ngram-count 1 (first (:traces (first us-measure))))
;(intent-count (first (:traces (first us-measure))))
;(into [] (game-state (first (:traces (first us-measure)))))

;; analysis

(defn analyze-glz [d-m o-m u-m]
	(doall
		(let [ad (analyze-dissimilarity-find-w d-m distance :glz)
					w (:w ad)
					ao (with-warp-window w (analyze-outlierness-find-k o-m distance :glz))
					k (:k ao)
					au (with-warp-window w (analyze-uniqueness u-m k distance :glz))]
			[{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
				:ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
				:mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
				:mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
			 {:w w :k k}
			 {:ad ad :ao ao :au au}])))

(def all-results
	{
	 ;regular means i=g>t
	 :glz-i=g>t (analyze-glz d-measure-regular os-measure-regular us-measure-regular)
	 :glz-i>g>t (analyze-glz d-measure-i>g>t os-measure-i>g>t us-measure-i>g>t)
	 :glz-i=g=t (analyze-glz d-measure-i=g=t os-measure-i=g=t us-measure-i=g=t)
	 :glz-game-major (analyze-glz d-measure-game-major os-measure-game-major us-measure-game-major)
	 :glz-intent-major (analyze-glz d-measure-intent-major os-measure-intent-major us-measure-intent-major)

	 ;;; n-gram-game
	 :n-gram-game
	 (let [ad (analyze-dissimilarity-find-n d-measure-regular (fn [n a b _doms] (ngram-distance :game n a b)) :n-gram)
				 n (:n ad)
				 ao (analyze-outlierness-find-k os-measure-regular (fn [a b _doms] (ngram-distance :game n a b)) :n-gram)
				 k (:k ao)
				 au (analyze-uniqueness us-measure-regular k (fn [a b _doms] (ngram-distance :game n a b)) :n-gram)]
		 [{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
			 :ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
			 :mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
			 :mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
			{:n n :k k}
			{:ad ad :ao ao :au au}])

	 ;;; n-gram-intent
	 :n-gram-intent
	 (let [ad (analyze-dissimilarity-find-n d-measure-regular (fn [n a b _doms] (ngram-distance :intent n a b)) :n-gram)
				 n (:n ad)
				 ao (analyze-outlierness-find-k os-measure-regular (fn [a b _doms] (ngram-distance :intent n a b)) :n-gram)
				 k (:k ao)
				 au (analyze-uniqueness us-measure-regular k (fn [a b _doms] (ngram-distance :intent n a b)) :n-gram)]
		 [{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
			 :ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
			 :mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
			 :mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
			{:n n :k k}
			{:ad ad :ao ao :au au}])

	 :action-intent-angular
	 (let [ad (analyze-dissimilarity d-measure-regular (fn [a b _doms] (action-distance :intent :angular a b)) :intent)
				 ao (analyze-outlierness-find-k os-measure-regular (fn [a b _doms] (action-distance :intent :angular a b)) :intent)
				 k (:k ao)
				 au (analyze-uniqueness us-measure-regular k (fn [a b _doms] (action-distance :intent :angular a b)) :intent)]
		 [{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
			 :ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
			 :mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
			 :mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
			{:k k}
			{:ad ad :ao ao :au au}])

	 :action-game-angular
	 (let [ad (analyze-dissimilarity d-measure-regular (fn [a b _doms] (action-distance :game :angular a b)) :intent)
				 ao (analyze-outlierness-find-k os-measure-regular (fn [a b _doms] (action-distance :game :angular a b)) :intent)
				 k (:k ao)
				 au (analyze-uniqueness us-measure-regular k (fn [a b _doms] (action-distance :game :angular a b)) :intent)]
		 [{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
			 :ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
			 :mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
			 :mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
			{:k k}
			{:ad ad :ao ao :au au}])

	 :action-intent-manhattan
	 (let [ad (analyze-dissimilarity d-measure-regular (fn [a b _doms] (action-distance :intent :manhattan a b)) :intent)
				 ao (analyze-outlierness-find-k os-measure-regular (fn [a b _doms] (action-distance :intent :manhattan a b)) :intent)
				 k (:k ao)
				 au (analyze-uniqueness us-measure-regular k (fn [a b _doms] (action-distance :intent :manhattan a b)) :intent)]
		 [{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
			 :ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
			 :mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
			 :mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
			{:k k}
			{:ad ad :ao ao :au au}])

	 :action-game-manhattan
	 (let [ad (analyze-dissimilarity d-measure-regular (fn [a b _doms] (action-distance :game :manhattan a b)) :intent)
				 ao (analyze-outlierness-find-k os-measure-regular (fn [a b _doms] (action-distance :game :manhattan a b)) :intent)
				 k (:k ao)
				 au (analyze-uniqueness us-measure-regular k (fn [a b _doms] (action-distance :game :manhattan a b)) :intent)]
		 [{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
			 :ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
			 :mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
			 :mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
			{:k k}
			{:ad ad :ao ao :au au}])

	 :state-angular
	 (let [ad (analyze-dissimilarity d-measure-regular (fn [a b _doms] (state-distance :angular a b)) :state)
				 ao (analyze-outlierness-find-k os-measure-regular (fn [a b _doms] (state-distance :angular a b)) :state)
				 k (:k ao)
				 au (analyze-uniqueness us-measure-regular k (fn [a b _doms] (state-distance :angular a b)) :state)]
		 [{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
			 :ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
			 :mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
			 :mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
			{:k k}
			{:ad ad :ao ao :au au}])

	 :state-manhattan
	 (let [ad (analyze-dissimilarity d-measure-regular (fn [a b _doms] (state-distance :manhattan a b)) :state)
				 ao (analyze-outlierness-find-k os-measure-regular (fn [a b _doms] (state-distance :manhattan a b)) :state)
				 k (:k ao)
				 au (analyze-uniqueness us-measure-regular k (fn [a b _doms] (state-distance :manhattan a b)) :state)]
		 [{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
			 :ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
			 :mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
			 :mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
			{:k k}
			{:ad ad :ao ao :au au}])

	 :state-euclidean
	 (let [ad (analyze-dissimilarity d-measure-regular (fn [a b _doms] (state-distance :euclidean a b)) :state)
				 ao (analyze-outlierness-find-k os-measure-regular (fn [a b _doms] (state-distance :euclidean a b)) :state)
				 k (:k ao)
				 au (analyze-uniqueness us-measure-regular k (fn [a b _doms] (state-distance :euclidean a b)) :state)]
		 [{:rms {:ad (:rms-err ad) :ao (:rms-err ao) :au (:rms-err au)}
			 :ms {:ad (:ms-err ad) :ao (:ms-err ao) :au (:ms-err au)}
			 :mean {:ad (:mean-err ad) :ao (:mean-err ao) :au (:mean-err au)}
			 :mean-abs {:ad (:mean-abs-err ad) :ao (:mean-abs-err ao) :au (:mean-abs-err au)}}
			{:k k}
			{:ad ad :ao ao :au au}])
	 })

(:state-manhattan all-results)

(defn write-results []
	(let [concise-results (into {} (map (fn [[k [r p _ts]]]
																				[k {:rms (:rms r) :mae (:mean-abs r) :me (:mean r) :params p}])
																			all-results))
				csv (write-csv (concat
												[["metric" "rms-diss" "rms-outlier" "rms-unique" "mae-diss" "mae-outlier" "mae-unique" "me-diss" "me-outlier" "me-unique" "w" "n" "k"]]
												(mapv (fn [[k v]]
																(mapv str [k
																					 (:ad (:rms v))
																					 (:ao (:rms v))
																					 (:au (:rms v))
																					 (:ad (:mae v))
																					 (:ao (:mae v))
																					 (:au (:mae v))
																					 (:ad (:me v))
																					 (:ao (:me v))
																					 (:au (:me v))
																					 (:w (:params v))
																					 (:n (:params v))
																					 (:k (:params v))
																					 ]))
															concise-results)))]
		(with-open [w (writer "results.csv")]
			(.write w csv))))

(write-results)
