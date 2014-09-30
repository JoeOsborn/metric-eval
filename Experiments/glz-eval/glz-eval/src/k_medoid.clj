(ns k_medoid
	(:require [clojure.math.numeric-tower :refer [abs ceil floor sqrt]]
						[clojure.core.memoize :as mem]))


; we'll solve exactly for now

(defn resolve-clustering [samples medoids distfn]
	;find each sample's min-medoid
	(let [clusters (group-by (fn [s] (apply min-key (fn [m] (distfn m s)) medoids)) samples)
				;build an annotated map {:weight weight :medoid m :samples samples} for each medoid
				annotated-clusters (map (fn [[m ss]]
																	(let [annotated-samples (map (fn [s] {:distance (distfn m s)
																																				:sample s})
																															 ss)]
																		{:weight (reduce (fn [wt s] (+ wt (:distance s)))
																										 0 annotated-samples)
																		 :medoid m
																		 :samples annotated-samples}))
																clusters)]
		; return an annotated map with the total weight and the annotated clusters
		{:weight (reduce + (map :weight annotated-clusters))
		 :clusters annotated-clusters}))

(defn k-medoids- [k samples medoids distfn]
	(if (= k 0)
		; for each medoid, gather up the samples closest to it
		; and record the total weight
		[(resolve-clustering samples medoids distfn)]
		; get all the clusterings below here
		(mapcat #(k-medoids- (dec k) samples (conj medoids %) distfn)
						samples)))

(defn k-medoids [k samples distfn]
	(let [mem (mem/memo distfn)
				clusterings (k-medoids- k samples [] mem)]
		(mem/memo-clear! mem)
		(apply min-key #(:weight %) clusterings)))

