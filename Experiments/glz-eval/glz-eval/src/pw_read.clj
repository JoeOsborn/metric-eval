(ns pw_read
  (:require [clojure.java.io :refer [reader]]
						[clojure-csv.core :refer [parse-csv]]
            [gamalyzer.read.edn :as edn]))

(defn- trial-folder-name [p]
	(second (re-find #"([A-Za-z]+)[0-9]+" p)))

(defn- get-traces [p]
	(edn/read-log-files [(str "../data/" p)] #{} nil))

(def dissimilarity (memoize (fn []
	(reduce (fn [acc trial]
						(let [trial-name (first trial)
									folder-name (trial-folder-name trial-name)
									path (str "experiment1/" folder-name "/" trial-name)
									ref-trace (second trial)
									sample-rating (nth trial 3)
									trace-ratings (apply hash-map (rest (rest trial)))
									actual-traces (get-traces path)]
							(if (or (= sample-rating "") (= sample-rating " "))
								acc
								(conj acc {:exp :dissimilarity :trial trial-name :path path
													 :reference ref-trace
													 :ratings (into {} (map (fn [[k v]] [k (/ (dec (read-string v)) 6.0)]) trace-ratings))
													 :traces actual-traces}))))
					[]
					(rest (parse-csv (reader "../data/experiment1Data.csv")))))))

(def uniqueness (memoize (fn []
	(reduce (fn [acc trial]
						(let [trial-name (first trial)
									folder-name (trial-folder-name trial-name)
									path (str "experiment2/" folder-name "/" trial-name)
									rating (second trial)
									traces (rest (rest trial))
									actual-traces (get-traces path)]
							(if (or (= rating "") (= rating " "))
								acc
								(conj acc {:exp :uniqueness :trial trial-name :path path
													 :rating (/ (dec (read-string rating)) 6.0)
													 :traces actual-traces}))))
					[]
					(rest (parse-csv (reader "../data/experiment2Data.csv")))))))


(def outlierness (memoize (fn []
	(reduce (fn [acc trial]
						(let [trial-name (first trial)
									folder-name (trial-folder-name trial-name)
									path (str "experiment3/" folder-name "/" trial-name)
									sample-rating (nth trial 2)
									trace-ratings (apply hash-map (rest trial))
									actual-traces (get-traces path)]
							(if (or (= sample-rating "") (= sample-rating " "))
								acc
								(conj acc {:exp :outlierness :trial trial-name :path path
													 :ratings (into {} (map (fn [[k v]] [k (/ (dec (read-string v)) 6.0)]) trace-ratings))
													 :traces actual-traces}))))
					[]
					(rest (parse-csv (reader "../data/experiment3Data.csv")))))))


; (dissimilarity-trials)
