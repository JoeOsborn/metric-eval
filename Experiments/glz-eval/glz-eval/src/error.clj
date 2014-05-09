(ns error
	(:require [clojure.math.numeric-tower :refer [abs ceil floor sqrt]]))

(defn sum [l] (reduce + 0 l))

(defn avg [l] (/ (sum l) (count l)))

(defn square [n] (* n n))

(defn mean-error [results]
	(if (map? results)
		(:error results)
		(avg (map :error (flatten results)))))

(defn mean-abs-error [results]
	(if (map? results)
		(abs (:error results))
		(avg (map #(abs (:error %)) (flatten results)))))

(defn ms-error [results]
	(let [rs (flatten results)
				n (count rs)
				sum (sum (map square (map :error rs)))]
		(/ sum n)))

(defn rms-error [results]
	(sqrt (ms-error results)))

(defn error [results]
	(rms-error results))

