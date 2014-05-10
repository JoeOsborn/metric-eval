(defproject glz-eval "1.0.0-SNAPSHOT"
  :description "Evaluating Gamalyzer versus other play trace distance metrics."
  :url "http://github.com/JoeOsborn/glz-eval"
	:license {:name "MIT License"
						:url "http://mit-license.org"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.cache "0.6.3"]
                 [org.clojure/core.memoize "0.5.6"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 [org.clojars.achim/multiset "0.1.0-SNAPSHOT"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [net.mikera/vectorz-clj "0.17.0"]
                 [net.mikera/core.matrix "0.15.0"]
                 [net.mikera/core.matrix.stats "0.3.0"]
                 [de.uni-konstanz.inf.algo/mdsj "0.2"]
                 [apporiented.com/hierarchical-clustering "1.0"]
                 [clojure-csv/clojure-csv "2.0.1"]
								 [gamalyzer-metric "0.2.0-SNAPSHOT"]]
;  :jar-exclusions [#"(?:^|/).svn/"
;	                 #"traces/"]
;  :aot []
  :repositories {"project" {:url "file:../maven_repository"
														:username ""
														:passphrase ""}}
  :plugins []
  :source-paths ["src"]
	:java-source-paths ["java-src"])
