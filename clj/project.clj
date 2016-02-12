(defproject poc "0.1.0-SNAPSHOT"
  :description "Project on Chess / Proof of Concept"
  :url "https://github.com/piotr-yuxuan/PoC"
  :license {:name "GNU GPL v3+"
            :url "http://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [net.mikera/core.matrix "0.49.0"]
                 [org.clojure/core.logic "0.8.10"]
                 [org.clojure/tools.cli "0.3.3"]
                 [instaparse "1.4.1"]]
  :main ^:skip-aot poc.core
  :jvm-opts ^:replace []
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
