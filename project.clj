(defproject euview "1.0.1"
  :description "euview"
  :url "http://buttware.com/euview"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [seesaw "1.4.5"]
                 [net.mikera/imagez "0.12.0"]
                 [org.apache.commons/commons-compress "1.13"]]
  :jvm-opts ["-Xmx8g"]
  :main ^:skip-aot euview.core
  :target-path "target/%s"
  :java-source-paths ["src/java"]
  :profiles {:uberjar {:aot :all}})
