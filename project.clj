(defproject euview "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [slingshot "0.12.2"]
                 [animated-gif-clj "0.1.0-SNAPSHOT"]
                 [net.mikera/imagez "0.12.0"]]
  :jvm-opts ["-Djava.awt.headless=true"]
  :main ^:skip-aot euview.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
