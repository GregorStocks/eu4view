(ns euview.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [euview.gui :as gui]))

(defn -main
  [& args]
  (gui/show-panel))
