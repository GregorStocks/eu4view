(ns euview.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [euview.savegame :as savegame]
            [euview.gui :as gui]
            [euview.parse :as parse]
            [euview.render :as render]))

(defn france-savegame []
  (-> "Ottomans1649_05_19.eu4"
      io/resource
      io/file
      gui/unzip-savegame))

(defn -main
  [& args]
  (if (= (first args) "headless")
    (let [s (france-savegame)]
      (println "What????")
      (render/render-gif (-> s :savegame parse/parse-file savegame/process-savegame)
                         (:map s)
                         "resources/Europa Universalis IV"
                         "target/headless-france.gif"))
    (gui/show-panel)))
