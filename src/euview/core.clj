(ns euview.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [euview.savegame :as savegame]
            [euview.gui :as gui]
            [euview.parse :as parse]
            [euview.render :as render]))

(defn france-savegame []
  (-> "France1739_06_08.eu4"
      io/resource
      io/input-stream
      gui/input-stream->eu4-file
      parse/parse-file
      savegame/process-savegame))

(defn -main
  [& args]
  (if (= (first args) "headless")
    (render/render-gif (france-savegame) "resources/Europa Universalis IV" "target/headless-france.gif")
    (gui/show-panel)))
