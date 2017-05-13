(ns euview.gui
  (:use [seesaw.core]
        [seesaw.chooser]
        [clojure.java.io :as io]))

(defn find-eu4-folder []
  (let [candidate-paths ["/Users/gregor/code/euview/resources"
                         "/Users/gregor/Library/Application Support/Steam/SteamApps/common/Europa Universalis IV"]]
    (or
     (first (filter #(.isDirectory (io/file %)) candidate-paths))
     "C:/Program Files/EU4")))

(defn find-eu4-savegame []
  (let [candidate-directories ["/Users/gregor/code/euview/resources"
                               "/Users/gregor/Documents/Paradox Interactive/Europa Universalis IV"]
        directory (first (filter #(.isDirectory (io/file %)) candidate-directories))]
    (or directory "C:/Program FIles/EU.eu4")))

(defn panel []
  (let [eu4-folder (text :text (find-eu4-folder))
        savegame-location (text :text (find-eu4-savegame))
        output-gif-location (text :text "BLEH.gif")]
    (grid-panel
     :columns 3
     :items ["EU4 Folder" eu4-folder (button :text "Pick EU4 Folder"
                                             :listen [:action (fn [e]
                                                                (if-let [f (choose-file :selection-mode :dirs-only)]
                                                                  (text! eu4-folder f)))])
             "Savegame" savegame-location (button :text "Pick Savegame"
                                                  :listen [:action (fn [e]
                                                                     (if-let [f (choose-file)]
                                                                       (text! savegame-location f)))])
             "Scale" "1/2" ""
             "Interval" "1 year" ""
             "Frame delay" "10ms" ""
             "Output Gif Location" output-gif-location (button :text "Pick Gif Location"
                                                               :listen [:action (fn [e]
                                                                                  (if-let [f (choose-file :selection-mode :dirs-only)]
                                                                                    (text! output-gif-location f)))])
             "" "" (button :text "DO IT"
                           :listen [:action (fn [] (println "IT IS DONE"))])])))

(defn show-panel []
  (-> (frame :title "Hello"
             :content panel
             :on-close :exit)
      pack!
      show!))