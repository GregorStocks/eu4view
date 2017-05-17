(ns euview.gui
  (:import java.util.zip.ZipFile
           org.apache.commons.compress.archivers.zip.ZipArchiveInputStream)
  (:use [seesaw.core]
        [seesaw.chooser])
  (:require [clojure.java.io :as io]
            [euview.savegame :as savegame]
            [euview.parse :as parse]
            [euview.render :as render]))

(defn find-eu4-folder []
  (let [candidate-paths ["/Users/gregor/code/euview/resources"
                         "C:/Program Files (x86)/Steam/SteamApps/common/Europa Universalis IV"
                         "/Users/gregor/Library/Application Support/Steam/SteamApps/common/Europa Universalis IV"]]
    (or
     (first (filter #(.isDirectory (io/file %)) candidate-paths))
     "C:/Program Files/Steam/SteamApps/common/Europa Universalis IV")))

(defn find-eu4-savegame []
  (let [candidate-directories ["/Users/gregor/code/euview/resources"
                               (str (System/getProperty "user.home") "/Documents/Paradox Interactive/Europa Universalis IV/save games")]
        directory (first (filter #(.isDirectory (io/file %)) candidate-directories))]
    (or directory "C:/Program Files/EU.eu4")))

(defn get-map [input-stream]
  (let [z (ZipArchiveInputStream. input-stream)]
    (loop []
      (let [f (.getNextEntry z)]
        (cond
          (= (.getName f) "provinces.bmp") (javax.imageio.ImageIO/read z)
          (not f) (throw (ex-info "Map not found" {}))
          :else (recur))))))

(defn unzip-savegame [f]
  (let [z (ZipFile. f)]
    (merge
     {:savegame (slurp (.getInputStream z (.getEntry z "game.eu4")))}
     (when-let [r (.getEntry z "rnw.zip")]
       (let [rnw (.getInputStream z r)
             map (get-map rnw)]
         (println "Neat I found a map" rnw map)
         {:map map})))))

(defn try-rendering [eu4-folder savegame-location output-gif-location error-text]
  (text! error-text "RENDERING!!!!!!!!!!!!!!!!!!!!!!")
  (try
    (let [unzipped (unzip-savegame (io/file savegame-location))]
      (when-let [savegame (try (savegame/process-savegame
                                (parse/parse-file (:savegame unzipped))))]
        (try
          (render/render-gif savegame (:map unzipped) eu4-folder output-gif-location)
          (text! error-text "ok i've done it")
          (catch Exception e
            (println e)
            (text! error-text (str "Error rendering gif: " e))
            nil))))
    (catch Exception e
      (println e)
      (text! error-text "Error loading savegame: " e)
      nil)))

(defn panel []
  (let [eu4-folder (text :text (find-eu4-folder))
        savegame-location (text :text (find-eu4-savegame))
        output-gif-location (text :text (str (System/getProperty "user.home") "/Desktop/eu4view.gif"))
        error-text (label)]
    (grid-panel
     :columns 3
     :items ["EU4 Folder" eu4-folder (button :text "Pick EU4 Folder"
                                             :listen [:action (fn [e]
                                                                (if-let [f (choose-file
                                                                            :selection-mode :dirs-only
                                                                            :filters [["Folders" #(.isDirectory %)]]
                                                                            :dir (.getText eu4-folder))]
                                                                  (text! eu4-folder (.getPath f))))])
             "Savegame" savegame-location (button :text "Pick Savegame"
                                                  :listen [:action (fn [e]
                                                                     (if-let [f (choose-file
                                                                                 :filters [["EU4 Savegames" ["eu4"]]]
                                                                                 :dir (.getText savegame-location))]
                                                                       (text! savegame-location (.getPath f))))])
             "Scale" "1/2" ""
             "Interval" "1 year" ""
             "Frame delay" "10ms" ""
             "Output Gif Location" output-gif-location (button :text "Pick Gif Location"
                                                               :listen [:action (fn [e]
                                                                                  (if-let [f (choose-file :dir "."
                                                                                                          :filters [["GIFs" ["gif"]]])]
                                                                                    (text! output-gif-location (.getPath f))))])
             error-text "" (button :text "DO IT"
                                   :listen [:action (fn [e] (try-rendering (.getText eu4-folder)
                                                                           (.getText savegame-location)
                                                                           (.getText output-gif-location)
                                                                           error-text))])])))

(defn show-panel []
  (-> (frame :title "Hello"
             :content (panel)
             :on-close :exit)
      pack!
      show!))