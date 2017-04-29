(ns euview.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn zip-contents->eu4-file [c]
  ;; wat
  (loop []
    (let [f (.getNextEntry c)]
      (if (= (.getName f) "game.eu4")
        (slurp c)
        (recur)))))

(defn resource->eu4-file [resource]
  (let [z (java.util.zip.ZipInputStream. (io/input-stream resource))]
    (zip-contents->eu4-file z)))

(defn france []
  (resource->eu4-file (io/resource "France1449_01_25.eu4")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [f (france)]
    ))
