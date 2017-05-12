(ns euview.render
  (:import [java.awt Color Font AlphaComposite Image]
           java.awt.image.BufferedImage
           com.github.gif.AnimatedGifEncoder)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [mikera.image.core :as image]))

(defn load-map []
  (javax.imageio.ImageIO/read (io/resource "provinces.bmp")))

(def ocean-color (Color. 60 120 250))
(defn draw-oceans [frame]
  (doto (.createGraphics frame)
    (.setColor ocean-color)
    (.fillRect 0 0 (.getWidth frame) (.getHeight frame))
    .dispose))

(def transparent-color (Color. 0 0 0))
(defn draw-transparent [frame]
  (doto (.createGraphics frame)
    (.setColor transparent-color)
    (.fillRect 0 0 (.getWidth frame) (.getHeight frame))
    .dispose))

(defn write-year [frame ymd]
  (doto (.createGraphics frame)
    (.setColor (Color. 1 1 1))
    (.setFont (Font. "SansSerif" Font/BOLD 14))
    (.drawString (str ymd) 50 50)
    .dispose))

(defn ymd-< [x y]
  (neg? (compare x y)))
(defn ymd-<= [x y]
  (not (pos? (compare x y))))

(defn latest-new-owner [province start-ymd end-ymd]
  (->> province
       :owners
       (filter #(and
                 (ymd-< start-ymd (first %))
                 (ymd-<= (first %) end-ymd)))
       (sort-by first)
       last
       second))

(def ac (AlphaComposite/getInstance AlphaComposite/DST_IN))
(defn render-owner [province country-colors frame owner]
  (if-let [o (:overlay province)]
    (let [owner-color (country-colors owner)
          g (.createGraphics o)
          fg (.createGraphics frame)]
      (println "rendering" (:name province) "for" owner owner-color)
      (.setComposite g ac)
      (.setColor g owner-color)
      (.fillRect g 0 0 (.getWidth o) (.getHeight o))
      (.drawImage fg o (:overlay-x province) (:overlay-y province) nil)
      (.dispose fg)
      (.dispose g))))

(defn add-initial-frame [{:keys [width
                                 height
                                 encoder
                                 provinces
                                 country-colors] :as params}
                         ymd]
  (println (country-colors "FRA"))
  (let [frame (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)]
    (draw-oceans frame)
    (write-year frame ymd)
    (doseq [province provinces]
      (when-let [o (:initial-owner province)]
        (render-owner province country-colors frame o)))
    ;; ????????????? REMOVING THIS BREAKS EVERYTHING    
    (doto (.createGraphics frame)
      (.setColor (Color. 2 3 4))
      (.fillRect 0 0 1 1)
      .dispose)
    (.addFrame encoder frame)))

(defn add-delta-frame [{:keys [width
                               height
                               encoder
                               provinces
                               country-colors]}
                       start-ymd
                       end-ymd]
  (println "New frame::" end-ymd)
  (let [frame (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)]
    (draw-transparent frame)
    (doto (.createGraphics frame)
      (.setColor ocean-color)
      (.fillRect 0 0 200 100)
      .dispose)
    (write-year frame end-ymd)
    (doseq [province provinces]
      (when-let [owner (latest-new-owner province start-ymd end-ymd)]
        (render-owner province country-colors frame owner)))
    (.addFrame encoder frame)))

(defn base-gif [gif-filename]
  (doto (AnimatedGifEncoder.)
    (.setDelay 10)
    (.setRepeat 0)
    (.setDispose 1)
    (.setTransparent transparent-color)
    (.start gif-filename)))

(defn add-frames [{:keys [encoder provinces colors] :as params} start-ymd end-ymd]
  (add-initial-frame params start-ymd)
  (if (= (first start-ymd) (first end-ymd))
    (add-delta-frame params start-ymd end-ymd)
    (do
      (add-delta-frame params start-ymd [(inc (first start-ymd)) 1 1])
      (doseq [year (range (inc (first start-ymd)) (first end-ymd))]
        (add-delta-frame params [year 1 1] [(inc year) 1 1]))
      (add-delta-frame params [(first end-ymd) 1 1] end-ymd))))

(defn da-min [x y]
  (if x
    (min x y)
    y))

(defn da-max [x y]
  (if x
    (max x y)
    y))
(defn colors->borders [map]
  (let [tops (atom {})
        bottoms (atom {})
        lefts (atom {})
        rights (atom {})]
    (doseq [x (range (.getWidth map))
            y (range (.getHeight map))]
      (let [rgb (.getRGB map x y)
            r (bit-shift-right (bit-and 0xff0000 rgb) 16)
            g (bit-shift-right (bit-and 0xff00 rgb) 8)
            b (bit-and 0xff rgb)
            c (Color. r g b)]
        (swap! tops update c da-min y)
        (swap! bottoms update c da-max y)
        (swap! lefts update c da-min x)
        (swap! rights update c da-max x)))
    (println (count @lefts))
    (juxt @lefts @rights @tops @bottoms)))

(defn add-overlays [provinces map scale-factor]
  (let [loaded (slurp (io/resource "definition.csv"))
        lines (drop 1 (string/split-lines loaded))
       definitions (into {} (for [line lines]
                             (let [[_ pid r g b name] (re-matches #"(\d+);(\d+);(\d+);(\d+);(.*)" line)]
                               [(- (Long/parseLong pid)) ;; lol
                                {:color (Color. (Long/parseLong r)
                                                (Long/parseLong g)
                                                (Long/parseLong b))
                                 :name name}])))
        borders (colors->borders map)]
    (for [p (sort-by :pid provinces)]
      (let [{:keys [color name]} (definitions (:pid p))
            [x1 x2 y1 y2] (borders color)]
        (if (and x1 (< (- x2 x1) 100) (< (- y2 y1) 100))
          (let [frame (BufferedImage. (int (/ (inc (- x2 x1)) scale-factor))
                                      (int (/ (inc (- y2 y1)) scale-factor))
                                      BufferedImage/TYPE_INT_ARGB)]
            (doto (.createGraphics frame)
              (.setColor (Color. 255 255 255 0))
              (.fillRect 0 0 (.getWidth frame) (.getHeight frame))
              .dispose)
            (doto (.createGraphics frame)
              (.setColor color)
              (.fillRect 0 0 (.getWidth frame) (.getHeight frame))
              .dispose)
            (assoc p
                   :name name
                   :overlay frame
                   :overlay-x (int (/ x1 scale-factor))
                   :overlay-y (int (/ y1 scale-factor))))
          (do
;;            (println "Failed to find province on map" (:pid p) color)
            (assoc p :overlay nil :overlay-x 0 :overlay-y 0)))))))

(def scale-factor 2)
(defn render-gif [{:keys [provinces start-ymd end-ymd country-colors] :as savegame} gif-filename]
  (let [map (load-map)
        provinces (add-overlays provinces map scale-factor)
        encoder (base-gif gif-filename)]
    (add-frames {:encoder encoder
                 :width (int (/ (.getWidth map) scale-factor))
                 :height (int (/ (.getHeight map) scale-factor))
                 :provinces provinces
                 :country-colors country-colors}
                start-ymd
                end-ymd)
    (.finish encoder)))