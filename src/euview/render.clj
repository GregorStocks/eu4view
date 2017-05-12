(ns euview.render
  (:import [java.awt Color Font AlphaComposite Image]
           java.awt.image.BufferedImage
           com.github.gif.AnimatedGifEncoder)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [euview.parse :as parse]
            [mikera.image.core :as image]))

(defn load-map []
  (javax.imageio.ImageIO/read (io/resource "Europa Universalis IV/map/provinces.bmp")))

(def ocean-color (Color. 60 120 250))

(def transparent-color (Color. 0x69 0x69 0x69))
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

(def ac (AlphaComposite/getInstance AlphaComposite/SRC_ATOP))
(defn render-owner [province country-colors frame owner]
  (if-let [o (:overlay province)]
    (let [owner-color (country-colors owner)
          g (.createGraphics o)
          fg (.createGraphics frame)]
      (when (not= owner-color Color/WHITE)
        (.setComposite g ac)
        (.setColor g owner-color)
        (.fill g (java.awt.geom.Rectangle2D$Double. 0 0 (.getWidth o) (.getHeight o)))
        (.dispose g)
        (.drawImage fg o (:overlay-x province) (:overlay-y province) nil)
        (.dispose fg)))))

(defn add-initial-frames [{:keys [width
                                  height
                                  encoder
                                  provinces
                                  country-colors] :as params}
                          ymd]
  (let [province-groups (map #(apply concat %)
                             (partition-all 250 (vals (group-by :initial-owner provinces))))]
    (doseq [ps province-groups]
      (let [frame (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)]
        (draw-transparent frame)
        (write-year frame ymd)
        (doseq [province ps]
          (when-let [o (:initial-owner province)]
            (render-owner province country-colors frame o)))
        (.addFrame encoder frame)))))

(defn add-delta-frames [{:keys [width
                                height
                                encoder
                                provinces
                                country-colors]}
                        start-ymd
                        end-ymd]
  (let [updates (keep (fn [province]
                        (when-let [owner (latest-new-owner province start-ymd end-ymd)]
                          [province owner]))
                      provinces)]
    (doseq [province-owners (map #(apply concat %)
                                 (partition-all 250 (vals (group-by second updates))))]
      (let [frame (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)]
        (draw-transparent frame)
        (doto (.createGraphics frame)
          (.setColor ocean-color)
          (.fillRect 0 0 200 100)
          .dispose)
        (write-year frame end-ymd)
        (doseq [[province owner] province-owners]
          (render-owner province country-colors frame owner))
        (.addFrame encoder frame)))))

(defn base-gif [gif-filename]
  (doto (AnimatedGifEncoder.)
    (.setDelay 10)
    (.setRepeat 0)
    (.setDispose 1)
    (.setTransparent transparent-color)
    (.start gif-filename)))

(defn add-frames [{:keys [encoder provinces colors] :as params} start-ymd end-ymd]
  (add-initial-frames params start-ymd)
  (if (= (first start-ymd) (first end-ymd))
    (add-delta-frames params start-ymd end-ymd)
    (do
      (add-delta-frames params start-ymd [(inc (first start-ymd)) 1 1])
      (doseq [year (range (inc (first start-ymd)) (first end-ymd))]
        (add-delta-frames params [year 1 1] [(inc year) 1 1]))
      (.setDelay encoder 20000) ;; todo make this work properly
      (add-delta-frames params [(first end-ymd) 1 1] end-ymd))))

(defn da-min [x y]
  (if x
    (min x y)
    y))

(defn da-max [x y]
  (if x
    (max x y)
    y))
(defn colors->borders [map scale-factor]
  (let [tops (atom {})
        bottoms (atom {})
        lefts (atom {})
        rights (atom {})]
    (doseq [x (range (int (/ (.getWidth map) scale-factor)))
            y (range (int (/ (.getHeight map) scale-factor)))]
      (let [rgb (.getRGB map (* x scale-factor) (* y scale-factor))
            r (bit-shift-right (bit-and 0xff0000 rgb) 16)
            g (bit-shift-right (bit-and 0xff00 rgb) 8)
            b (bit-and 0xff rgb)
            c (Color. r g b)]
        (swap! tops update c da-min y)
        (swap! bottoms update c da-max y)
        (swap! lefts update c da-min x)
        (swap! rights update c da-max x)))
    (juxt @lefts @rights @tops @bottoms)))

(defn add-overlays [provinces map scale-factor]
  (let [map-file (parse/parse-file (io/resource "Europa Universalis IV/map/default.map"))
        ocean-provinces (set (get (:variables map-file) "sea_starts"))
        _ (println "NEato" map-file ocean-provinces)
        loaded (slurp (io/resource "Europa Universalis IV/map/definition.csv"))
        lines (drop 1 (string/split-lines loaded))
        definitions (into {} (for [line lines]
                               (let [[_ pid r g b name] (re-matches #"(\d+);(\d+);(\d+);(\d+);(.*)" line)]
                                 [(- (Long/parseLong pid)) ;; lol
                                  {:color (Color. (Long/parseLong r)
                                                  (Long/parseLong g)
                                                  (Long/parseLong b))
                                   :name name}])))
        borders (colors->borders map scale-factor)]
    (for [p (sort-by :pid provinces)]
      (let [{:keys [color name]} (definitions (:pid p))
            [x1 x2 y1 y2] (borders color)]
        (if x1
          (let [frame (BufferedImage. (inc (- x2 x1))
                                      (inc (- y2 y1))
                                      BufferedImage/TYPE_INT_ARGB)]
            (doseq [x (range (.getWidth frame))
                    y (range (.getHeight frame))]
              (.setRGB frame x y (if (= (.getRGB color)
                                        (.getRGB map
                                                 (* (+ x1 x) scale-factor)
                                                 (* (+ y1 y) scale-factor)))
                                   (.getRGB Color/BLACK)
                                   ;; transparent!!
                                   0)))
            (assoc p
                   :name name
                   :overlay frame
                   :overlay-x x1
                   :overlay-y y1))
          (do
            ;;            (println "Failed to find province on map" (:pid p) color)
            (assoc p :overlay nil :overlay-x 0 :overlay-y 0)))))))

(def scale-factor 2)
(defn render-gif [{:keys [provinces start-ymd end-ymd country-colors] :as savegame} gif-filename]
  (let [map (load-map)
        provinces (doall (add-overlays provinces map scale-factor))
        encoder (base-gif gif-filename)]
    (println "Rendering gif")
    (add-frames {:encoder encoder
                 :width (int (/ (.getWidth map) scale-factor))
                 :height (int (/ (.getHeight map) scale-factor))
                 :provinces provinces
                 :country-colors country-colors}
                start-ymd
                end-ymd)
    (.finish encoder)))