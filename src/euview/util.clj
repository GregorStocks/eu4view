(ns euview.util)

(defn ymd-< [x y]
  (neg? (compare x y)))
(defn ymd-<= [x y]
  (not (pos? (compare x y))))

