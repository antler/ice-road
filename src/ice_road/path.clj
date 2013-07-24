(ns ice-road.path
  (:require [ice-road.protocol :as protocol]
            [ice-road.url :as url]
            [ice-road.file :as file]))


(defn under?
  [& paths]
  (every? (partial apply protocol/under?) (partition 2 1 paths)))

(defn append
  [path & other-paths]
  (reduce protocol/append path other-paths))

(defn equivalent?
  [path & other-paths]
  (every? (partial protocol/equivalent? path) other-paths))