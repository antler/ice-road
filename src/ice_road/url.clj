(ns ice-road.url
  (:require [ice-road.protocol :as protocol]
            [clojure.string :as string]))

(defrecord URLpath
    [protocol path extension queries]
  protocol/Road
  (to-string [this]
    (str (when protocol
           (str protocol \:))
         "//"
         (string/join \/ path)
         (when extension
           (str \. extension))
         (when (not (contains? #{"" nil [] {}} queries))
           (str \?
                (string/join \& (map (partial string/join \=) queries))))))
  (as-object [this] (-> this protocol/to-string java.net.URL.))
  (under? [this that]
    (let [this-path (remove empty? path)
          that-path (remove empty? (:path that))]
      (and (= protocol
              (:protocol that))
           (= extension
              (:extension that))
           (> (count this-path)
              (count that-path))
           (= (take (count that-path) path)
              that-path))))
  (absolute? [this] (boolean protocol))
  (root? [this] (#{[""] []} path))
  (component [this] (str (last path) (when extension (str \. extension))))
  (extension [this] extension)
  (parameters [this] queries)
  (append [this that] (map->URLpath {:protocol protocol
                                     :path (concat (reverse
                                                    (drop-while empty?
                                                                (reverse path)))
                                                   (drop-while empty?
                                                               (:path that)))
                                     :extension (:extension that)
                                     :queries (:queries that)}))
  (equivalent? [this that] (= this that))
  (parent [this] (map->URLpath {:protocol protocol
                                :path (butlast path)
                                :extension nil
                                :queries {}})))

(defn <-string
  [url]
  (let [destructured (re-matches #"([^\?]+)\??(.*)" url)
        [url-string query-string] (rest destructured)
        protocol (get (re-matches #"([a-zA-Z]+)://.*" url-string) 1)
        address (if protocol
                  (get (re-matches #"[a-zA-Z]+://(.*)" url-string) 1)
                  url-string)
        extension (get (re-matches #".*/[^/]*\.([^/\?]+)$" address) 1)
        path-str (if extension
                   (get (re-matches #"(.*[^/]+)\..*" address) 1)
                   address)
        path (string/split path-str #"/")
        path (if (= (last path-str) \/)
               (concat path [""])
               path)
        queries (if (contains? #{nil "" false []} query-string)
                  {}
                  (into {} (map (fn [q] (vec (string/split q #"=")))
                                (string/split query-string #"&"))))]
    (map->URLpath {:protocol protocol
                   :path path
                   :extension extension
                   :queries queries})))
