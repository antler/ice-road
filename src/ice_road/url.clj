(ns ice-road.url
  (:require [ice-road.protocol :as protocol]
            [clojure.string :as string]))

(defrecord URLpath
    [protocol path extension queries]
  protocol/Road
  (to-string [this] (str (when protocol
                           (str protocol \:))
                         "//"
                         (string/join \/ path)
                         (when extension
                           (str \. extension))
                         (when (not (contains? #{"" nil [] {}} queries))
                           (str \?
                                (string/join \&
                                             (map (partial string/join \=)
                                                  queries))))))
  (as-object [this] (-> this protocol/to-string java.net.URL.))
  (under? [this that] (and (= protocol
                              (:protocol that))
                           (= extension
                              (:extension that))
                           (> (count path)
                              (count (:path that)))
                           (= path
                              (take (count this) (:path that)))))
  (absolute? [this] (boolean protocol))
  (root? [this] false)
  (component [this] (str (last path) (when extension (str \. extension))))
  (extension [this] extension)
  (parameters [this] queries)
  (append [this that] (map->URLpath {:protocol protocol
                                     :path (concat path (:path that))
                                     :extension (:extension that)
                                     :queries (:queries that)}))
  (equivalent? [this that] (= this that))
  (parent [this] (map->URLpath {:protocol protocol
                                :path (butlast path)
                                :extension ""
                                :queries nil})))

(defn <-string
  [url]
  (let [destructured (re-matches #"([^\?]+)\??(.*)" url)
        [url-string query-string] (rest destructured)
        protocol (get (re-matches #"([a-zA-Z]+)://.*" url-string) 1)
        extension (get (re-matches #".*\.([^/\?]+)$" url-string) 1)
        address (if protocol
                  (get (re-matches #"[a-zA-Z]+://(.*)" url-string) 1)
                  url-string)
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
