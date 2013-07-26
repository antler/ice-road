(ns ice-road.file
  (:require [ice-road.protocol :as protocol]
            [clojure.string :as string]))

(defn canonical-path
  [road-path]
  (reduce (fn [path el]
            (cond (= el "..") (butlast path)
                  (#{"" "."} el) path
                  :default (concat path [el])))
          []
          road-path))

(defrecord Filepath
    [path extension]
  protocol/Road
  (to-string [this] (str (string/join java.io.File/separatorChar path)
                         (when (not (contains? #{nil "" []} extension))
                           (str \. extension))))
  (as-object [this] (-> this protocol/to-string java.io.File.))
  (under? [this that] (let [this-path (canonical-path path)
                            that-path (canonical-path (:path that))]
                        (and (> (count this-path)
                                (count that-path))
                             (= (take (count that-path) this-path)
                                that-path))))
  (absolute? [this] (or (= (first path) "")
                        (= path [])))
  (root? [this] (#{[""] []} path))
  (component [this] (str (last path) (when extension (str \. extension))))
  (extension [this] extension)
  (parameters [this] nil)
  (append [this that] (map->Filepath {:path (concat (reverse
                                                     (drop-while empty?
                                                                 (reverse
                                                                  path)))
                                                    (drop-while empty?
                                                                (:path that)))
                                      :extension (:extension that)}))
  (equivalent? [this that] (and (= extension (:extension that))
                               (= (canonical-path path)
                                  (canonical-path (:path that)))))
  (parent [this] (map->Filepath {:path (butlast path)
                                 :extension nil})))

(defn <-string
  [file]
  (let [sep java.io.File/separator
        extension-re (re-pattern (str ".*\\.([^" sep "]+)$"))
        extension (get (re-matches extension-re file) 1)
        path-re (re-pattern (str "(.*)\\." extension))
        path-str (if extension
                   (get (re-matches path-re file) 1)
                   file)
        path (string/split path-str (re-pattern sep))
        path (if (= (last path-str) (first sep))
               (concat path [""])
               path)]
    (map->Filepath {:path path
                    :extension extension})))
