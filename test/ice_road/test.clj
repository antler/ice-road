(ns ice-road.test
  (:require [clojure.string :as string]
            [clojure.test :as test :refer [deftest is testing]]
            [ice-road.protocol :as protocol]
            [ice-road.path :as path]
            [ice-road.url :as url]
            [ice-road.file :as file]))

(defn mkf
  [path]
  (string/join java.io.File/separator path))

(deftest <-string
  (testing "url path construction"
    (is (= (url/map->URLpath {:protocol "http"
                              :path ["www.example.com" "query" "api" "index"]
                              :extension "html"
                              :queries {"OK" "true"
                                        "count" "-1"}})
           (url/<-string
            "http://www.example.com/query/api/index.html?OK=true&count=-1")))
    (is (= (url/map->URLpath {:protocol "ftp"
                              :path ["warez.ru" "steal" "hax"]
                              :extension "exe"
                              :queries {}})
           (url/<-string "ftp://warez.ru/steal/hax.exe")))
    (is (= (url/map->URLpath {:protocol "ssh"
                              :path ["twerknet.com" "jiggle" ""]
                              :extension nil
                              :queries {}})
           (url/<-string "ssh://twerknet.com/jiggle/")))
    (is (= (url/map->URLpath {:protocol nil
                              :path ["slashdot.com"]
                              :extension nil
                              :queries {}})
           (url/<-string "slashdot.com"))))
  (testing "file path construction"
    (is (= (file/map->Filepath {:path ["" "home" "antler" "todo"]
                                :extension "org"})
           (file/<-string (mkf ["" "home" "antler" "todo.org"]))))
    (is (= (file/map->Filepath {:path ["" "home" "antler" ""]
                                :extension "bashrc"})
           (file/<-string (mkf ["" "home" "antler" ".bashrc"]))))))

(deftest to-string
  (testing "url to-string"
    (is (let [url "http://www.example.com/foo?a=0&b=1"]
          (= url
             (protocol/to-string (url/<-string url)))))
    (is (let [url "http://www.google.com"]
          (= url
             (protocol/to-string (url/<-string url)))))
    (is (let [url "gopher://archive.uiuc.edu"]
          (= url
             (protocol/to-string (url/<-string url))))))
  (testing "file to-string"
    (is (let [file (mkf ["" "usr" "share" "doc" "dwm" "changelog.gz"])]
          (= file
             (protocol/to-string (file/<-string file)))))
    (is (let [file (mkf ["" "usr" "local" "lib"])]
          (= file
             (protocol/to-string (file/<-string file)))))
    (is (let [file (mkf ["" "var" "cache" "apt"])]
          (= file
             (protocol/to-string (file/<-string file)))))
    (is (let [file (mkf ["" "usr" ".." "bin" "." "ls"])]
          (= file
             (protocol/to-string (file/<-string file)))))))

(deftest as-object
  (testing "url as object"
    (is (= (-> "http://metafilter.com"
               url/<-string
               protocol/as-object
               class)
           java.net.URL)))
  (testing "file as object"
    (is (= (-> (mkf ["" "var" "games" "nethack" "bones" ""])
               file/<-string
               protocol/as-object
               class)
           java.io.File))))

(deftest under?
  (testing "url under url"
    (is (not (protocol/under? (url/<-string "http://reddit.com")
                              (url/<-string "http://reddit.com/r/trees"))))
    (is (protocol/under? (url/<-string "http://ubu.com/beckett")
                         (url/<-string "http://ubu.com/"))))
  (testing "file under file"
    (is (protocol/under? (file/<-string (mkf ["" "usr" "bin" "ls"]))
                         (file/<-string (mkf ["" "usr" "bin" ""]))))))

(deftest absolute?
  (testing "url absolute"
    (is (protocol/absolute? (url/<-string "http://kuro5hin.com")))
    (is (not (protocol/absolute? (url/<-string "amazon.com?action=add_cart")))))
  (testing "file absolute"
    (is (protocol/absolute? (file/<-string (mkf ["" "home" "ubuntu" ""]))))
    (is (not (protocol/absolute? (file/<-string
                                  (mkf ["." "config" "options.xml"])))))
    (is (not (protocol/absolute? (file/<-string
                                  (mkf ["project" "todo.org"])))))))

(deftest root?
  (testing "url root"
    (is (protocol/root? (url/<-string "http://")))
    (is (not (protocol/root? (url/<-string "http://example.com")))))
  (testing "file root"
    (is (protocol/root? (file/<-string (mkf ["" ""]))))
    (is (not (protocol/root? (file/<-string (mkf ["."])))))))

(deftest component
  (testing "url component"
    (is (= "index.html"
           (-> "yahoo.com/index.html" url/<-string protocol/component))))
  (testing "file component"
    (is (= "hello.jpg"
           (-> ["~" "Pictures" "yuck" "hello.jpg"] mkf file/<-string
               protocol/component)))))

(deftest extension
  (testing "url extension"
    (is (= "html"
           (-> "http://cnn.com/index.html" url/<-string protocol/extension)))
    (is (= nil
           (-> "http://cnn.com" url/<-string protocol/extension))))
  (testing "file extension"
    (is (= "me"
           (-> ["" "usr" "local" "bin" "plan9" "tmac" "me" "null.me"] mkf
               file/<-string protocol/extension)))
    (is (= nil
           (-> ["" "etc" "hosts"] mkf file/<-string protocol/extension)))))

(deftest parameters
  (testing "url parameters"
    (is (= {"count" "10"
            "page" "2"}
           (-> "http://www.craigslist.org/all/isawu?count=10&page=2"
               url/<-string protocol/parameters)))
    (is (empty?
         (-> "http://www.youtube.com" url/<-string protocol/parameters))))
  (testing "file parameters"
    (is (empty?
         (-> ["usr" "lib" "ams" "ams.real"] mkf file/<-string
             protocol/parameters)))
    (is (empty?
         (-> [".config" "weird?file?name&wtf?.txt"] mkf file/<-string
             protocol/parameters)))))

(deftest append
  (testing "url appending"
    (is (= "http://a/b/c/d/e"
           (protocol/to-string
            (protocol/append (url/<-string "http://a/b/")
                             (url/<-string "/c/d/e")))))
    (is (= "http://a/b/c/d/e/f/g.html"
           (protocol/to-string
            (protocol/append (url/<-string "http://a/b.cgi")
                             (url/<-string "http://c/d/e/f/g.html"))))))
  (testing "file appending"
    (is (= (mkf ["" "tmp" "socket"])
           (protocol/to-string
            (protocol/append (file/<-string (mkf ["" "tmp" ""]))
                             (file/<-string (mkf ["socket"]))))))
    (is (= (mkf ["" "Media" "Video" "embarrassing" "pov.mpg"])
           (protocol/to-string
            (protocol/append (file/<-string (mkf ["" "Media" "Video"
                                                  "embarrassing.flv"]))
                             (file/<-string (mkf ["pov.mpg"]))))))))
                
(deftest equivalent?
  (testing "url equivalence"
    (is (protocol/equivalent? (url/<-string "http://www.reddit.com")
                              (url/<-string "http://www.reddit.com"))))
  (testing "file equivalence"
    (is (protocol/equivalent?
         (file/<-string (mkf ["" "tmp" ".." "usr" ".." "home"]))
         (file/<-string (mkf ["" "home"]))))))

(deftest parent
  (testing "url parent"
    (is (= (url/<-string "http://zombo.com")
           (protocol/parent (url/<-string "http://zombo.com/default.asp"))))
    (is (= (url/<-string "http://gmail.google.com")
           (protocol/parent
            (url/<-string "http://gmail.google.com/inbox.html?shva=1")))))
  (testing "file parent"
    (is (= (file/<-string (mkf ["" "usr" "share" "games"]))
           (protocol/parent (file/<-string (mkf ["" "usr" "share" "games"
                                                 "nethack.cfg"])))))))
