(ns ice-road.test
  (:require [clojure.test :as test :refer [deftest is testing]]
            [ice-road.protocol :as protocol]
            [ice-road.path :as path]
            [ice-road.url :as url]
            [ice-road.file :as file]))

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
                              :path ["slashdot"]
                              :extension "com"
                              :queries {}})
           (url/<-string "slashdot.com"))))
  (testing "file path construction"
    (is (= (file/map->Filepath {:path ["" "home" "antler" "todo"]
                                :extension "org"})
           (file/<-string "/home/antler/todo.org")))
    (is (= (file/map->Filepath {:path ["" "home" "antler" ""]
                                :extension "bashrc"})
           (file/<-string "/home/antler/.bashrc")))))

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
    (is (let [file "/usr/share/doc/dwm/changelog.gz"]
          (= file
             (protocol/to-string (file/<-string file)))))
    (is (let [file "/usr/local/lib/"]
          (= file
             (protocol/to-string (file/<-string file)))))
    (is (let [file "/var/cache/apt"]
          (= file
             (protocol/to-string (file/<-string file)))))
    (is (let [file "/usr/../bin/./ls"]
          (= file
             (protocol/to-string (file/<-string file)))))))

(deftest as-object)

(deftest under?)

(deftest absolute?)

(deftest root?)

(deftest component)

(deftest extension)

(deftest parameters)

(deftest append)

(deftest equivalent?)

(deftest parent)
