(ns pac.core
  (:require [org.httpkit.client :as http]
            [cheshire.core :as cheshire])
  (:import (java.security MessageDigest)))

(def base-url "http://10.30.0.114/dev/ClientService.svc")

(defn sha1-str [s]
  (->> (-> "sha1"
           MessageDigest/getInstance
           (.digest (.getBytes s "UTF-8")))
       (map #(.substring
              (Integer/toString
                (+ (bit-and % 0xff) 0x100) 16) 1))
       (apply str)))

(defn generate-auth [secret payload]
  (-> (str payload secret)
      (sha1-str)
      (clojure.string/lower-case)))

(defn auth [team session secret client sequence]
  {:TeamName       team
   :ClientName     client
   :SessionId      session
   :SequenceNumber sequence
   :AuthCode       (generate-auth
                     secret
                     (str team ":" client ":" session ":" sequence))})

(def inventi-dev (partial auth "Inventi" 5658 "gkOOdYu9" "Karolis"))

(defn create-player [auth]
  "Registers a player. Returns player id which
  should be used in follow-up requests"
  (let [response (http/post
                   (str base-url "/json/CreatePlayer")
                   {:body    (cheshire/generate-string {:Auth (auth 1)})
                    :headers {"Content-Type" "application/json"}})
        body (:body @response)]
    (cheshire/parse-string body keyword)))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
