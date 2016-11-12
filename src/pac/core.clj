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

(def luke       (partial auth "Inventi" 5668 "gkOOdYu9" "Luke" ))
(def vader      (partial auth "Inventi" 5659 "gkOOdYu9" "Vader"))
(def inventi-dev(partial auth "Inventi" 5660 "gkOOdYu9"  "Karolis"))

(defonce sq (atom 1))

(defn post-params [player session body]
  (swap! sq inc)
  {:body
   (cheshire/generate-string
     (merge body
            {:Auth (auth "Inventi" session "gkOOdYu9" player @sq)}))
   :headers {"Content-Type" "application/json"}})

(defn post [player session path body]
  (http/post
    (str base-url (str "/json/" path))
    (post-params player session body)))

(defn create-player [player session]
  "Registers a player. Returns player id which
  should be used in follow-up requests"
  (println "create" auth)
  (let [response (post player session "CreatePlayer" {})
        body (:body @response)]
    (cheshire/parse-string body keyword)))


(defn first-do []
  (create-player "Luke4" 1234)
  (create-player "Luke5" 1235))
