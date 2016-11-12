(ns pac.auth
  (:require [org.httpkit.client :as http]
            [cheshire.core :as cheshire]
            [pac.protocol :as protocol])
  (:import (java.security MessageDigest)))

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

(def dev-url "http://10.30.0.114/dev/ClientService.svc")
(def dev-pwd "gkOOdYu9")

(def prod-url "http://10.30.0.114/prod/ClientService.svc")
(def prod-pwd "DRI9idEoD91j")

(defonce sq (atom 1))

(defn post-params [player session body]
  (swap! sq inc)
  {:body
   (cheshire/generate-string
     (merge body
            {:Auth (auth "Inventi" session prod-pwd player @sq)}))
   :headers {"Content-Type" "application/json"}})

(defn post [player session path body]
  (http/post
    (str prod-url (str "/json/" path))
    (post-params player session body)))
