(ns pac.core
  (:require [org.httpkit.client :as http]
            [cheshire.core :as cheshire]
            [pac.protocol :as protocol])
  (:import (java.security MessageDigest)))

(def dev-url "http://10.30.0.114/dev/ClientService.svc")
(def dev-pwd "gkOOdYu9")

(def prod-url "http://10.30.0.114/prod/ClientService.svc")
(def prod-pwd "DRI9idEoD91j")

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

(defonce sq (atom 1))

(defn post-params [player session body]
  (swap! sq inc)
  {:body
   (cheshire/generate-string
     (merge body
            {:Auth (auth "Inventi" session dev-pwd player @sq)}))
   :headers {"Content-Type" "application/json"}})

(defn post [player session path body]
  (http/post
    (str dev-url (str "/json/" path))
    (post-params player session body)))

(defn create-player [player session]
  "Registers a player. Returns player id which
  should be used in follow-up requests"
  (println "create" auth)
  (let [response (post player session "CreatePlayer" {})
        body (:body @response)]
    (cheshire/parse-string body keyword)))


(defonce turn (atom 0))

(defn wait-next-turn [player session player-id]
  (loop []
    (let [response (post player session "WaitNextTurn" {:PlayerId player-id
                                                        :RefTurn  @turn})
          body (cheshire/parse-string (:body @response) keyword)]
      (println body)
      (if-not (:TurnComplete body)
        (recur)
        true))))

(defn get-player-view [player session player-id]
  (let [response (post player session "GetPlayerView" {:PlayerId player-id})
        body (cheshire/parse-string (:body @response) keyword)]
    (protocol/status-ok? body)
    (reset! turn (protocol/turn body))
    (protocol/game-map body)))

