(ns pac.core
  (:require [org.httpkit.client :as http]
            [cheshire.core :as cheshire]
            [pac.protocol :as protocol]
            [pac.auth :refer [post]])
  (:import (java.security MessageDigest)))

(defn create-player [player session]
  "Registers a player. Returns player id which
  should be used in follow-up requests"
  (let [response (post player session "CreatePlayer" {})
        body (:body @response)]
    (:PlayerId (cheshire/parse-string body keyword))))

(defonce turn (atom 0))

(defn wait-next-turn [player session player-id]
  (loop []
    (let [response (post player session "WaitNextTurn" {:PlayerId player-id
                                                        :RefTurn  @turn})
          body (cheshire/parse-string (:body @response) keyword)]
      (println "WaitNextTurn" body)
      (if-not (:TurnComplete body)
        (recur)
        (not (:GameFinished body))))))

(defn get-player-view [player session player-id]
  (let [response (post player session "GetPlayerView" {:PlayerId player-id})
        body (cheshire/parse-string (:body @response) keyword)]
    (println "GetPlayerView" body)
    (when (protocol/status-ok? body)
      (do (reset! turn (protocol/turn body))
          (protocol/game-map body)))))

(defn perform-move [player session player-id coords]
  (let [response (post player session "PerformMove" (merge {:PlayerId player-id}
                                                           (protocol/positions-req coords)))
        body (cheshire/parse-string (:body @response) keyword)]
    (println "PerformMove" body)
    (protocol/status-ok? body)
    nil))

(defn play-until-completes [player-name session-id player-id]
  (loop []
    (when (wait-next-turn player-name session-id player-id)
      (when-let [view (get-player-view player-name session-id player-id)]
        (perform-move player-name session-id player-id (ai/external-advice-turn view)))
      (recur))))

(defn -main []
  (let [session-id (int (* 10000 (Math/random)))
        player-name (str "Inventi-" session-id)
        player-id (create-player player-name session-id)]
    (loop []
      (reset! turn 0)
      (play-until-completes player-name session-id player-id)
      (recur))))
