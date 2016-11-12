(ns pac.core
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

(defn auth [team client session secret sequence]
  {:TeamName       team
   :ClientName     client
   :SessionId      session
   :SequenceNumber sequence
   :AuthCode       (generate-auth
                     secret
                     (str team ":" client ":" session ":" sequence))})



(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
