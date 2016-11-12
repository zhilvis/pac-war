(ns pac.protocol)

(defn status-ok? [resp]
  (= "OK" (:Status resp)))

(defn status-wait? [resp]
  (= "WAIT" (:Status resp)))

(defn status-error? [resp]
  (#{"FAIL" "AUTH"} (:Status resp)))

(defn game-map [{:keys [Map]}]
  (let [{:keys [Rows]} Map
        cell-by-char {\# {:type :wall} \. {:type :cookie}}]
    {:map (->> Rows (map vec) (mapv #(mapv cell-by-char %)))}))

(comment

  (def player-view-resp
    {:Turn                   1
     :Mode                   "TECMAN"
     :Map                    {:Width  10
                              :Height 7
                              :Rows   ["##########",
                                       "#        #",
                                       "#        #",
                                       "#        #",
                                       "#.######.#",
                                       "#.... ...#",
                                       "##########"]}
     :TecmanPosition         {:Row 5, :Col 6}
     :PreviousTecmanPosition {:Row 4, :Col 6}
     :GhostPositions         [{:Row 1, :Col 9}
                              {:Row 2, :Col 8}
                              {:Row 3, :Col 7}
                              {:Row 4, :Col 6}]
     :PreviousGhostPositions [{:Row 1, :Col 9}
                              {:Row 2, :Col 8}
                              {:Row 3, :Col 7}
                              {:Row 4, :Col 6}]
     :Status                 "OK"
     :Message                nil})

  (status-ok? player-view-resp)

  (status-wait? player-view-resp)
 
  (status-error? player-view-resp)

  (game-map player-view-resp)

  )