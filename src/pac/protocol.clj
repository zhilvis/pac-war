(ns pac.protocol)

(defn status-ok? [resp]
  (= "OK" (:Status resp)))

(defn status-wait? [resp]
  (= "WAIT" (:Status resp)))

(defn status-error? [resp]
  (#{"FAIL" "AUTH"} (:Status resp)))

(defn to-position-vector [{:keys [Row Col]}]
  [Row Col])

(defn to-position-map [[y x]]
  {:Row y :Col x})

(defn game-map [{:keys [Map TecmanPosition GhostPositions]}]
  (let [{:keys [Width Height Rows]} Map
        cell-by-char {\# {:type :wall} \. {:type :cookie}}
        rows (->> Rows (map vec) (mapv #(mapv cell-by-char %)))]
    {:width Width
     :height Height
     :map rows
     :tecman-position (to-position-vector TecmanPosition)
     :ghost-positions (mapv to-position-vector GhostPositions)}))

(comment

  (def player-view-resp
    {:Turn                   1
     :Mode                   "TECMAN"
     :Map                    {:Width  10
                              :Height 7
                              :Rows   ["######",
                                       "#    #",
                                       "#.##.#",
                                       "#....#",
                                       "######"]}
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