(ns pac.protocol)

(defn status-error? [resp]
  (#{"FAIL" "AUTH"} (:Status resp)))

(defn ensure-no-errors [resp]
  (when (status-error? resp)
    (throw (IllegalStateException. (str "Response status: " (:Status resp) ", message: " (:Message resp))))))

(defn status-ok? [resp]
  (ensure-no-errors resp)
  (= "OK" (:Status resp)))

(defn status-wait? [resp]
  (ensure-no-errors resp)
  (= "WAIT" (:Status resp)))

(defn turn [{:keys [Turn]}]
  (if Turn Turn 0))

(defn to-position-vector [{:keys [Row Col]}]
  [Row Col])

(defn to-position-map [[y x]]
  {:Row y :Col x})

(defn game-map
  [{:keys [Mode Map TecmanPosition PreviousTecmanPosition GhostPositions PreviousGhostPositions]}]
  (let [{:keys [Width Height Rows]} Map
        cell-by-char {\# {:type :wall} \. {:type :cookie}}
        rows (->> Rows (map vec) (mapv #(mapv cell-by-char %)))
        tecman-pos (to-position-vector TecmanPosition)
        ghost-poses (mapv to-position-vector GhostPositions)]
    {:mode            (if (= "TECMAN" Mode) :tecman :ghost)
     :width           Width
     :height          Height
     :map             (-> rows
                          (assoc-in tecman-pos {:type :tecman})
                          (assoc-in (ghost-poses 0) {:type :ghost})
                          (assoc-in (ghost-poses 1) {:type :ghost})
                          (assoc-in (ghost-poses 2) {:type :ghost})
                          (assoc-in (ghost-poses 3) {:type :ghost}))
     :tecman-position tecman-pos
     :tecman-previous-position (to-position-vector PreviousTecmanPosition)
     :ghost-positions ghost-poses
     :ghost-previous-positions (mapv to-position-vector PreviousGhostPositions)}))

(defn positions-req [position-vectors]
  {:Positions (map to-position-map position-vectors)})

(comment

  (def player-view-resp
    {:Turn                   1
     :Mode                   "TECMAN"
     :Map                    {:Width  6
                              :Height 5
                              :Rows   ["######",
                                       "#    #",
                                       "#.##.#",
                                       "# . .#",
                                       "######"]}
     :TecmanPosition         {:Row 1, :Col 4}
     :PreviousTecmanPosition {:Row 4, :Col 6}
     :GhostPositions         [{:Row 1, :Col 1}
                              {:Row 1, :Col 1}
                              {:Row 1, :Col 2}
                              {:Row 1, :Col 2}]
     :PreviousGhostPositions [{:Row 1, :Col 9}
                              {:Row 2, :Col 8}
                              {:Row 3, :Col 7}
                              {:Row 4, :Col 6}]
     :Status                 "OK"
     :Message                nil})

  (status-ok? player-view-resp)

  (status-wait? player-view-resp)
 
  (status-error? player-view-resp)

  (turn player-view-resp)
  
  (game-map player-view-resp)

  (positions-req [[5 6]])
  
  )