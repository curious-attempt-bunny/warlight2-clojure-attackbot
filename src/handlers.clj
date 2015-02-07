(ns handlers)

(defn settings
    [state key number]
    (assoc state (keyword key) (Integer/parseInt number)))

(defn settings_starting_regions
    [state & ids]
    (assoc state
        :our_starting_regions
        (map #(Integer/parseInt %) ids)))

(defn settings_your_bot 
    [state name]
    (assoc state :our_name name))

(defn settings_opponent_bot
    [state name]
    (assoc state :their_name name))

(defn setup_map_super_regions
    [state & args]
    (assoc state
        :super_regions
        (reduce
            (fn [super_regions [id reward]]
                (assoc super_regions
                    (Integer/parseInt id)
                    {:id (Integer/parseInt id) :reward (Integer/parseInt reward)}))
            {}
            (partition 2 args))))

(defn setup_map_regions
    [state & args]
    (assoc state
        :regions
        (reduce
            (fn [regions [id super_region_id]]
                (assoc regions
                    (Integer/parseInt id)
                    {   :id (Integer/parseInt id)
                        :super_region_id (Integer/parseInt super_region_id)
                        :armies 2
                        :neighbours []
                        }))
            {}
            (partition 2 args))))

(defn setup_map_neighbors
    [state & args]
    (->> (partition 2 args)
        (reduce
            (fn [state [_region_id _neighbours]]
                (let [region_id  (Integer/parseInt _region_id)
                      neighbours (map #(Integer/parseInt %) (clojure.string/split _neighbours #","))
                      state2     (update-in state
                                    [:regions region_id :neighbours]
                                    (partial concat neighbours))]
                        (reduce (fn [state neighbour_id]
                            (update-in state
                                [:regions neighbour_id :neighbours]
                                (partial cons region_id)))
                            state2
                            neighbours)))
            state)))

(defn setup_map_wastelands
    [state & wasteland_ids]
    (reduce
        (fn [state region_id]
            (assoc-in state
                [:regions (Integer/parseInt region_id) :wasteland]
                true))
        state
        wasteland_ids))

(defn setup_map_opponent_starting_regions
    [state & ids]
    (assoc state
        :their_starting_regions
        (map #(Integer/parseInt %) ids)))

(defn pick_starting_region
    [state timebank & ids]
    (bot/send-command (brain/pick_starting_region state (map #(Integer/parseInt %) ids)))
    state)

(defn Output
    [state _ _ _ & args]
    state)

(defn update_map
    [state & args]
    (reduce
        (fn [state [region_id owner armies]]
            (-> state
                (assoc-in
                    [:regions (Integer/parseInt region_id) :owner]
                    owner)
                (assoc-in
                    [:regions (Integer/parseInt region_id) :armies]
                    (Integer/parseInt armies))))
        state
        (partition 3 args)))

; left as an exercise for the reader
(defn opponent_moves
    [state & args]
    state)

(defn Round
    [state number]
    (assoc state :round (Integer/parseInt number)))

(defn go_place_armies
    [state timebank]
    (let [moves (brain/place_armies state)]
        (if (empty? moves)
            (do
                (bot/send-command "No moves")
                state)
            (do
                (->> moves
                    (map (fn [[id armies]]
                            (str (:our_name state) " place_armies " id " " armies)))
                    (clojure.string/join ",")
                    (bot/send-command))
                (reduce (fn [state [id armies]]
                    (update-in state
                        [:regions id :armies]
                        (partial + armies)))
                    state
                    moves)))))

(defn go_attack_transfer
    [state timebank]
    (let [moves (brain/attack state)]
        (if (empty? moves)
            (bot/send-command "No moves")
            (->> moves
                (map (fn [[from_id to_id armies]]
                        (str (:our_name state) " attack/transfer " from_id " " to_id " " armies)))
                (clojure.string/join ",")
                (bot/send-command))))
    state)