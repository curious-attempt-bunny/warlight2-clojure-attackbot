(ns handlers
    (:require bot))

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
                    {:id (Integer/parseInt id) :super_region_id (Integer/parseInt super_region_id)}))
            {}
            (partition 2 args))))

(defn setup_map_neighbors
    [state & args]
    (reduce
        (fn [state [region_id neighbours]]
            (assoc-in state
                [:regions (Integer/parseInt region_id) :neighbours]
                (map #(Integer/parseInt %) (clojure.string/split neighbours #","))))
        state
        (partition 2 args)))

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
    [state timebank & args]
    (bot/send-command (rand-nth args))
    state)

(defn Output
    [state _ _ _ & args]
    state)

(defn update_map
    [state & args]
    state)

; left as an exercise for the reader
(defn opponent_moves
    [state & args]
    state)

(defn Round
    [state number]
    (assoc state :round (Integer/parseInt number)))

(defn go_place_armies
    [state timebank]
    ; (let [state      (assoc state :timebank timebank)
    ;       placements (brain/army_placements state)
    ;       command    (clojure.string/join
    ;                     (map (fn )))]
    state)

(defn go_attack_transfer
    [state timebank]
    state)
