(ns handlers)

(defn settings
    [state key number]
    (assoc state (keyword key) (Integer/parseInt number)))

(defn settings_starting_regions
    [state & ids]
    state)

(defn settings_your_bot
    [state name]
    (assoc state :our_name name))

(defn settings_opponent_bot
    [state name]
    (assoc state :their_name name))

(defn setup_map
    [state type & args]
    state)

(defn pick_starting_region
    [state timebank & args]
    state)

(defn Output
    [state _ _ _ & args]
    state)

(defn update_map
    [state & args]
    state)

(defn opponent_moves
    [state & args]
    state)

(defn Round
    [state number]
    (assoc state :round (Integer/parseInt number)))

(defn go
    [state type & args]
    state)