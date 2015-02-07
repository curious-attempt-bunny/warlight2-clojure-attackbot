(ns brain)

(defn pick_starting_region
    [state ids]
    (->> ids
        (sort-by
            (fn [id]
                (/ (:reward (state/super_region state id))
                   (state/super_region_armies state (state/super_region state id))))
            >)
        first))
            
(defn place_armies
    [state]
    [[(->> (state/our_regions state)
        (map :id)
        (rand-nth)) (:starting_armies state)]])

(defn attack
    [state]
    (->> (state/our_regions state)
        (filter #(> (:armies %) 1))
        (map (fn [region]
            (bot/log [(:id region) (rand-nth (:neighbours region)) (dec (:armies region))])
            [(:id region) (rand-nth (:neighbours region)) (dec (:armies region))]))))
