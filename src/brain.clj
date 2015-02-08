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
    [[(->> (state/border_regions state)
        (map :id)
        (rand-nth)) (:starting_armies state)]])

(defn sort-targets
    [state targets]
    (sort-by
        (fn [region_id]
            (let [region       (get-in state [:regions region_id])
                  super_region (state/super_region state region_id)
                  reward       (:reward super_region)
                  armies       (state/super_region_armies state super_region)
                  owner        (state/super_region_owner state super_region)]
                (if (= (:our_name state) owner)
                    0
                    (if (= (:our_name state) (:owner region))
                        (/ reward armies)
                        (* 100 (/ reward armies))))))
        >
        targets))

(defn attack
    [state]
    (->> (state/our_regions state)
        (filter #(> (:armies %) 1))
        (map (fn [region]
            (let [targets    (:neighbours region)
                  proritized (sort-targets state targets)]
                ; (bot/log proritized)
                [(:id region) (first proritized) (dec (:armies region))])))))
