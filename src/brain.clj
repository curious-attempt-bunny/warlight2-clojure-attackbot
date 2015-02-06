(ns brain)

(defn pick_starting_region
    [state ids]
    (->> ids
        (sort-by
            (fn [id]
                (/ (state/super_region_armies state (state/super_region state id))
                   (:reward (state/super_region state id)))))
        first))
            