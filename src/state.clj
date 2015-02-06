(ns state)

(defn super_region
    [state id]
    (get-in state [:super_regions (get-in state [:regions id :super_region_id])]))

(defn super_region_armies
    [state super_region]
    (->> (vals (:regions state))
        ; (map #(.println *err* (:super_region_id %)))
        (filter #(= (:super_region_id %) (:id super_region)))
        ; (count)
        ; (map #(.println *err* %))
        (map :armies)
        (reduce +)
        ; (first)
    ))
