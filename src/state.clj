(ns state)

(defn super_region
    [state id]
    (get-in state [:super_regions (get-in state [:regions id :super_region_id])]))

(defn super_region_armies
    [state super_region]
    (let [armies (->> (vals (:regions state))
                        (filter #(= (:super_region_id %) (:id super_region)))
                        (filter #(not= (:our_name state) (:owner %)))
                        (map :armies)
                        (reduce +))]
        armies))

(defn super_region_owner
    [state super_region]
    (let [owners (->> (vals (:regions state))
                        (filter #(= (:super_region_id %) (:id super_region)))
                        (map :owner)
                        (set))]
        (if (= (count owners) 1)
            (first owners)
            nil)))

(defn our_regions
    [state]
    (->> (vals (:regions state))
        (filter #(= (:owner %) (:our_name state)))))

(defn border_regions
    [state]
    (->> (our_regions state)
        (filter (fn [region]
            (some (fn [neighbour_id]
                    (not= (:our_name state)
                        (get-in state [:regions neighbour_id :owner])))
                (:neighbours region))))))