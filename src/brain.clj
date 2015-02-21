(ns brain)
(require 'clojure.set)

; see rollouts/attack_map
; 2x1 -- (-1) --> 1
; 3x2 -- (-1) --> 2
; 5x3 -- (-2) --> 3
; 7x4 -- (-3) --> 4
; 9x5 -- (-4) --> 5
; 11x6 -- (-4) --> 7
; 13x7 -- (-5) --> 8
; 15x8 -- (-6) --> 9
; 17x9 -- (-7) --> 10
; 19x10 -- (-7) --> 12
; 21x11 -- (-8) --> 13
; 23x12 -- (-9) --> 14
; 25x13 -- (-10) --> 15
; 27x14 -- (-10) --> 17
; 29x15 -- (-11) --> 18
; 31x16 -- (-12) --> 19
; 33x17 -- (-13) --> 20
; 35x18 -- (-13) --> 22
; 37x19 -- (-14) --> 23
(defn armies_to_kill
    [defending_armies]
    (nth (concat [0 2 3] (iterate (partial + 2) 5)) defending_armies))

(defn regions
    [state]
    (vals (:regions state)))

(defn ours?
    [region]
    (= :us (:owner region)))

(defn our_regions
    [state]
    (filter ours? (regions state)))

(defn neighbours
    [state region]
    (map (fn [region_id] (get-in state [:regions region_id])) (:neighbours region)))

(defn targets
    [state from_regions]
    (mapcat
        (fn [region]
            (map (fn [neighbour] {:from region :to neighbour :armies (armies_to_kill (:armies neighbour))})
                (remove ours? (neighbours state region))))
        from_regions))

(defn ranked_targets
    [state from_regions]
    (targets state from_regions))

(defn pick_starting_region
    [state ids]
    (let [{:keys [from]} (first (ranked_targets state (our_regions state)))]
        (:id from)))

(defn place_required_armies
    [[state placements] {:keys [from to armies]}]
    (cond
        (zero? (:starting_armies state))
            [state placements]
        :else
            (let [from2         (get-in state [:regions (:id from)]) ; may have been updated
                  needed-armies (- armies (dec (:armies from2)))]
                (cond
                    (> (:armies from) armies)
                        (let [next-state (update-in state [:regions (:id from) :armies] #(- % armies))]
                            [next-state placements])
                    (> (:starting_armies state) needed-armies)
                        (let [next-state    (assoc-in state [:regions (:id from) :armies] 1)
                              next-state2   (update-in state [:starting_armies] #(- % needed-armies))
                              placement     {:region from :armies needed-armies}]
                            (bot/log (str "Placing " needed-armies " armies on " (:id from) " so that we can attack " (:id to) " " armies "v" (:armies to)))
                            [next-state2 (conj placements placement)])
                    :else
                        [state placements]))))

(defn place_armies
    [state]
    (let [targets            (ranked_targets state (our_regions state))
          [state placements] (reduce place_required_armies [state []] targets)
          final_placement    {:region (:from (first targets)) :armies (:starting_armies state)}]
        (conj placements final_placement)))

(defn attack_when_appropriate
    [[state attacks] {:keys [from to armies] :as attack}]
    (let [from2 (get-in state [:regions (:id from)])
          to2   (get-in state [:regions (:id to)])] ; may have been updated
        ; (bot/log [(:id from) (:id to) (:armies from2) (:armies to)])
        (cond
            (and (> (:armies from2) armies) (not= :us (:owner to2)))
                (let [next-state  (update-in state [:regions (:id from) :armies] #(- % armies))
                      next-state2 (update-in next-state [:regions (:id to) :owner] :us)]
                    ; (bot/log [(:id from) (:id to)])
                    [next-state2 (conj attacks attack)])
            :else
                [state attacks])))

(defn attack
    [state]
    (last (reduce attack_when_appropriate [state []] (ranked_targets state (our_regions state)))))
        
