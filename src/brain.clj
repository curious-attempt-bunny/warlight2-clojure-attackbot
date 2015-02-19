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

(defn place_armies
    [state]
    (let [{:keys [from]} (first (ranked_targets state (our_regions state)))]
        [[(:id from) (:starting_armies state)]]))

(defn attack
    [state]
    (ranked_targets state (our_regions state)))
        
