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
    ([state]
        (vals (:regions state)))
    ([state ids]
        (let [region_ids (set ids)]
            (->> (regions state) (filter #(contains? region_ids (:id %)))))))

(defn ours?
    [region]
    (and (= :us (:owner region)) (not= false (:newly-captured region))))

(defn our_regions
    [state]
    (filter ours? (regions state)))

(defn neighbours
    [state region]
    (map (fn [region_id] (get-in state [:regions region_id])) (:neighbours region)))

(defn enemy_neighbours
    [state region]
    (->> (neighbours state region)
        (remove #(= :us (:owner %)))))

(defn super_region
    [state region]
    (get-in state [:super_regions (:super_region_id region)]))

(defn super_regions
    [state regions]
    (set (map (partial super_region state) regions)))

(defn super_region_armies
    [state super_region]
    (let [armies (->> (vals (:regions state))
                        (filter #(= (:super_region_id %) (:id super_region)))
                        (filter #(not= :us (:owner %)))
                        (map :armies)
                        (reduce +))]
        armies))

(defn super_region_score
    ([state super_region] (super_region_score state super_region 0))
    ([state super_region army_delta]
        (let [reward (:reward super_region)
              armies (- (super_region_armies state super_region) army_delta)
              super_region_size (count (filter (fn [region] (= (:id super_region) (:super_region_id region))) (regions state)))              
              score  (+
                        (if (zero? armies) reward (/ reward armies))
                        (if (zero? reward) 0 (/ 1 super_region_size)))]
            ; (bot/log (:super_regions state))
            ; (bot/log (str "super region " (:id super_region) " scores " score " because of reward " reward " and armies " armies " and size " super_region_size))
            score)))

(defn update_super_region_scores
    [state]
    (reduce
        (fn [state super_region]
            (assoc-in state
                [:super_regions (:id super_region) :score]
                (super_region_score state super_region)))
        state
        (vals (:super_regions state))))

;; ----- picking regions

(defn region_pick_score
    [state region]
    (let [score (get-in state [:super_regions (:super_region_id region) :score])]
        ; (bot/log (str "Region " (:id region) " scores " score))
        score))

(defn ranked_pick_regions
    [state from_regions]
    (let [state (update_super_region_scores state)]
        (sort-by (partial region_pick_score state) > from_regions)))

(defn pick_starting_region
    [state ids]
    (let [region (first (ranked_pick_regions state (regions state ids)))]
        (:id region)))

;; ----- placement and attacking

(defn targets
    [state from_regions]
    (mapcat
        (fn [region]
            (map (fn [neighbour] {:from region :to neighbour :armies (armies_to_kill (:armies neighbour))})
                (remove ours? (neighbours state region))))
        from_regions))

(defn target_attack_score
    [state {:keys [from to armies]}]
    (let [required-armies    (inc armies)
          extra-armies       (max 0 (- required-armies (:armies from)))
          super-region-score (:score (super_region state to))
          score              (/ super-region-score (inc extra-armies))]
        score))

(defn ranked_attack_targets
    [state from_regions]
    (let [state (update_super_region_scores state)]
        (->> (targets state from_regions)
            (sort-by (partial target_attack_score state) >))))

(defn place_required_armies
    [[state placements] {:keys [from to armies]}]
    (cond
        (zero? (:starting_armies state))
            [state placements]
        (= :us (get-in state [:regions (:id to) :owner]))
            [state placements]
        :else
            (let [from2         (get-in state [:regions (:id from)]) ; may have been updated
                  to2           (get-in state [:regions (:id to)])
                  needed-armies (- armies (dec (:armies from2)))]
                (bot/log (str "From " (:id from2) " to " (:id to) " need " armies " to win. Have " (:armies from2) ". Need " needed-armies " more. Have " (:starting_armies state) " left to place."))
                (cond
                    (= true (:newly-captured to2))
                        [state placements]
                    (<= needed-armies 0)
                        (let [next-state (update-in state [:regions (:id from2) :armies] #(- % armies))
                              next-state2 (update-in next-state [:regions (:id to) :owner] :us)
                              next-state3 (assoc-in next-state2 [:regions (:id to) :newly-captured] true)]
                            (bot/log (str "  enough to attack so removed the armies needed. Now we have " (get-in state [:regions (:id from2) :armies])))
                            [next-state3 placements])
                    (>= (:starting_armies state) needed-armies)
                        (let [next-state    (assoc-in state [:regions (:id from2) :armies] 1)
                              next-state2   (update-in next-state [:starting_armies] #(- % needed-armies))
                              next-state3   (update-in next-state2 [:regions (:id to) :owner] :us)
                              next-state4   (assoc-in next-state3 [:regions (:id to) :newly-captured] true)
                              placement     {:region from :armies needed-armies}]
                            (bot/log (str "  Placing " needed-armies " armies on " (:id from2) " so that we can attack " (:id to) " " armies "v" (:armies to) ". This leaves " (get-in next-state2 [:regions (:id from2) :armies]) " behind."))
                            [next-state4 (conj placements placement)])
                    :else
                        [state placements]))))

(defn place_armies
    [state]
    (let [targets            (ranked_attack_targets state (our_regions state))
          [state placements] (reduce place_required_armies [state []] targets)
          final_placement    {:region (:from (first targets)) :armies (:starting_armies state)}]
        (if (zero? (:starting_armies state))
            placements
            (conj placements final_placement))))

(defn attack
    ([state] 
        ; (bot/log "considering attacks")
        (attack state (ranked_attack_targets state (our_regions state))))
    ([state attacks]
        (cond
            (empty? attacks)
                []
            :else
                (let [{:keys [from to armies] :as an_attack} (first attacks)]
                    ; (bot/log (str "considering " (:id from) " to " (:id to) " - enough? " (> (:armies from) armies)))
                    (if (> (:armies from) armies)
                        (let [next-state   (update-in state [:regions (:id from) :armies] #(- % armies))
                              next-state2  (assoc-in next-state [:regions (:id to) :owner] :us)
                              next-state3  (assoc-in next-state2 [:regions (:id to) :newly-captured] true)
                              next-attacks (attack next-state3)]
                            ; (bot/log (str an_attack next-attacks))
                            (conj next-attacks an_attack))
                        (attack state (rest attacks)))))))
