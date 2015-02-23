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

(defn attackers_killed
    [defending_armies]
    (java.lang.Math/round (+
        (* (* 0.7 defending_armies) (- 1 0.16))
        (* defending_armies 0.16))))

(defn regions
    ([state]
        (vals (:regions state)))
    ([state ids]
        (let [region_ids (set ids)]
            (->> (regions state) (filter #(contains? region_ids (:id %)))))))

(defn ours?
    [region]
    (= :us (:owner region)))

(defn ours_at_start_of_turn?
    [region]
    (and (ours? region) (not= true (:newly-captured region))))

(defn our_regions
    [state]
    (filter ours? (regions state)))

(defn our_starting_regions
    [state]
    (filter ours_at_start_of_turn? (regions state)))

(defn neighbours
    [state region]
    (map (fn [region_id] (get-in state [:regions region_id])) (:neighbours region)))

(defn enemy_neighbours
    [state region]
    (->> (neighbours state region)
        (remove ours?)))

(defn border_regions
    [state]
    (->> (our_regions state)
        (filter (fn [region]
            (some (fn [neighbour_id]
                    (not= :us
                        (get-in state [:regions neighbour_id :owner])))
                (:neighbours region))))))

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
          multiplier         (if (= :them (:owner to)) 2.0 1.0)
          score              (* multiplier (/ super-region-score (inc extra-armies)))]
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
            (let [from          (get-in state [:regions (:id from)]) ; may have been updated
                  to            (get-in state [:regions (:id to)])
                  needed-armies (- armies (dec (:armies from)))]
                (bot/log (str "From " (:id from) " to " (:id to) " need " armies " to win. Have " (:armies from) ". Need " needed-armies " more. Have " (:starting_armies state) " left to place."))
                (cond
                    (= true (:newly-captured to))
                        [state placements]
                    (<= needed-armies 0)
                        (let [state (update-in state [:regions (:id from) :armies] #(- % armies))
                              state (update-in state [:regions (:id to) :owner] :us)
                              state (assoc-in state [:regions (:id to) :newly-captured] true)
                              remaining_armies (- armies (attackers_killed armies))
                              state (assoc-in state [:regions (:id to) :armies] remaining_armies)]
                            (bot/log (str "  enough to attack so removed the armies needed. Now we have " (get-in state [:regions (:id from) :armies])))
                            [state placements])
                    (>= (:starting_armies state) needed-armies)
                        (let [state     (update-in state [:regions (:id from) :armies] (partial + needed-armies))
                              state     (update-in state [:regions (:id from) :armies] #(- % armies))
                              state     (update-in state [:starting_armies] #(- % needed-armies))
                              state     (update-in state [:regions (:id to) :owner] :us)
                              state     (assoc-in state [:regions (:id to) :newly-captured] true)
                              placement {:region from :armies needed-armies}
                              remaining_armies (- armies (attackers_killed armies))
                              state (assoc-in state [:regions (:id to) :armies] remaining_armies)]
                            (bot/log (str "  Placing " needed-armies " armies on " (:id from) " so that we can attack " (:id to) " " armies "v" (:armies to) ". This leaves " (get-in state [:regions (:id from) :armies]) " behind."))
                            [state (conj placements placement)])
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

(defn transfer_region_score
    [state region]
    ; (bot/log (str "region " (:id region) " has transfer score " (:armies region)))
    (:armies region))

(defn ranked_transfer_regions
    [state regions]
    (sort-by
        (partial transfer_region_score state)
        >
        regions))

(defn transfers
    ([state]
        ; (bot/log "considering transfers")
        (transfers state (ranked_transfer_regions state (border_regions state)) (set (map :id (border_regions state)))))
    ([state regions considered]
        (if (empty? regions)
            []
            (let [region          (first regions)
                  neighbours      (:neighbours region)
                  unvisited       (filter #(not (contains? considered %)) neighbours)
                  resolved        (map #(get-in state [:regions %]) unvisited)
                  ours            (filter ours? resolved)
                  next-regions    (concat (rest regions) ours)
                  next-considered (clojure.set/union considered (set (map :id ours)))
                  with_armies     (filter #(> (:armies %) 1) ours)
                  moves           (map (fn [source] {:from source :to region :armies (dec (:armies source))}) with_armies)]
                ; (bot/log (str "transfers " (pr-str moves)))
                (concat moves (transfers state next-regions next-considered))))))

(defn attack
    ([state] 
        ; (bot/log "considering attacks")
        (attack state (ranked_attack_targets state (our_starting_regions state))))
    ([state attacks]
        (cond
            (empty? attacks)
                (transfers state)
            :else
                (let [{:keys [from to armies]} (first attacks)
                      from                     (get-in state [:regions (:id from)])
                      to                       (get-in state [:regions (:id to)])
                      from_deadend             (= 1 (count (enemy_neighbours state from)))
                      ; to_deadend               (empty? (enemy_neighbours state to))
                      armies                   (if (and from_deadend) ; (not to_deadend))
                                                    (max armies (dec (:armies from)))
                                                    armies)]
                    ; (bot/log (str "considering " (:id from) " to " (:id to) (:owner to) " - enough? " (> (:armies from) armies) " - from_deadend? " from_deadend " to_deadend " to_deadend))
                    (if (> (:armies from) armies)
                        (let [state (update-in state [:regions (:id from) :armies] #(- % armies))
                              state (assoc-in state [:regions (:id to) :owner] :us)
                              state (assoc-in state [:regions (:id to) :newly-captured] true)
                              next  (attack state)
                              att   {:from from :to to :armies armies}]
                            (conj next att))
                        (attack state (rest attacks)))))))
