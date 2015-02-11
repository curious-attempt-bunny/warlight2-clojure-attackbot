(ns brain)

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

(defn super_region_score
    ([state super_region] (super_region_score state super_region 0))
    ([state super_region army_delta]
        (let [reward (:reward super_region)
              armies (- (state/super_region_armies state super_region) army_delta)
              score  (if (zero? armies) reward (/ reward armies))]
            score)))

(defn pick_starting_region
    [state ids]
    (->> ids
        (sort-by
            (fn [id] (super_region_score state (state/super_region state id) 2))
            >)
        first))

(defn place_armies
    [state]
    [[(->> (state/border_regions state)
        (sort-by
            (fn [region]
                (let [neighbours    (state/enemy_neighbours state region)
                      super_regions (set (map (partial state/super_region state) neighbours))
                      filtered      (filter #(not= (:our_name state) (state/super_region_owner state %)) super_regions)
                      scores        (map (partial super_region_score state) filtered)
                      best_score    (reduce max scores)]
                    best_score))
            >)
        (map :id)
        (first)) (:starting_armies state)]])

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

(defn next_attacks
    ([state already_attacked region]
        (let [targets    (state/enemy_neighbours state region)
              filtered   (filter #(not (contains? already_attacked %)) targets)
              proritized (sort-targets state filtered)]
            (next_attacks state already_attacked region proritized)))
    ([state already_attacked region proritized]
        ; (bot/log [region proritized])
        (if (empty? proritized)
            [state []]
            (let [target               (first proritized)
                  defending_armies     (get-in state [:regions target :armies])
                  attacking_armies     (armies_to_kill defending_armies)
                  armies               (:armies region)
                  enemy_border_count   (state/region_borders_player_count state region (:their_name state))
                  neutral_border_count (state/region_borders_player_count state region "neutral")
                  attack_with          (if (and (<= enemy_border_count 1)
                                                (zero? neutral_border_count)
                                                (< (- armies attacking_armies) 3)) ; attack with all if no risk and not enough to attack further with
                                        (Math/max (dec armies) attacking_armies)
                                        attacking_armies)]
                ; (bot/log ["From " (:id region) " (" armies ") considering " target " needing " attack_with])
                (if (> armies attacking_armies)
                    (let [next-state     (update-in state [:regions (:id region) :armies] #(- % attack_with))
                          [state2 moves] (next_attacks next-state already_attacked (get-in next-state [:regions (:id region)]) (rest proritized))]
                        ; (bot/log (get-in next-state [:regions (:id region)]))
                        [state2 (cons [(:id region) target attack_with] moves)])
                    (next_attacks state already_attacked region (rest proritized)))))))

(defn transfers
    ([state]
        (transfers state (state/border_regions state) #{}))
    ([state regions considered]
        (if (empty? regions)
            []
            (let [region          (first regions)
                  neighbours      (:neighbours region)
                  unvisited       (filter #(not (contains? considered %)) neighbours)
                  resolved        (map #(get-in state [:regions %]) unvisited)
                  ours            (filter #(= (:our_name state) (:owner %)) resolved)
                  next-regions    (concat (rest regions) ours)
                  next-considered (clojure.set/union considered (set (map :id ours)))
                  with_armies     (filter #(> (:armies %) 1) ours)
                  moves           (map (fn [source] [(:id source) (:id region) (dec (:armies source))]) with_armies)]
                (concat moves (transfers state next-regions next-considered))))))

(defn attack
    [state]
    (->> (state/our_regions state)
        (filter #(> (:armies %) 1))
        (reduce
            (fn [[state attacks] region]
                (let [already_attacked (set (map second attacks))
                      [state new_attacks] (next_attacks state already_attacked region)]
                    [state (concat attacks new_attacks)]))
            [state []])
        (last)
        (concat (transfers state))))