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

(defn super_region_score
    ([state super_region] (super_region_score state super_region 0))
    ([state super_region army_delta]
        (let [reward (:reward super_region)
              armies (- (state/super_region_armies state super_region) army_delta)
              score  (if (zero? armies) reward (/ reward armies))]
            ; (bot/log (:super_regions state))
            ; (bot/log (str "super region " super_region " scores " score))
            score)))

(defn pick_starting_region
    [state ids]
    (->> ids
        (sort-by
            (fn [id] (super_region_score state (state/super_region state id) 2))
            >)
        first))

(defn placement_priorities
    [state]
    (->> (state/border_regions state)
        (sort-by
            (fn [region]
                (let [neighbours    (state/enemy_neighbours state region)
                      super_regions (set (map (partial state/super_region state) neighbours))
                      filtered      (filter #(not= (:our_name state) (state/super_region_owner state %)) super_regions)
                      best_super    (last (sort-by (fn [super_region] (super_region_score state super_region)) filtered))
                      neighbours_in_super (filter (fn [region_id] (= (:id best_super) (get-in state [:regions region_id :super_region_id]))) neighbours)
                      best_score    (+
                                      (* 10000 (super_region_score state best_super))
                                      (* 10 (count neighbours_in_super))
                                      (count neighbours))]
                    ; (bot/log [(:id region) best_score neighbours_in_super])
                    best_score))
            >)))

(defn bordering_neutral
    [state regions]
    (filter (fn [region]
        (let [neighbours (state/enemy_neighbours state region)]
            (some #(and
                (= "neutral" (:owner %))
                (= 2 (:armies %))) neighbours)))
        regions))

(defn place_armies
    ([state]
        (if (zero? (:starting_armies state))
            []
            (let [proritized    (placement_priorities state)
                  border_neutral (bordering_neutral state proritized)
                  [state moves] (place_armies state border_neutral [])
                  main_region   (first proritized)]
                    (conj moves [(:id main_region) (:starting_armies state)] ))))
    ([state proritized moves]
        (if (empty? proritized)
            [state moves]
            (let [region           (first proritized)
                  armies           (:armies region)
                  remaining_armies (:starting_armies state)
                  add              (min remaining_armies (- 4 (mod armies 4)))
                  move             [(:id region) add]
                  next-state       (-> state
                                    (update-in [:regions (:id region) :armies] (partial + add))
                                    (update-in [:starting_armies] #(- % add)))]
                (if (and (= armies 3) (= add 1))
                    (place_armies next-state (rest proritized) (conj moves move))
                    (place_armies next-state (rest proritized) moves))))))


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
                  attack_with          (if (and (<= (+ neutral_border_count enemy_border_count) 1)
                                                (>= (- armies attacking_armies) 3)) ; attack with all if no risk and not enough to attack further with
                                        (Math/max (dec armies) attacking_armies)
                                        attacking_armies)]
                ; (bot/log [(:id region) :-> target :have armies :need attack_with])
                ; (bot/log ["(<= enemy_border_count 1)" (<= enemy_border_count 1)])
                ; (bot/log ["(zero? neutral_border_count)" (zero? neutral_border_count)])
                ; (bot/log ["(zero? neutral_border_count)" (zero? neutral_border_count)])
                (if (> armies attacking_armies)
                    (let [next-state     (update-in state [:regions (:id region) :armies] #(- % attack_with))
                          next-state2    (assoc-in next-state [:regions target :owner] (:our_name state)) ; assume that we capture
                          next-state3    (assoc-in next-state [:regions target :newly-captured] true)
                          [state2 moves] (next_attacks next-state2 already_attacked (get-in next-state2 [:regions (:id region)]) (rest proritized))]
                        ; (bot/log (get-in next-state [:regions (:id region)]))
                        [state2 (cons [(:id region) target attack_with] moves)])
                    (next_attacks state already_attacked region (rest proritized)))))))

(defn transfers
    ([state]
        (transfers
          state
          (filter #(not= true (:newly-captured %)) (state/border_regions state))
          (set (map :id (state/border_regions state)))))
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
        ((fn [[state moves]] (concat (transfers state) moves)))))