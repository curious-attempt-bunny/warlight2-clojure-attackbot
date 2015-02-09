(ns rollouts)

; https://github.com/theaigames/conquest-engine/blob/master/main/Engine.java#L389-L400

; https://www.warlight.net/wiki/Luck_Modifier
; "When an attack happens, WarLight calculates the "expected" number of armies that should get killed, assuming an average roll. The attack is also randomed as normal and these two numbers are interpolated based on the luck percentage."

; https://www.warlight.net/wiki/Rounding_mode

(defn luck_kills
    [ratio number]
    (->> (map (fn [_] (< (Math/random) ratio)) (range 0 number))
         (filter (partial = true))
         (count)))

(defn no_luck_kills
    [ratio number]
    (* ratio number))

(defn rollout
    [desc ratio]
    (for [armies (range 1 15)
          :let [kills_no_luck (no_luck_kills ratio armies)
                kills_luck    (luck_kills ratio armies)
                kills         (java.lang.Math/round (+ (* kills_no_luck (- 1 0.16)) (* kills_luck 0.16)))
                min_kills     (java.lang.Math/round (+ (* kills_no_luck (- 1 0.16)) (* 0 0.16)))
                max_kills     (java.lang.Math/round (+ (* kills_no_luck (- 1 0.16)) (* armies 0.16)))
                est_max       (java.lang.Math/ceil (/ armies 2))]]
        (.println *out* (str desc " with " armies " kills [" min_kills "," max_kills "] e.g " kills " est_max " est_max))))

(defn attacking_minimums
    []
    (for [defending_armies (range 1 20)
          :let [max_attackers_killed (java.lang.Math/round (+
                                        (* (no_luck_kills 0.7 defending_armies) (- 1 0.16))
                                        (* defending_armies 0.16)))
                attackers_needed     (Math/max 1 (dec (* defending_armies 2)))
                needed_for_success   (Math/max (inc max_attackers_killed) attackers_needed)
                remaining_attackers  (- needed_for_success max_attackers_killed)]]
        (.println *out* (str needed_for_success "x" defending_armies " -- (-" max_attackers_killed ") --> " remaining_attackers))))

(defn attack_map
    []
    (for [defending_armies (range 1 20)
          :let [max_attackers_killed (java.lang.Math/round (+
                                        (* (no_luck_kills 0.7 defending_armies) (- 1 0.16))
                                        (* defending_armies 0.16)))
                attackers_needed     (Math/max 1 (dec (* defending_armies 2)))
                needed_for_success   (Math/max (inc max_attackers_killed) attackers_needed)
                remaining_attackers  (- needed_for_success max_attackers_killed)]]
        needed_for_success))