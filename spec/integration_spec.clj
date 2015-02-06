(ns integration-spec
  (:require [speclj.core :refer :all]
            [bot :refer :all]
            [state]
            [brain]
            [handlers]))

(defn verify
    [game-name]
    (describe game-name
        (let [game  (slurp (str "spec/" game-name ".txt"))
              out   (java.io.ByteArrayOutputStream.)
              in    (java.io.ByteArrayInputStream. (.getBytes game))]
            (binding [*in*  (java.io.InputStreamReader. in)
                      *out* (java.io.OutputStreamWriter. out)]
                (bot/-main))
            (let [output   (String. (.toByteArray out))
                  actual   (last (clojure.string/split-lines output))
                  expected (->> (re-seq #"(?m)^# Valid: (.*)$" game) (map last))]
                (it (str "should output " (clojure.string/join " OR " expected))
                    (should-contain actual expected))))))

(describe "Sample game"
    (verify "PickSmall"))