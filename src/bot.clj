(ns bot
    (:gen-class
        :name Bot
        :main true)
    (:require handlers))

(defn parse
    [state line]
    (if (empty? line)
        state
        (let [parts            (clojure.string/split line #" ")
              [[handler args]] (for [s    [2 1]
                                    :let  [name    (clojure.string/join "_" (take s parts))
                                          handler (find-var (symbol (str "handlers/" name)))
                                          args    (drop s parts)]
                                    :when handler]
                                    [handler args])]
            (if (nil? handler)
                (throw (Exception. (str "Don't recognize: " line)))
                (let [next-state (apply handler (cons state args))]
                    (if (= state next-state)
                        (do (prn handler args)
                            (prn next-state)
                            (prn)))
                    next-state)))))

(defn main
    [filename] 
    (let [input (if (nil? filename)
                    *in*
                    (java.io.FileReader. filename))]
        (reduce parse {} (line-seq (java.io.BufferedReader. input)))))