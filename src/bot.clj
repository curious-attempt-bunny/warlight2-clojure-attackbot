(ns bot
    (:require handlers))

(defn send-command
    [command]
    (println command))

(defn parse
    [state line]
    (if (empty? line)
        state
        (let [parts            (clojure.string/split line #" ")
              [[handler args]] (for [s    [2 1]
                                    :let  [name   (clojure.string/replace (clojure.string/join "_" (take s parts)) "/" "_")
                                          handler (find-var (symbol (str "handlers/" name)))
                                          args    (drop s parts)]
                                    :when handler]
                                    [handler args])]
            (if (nil? handler)
                (throw (Exception. (str "Don't recognize: " line)))
                (do (prn handler args)
                (apply handler (cons state args)))))))

(defn -main
    [filename] 
    (let [input (if (nil? filename)
                    *in*
                    (java.io.FileReader. filename))]
        (reduce parse {} (line-seq (java.io.BufferedReader. input)))))