(ns clobol.clojure.sum-of-integers)

(defn sum-of-integers [n]
  (reduce + (range (inc n))))


(defn -main [& args]
  (println "Enter a positive integer")
  (let [n (Integer. (read-line))]
    (println (sum-of-integers n))))

