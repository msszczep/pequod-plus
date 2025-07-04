(ns pequod-plus.ccs-revised
  (:require
    [pequod-plus.util :as util]))

(defn add-ids [cs]
  (loop [i 1
         cs cs
         updated-cs []]
    (if (empty? cs)
      updated-cs
      (recur (inc i) (rest cs) (conj updated-cs (assoc (first cs) :id i))))))

(defn generate-goods [max-exponent-threshold num-goods]
  (mapv (fn [x] (hash-map :demand 0
                          :exponent (+ x (rand x))
                          :augment (rand-nth [(- 0.002) (- 0.001) 0 0.001 0.002])))
        (repeat num-goods max-exponent-threshold)))

(defn gen-cc [num-private-goods num-public-goods num-pollutants i]
  (let [max-exponent-threshold 0.005
        private-goods (add-ids (generate-goods max-exponent-threshold num-private-goods))
        public-goods (add-ids (generate-goods max-exponent-threshold num-public-goods))
        pollutant-permissions (add-ids (generate-goods max-exponent-threshold num-pollutants))
        pollutant-utilities {:positive-utility-from-income (rand-nth [0.11 0.13 0.15 0.17 0.19])
                             :negative-utility-from-exposure (rand-nth [1.11 1.13 1.15 1.17 1.19])}]
     (hash-map :id i
               :income 5000
               :private-goods private-goods
               :public-goods public-goods
               :pollutant-permissions pollutant-permissions
               :cohort {:region 1}
               :pollutant-utilities pollutant-utilities)))

(defn create-ccs-bulk [num-of-consumer-councils num-private-goods num-public-goods num-pollutants]
  (mapv (partial gen-cc num-private-goods num-public-goods num-pollutants)
        (range 1 (inc num-of-consumer-councils))))

; (pprint (create-ccs-bulk 30 10 1 1))
