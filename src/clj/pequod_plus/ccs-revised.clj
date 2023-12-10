(ns pequod-plus.ccs-revised
  (:require
    [pequod-plus.util :as util]))

(defn generate-goods [max-exponent-threshold num-goods]
  (->> #(hash-map
         :exponent (+ max-exponent-threshold (rand max-exponent-threshold))
         :demand 0)
       repeatedly
       (take num-goods)
       (into [])))

(defn gen-cc [num-private-goods num-public-goods num-pollutants i]
  (let [max-exponent-threshold 0.005
        private-goods (add-ids (generate-goods max-exponent-threshold num-private-goods))
        public-goods (add-ids (generate-goods max-exponent-threshold num-public-goods))
        pollutants (add-ids (generate-goods max-exponent-threshold num-pollutants))
        pollutant-utilities {:positive-utility-from-income (rand-nth [0.11 0.13 0.15 0.17 0.19])
                             :negative-utility-from-exposure (rand-nth [1.11 1.13 1.15 1.17 1.19])}]
     (hash-map :id i
               :income 5000
               :private-goods private-goods
               :public-goods public-goods
               :pollutants pollutants
               :cohort {:region 1}
               :pollutant-utilities pollutant-utilities)))

(defn create-ccs-bulk [num-of-consumer-councils num-private-goods num-public-goods num-pollutants]
  (mapv (partial gen-cc num-private-goods num-public-goods num-pollutants)
        (range 1 (inc num-of-consumer-councils))))
