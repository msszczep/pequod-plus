(ns pequod-plus.ccs)

(defn create-ccs-bulk [consumer-councils num-private-goods num-public-goods num-pollutants]
  (let [max-exponent-threshold 0.005
        private-good-exponents (->> #(+ max-exponent-threshold (rand max-exponent-threshold))
                                    repeatedly
                                    (take (* num-private-goods consumer-councils))
                                    (partition num-private-goods))
        public-good-exponents (->> #(+ max-exponent-threshold (rand max-exponent-threshold))
                                   repeatedly
                                   (take (* num-public-goods consumer-councils))
                                   (partition num-public-goods))]
    (mapv #(hash-map :income 5000
                     :private-good-exponents (vec (first %))
                     :public-good-demands (vec (repeat 5 0))
                     :private-good-demands (vec (repeat 1 0))
                     :public-good-exponents (vec (second %))
                     :pollutant-positive-utility-from-income (rand-nth [0.11 0.13 0.15 0.17 0.19])
                     :pollutant-negative-utility-from-exposure (rand-nth [1.11 1.13 1.15 1.17 1.19])
                     :pollutant-permissions (vec (repeat 1 0)))
          (partition 2 (interleave private-good-exponents
                                   public-good-exponents)))))
