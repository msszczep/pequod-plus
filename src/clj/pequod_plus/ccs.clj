(ns pequod-plus.ccs)

(defn create-ccs-bulk [consumer-councils workers-per-council num-private-goods num-public-goods num-pollutants]
  (let [effort 1
        max-exponent-threshold 0.005
        private-good-exponents (->> #(+ max-exponent-threshold (rand max-exponent-threshold))
                                    repeatedly
                                    (take (* num-private-goods consumer-councils))
                                    (partition num-private-goods))
        public-good-exponents (->> #(+ max-exponent-threshold (rand max-exponent-threshold))
                                   repeatedly
                                   (take (* num-public-goods consumer-councils))
                                   (partition num-public-goods))
        pollutant-exponents (->> #(+ max-exponent-threshold (rand max-exponent-threshold))
                                   repeatedly
                                   (take (* num-pollutants consumer-councils))
                                   (partition num-pollutants))]
    (mapv #(hash-map :num-workers workers-per-council
                     :effort effort
                     :income (* 500 effort workers-per-council)
                     :total-factor-utility 1
                     :private-good-exponents (vec (first %))
                     :public-good-demands (vec (repeat 5 0))
                     :private-good-demands (vec (repeat 1 0))
                     :public-good-exponents (vec (second %))
                     :pollutant-permissions (vec (repeat 1 0))
                     :pollutant-exponents (vec (last %)))
          (partition 2 (interleave private-good-exponents
                                   public-good-exponents
                                   pollutant-exponents)))))
