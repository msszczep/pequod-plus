(ns pequod-plus.wcs-revised)

(defn create-wcs [num-of-worker-councils goods industry]
  (->> goods
       (mapv #(vec (concat (repeat (/ num-of-worker-councils (count goods))
                                   {:industry industry 
                                    :product %}))))
       flatten))

(defn gen-wc [intermediate-input-types nature-types labor-types pollutants wc]
  (letfn [(get-random-subsets []
            (let [s (->> [1 1 1 1 2 2 3 3 4 4]
                         shuffle
                         (take (inc (rand-int 4)))
                         frequencies)]
              (->> [1 2 3 4]
                   (map #(get s % 0))
                   (mapv inc))))
          (rand-range [start end]
            (+ start (clojure.core/rand (- end start))))
          (generate-exponents [n inputs-to-use]
            (->> #(rand-range (/ 0.75 n) (/ 0.85 n))
                 repeatedly
                 (take (count inputs-to-use))
                 vec))
          (determine-production-input [subset types]
            (->> types
                 shuffle
                 (take subset)
                 sort))
          (populate-c-and-e [coefficients exponents]
             (->> exponents
                  (interleave coefficients)
                  (partition 2)
                  (mapv (fn [[a b]] (hash-map :coefficient a :exponent b :augment (rand-nth [0 0.001 0.002 0.003 0.004]))))))]
    (let [[intermediate-inputs-subset
           nature-subset
           labor-subset
           pollutant-subset] (get-random-subsets)
          production-inputs-intermediate-input (determine-production-input intermediate-inputs-subset intermediate-input-types)
          production-inputs-nature (determine-production-input nature-subset nature-types)
          production-inputs-labor (determine-production-input labor-subset labor-types)
          production-inputs-pollutants (determine-production-input pollutant-subset pollutants)
          production-inputs-count (+ intermediate-inputs-subset nature-subset labor-subset pollutant-subset)
          intermediate-input-exponents (generate-exponents production-inputs-count production-inputs-intermediate-input)
          nature-exponents (generate-exponents production-inputs-count production-inputs-nature)
          labor-exponents (generate-exponents production-inputs-count production-inputs-labor)
          pollutant-exponents (generate-exponents production-inputs-count production-inputs-pollutants)
          ]
      (merge wc
        (hash-map :output 0
                  :effort 0
                  :intermediate-inputs (populate-c-and-e production-inputs-intermediate-input intermediate-input-exponents)
                  :nature (populate-c-and-e production-inputs-nature nature-exponents)
                  :labor (populate-c-and-e production-inputs-labor labor-exponents)
                  :pollutants (populate-c-and-e production-inputs-pollutants pollutant-exponents)
                  :disutility-of-effort {:exponent (rand-range 3 4) :coefficient 1}
                  :total-factor-productivity (rand-range 4 6)
                  :effort-elasticity (rand-range 0.05 0.1)
                  :labor-quantities [0]
                  )))))

(defn create-wcs-bulk [num-ind-0 num-ind-1 num-ind-2]
  (let [input-cats (vec (range 1 11))]
    (->> (merge (create-wcs num-ind-0 input-cats 0)
                (create-wcs num-ind-1 input-cats 1)
                (create-wcs num-ind-2 input-cats 2))
         flatten
         (mapv (partial gen-wc
                        input-cats ; intermediate-inputs
                        input-cats ; nature-types
                        input-cats ; labor-types
                        (range 1 2) ; pollutants
                        )))))

; (pequod-plus.wcs/create-wcs-bulk 100 100 100)

; (pprint (create-wcs-bulk 10 10 10))

; Why doesn't work for values < 10?
