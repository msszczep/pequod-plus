(ns pequod-plus.wcs)

; for however many inputs we have
;  apportion 95 points among those inputs
;  do some randomization such that it's within a suitable range
;  the sum must be greater than 0.5, less than 1

; range for 0.5 / number of inputs (length of flattened production-inputs)
;  up to 1 / number of inputs

(defn gen-wc [i product industry intermediate-input-types nature-types labor-types pollutants]
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
                 sort))]
    (let [[intermediate-input-subset 
           nature-subset 
           labor-subset 
           pollutant-subset] (get-random-subsets)
          production-inputs-intermediate-input (determine-production-input intermediate-input-subset intermediate-input-types) 
          production-inputs-nature (determine-production-input nature-subset nature-types)
          production-inputs-labor (determine-production-input labor-subset labor-types)
          production-inputs-pollutants (determine-production-input pollutant-subset pollutants)
          production-inputs-count (+ intermediate-inputs-suset nature-subset labor-subset pollutant-subset)
          intermediate-input-exponents (generate-exponents production-inputs-count production-inputs-intermediate-input)
          nature-exponents (generate-exponents production-inputs-count production-inputs-nature)
          labor-exponents (generate-exponents production-inputs-count production-inputs-labor)
          pollutant-exponents (generate-exponents production-inputs-count production-inputs-pollutants)
          ; TODO interleave and populate exponents, inputs
          ]
     (hash-map :id i
               :output 0
               :effort 0
               :product product
               :industry industry
               :intermediate-inputs []
               :cohort {:region 1}))))

(defn continue-wc-setup [intermediate-input-types nature-types labor-types pollutants wc]
  "Assumes wc is a map"
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
          (generate-exponents [n i inputs]
            (->> #(rand-range (/ 0.75 n) (/ 0.85 n))
                 repeatedly
                 (take (count (nth inputs i)))
                 vec))]
    (let [[s1 s2 s3 s4] (get-random-subsets)
          production-inputs (vector (into [] (sort (take s1 (shuffle intermediate-input-types))))
                                    (into [] (sort (take s2 (shuffle nature-types))))
                                    (into [] (sort (take s3 (shuffle labor-types))))
                                    (into [] (sort (take s4 (shuffle pollutants)))))
          production-inputs-count (->> production-inputs
                                       flatten
                                       count)
          intermediate-input-exponents (generate-exponents production-inputs-count 0 production-inputs)
          nature-exponents (generate-exponents production-inputs-count 1 production-inputs)
          labor-exponents (generate-exponents production-inputs-count 2 production-inputs)
          pollutant-exponents (generate-exponents production-inputs-count 3 production-inputs)]
      (merge wc {:production-inputs production-inputs
                 :effort-elasticity (rand-range 0.05 0.1) ; previously c
                 :intermediate-input-exponents intermediate-input-exponents
                 :nature-exponents nature-exponents
                 :labor-exponents labor-exponents
                 :pollutant-exponents pollutant-exponents
                 :disutility-of-effort-exponent (rand-range 3 4) ; previously du
                 :disutility-of-effort-coefficient 1 ; previously s
                 :total-factor-productivity (rand-range 4 6) ; previously a
                 :effort 0.5
                 :output 0
                 :labor-quantities [0]}))))

; TODO : update input here?
(defn create-wcs-bulk [num-ind-0 num-ind-1 num-ind-2]
  (let [input-cats (vec (range 1 11))]
    (->> (merge (create-wcs num-ind-0 input-cats 0)
                (create-wcs num-ind-1 input-cats 1)
                (create-wcs num-ind-2 input-cats 2))
         flatten
         (mapv (partial continue-wc-setup
                        input-cats ; intermediate-inputs
                        input-cats ; nature-types
                        input-cats ; labor-types
                        (range 1 2) ; pollutants
                        )))))

; (pequod-plus.wcs/create-wcs-bulk 100 100 100)
