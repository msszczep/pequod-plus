(ns pequod-plus.wcs)

(defn create-wcs [num-of-worker-councils goods industry]
  (->> goods
       (mapv #(vec (concat (repeat (/ num-of-worker-councils (count goods))
                                   {:industry industry 
                                    :product %}))))
       flatten))

; for however many inputs we have
;  apportion 95 points among those inputs
;  do some randomization such that it's within a suitable range
;  the sum must be greater than 0.5, less than 1

; range for 0.5 / number of inputs (length of flattened production-inputs)
;  up to 1 / number of inputs

(defn continue-wc-setup [intermediate-inputs nature-types labor-types pollutants wc]
  "Assumes wc is a map"
  (letfn [(get-random-subset [input-seq input-type]
            (let [take-num (if (= input-type :intermediate-inputs) 4 2)]
              (->> input-seq
                   shuffle
                   (take (inc (rand-int take-num)))
                   sort
                   vec)))
          (rand-range [start end]
            (+ start (clojure.core/rand (- end start))))
          (generate-exponents [n i inputs]
            (->> #(rand-range (/ 0.75 n) (/ 0.85 n))
                 repeatedly
                 (take (count (nth inputs i)))
                 vec))]
    (let [production-inputs (vector (get-random-subset intermediate-inputs :intermediate-inputs)
                                    (get-random-subset nature-types :nature-types)
                                    (get-random-subset labor-types :labor-types)
                                    (get-random-subset pollutants :pollutants))
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
                 :pollutant-expontents pollutant-exponents
                 :disutility-of-effort-exponent (rand-range 3 4) ; previously du
                 :disutility-of-effort-coefficient 1 ; previously s
                 :total-factor-productivity (rand-range 4 6) ; previously a
                 :effort 0.5
                 :output 0
                 :labor-quantities [0]}))))

; (def input-cats (into [] (range 1 101)))

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
                        input-cats ; pollutants
                        )))))

; (pequod-plus.wcs/create-wcs-bulk 100 100 100)
