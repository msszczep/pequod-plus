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
