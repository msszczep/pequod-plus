(ns pequod-plus.util)

(defn third [s]
  (first (next (next s))))

(defn add-ids [cs]
  (loop [i 1
         cs cs
         updated-cs []]
    (if (empty? cs)
      updated-cs
      (recur (inc i) (rest cs) (conj updated-cs (assoc (first cs) :id i))))))

(defn initialize-prices [t]
  (let [num-private-goods (t :private-goods)
        num-im-inputs (t :intermediate-inputs)
        num-resources (t :resources)
        num-labor (t :labors)
        num-public-goods (t :public-goods)
        num-pollutants (t :pollutants)]
    (assoc t
           :private-good-prices (vec (repeat num-private-goods (t :init-private-good-price)))
           :intermediate-good-prices (vec (repeat num-im-inputs (t :init-intermediate-price)))
           :nature-prices (vec (repeat num-resources (t :init-nature-price)))
           :labor-prices (vec (repeat num-labor (t :init-labor-price)))
           :public-good-prices (vec (repeat num-public-goods (t :init-public-good-price)))
           :pollutant-prices (vec (repeat num-pollutants (t :init-pollutant-price)))
           :price-deltas (vec (repeat (+ num-private-goods num-im-inputs num-resources num-labor num-public-goods num-pollutants) 0.05))
           :pdlist (vec (repeat (+ num-private-goods num-im-inputs num-resources num-labor num-public-goods num-pollutants) 0.25)))))

(defn augment-exponents [council-type exponents]
  (let [augments-to-use (if (= :wc council-type)
                            [0 0.001 0.002 0.003 0.004]
                            [(- 0.002) (- 0.001) 0 0.001 0.002])]
   (->> #(rand-nth augments-to-use)
        repeatedly
        (take (count exponents))
        (interleave exponents)
        (partition 2)
        (map (fn [[a b]] (+ a b))))))

(defn augment-wc [wc]
  (assoc wc :intermediate-input-exponents (augment-exponents :wc (get wc :intermediate-input-exponents))
            :nature-exponents (augment-exponents :wc (get wc :nature-exponents))
            :labor-exponents (augment-exponents :wc (get wc :labor-exponents))
            :pollutant-exponents (augment-exponents :wc (get wc :pollutant-exponents))))

(defn augment-cc [cc]
  (assoc cc :private-good-exponents (augment-exponents :cc (get cc :private-good-exponents))
            :public-good-exponents (augment-exponents :cc (get cc :public-good-exponents))))

(defn augmented-reset [t]
  (assoc t :iteration 0
           :ccs (mapv augment-cc (get t :ccs))
           :wcs (mapv augment-wc (get t :wcs))))

; NB: Watch for pollutant-prices and scaling effects -- i.e., does a price affect all CCs or just one CC?
; TODO: Revise once we have revised data structures
(defn consume [private-goods private-good-prices public-goods public-good-prices pollutants pollutant-prices num-of-ccs cc]
  (let [private-good-exponents (cc :private-good-exponents)
        public-good-exponents (cc :public-good-exponents)
        pollutant-positive-utility-from-income (cc :pollutant-positive-utility-from-income) 
        pollutant-negative-utility-from-exposure (cc :pollutant-negative-utility-from-exposure)
        pollutant-permissions (mapv (fn [pollutant]
                                      (let [p (nth pollutant-prices (dec pollutant))
                                            k pollutant-negative-utility-from-exposure
                                            j pollutant-positive-utility-from-income]
                                       (* (Math/pow 5 (/ 1 (- k j)))
                                          (Math/pow (/ (* j (Math/pow p j)) k) (/ 1 (- k j))))))
                                  pollutants)
        income (apply + (cc :income) pollutant-permissions)
        private-good-demands (mapv
                               (fn [private-good]
                                 (/ (* income (nth private-good-exponents (dec private-good)))
                                    (* (apply + (concat private-good-exponents public-good-exponents))
                                       (nth private-good-prices (dec private-good)))))
                               private-goods)
        public-good-demands (mapv (fn [public-good]
                                    (/ (* income (nth public-good-exponents (dec public-good)))
                                       (* (apply + (concat private-good-exponents public-good-exponents))
                                          (/ (nth public-good-prices (dec public-good))
                                             num-of-ccs))))
                                  public-goods)]
    (assoc cc :private-good-demands private-good-demands
              :public-good-demands public-good-demands
              :pollutant-permissions pollutant-permissions
              :income income)))

(defn mean [L]
  (/ (reduce + L) (count L)))

; TODO: Delete after restructuring?
(defn allot-production-quantities [production-inputs xs]
  (let [[num-intermediate-input-quantities num-nature-quantities num-labor-quantities num-pollutant-quantities] (map count production-inputs)
        intermediate-input-quantities (->> xs
                                           (take num-intermediate-input-quantities)
                                           (into []))
        nature-quantities (->> xs
                               (drop num-intermediate-input-quantities)
                               (take num-nature-quantities)
                               (into []))
        labor-quantities (->> xs
                              (drop (+ num-intermediate-input-quantities num-nature-quantities))
                              (take num-labor-quantities)
                              (into []))
        pollutant-quantities (->> xs
                                  (drop (+ num-intermediate-input-quantities num-nature-quantities num-labor-quantities))
                                  (into []))]
    [intermediate-input-quantities nature-quantities labor-quantities pollutant-quantities]))

(defn solution-1 [a s c k ps b λ p-i]
  (let [b1 (first b)
        p1 (first (flatten ps))
        output (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ)))) (+ c (- k) (* k b1))))
        x1 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* c (Math/log b1)) (- (* k (Math/log b1))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* c (Math/log p1))) (* k (Math/log p1)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1))))
        effort (Math/pow Math/E (/ (+ (- (* (Math/log a))) (- (* b1 (Math/log b1))) (- (* (Math/log c))) (* b1 (Math/log c)) (* (Math/log k)) (- (* b1 (Math/log k))) (* b1 (Math/log p1)) (* (Math/log s)) (- (* b1 (Math/log s))) (- (* (Math/log λ)))) (+ c (- k) (* k b1))))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1])]
    {:output output
     :x1 x1
     :effort effort
     :intermediate-input-quantities intermediate-input-qs
     :nature-quantities nature-qs
     :labor-quantities labor-qs
     :pollutant-quantities pollutant-qs}))

(defn solution-2 [a s c k ps b λ p-i]
  (let [[b1 b2] b
        [p1 p2] (flatten ps)
        output (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2))))
        x1 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* c (Math/log b1)) (- (* k (Math/log b1))) (* b2 k (Math/log b1)) (- (* b2 k (Math/log b2))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* c (Math/log p1))) (* k (Math/log p1)) (- (* b2 k (Math/log p1))) (* b2 k (Math/log p2)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2))))
        x2 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (* c (Math/log b2)) (- (* k (Math/log b2))) (* b1 k (Math/log b2)) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (- (* c (Math/log p2))) (* k (Math/log p2)) (- (* b1 k (Math/log p2))) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2))))
        effort (Math/pow Math/E (/ (+ (- (* (Math/log a))) (- (* b1 (Math/log b1))) (- (* b2 (Math/log b2))) (- (* (Math/log c))) (* b1 (Math/log c)) (* b2 (Math/log c)) (* (Math/log k)) (- (* b1 (Math/log k))) (- (* b2 (Math/log k))) (* b1 (Math/log p1)) (* b2 (Math/log p2)) (* (Math/log s)) (- (* b1 (Math/log s))) (- (* b2 (Math/log s))) (- (* (Math/log λ)))) (+ c (- k) (* k b1) (* k b2))))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2])]
    {:output output
     :x1 x1
     :x2 x2
     :effort effort
     :intermediate-input-quantities intermediate-input-qs
     :nature-quantities nature-qs
     :labor-quantities labor-qs
     :pollutant-quantities pollutant-qs}))

(defn solution-3 [a s c k ps b λ p-i]
  (let [[b1 b2 b3] b
        [p1 p2 p3] (flatten ps)
        output (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3))))
        x1 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* c (Math/log b1)) (- (* k (Math/log b1))) (* b2 k (Math/log b1)) (* b3 k (Math/log b1)) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* c (Math/log p1))) (* k (Math/log p1)) (- (* b2 k (Math/log p1))) (- (* b3 k (Math/log p1))) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3))))
        x2 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (* c (Math/log b2)) (- (* k (Math/log b2))) (* b1 k (Math/log b2)) (* b3 k (Math/log b2)) (- (* b3 k (Math/log b3))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (- (* c (Math/log p2))) (* k (Math/log p2)) (- (* b1 k (Math/log p2))) (- (* b3 k (Math/log p2))) (* b3 k (Math/log p3)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3))))
        x3 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (* c (Math/log b3)) (- (* k (Math/log b3))) (* b1 k (Math/log b3)) (* b2 k (Math/log b3)) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (- (* c (Math/log p3))) (* k (Math/log p3)) (- (* b1 k (Math/log p3))) (- (* b2 k (Math/log p3))) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3))))
        effort (Math/pow Math/E (/ (+ (- (* (Math/log a))) (- (* b1 (Math/log b1))) (- (* b2 (Math/log b2))) (- (* b3 (Math/log b3))) (* b1 (Math/log p1)) (* b2 (Math/log p2)) (* b3 (Math/log p3)) (- (* b1 (Math/log λ))) (- (* b2 (Math/log λ))) (- (* b3 (Math/log λ))) (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3))) (- (/ (* b1 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ)))))  (+ c (- k) (* k b1) (* k b2) (* k b3)))) (- (/ (* b2 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))))) (+ c (- k) (* k b1) (* k b2) (* k b3)))) (- (/ (* b3 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))))) (+ c (- k) (* k b1) (* k b2) (* k b3))))) c))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3])]
    {:output output
     :x1 x1
     :x2 x2
     :x3 x3
     :effort effort
     :intermediate-input-quantities intermediate-input-qs
     :nature-quantities nature-qs
     :labor-quantities labor-qs
     :pollutant-quantities pollutant-qs}))

(defn solution-4 [a s c k ps b λ p-i]
  (let [[b1 b2 b3 b4] b
        [p1 p2 p3 p4] (flatten ps)
        output (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4))))
        x1 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* c (Math/log b1)) (- (* k (Math/log b1))) (* b2 k (Math/log b1)) (* b3 k (Math/log b1)) (* b4 k (Math/log b1)) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* c (Math/log p1))) (* k (Math/log p1)) (- (* b2 k (Math/log p1))) (- (* b3 k (Math/log p1))) (- (* b4 k (Math/log p1))) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4))))
        x2 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (* c (Math/log b2)) (- (* k (Math/log b2))) (* b1 k (Math/log b2)) (* b3 k (Math/log b2)) (* b4 k (Math/log b2)) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (- (* c (Math/log p2))) (* k (Math/log p2)) (- (* b1 k (Math/log p2))) (- (* b3 k (Math/log p2))) (- (* b4 k (Math/log p2))) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4))))
        x3 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (* c (Math/log b3)) (- (* k (Math/log b3))) (* b1 k (Math/log b3)) (* b2 k (Math/log b3)) (* b4 k (Math/log b3)) (- (* b4 k (Math/log b4))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (- (* c (Math/log p3))) (* k (Math/log p3)) (- (* b1 k (Math/log p3))) (- (* b2 k (Math/log p3))) (- (* b4 k (Math/log p3))) (* b4 k (Math/log p4)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4))))
        x4 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (* c (Math/log b4)) (- (* k (Math/log b4))) (* b1 k (Math/log b4)) (* b2 k (Math/log b4)) (* b3 k (Math/log b4)) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (- (* c (Math/log p4))) (* k (Math/log p4)) (- (* b1 k (Math/log p4))) (- (* b2 k (Math/log p4))) (- (* b3 k (Math/log p4))) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4))))
     effort (Math/pow Math/E (/ (+ (- (* (Math/log a))) (- (* b1 (Math/log b1))) (- (* b2 (Math/log b2))) (- (* b3 (Math/log b3))) (- (* b4 (Math/log b4))) (- (* (Math/log c))) (* b1 (Math/log c)) (* b2 (Math/log c)) (* b3 (Math/log c)) (* b4 (Math/log c)) (* (Math/log k)) (- (* b1 (Math/log k))) (- (* b2 (Math/log k))) (- (* b3 (Math/log k))) (- (* b4 (Math/log k))) (* b1 (Math/log p1)) (* b2 (Math/log p2)) (* b3 (Math/log p3)) (* b4 (Math/log p4)) (* (Math/log s)) (- (* b1 (Math/log s))) (- (* b2 (Math/log s))) (- (* b3 (Math/log s))) (- (* b4 (Math/log s))) (- (* (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4))))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4])]
    {:output output
     :x1 x1
     :x2 x2
     :x3 x3
     :x4 x4
     :effort effort
     :intermediate-input-quantities intermediate-input-qs
     :nature-quantities nature-qs
     :labor-quantities labor-qs
     :pollutant-quantities pollutant-qs}))

(defn solution-5 [a s c k ps b λ p-i]
  (let [[b1 b2 b3 b4 b5] b
        [p1 p2 p3 p4 p5] (flatten ps)
        output (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))))
        x1 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* c (Math/log b1)) (- (* k (Math/log b1))) (* b2 k (Math/log b1)) (* b3 k (Math/log b1)) (* b4 k (Math/log b1)) (* b5 k (Math/log b1)) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* c (Math/log p1))) (* k (Math/log p1)) (- (* b2 k (Math/log p1))) (- (* b3 k (Math/log p1))) (- (* b4 k (Math/log p1))) (- (* b5 k (Math/log p1))) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))))
        x2 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (* c (Math/log b2)) (- (* k (Math/log b2))) (* b1 k (Math/log b2)) (* b3 k (Math/log b2)) (* b4 k (Math/log b2)) (* b5 k (Math/log b2)) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (- (* c (Math/log p2))) (* k (Math/log p2)) (- (* b1 k (Math/log p2))) (- (* b3 k (Math/log p2))) (- (* b4 k (Math/log p2))) (- (* b5 k (Math/log p2))) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))))
        x3 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (* c (Math/log b3)) (- (* k (Math/log b3))) (* b1 k (Math/log b3)) (* b2 k (Math/log b3)) (* b4 k (Math/log b3)) (* b5 k (Math/log b3)) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (- (* c (Math/log p3))) (* k (Math/log p3)) (- (* b1 k (Math/log p3))) (- (* b2 k (Math/log p3))) (- (* b4 k (Math/log p3))) (- (* b5 k (Math/log p3))) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))))
        x4 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (* c (Math/log b4)) (- (* k (Math/log b4))) (* b1 k (Math/log b4)) (* b2 k (Math/log b4)) (* b3 k (Math/log b4)) (* b5 k (Math/log b4)) (- (* b5 k (Math/log b5))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (- (* c (Math/log p4))) (* k (Math/log p4)) (- (* b1 k (Math/log p4))) (- (* b2 k (Math/log p4))) (- (* b3 k (Math/log p4))) (- (* b5 k (Math/log p4))) (* b5 k (Math/log p5)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))))
        x5 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (* c (Math/log b5)) (- (* k (Math/log b5))) (* b1 k (Math/log b5)) (* b2 k (Math/log b5)) (* b3 k (Math/log b5)) (* b4 k (Math/log b5)) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (- (* c (Math/log p5))) (* k (Math/log p5)) (- (* b1 k (Math/log p5))) (- (* b2 k (Math/log p5))) (- (* b3 k (Math/log p5))) (- (* b4 k (Math/log p5))) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))))
        effort (Math/pow Math/E (/ (+ (- ( * (Math/log a))) (- (* b1 (Math/log b1))) (- (* b2 (Math/log b2))) (- (* b3 (Math/log b3))) (- (* b4 (Math/log b4))) (- (* b5 (Math/log b5))) (* b1 (Math/log p1)) (* b2 (Math/log p2)) (* b3 (Math/log p3)) (* b4 (Math/log p4)) (* b5 (Math/log p5)) (- (* b1 (Math/log λ))) (- (* b2 (Math/log λ))) (- (* b3 (Math/log λ))) (- (* b4 (Math/log λ))) (- (* b5 (Math/log λ))) (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))) (- (/ (* b1 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ)))))  (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5)))) (- (/ (* b2 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ))))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5)))) (- (/ (* b3 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ)))))  (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5)))) (- (/ (* b4 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ))))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5)))) (- (/ (* b5 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ))))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))))) c))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5])]
     {:output output
      :x1 x1
      :x2 x2
      :x3 x3
      :x4 x4
      :x5 x5
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs
      :pollutant-quantities pollutant-qs}))

(defn solution-6 [a s c k ps b λ p-i]
  (let [[b1 b2 b3 b4 b5 b6] b
        [p1 p2 p3 p4 p5 p6] (flatten ps)
        output (Math/pow Math/E (- (/ (+ (* k (Math/log a)) (* b1 k (Math/log b1)) (* b2 k (Math/log b2)) (* b3 k (Math/log b3)) (* b4 k (Math/log b4)) (* b5 k (Math/log b5)) (* b6 k (Math/log b6)) (* c (Math/log c)) (- (* c (Math/log k))) (- (* b1 k (Math/log p1))) (- (* b2 k (Math/log p2))) (- (* b3 k (Math/log p3))) (- (* b4 k (Math/log p4))) (- (* b5 k (Math/log p5))) (- (* b6 k (Math/log p6)))  (- (* c (Math/log s))) (* c (Math/log λ)) (* b1 k (Math/log λ)) (* b2 k (Math/log λ)) (* b3 k (Math/log λ)) (* b4 k (Math/log λ)) (* b5 k (Math/log λ)) (* b6 k (Math/log λ))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6)))))
        x1 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* c (Math/log b1)) (- (* k (Math/log b1))) (* b2 k (Math/log b1)) (* b3 k (Math/log b1)) (* b4 k (Math/log b1)) (* b5 k (Math/log b1)) (* b6 k (Math/log b1)) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* c (Math/log p1))) (* k (Math/log p1)) (- (* b2 k (Math/log p1))) (- (* b3 k (Math/log p1))) (- (* b4 k (Math/log p1))) (- (* b5 k (Math/log p1))) (- (* b6 k (Math/log p1))) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6))))
        x2 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (* c (Math/log b2)) (- (* k (Math/log b2))) (* b1 k (Math/log b2)) (* b3 k (Math/log b2)) (* b4 k (Math/log b2)) (* b5 k (Math/log b2)) (* b6 k (Math/log b2)) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (- (* c (Math/log p2))) (* k (Math/log p2)) (- (* b1 k (Math/log p2))) (- (* b3 k (Math/log p2))) (- (* b4 k (Math/log p2))) (- (* b5 k (Math/log p2))) (- (* b6 k (Math/log p2))) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6))))
        x3 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (* c (Math/log b3)) (- (* k (Math/log b3))) (* b1 k (Math/log b3)) (* b2 k (Math/log b3)) (* b4 k (Math/log b3)) (* b5 k (Math/log b3)) (* b6 k (Math/log b3)) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (- (* c (Math/log p3))) (* k (Math/log p3)) (- (* b1 k (Math/log p3))) (- (* b2 k (Math/log p3))) (- (* b4 k (Math/log p3))) (- (* b5 k (Math/log p3))) (- (* b6 k (Math/log p3))) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6))))
        x4 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (* c (Math/log b4)) (- (* k (Math/log b4))) (* b1 k (Math/log b4)) (* b2 k (Math/log b4)) (* b3 k (Math/log b4)) (* b5 k (Math/log b4)) (* b6 k (Math/log b4)) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (- (* c (Math/log p4))) (* k (Math/log p4)) (- (* b1 k (Math/log p4))) (- (* b2 k (Math/log p4))) (- (* b3 k (Math/log p4))) (- (* b5 k (Math/log p4))) (- (* b6 k (Math/log p4))) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6))))
        x5 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (* c (Math/log b5)) (- (* k (Math/log b5))) (* b1 k (Math/log b5)) (* b2 k (Math/log b5)) (* b3 k (Math/log b5)) (* b4 k (Math/log b5)) (* b6 k (Math/log b5)) (- (* b6 k (Math/log b6))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (- (* c (Math/log p5))) (* k (Math/log p5)) (- (* b1 k (Math/log p5))) (- (* b2 k (Math/log p5))) (- (* b3 k (Math/log p5))) (- (* b4 k (Math/log p5))) (- (* b6 k (Math/log p5))) (* b6 k (Math/log p6)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6))))
        x6 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (* c (Math/log b6)) (- (* k (Math/log b6))) (* b1 k (Math/log b6)) (* b2 k (Math/log b6)) (* b3 k (Math/log b6)) (* b4 k (Math/log b6)) (* b5 k (Math/log b6)) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (- (* c (Math/log p6))) (* k (Math/log p6)) (- (* b1 k (Math/log p6))) (- (* b2 k (Math/log p6))) (- (* b3 k (Math/log p6))) (- (* b4 k (Math/log p6))) (- (* b5 k (Math/log p6))) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6))))
        effort (Math/pow Math/E (/ (+ (- (* (Math/log a))) (- (* b1 (Math/log b1))) (- (* b2 (Math/log b2))) (- (* b3 (Math/log b3))) (- (* b4 (Math/log b4))) (- (* b5 (Math/log b5))) (- (* b6 (Math/log b6))) (- (* (Math/log c))) (* b1 (Math/log c)) (* b2 (Math/log c)) (* b3 (Math/log c)) (* b4 (Math/log c)) (* b5 (Math/log c)) (* b6 (Math/log c)) (* (Math/log k)) (- (* b1 (Math/log k))) (- (* b2 (Math/log k))) (- (* b3 (Math/log k))) (- (* b4 (Math/log k))) (- (* b5 (Math/log k))) (- (* b6 (Math/log k))) (* b1 (Math/log p1)) (* b2 (Math/log p2)) (* b3 (Math/log p3)) (* b4 (Math/log p4)) (* b5 (Math/log p5)) (* b6 (Math/log p6)) (* (Math/log s)) (- (* b1 (Math/log s))) (- (* b2 (Math/log s))) (- (* b3 (Math/log s))) (- (* b4 (Math/log s))) (- (* b5 (Math/log s))) (- (* b6 (Math/log s))) (- (* (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6))))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5 x6])]
     {:output output
      :x1 x1
      :x2 x2
      :x3 x3
      :x4 x4
      :x5 x5
      :x6 x6
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs
      :pollutant-quantities pollutant-qs}))

(defn solution-7 [a s c k ps b λ p-i]
  (let [[b1 b2 b3 b4 b5 b6 b7] b
        [p1 p2 p3 p4 p5 p6 p7] (flatten ps)
        output (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* c (Math/log k)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* c (Math/log c))) (- (* b1 k (Math/log b1))) (* b1 k (Math/log p1)) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log b2))) (* b2 k (Math/log p2)) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log b3))) (* b3 k (Math/log p3)) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log b4))) (* b4 k (Math/log p4)) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log b5))) (* b5 k (Math/log p5)) (- (* b5 k (Math/log λ))) (- (* b6 k (Math/log b6))) (* b6 k (Math/log p6)) (- (* b6 k (Math/log λ))) (- (* b7 k (Math/log b7))) (* b7 k (Math/log p7)) (- (* b7 k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7))))
        x1 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b2 k (Math/log b1)) (* b3 k (Math/log b1)) (* b4 k (Math/log b1)) (* b5 k (Math/log b1)) (* b6 k (Math/log b1)) (* b7 k (Math/log b1)) (* c (Math/log b1)) (- (* k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b2 k (Math/log p1))) (- (* b3 k (Math/log p1))) (- (* b4 k (Math/log p1))) (- (* b5 k (Math/log p1))) (- (* b6 k (Math/log p1))) (- (* b7 k (Math/log p1))) (* k (Math/log p1)) (- (* c (Math/log p1))) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7))))
        x2 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b1 k (Math/log b2)) (* b3 k (Math/log b2)) (* b4 k (Math/log b2)) (* b5 k (Math/log b2)) (* b6 k (Math/log b2)) (* b7 k (Math/log b2)) (* c (Math/log b2)) (- (* k (Math/log b2))) (- (* b1 k (Math/log b1))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b1 k (Math/log p2))) (- (* b3 k (Math/log p2))) (- (* b4 k (Math/log p2))) (- (* b5 k (Math/log p2))) (- (* b6 k (Math/log p2))) (- (* b7 k (Math/log p2))) (* k (Math/log p2)) (- (* c (Math/log p2))) (* b1 k (Math/log p1)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7))))
        x3 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b1 k (Math/log b3)) (* b2 k (Math/log b3)) (* b4 k (Math/log b3)) (* b5 k (Math/log b3)) (* b6 k (Math/log b3)) (* b7 k (Math/log b3)) (* c (Math/log b3)) (- (* k (Math/log b3))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b1 k (Math/log p3))) (- (* b2 k (Math/log p3))) (- (* b4 k (Math/log p3))) (- (* b5 k (Math/log p3))) (- (* b6 k (Math/log p3))) (- (* b7 k (Math/log p3))) (* k (Math/log p3)) (- (* c (Math/log p3))) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7))))
        x4 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b1 k (Math/log b4)) (* b2 k (Math/log b4)) (* b3 k (Math/log b4)) (* b5 k (Math/log b4)) (* b6 k (Math/log b4)) (* b7 k (Math/log b4)) (* c (Math/log b4)) (- (* k (Math/log b4))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b1 k (Math/log p4))) (- (* b2 k (Math/log p4))) (- (* b3 k (Math/log p4))) (- (* b5 k (Math/log p4))) (- (* b6 k (Math/log p4))) (- (* b7 k (Math/log p4))) (* k (Math/log p4)) (- (* c (Math/log p4))) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7))))
        x5 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b1 k (Math/log b5)) (* b2 k (Math/log b5)) (* b3 k (Math/log b5)) (* b4 k (Math/log b5)) (* b6 k (Math/log b5)) (* b7 k (Math/log b5)) (* c (Math/log b5)) (- (* k (Math/log b5))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b1 k (Math/log p5))) (- (* b2 k (Math/log p5))) (- (* b3 k (Math/log p5))) (- (* b4 k (Math/log p5))) (- (* b6 k (Math/log p5))) (- (* b7 k (Math/log p5))) (* k (Math/log p5)) (- (* c (Math/log p5))) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7))))
        x6 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b1 k (Math/log b6)) (* b2 k (Math/log b6)) (* b3 k (Math/log b6)) (* b4 k (Math/log b6)) (* b5 k (Math/log b6)) (* b7 k (Math/log b6)) (* c (Math/log b6)) (- (* k (Math/log b6))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b1 k (Math/log p6))) (- (* b2 k (Math/log p6))) (- (* b3 k (Math/log p6))) (- (* b4 k (Math/log p6))) (- (* b5 k (Math/log p6))) (- (* b7 k (Math/log p6))) (* k (Math/log p6)) (- (* c (Math/log p6))) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7))))
        x7 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b1 k (Math/log b7)) (* b2 k (Math/log b7)) (* b3 k (Math/log b7)) (* b4 k (Math/log b7)) (* b5 k (Math/log b7)) (* b6 k (Math/log b7)) (* c (Math/log b7)) (- (* k (Math/log b7))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b1 k (Math/log p7))) (- (* b2 k (Math/log p7))) (- (* b3 k (Math/log p7))) (- (* b4 k (Math/log p7))) (- (* b5 k (Math/log p7))) (- (* b6 k (Math/log p7))) (* k (Math/log p7)) (- (* c (Math/log p7))) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7))))
        effort (Math/pow Math/E (/ (+ (- (* (Math/log a))) (- (* b1 (Math/log b1))) (- (* b2 (Math/log b2))) (- (* b3 (Math/log b3))) (- (* b4 (Math/log b4))) (- (* b5 (Math/log b5))) (- (* b6 (Math/log b6))) (- (* b7 (Math/log b7))) (* b1 (Math/log p1)) (* b2 (Math/log p2)) (* b3 (Math/log p3)) (* b4 (Math/log p4)) (* b5 (Math/log p5)) (* b6 (Math/log p6)) (* b7 (Math/log p7)) (- (* b1 (Math/log λ))) (- (* b2 (Math/log λ))) (- (* b3 (Math/log λ))) (- (* b4 (Math/log λ))) (- (* b5 (Math/log λ))) (- (* b6 (Math/log λ))) (- (* b7 (Math/log λ))) (/ (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ))) (- (* b6 k (Math/log λ))) (- (* b7 k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7))) (- (/ (* b1 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ))) (- (* b6 k (Math/log λ))) (- (* b7 k (Math/log λ))))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7)))) (- (/ (* b2 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ))) (- (* b6 k (Math/log λ))) (- (* b7 k (Math/log λ))))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7)))) (- (/ (* b3 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ))) (- (* b6 k (Math/log λ))) (- (* b7 k (Math/log λ))))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7)))) (- (/ (* b4 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ))) (- (* b6 k (Math/log λ))) (- (* b7 k (Math/log λ))))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7)))) (- (/ (* b5 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ))) (- (* b6 k (Math/log λ))) (- (* b7 k (Math/log λ))))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7)))) (- (/ (* b6 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ))) (- (* b6 k (Math/log λ))) (- (* b7 k (Math/log λ))))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7)))) (- (/ (* b7 (+ (- (* k (Math/log a))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log λ))) (- (* b6 k (Math/log λ))) (- (* b7 k (Math/log λ))))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7))))) c))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5 x6 x7])]
     {:output output
      :x1 x1
      :x2 x2
      :x3 x3
      :x4 x4
      :x5 x5
      :x6 x6
      :x7 x7
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs
      :pollutant-quantities pollutant-qs}))

(defn solution-8 [a s c k ps b λ p-i]
  (let [[b1 b2 b3 b4 b5 b6 b7 b8] b
        [p1 p2 p3 p4 p5 p6 p7 p8] (flatten ps)
        output (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* c (Math/log k)) (* c (Math/log s)) (- (* c (Math/log λ))) (- (* c (Math/log c))) (- (* b1 k (Math/log b1))) (* b1 k (Math/log p1)) (- (* b1 k (Math/log λ))) (- (* b2 k (Math/log b2))) (* b2 k (Math/log p2)) (- (* b2 k (Math/log λ))) (- (* b3 k (Math/log b3))) (* b3 k (Math/log p3)) (- (* b3 k (Math/log λ))) (- (* b4 k (Math/log b4))) (* b4 k (Math/log p4)) (- (* b4 k (Math/log λ))) (- (* b5 k (Math/log b5))) (* b5 k (Math/log p5)) (- (* b5 k (Math/log λ))) (- (* b6 k (Math/log b6))) (* b6 k (Math/log p6)) (- (* b6 k (Math/log λ))) (- (* b7 k (Math/log b7))) (* b7 k (Math/log p7)) (- (* b7 k (Math/log λ))) (- (* b8 k (Math/log b8))) (* b8 k (Math/log p8)) (- (* b8 k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7) (* k b8))))
        x1 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b2 k (Math/log b1)) (* b3 k (Math/log b1)) (* b4 k (Math/log b1)) (* b5 k (Math/log b1)) (* b6 k (Math/log b1)) (* b7 k (Math/log b1)) (* b8 k (Math/log b1)) (* c (Math/log b1)) (- (* k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* b8 k (Math/log b8))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b2 k (Math/log p1))) (- (* b3 k (Math/log p1))) (- (* b4 k (Math/log p1))) (- (* b5 k (Math/log p1))) (- (* b6 k (Math/log p1))) (- (* b7 k (Math/log p1))) (- (* b8 k (Math/log p1))) (* k (Math/log p1)) (- (* c (Math/log p1))) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* b8 k (Math/log p8)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7) (* k b8))))
        x2 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b1 k (Math/log b2)) (* b3 k (Math/log b2)) (* b4 k (Math/log b2)) (* b5 k (Math/log b2)) (* b6 k (Math/log b2)) (* b7 k (Math/log b2)) (* b8 k (Math/log b2)) (* c (Math/log b2)) (- (* k (Math/log b2))) (- (* b1 k (Math/log b1))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* b8 k (Math/log b8))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b1 k (Math/log p2))) (- (* b3 k (Math/log p2))) (- (* b4 k (Math/log p2))) (- (* b5 k (Math/log p2))) (- (* b6 k (Math/log p2))) (- (* b7 k (Math/log p2))) (- (* b8 k (Math/log p2))) (* k (Math/log p2)) (- (* c (Math/log p2))) (* b1 k (Math/log p1)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* b8 k (Math/log p8)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7) (* k b8))))
        x3 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b1 k (Math/log b3)) (* b2 k (Math/log b3)) (* b4 k (Math/log b3)) (* b5 k (Math/log b3)) (* b6 k (Math/log b3)) (* b7 k (Math/log b3)) (* b8 k (Math/log b3)) (* c (Math/log b3)) (- (* k (Math/log b3))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* b8 k (Math/log b8))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b1 k (Math/log p3))) (- (* b2 k (Math/log p3))) (- (* b4 k (Math/log p3))) (- (* b5 k (Math/log p3))) (- (* b6 k (Math/log p3))) (- (* b7 k (Math/log p3))) (- (* b8 k (Math/log p3))) (* k (Math/log p3)) (- (* c (Math/log p3))) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* b8 k (Math/log p8)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7) (* k b8))))
        x4 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b1 k (Math/log b4)) (* b2 k (Math/log b4)) (* b3 k (Math/log b4)) (* b5 k (Math/log b4)) (* b6 k (Math/log b4)) (* b7 k (Math/log b4)) (* b8 k (Math/log b4)) (* c (Math/log b4)) (- (* k (Math/log b4))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* b8 k (Math/log b8))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b1 k (Math/log p4))) (- (* b2 k (Math/log p4))) (- (* b3 k (Math/log p4))) (- (* b5 k (Math/log p4))) (- (* b6 k (Math/log p4))) (- (* b7 k (Math/log p4))) (- (* b8 k (Math/log p4))) (* k (Math/log p4)) (- (* c (Math/log p4))) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* b8 k (Math/log p8)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7) (* k b8))))
        x5 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b1 k (Math/log b5)) (* b2 k (Math/log b5)) (* b3 k (Math/log b5)) (* b4 k (Math/log b5)) (* b6 k (Math/log b5)) (* b7 k (Math/log b5)) (* b8 k (Math/log b5)) (* c (Math/log b5)) (- (* k (Math/log b5))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* b8 k (Math/log b8))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b1 k (Math/log p5))) (- (* b2 k (Math/log p5))) (- (* b3 k (Math/log p5))) (- (* b4 k (Math/log p5))) (- (* b6 k (Math/log p5))) (- (* b7 k (Math/log p5))) (- (* b8 k (Math/log p5))) (* k (Math/log p5)) (- (* c (Math/log p5))) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* b8 k (Math/log p8)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7) (* k b8))))
        x6 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b1 k (Math/log b6)) (* b2 k (Math/log b6)) (* b3 k (Math/log b6)) (* b4 k (Math/log b6)) (* b5 k (Math/log b6)) (* b7 k (Math/log b6)) (* b8 k (Math/log b6)) (* c (Math/log b6)) (- (* k (Math/log b6))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b7 k (Math/log b7))) (- (* b8 k (Math/log b8))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b1 k (Math/log p6))) (- (* b2 k (Math/log p6))) (- (* b3 k (Math/log p6))) (- (* b4 k (Math/log p6))) (- (* b5 k (Math/log p6))) (- (* b7 k (Math/log p6))) (- (* b8 k (Math/log p6))) (* k (Math/log p6)) (- (* c (Math/log p6))) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b7 k (Math/log p7)) (* b8 k (Math/log p8)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7) (* k b8))))
        x7 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b1 k (Math/log b7)) (* b2 k (Math/log b7)) (* b3 k (Math/log b7)) (* b4 k (Math/log b7)) (* b5 k (Math/log b7)) (* b6 k (Math/log b7)) (* b8 k (Math/log b7)) (* c (Math/log b7)) (- (* k (Math/log b7))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b8 k (Math/log b8))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b1 k (Math/log p7))) (- (* b2 k (Math/log p7))) (- (* b3 k (Math/log p7))) (- (* b4 k (Math/log p7))) (- (* b5 k (Math/log p7))) (- (* b6 k (Math/log p7))) (- (* b8 k (Math/log p7))) (* k (Math/log p7)) (- (* c (Math/log p7))) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b8 k (Math/log p8)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7) (* k b8))))
        x8 (Math/pow Math/E (/ (+ (- (* k (Math/log a))) (* b1 k (Math/log b8)) (* b2 k (Math/log b8)) (* b3 k (Math/log b8)) (* b4 k (Math/log b8)) (* b5 k (Math/log b8)) (* b6 k (Math/log b8)) (* b7 k (Math/log b8)) (* c (Math/log b8)) (- (* k (Math/log b8))) (- (* b1 k (Math/log b1))) (- (* b2 k (Math/log b2))) (- (* b3 k (Math/log b3))) (- (* b4 k (Math/log b4))) (- (* b5 k (Math/log b5))) (- (* b6 k (Math/log b6))) (- (* b7 k (Math/log b7))) (- (* c (Math/log c))) (* c (Math/log k)) (- (* b1 k (Math/log p8))) (- (* b2 k (Math/log p8))) (- (* b3 k (Math/log p8))) (- (* b4 k (Math/log p8))) (- (* b5 k (Math/log p8))) (- (* b6 k (Math/log p8))) (- (* b7 k (Math/log p8))) (* k (Math/log p8)) (- (* c (Math/log p8))) (* b1 k (Math/log p1)) (* b2 k (Math/log p2)) (* b3 k (Math/log p3)) (* b4 k (Math/log p4)) (* b5 k (Math/log p5)) (* b6 k (Math/log p6)) (* b7 k (Math/log p7)) (* c (Math/log s)) (- (* k (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7) (* k b8))))
        effort (Math/pow Math/E (/ (+ (- (* (Math/log a))) (- (* b1 (Math/log b1))) (- (* b2 (Math/log b2))) (- (* b3 (Math/log b3))) (- (* b4 (Math/log b4))) (- (* b5 (Math/log b5))) (- (* b6 (Math/log b6))) (- (* b7 (Math/log b7))) (- (* b8 (Math/log b8))) (- (* (Math/log c))) (* b1 (Math/log c)) (* b2 (Math/log c)) (* b3 (Math/log c)) (* b4 (Math/log c)) (* b5 (Math/log c)) (* b6 (Math/log c)) (* b7 (Math/log c)) (* b8 (Math/log c)) (* (Math/log k)) (* b1 (Math/log k)) (* b2 (Math/log k)) (* b3 (Math/log k)) (* b4 (Math/log k)) (* b5 (Math/log k)) (* b6 (Math/log k)) (* b7 (Math/log k)) (* b8 (Math/log k)) (* b1 (Math/log p1)) (* b2 (Math/log p2)) (* b3 (Math/log p3)) (* b4 (Math/log p4)) (* b5 (Math/log p5)) (* b6 (Math/log p6)) (* b7 (Math/log p7)) (* b8 (Math/log p8)) (* (Math/log s)) (* b1 (Math/log s)) (* b2 (Math/log s)) (* b3 (Math/log s)) (* b4 (Math/log s)) (* b5 (Math/log s)) (* b6 (Math/log s)) (* b7 (Math/log s)) (* b8 (Math/log s)) (- (* (Math/log λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7) (* k b8))))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5 x6 x7 x8])]
     {:output output
      :x1 x1
      :x2 x2
      :x3 x3
      :x4 x4
      :x5 x5
      :x6 x6
      :x7 x7
      :x8 x8
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs
      :pollutant-quantities pollutant-qs}))

; TODO : Add automated tests to all functions in this file.

(defn get-intermediate-input-quantity [f ii [production-inputs intermediate-input-quantities]]
  (->> ii
       first
       (.indexOf (f production-inputs))
       (nth intermediate-input-quantities)))

(defn get-deltas [J price-delta pdlist]
  (max 0.001 (min price-delta (Math/abs (* price-delta (nth pdlist J))))))

; TODO Change to map?
(defn update-surpluses-prices
  [type inputs prices wcs ccs natural-resources-supply labor-supply pdlist offset-1 offset-2 offset-3 offset-4 offset-5]
  (loop [inputs inputs
         prices prices
         surpluses []
         new-deltas []
         J 0]
    (if (empty? inputs)
      {:prices prices :surpluses surpluses :new-deltas new-deltas}
      (let [supply (condp = type
                     "private-goods" (->> wcs
                                  (filter #(and (= 0 (get % :industry))
                                                (= (first inputs)
                                                   (get % :product))))
                                  (map :output)
                                  (reduce +))
                     "intermediate" (->> wcs
                                         (filter #(and (= 1 (get % :industry))
                                                       (= (first inputs)
                                                          (get % :product))))
                                         (map :output)
                                         (reduce +))
                     "nature" (nth natural-resources-supply J)
                     "labor"  (nth labor-supply J)
                     "public-goods" (->> wcs
                                         (filter #(and (= 2 (get % :industry))
                                                       (= (first inputs)
                                                          (get % :product))))
                                         (map :output)
                                         (reduce +)) 
                     "pollutants" (/ (->> ccs
                                         (map :pollutant-permissions)
                                         flatten
                                         (reduce +))
                                     (count ccs)))
            demand (condp = type
                     "private-goods"  (->> ccs
                                   (mapv :private-good-demands)
                                   (mapv #(nth % (dec (first inputs))))
                                   (reduce +))
                     "intermediate" (->> wcs
                                         (filter #(contains? (set (first (:production-inputs %)))
                                                             (first inputs)
                                                             ))
                                         (map (juxt :production-inputs :intermediate-input-quantities))
                                         (map (partial get-intermediate-input-quantity first inputs))
                                         (reduce +))
                     "nature" (->> wcs
                                   (filter #(contains? (set (second (:production-inputs %)))
                                                       (first inputs)))
                                   (map (juxt :production-inputs :nature-quantities))
                                   (map (partial get-intermediate-input-quantity second inputs))
                                   (reduce +))
                     "labor" (->> wcs
                                  (filter #(contains? (set (third (:production-inputs %)))
                                                      (first inputs)))
                                  (map (juxt :production-inputs :labor-quantities))
                                  (map (partial get-intermediate-input-quantity third inputs))
                                  (reduce +))
                     "public-goods" (/ (->> ccs
                                           (mapv :public-good-demands)
                                           (mapv #(nth % (dec (first inputs))))
                                           (reduce +))
                                       (count ccs))
                     "pollutants" (->> wcs
                                       (filter #(contains? (set (last (:production-inputs %)))
                                                           (first inputs)))
                                       (map (juxt :production-inputs :pollutant-quantities))
                                       (map (partial get-intermediate-input-quantity last inputs))
                                       (reduce +)))
            j-offset (condp = type
                       "private-goods" 0
                       "intermediate" offset-1
                       "nature" (+ offset-1 offset-2)
                       "labor" (+ offset-1 offset-2 offset-3)
                       "public-goods" (+ offset-1 offset-2 offset-3 offset-4)
                       "pollutants" (+ offset-1 offset-2 offset-3 offset-4 offset-5))
            surplus (- supply demand)
            price-delta-to-use (- 1.05 (Math/pow 0.5 (/ (Math/abs (* 2 surplus)) (+ demand supply))))
            new-delta (get-deltas (+ J j-offset) price-delta-to-use pdlist)
            new-price (cond (pos? surplus) (* (- 1 new-delta) (nth prices (dec (first inputs))))
                            (neg? surplus) (* (+ 1 new-delta) (nth prices (dec (first inputs))))
                            :else (nth prices (dec (first inputs))))]
        (recur (rest inputs)
               (assoc prices J new-price)
               (conj surpluses surplus)
               (conj new-deltas new-delta)
               (inc J))))))

(defn proposal [private-good-prices input-prices nature-prices labor-prices public-good-prices pollutant-prices wc]
  (letfn [(count-inputs [w]
            ((comp count flatten :production-inputs) w))
          (get-input-prices [[indexes prices]]
            (map #(nth prices (dec %)) indexes))
          (get-lambda-o [w private-good-prices input-prices public-good-prices]
            (let [industry (:industry w)
                  product (:product w)]
              (cond (= 0 industry) (nth private-good-prices (dec product))
                    (= 1 industry) (nth input-prices (dec product))
                    (= 2 industry) (first public-good-prices))))]
    (let [prices-and-indexes (->> (vector input-prices nature-prices labor-prices pollutant-prices)
                                  (interleave (get wc :production-inputs))
                                  (partition 2))
          input-count-r (count-inputs wc)
          total-factor-productivity (get wc :total-factor-productivity)
          disutility-of-effort-coefficient (get wc :disutility-of-effort-coefficient)
          effort-elasticity (get wc :effort-elasticity)
          disutility-of-effort-exponent (get wc :disutility-of-effort-exponent)
          ps (into [] (flatten (map get-input-prices prices-and-indexes))) ; rename?
          b-input (get wc :intermediate-input-exponents)
          b-labor (get wc :labor-exponents)
          b-nature (get wc :nature-exponents)
          b-pollutant (get wc :pollutant-exponents)
          b (concat b-input b-nature b-labor b-pollutant)
          λ (get-lambda-o wc private-good-prices input-prices public-good-prices)
          p-i (get wc :production-inputs)]
      (condp = input-count-r
        1 (merge wc (solution-1 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i))
        2 (merge wc (solution-2 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i))
        3 (merge wc (solution-3 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i))
        4 (merge wc (solution-4 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i))
        5 (merge wc (solution-5 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i))
        6 (merge wc (solution-6 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i))
        7 (merge wc (solution-7 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i))
        8 (merge wc (solution-8 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i))
        (str "unexpected input-count value: " input-count-r)))))

(defn update-percent-surplus [supply-list demand-list surplus-list]
  (letfn [(force-to-one [n]
            (let [cap 0.25]
              (if (or (> n cap) (< n (- cap))) cap (Math/abs n))))]
    (let [averaged-s-and-d (->> (interleave (flatten supply-list)
                                           (flatten demand-list))
                                (partition 2)
                                (mapv mean))]
      (->> (interleave (flatten surplus-list) averaged-s-and-d)
           (partition 2)
           (mapv #(/ (first %) (last %)))
           (mapv force-to-one)))))

(defn update-price-deltas [supply-list demand-list surplus-list]
  (let [supply-list-means (map mean supply-list)
        demand-list-means (map mean demand-list)
        surplus-list-means (map mean surplus-list)
        averaged-s-and-d (->> (interleave supply-list-means
                                          demand-list-means)
                              (partition 2)
                              (map mean))]
    (->> (interleave surplus-list-means averaged-s-and-d)
         (partition 2)
         (mapv #(Math/abs (/ (first %) (last %)))))))

(defn get-demand-list [t]
  (letfn [(merge-inputs-and-quantities [type ks vs]
            (map #(hash-map :type type :key (first %) :value (last %))
                 (partition 2 (interleave ks vs))))
          (get-inputs-and-quantities [m]
            [(merge-inputs-and-quantities :intermediate-input-quantity
                                          (first (:production-inputs m))
                                          (:intermediate-input-quantities m))
             (merge-inputs-and-quantities :nature-quantity
                                          (second (:production-inputs m))
                                          (:nature-quantities m))
             (merge-inputs-and-quantities :labor-quantity
                                          (nth (:production-inputs m) 2)
                                          (:labor-quantities m))
             (merge-inputs-and-quantities :pollutant-quantity
                                          (nth (:production-inputs m) 3)
                                          (:pollutant-quantities m))])
          (sum-input-quantities [qs pos type]
            (->> qs
                 (filter #(and (= pos (:key %)) (= type (:type %))))
                 (map :value)
                 (reduce +)))]
    (let [im-goods-to-use (:intermediate-inputs t) ; already a vector from range
          resources-to-use (range 1 (inc (:resources t)))
          labors-to-use (range 1 (inc (:labors t)))
          pollutants-to-use (range 1 (inc (:pollutants t)))
          private-good-demands (->> t
                             :private-goods
                             (mapv (fn [i] (mapv #(nth (:private-good-demands %) (dec i)) (:ccs t))))
                             (mapv (partial reduce +)))
          all-quantities (->> t
                              :wcs
                              (map get-inputs-and-quantities)
                              flatten)
          input-quantity (mapv (fn [n] (sum-input-quantities all-quantities n :input-quantity)) im-goods-to-use)
          nature-quantity (mapv (fn [n] (sum-input-quantities all-quantities n :nature-quantity)) resources-to-use)
          labor-quantity (mapv (fn [n] (sum-input-quantities all-quantities n :labor-quantity)) labors-to-use)
          public-good-demands
                     (mapv (fn [public-good]
                             (mean (map #(nth (:public-good-demands %) (dec public-good))
                                         (t :ccs))))
                           (t :public-good-types))
          pollutants (mapv (fn [n] (sum-input-quantities all-quantities n :pollutant-quantity)) pollutants-to-use)]
      [private-good-demands
       input-quantity
       nature-quantity
       labor-quantity
       public-good-demands
       pollutants])))

(defn get-supply-list [t]
  (letfn [(get-producers [t industry product]
            (->> t
                 :wcs
                 (filter #(and (= industry (get % :industry))
                               (= product (get % :product))))
                 (map :output)
                 flatten
                 (reduce +)))]
   (let [private-goods (:private-goods t)
         private-producers (mapv (partial get-producers t 0) private-goods)
         intermediate-inputs (:intermediate-inputs t)
         input-producers (mapv (partial get-producers t 1) intermediate-inputs)
         natural-resources-supply (t :natural-resources-supply)
         labor-supply (t :labor-supply)
         public-good-supply (mapv (partial get-producers t 2) (:public-good-types t))
         pollutant-permissions (->> t
                                    :ccs
                                    (map :pollutant-permissions)
                                    (apply map vector)
                                    (map (partial apply +)))]
     (vector private-producers input-producers natural-resources-supply labor-supply public-good-supply pollutant-permissions))))

(defn report-threshold [surplus-list supply-list demand-list]
  (->> (interleave (flatten surplus-list) (flatten demand-list) (flatten supply-list))
       (partition 3)
       (mapv #(* 100 (/ (Math/abs (* 2 (first %))) (+ (second %) (last %)))))))
