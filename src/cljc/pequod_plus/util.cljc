(ns pequod-plus.util)

(def globals 
  {:init-private-good-price 700
   :init-intermediate-price 700
   :init-labor-price        700
   :init-nature-price       700
   :init-public-good-price  700
   :init-pollutant-price    700
   :private-goods            10
   :intermediate-inputs      10
   :resources                10
   :labors                   10
   :public-goods              1
   :pollutants                1
   :price-data               {}
   :surplus-data             {}
   :supply-data              {}
   :demand-data              {}
   :threshold-report         []
   :wcs                      []
   :ccs                      []
   :iteration                0})

(defn mean [L]
  (/ (reduce + L) (count L)))

(defn make-price-maps [price n price-delta pd]
  (mapv #(assoc {} :id % :price price :price-delta price-delta :pd pd) (range 1 (inc n))))

(defn initialize-prices [t]
  (let [num-private-goods (t :private-goods)
        num-im-inputs (t :intermediate-inputs)
        num-resources (t :resources)
        num-labor (t :labors)
        num-public-goods (t :public-goods)
        num-pollutants (t :pollutants)
        price-delta-to-use 0.05
        pd-to-use 0.25]
    (assoc t
           :price-data {:private-goods (make-price-maps (t :init-private-good-price) num-private-goods price-delta-to-use pd-to-use)
                        :intermediate-inputs (make-price-maps (t :init-intermediate-price) num-im-inputs price-delta-to-use pd-to-use)
                        :nature (make-price-maps (t :init-nature-price) num-resources price-delta-to-use pd-to-use)
                        :labor (make-price-maps (t :init-labor-price) num-labor price-delta-to-use pd-to-use)
                        :public-goods (make-price-maps (t :init-public-good-price) num-public-goods price-delta-to-use pd-to-use)
                        :pollutants (make-price-maps (t :init-pollutant-price) num-pollutants price-delta-to-use pd-to-use)})))

(defn add-ids [cs]
  (loop [i 1
         cs cs
         updated-cs []]
    (if (empty? cs)
      updated-cs
      (recur (inc i) (rest cs) (conj updated-cs (assoc (first cs) :id i))))))

; TODO turn natural-resources-supply and labor-supply into list of maps?
; TODO get rid of inputs/types?
#_(defn setup [t]
  (let [intermediate-inputs (vec (range 1 (inc (t :intermediate-inputs))))
        nature-types (vec (range 1 (inc (t :resources))))
        labor-types (vec (range 1 (inc (t :labors))))
        private-goods (vec (range 1 (inc (t :private-goods))))
        public-good-types (vec (range 1 (inc (t :public-goods))))
        pollutant-types (vec (range 1 (inc (t :pollutants))))]
    (-> t
        initialize-prices
        (assoc :natural-resources-supply (repeat (t :resources) 1000)
               :labor-supply (repeat (t :labors) 1000)
               :private-goods private-goods
               :intermediate-inputs intermediate-inputs
               :nature-types nature-types
               :labor-types labor-types
               :public-good-types public-good-types
               :pollutant-types pollutant-types
               :ccs (add-ids
                     councils/ccs)
               :wcs (add-ids
                     councils/wcs)))))

(defn allot-production-quantities [production-inputs xs include-pollutants?]
  (let [[I N L P] (map count production-inputs)
        intermediate-input-quantities (->> xs
                                           (take I)
                                           (into []))
        nature-quantities (->> xs
                               (drop I)
                               (take N)
                               (into []))
        labor-quantities (->> xs
                              (drop (+ I N))
                              (take L)
                              (into []))
        pollutant-quantities (if include-pollutants?
                                (->> xs
                                    (drop (+ I N L))
                                    (take P)
                                    (into []))
                                nil)]
    [intermediate-input-quantities nature-quantities labor-quantities pollutant-quantities]))

(defn solution-3 [a s c k ps b λ p-i include-pollutants?]
  (let [[b1 b2 b3] (flatten b)
        [p1 p2 p3] (flatten ps)
        log-a (Math/log a)
        log-b1 (Math/log b1)
        log-b2 (Math/log b2)
        log-b3 (Math/log b3)
        log-c (Math/log c)
        log-k (Math/log k)
        log-s (Math/log s)
        log-p1 (Math/log p1)
        log-p2 (Math/log p2)
        log-p3 (Math/log p3)
        log-λ (Math/log λ)
        output (Math/pow Math/E (/ (+ (- (* k log-a)) (- (* b1 k log-b1)) (- (* b2 k log-b2)) (- (* b3 k log-b3)) (- (* c log-c)) (* c log-k) (* b1 k log-p1) (* b2 k log-p2) (* b3 k log-p3) (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ))) (+ c (- k) (* k b1) (* k b2) (* k b3))))
        x1 (Math/pow Math/E (/ (+ (- (* k log-a)) (* c log-b1) (- (* k log-b1)) (* b2 k log-b1) (* b3 k log-b1) (- (* b2 k log-b2)) (- (* b3 k log-b3)) (- (* c log-c)) (* c log-k) (- (* c log-p1)) (* k log-p1) (- (* b2 k log-p1)) (- (* b3 k log-p1)) (* b2 k log-p2) (* b3 k log-p3) (* c log-s) (- (* k log-λ))) (+ c (- k) (* k b1) (* k b2) (* k b3))))
        x2 (Math/pow Math/E (/ (+ (- (* k log-a)) (- (* b1 k log-b1)) (* c log-b2) (- (* k log-b2)) (* b1 k log-b2) (* b3 k log-b2) (- (* b3 k log-b3)) (- (* c log-c)) (* c log-k) (* b1 k log-p1) (- (* c log-p2)) (* k log-p2) (- (* b1 k log-p2)) (- (* b3 k log-p2)) (* b3 k log-p3) (* c log-s) (- (* k log-λ))) (+ c (- k) (* k b1) (* k b2) (* k b3))))
        x3 (Math/pow Math/E (/ (+ (- (* k log-a)) (- (* b1 k log-b1)) (- (* b2 k log-b2)) (* c log-b3) (- (* k log-b3)) (* b1 k log-b3) (* b2 k log-b3) (- (* c log-c)) (* c log-k) (* b1 k log-p1) (* b2 k log-p2) (- (* c log-p3)) (* k log-p3) (- (* b1 k log-p3)) (- (* b2 k log-p3)) (* c log-s) (- (* k log-λ))) (+ c (- k) (* k b1) (* k b2) (* k b3))))
        effort (Math/pow Math/E (/ (+ (- (* log-a)) (- (* b1 log-b1)) (- (* b2 log-b2)) (- (* b3 log-b3)) (* b1 log-p1) (* b2 log-p2) (* b3 log-p3) (- (* b1 log-λ)) (- (* b2 log-λ)) (- (* b3 log-λ)) (/ (+ (- (* k log-a)) (- (* b1 k log-b1)) (- (* b2 k log-b2)) (- (* b3 k log-b3)) (- (* c log-c)) (* c log-k) (* b1 k log-p1) (* b2 k log-p2) (* b3 k log-p3) (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ))) (+ c (- k) (* k b1) (* k b2) (* k b3))) (- (/ (* b1 (+ (- (* k log-a)) (- (* b1 k log-b1)) (- (* b2 k log-b2)) (- (* b3 k log-b3)) (- (* c log-c)) (* c log-k) (* b1 k log-p1) (* b2 k log-p2) (* b3 k log-p3) (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ))))  (+ c (- k) (* k b1) (* k b2) (* k b3)))) (- (/ (* b2 (+ (- (* k log-a)) (- (* b1 k log-b1)) (- (* b2 k log-b2)) (- (* b3 k log-b3)) (- (* c log-c)) (* c log-k) (* b1 k log-p1) (* b2 k log-p2) (* b3 k log-p3) (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3)))) (- (/ (* b3 (+ (- (* k log-a)) (- (* b1 k log-b1)) (- (* b2 k log-b2)) (- (* b3 k log-b3)) (- (* c log-c)) (* c log-k) (* b1 k log-p1) (* b2 k log-p2) (* b3 k log-p3) (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)))) (+ c (- k) (* k b1) (* k b2) (* k b3))))) c))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3] include-pollutants?)]
    (if include-pollutants?
      {:output output
       :effort effort
       :intermediate-input-quantities intermediate-input-qs
       :nature-quantities nature-qs
       :labor-quantities labor-qs
       :pollutant-quantities pollutant-qs}
      {:output output
       :effort effort
       :intermediate-input-quantities intermediate-input-qs
       :nature-quantities nature-qs
       :labor-quantities labor-qs})))

(defn solution-4 [a s c k ps b λ p-i include-pollutants?]
  (let [[b1 b2 b3 b4] (flatten b)
        [p1 p2 p3 p4] (flatten ps)
        log-a (Math/log a)
        log-b1 (Math/log b1)
        log-b2 (Math/log b2)
        log-b3 (Math/log b3)
        log-b4 (Math/log b4)
        log-c (Math/log c)
        log-k (Math/log k)
        log-s (Math/log s)
        log-p1 (Math/log p1)
        log-p2 (Math/log p2)
        log-p3 (Math/log p3)
        log-p4 (Math/log p4)
        log-λ (Math/log λ)
        denominator (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4))
        k-log-a (- (* k log-a))
        b1-k-log-b1 (- (* b1 k log-b1))
        b2-k-log-b2 (- (* b2 k log-b2))
        b3-k-log-b3 (- (* b3 k log-b3))
        b4-k-log-b4 (- (* b4 k log-b4))
        b1-k-log-p1 (* b1 k log-p1)
        b2-k-log-p2 (* b2 k log-p2)
        b3-k-log-p3 (* b3 k log-p3)
        b4-k-log-p4 (* b4 k log-p4)
        c-log-c (- (* c log-c))
        c-log-k (* c log-k)
        output (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ))) denominator))
        x1 (Math/pow Math/E (/ (+ k-log-a (* c log-b1) (- (* k log-b1)) (* b2 k log-b1) (* b3 k log-b1) (* b4 k log-b1) b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 c-log-c c-log-k (- (* c log-p1)) (* k log-p1) (- (* b2 k log-p1)) (- (* b3 k log-p1)) (- (* b4 k log-p1)) b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 (* c log-s) (- (* k log-λ))) denominator))
        x2 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 (* c log-b2) (- (* k log-b2)) (* b1 k log-b2) (* b3 k log-b2) (* b4 k log-b2) b3-k-log-b3 b4-k-log-b4 c-log-c c-log-k b1-k-log-p1 (- (* c log-p2)) (* k log-p2) (- (* b1 k log-p2)) (- (* b3 k log-p2)) (- (* b4 k log-p2)) b3-k-log-p3 b4-k-log-p4 (* c log-s) (- (* k log-λ))) denominator))
        x3 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 (* c log-b3) (- (* k log-b3)) (* b1 k log-b3) (* b2 k log-b3) (* b4 k log-b3) b4-k-log-b4 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 (- (* c log-p3)) (* k log-p3) (- (* b1 k log-p3)) (- (* b2 k log-p3)) (- (* b4 k log-p3)) b4-k-log-p4 (* c log-s) (- (* k log-λ))) denominator))
        x4 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 (* c log-b4) (- (* k log-b4)) (* b1 k log-b4) (* b2 k log-b4) (* b3 k log-b4) c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 (- (* c log-p4)) (* k log-p4) (- (* b1 k log-p4)) (- (* b2 k log-p4)) (- (* b3 k log-p4)) (* c log-s) (- (* k log-λ))) denominator))
     effort (Math/pow Math/E (/ (+ (- (* log-a)) (- (* b1 log-b1)) (- (* b2 log-b2)) (- (* b3 log-b3)) (- (* b4 log-b4)) (- (* log-c)) (* b1 log-c) (* b2 log-c) (* b3 log-c) (* b4 log-c) (* log-k) (- (* b1 log-k)) (- (* b2 log-k)) (- (* b3 log-k)) (- (* b4 log-k)) (* b1 log-p1) (* b2 log-p2) (* b3 log-p3) (* b4 log-p4) (* log-s) (- (* b1 log-s)) (- (* b2 log-s)) (- (* b3 log-s)) (- (* b4 log-s)) (- (* log-λ))) denominator))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4] include-pollutants?)]
    (if include-pollutants? 
     {:output output
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs
      :pollutant-quantities pollutant-qs}
     {:output output
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs})))

(defn solution-5 [a s c k ps b λ p-i include-pollutants?]
  (let [[b1 b2 b3 b4 b5] (flatten b)
        [p1 p2 p3 p4 p5] (flatten ps)
        log-a (Math/log a)
        log-b1 (Math/log b1)
        log-b2 (Math/log b2)
        log-b3 (Math/log b3)
        log-b4 (Math/log b4)
        log-b5 (Math/log b5)
        log-c (Math/log c)
        log-k (Math/log k)
        log-s (Math/log s)
        log-p1 (Math/log p1)
        log-p2 (Math/log p2)
        log-p3 (Math/log p3)
        log-p4 (Math/log p4)
        log-p5 (Math/log p5)
        log-λ (Math/log λ)
        denominator (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5))
        k-log-a (- (* k log-a))
        b1-k-log-b1 (- (* b1 k log-b1))
        b2-k-log-b2 (- (* b2 k log-b2))
        b3-k-log-b3 (- (* b3 k log-b3))
        b4-k-log-b4 (- (* b4 k log-b4))
        b5-k-log-b5 (- (* b5 k log-b5))
        b1-k-log-p1 (* b1 k log-p1)
        b2-k-log-p2 (* b2 k log-p2)
        b3-k-log-p3 (* b3 k log-p3)
        b4-k-log-p4 (* b4 k log-p4)
        b5-k-log-p5 (* b5 k log-p5)
        c-log-c (- (* c log-c))
        c-log-k (* c log-k)
        output (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ))) denominator))
        x1 (Math/pow Math/E (/ (+ k-log-a (* c log-b1) (- (* k log-b1)) (* b2 k log-b1) (* b3 k log-b1) (* b4 k log-b1) (* b5 k log-b1) b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k (- (* c log-p1)) (* k log-p1) (- (* b2 k log-p1)) (- (* b3 k log-p1)) (- (* b4 k log-p1)) (- (* b5 k log-p1)) b2-k-log-p2 (* b3 k log-p2) b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* k log-λ))) denominator))
        x2 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 (* c log-b2) (- (* k log-b2)) (* b1 k log-b2) (* b3 k log-b2) (* b4 k log-b2) (* b5 k log-b2) b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 (- (* c log-p2)) (* k log-p2) (- (* b1 k log-p2)) (- (* b3 k log-p2)) (- (* b4 k log-p2)) (- (* b5 k log-p2)) b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* k log-λ))) denominator))
        x3 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 (* c log-b3) (- (* k log-b3)) (* b1 k log-b3) (* b2 k log-b3) (* b4 k log-b3) (* b5 k log-b3) b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 (- (* c log-p3)) (* k log-p3) (- (* b1 k log-p3)) (- (* b2 k log-p3)) (- (* b4 k log-p3)) (- (* b5 k log-p3)) b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* k log-λ))) denominator))
        x4 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 (* c log-b4) (- (* k log-b4)) (* b1 k log-b4) (* b2 k log-b4) (* b3 k log-b4) (* b5 k log-b4) b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 (- (* c log-p4)) (* k log-p4) (- (* b1 k log-p4)) (- (* b2 k log-p4)) (- (* b3 k log-p4)) (- (* b5 k log-p4)) b5-k-log-p5 (* c log-s) (- (* k log-λ))) denominator))
        x5 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 (* c log-b5) (- (* k log-b5)) (* b1 k log-b5) (* b2 k log-b5) (* b3 k log-b5) (* b4 k log-b5) c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 (- (* c log-p5)) (* k log-p5) (- (* b1 k log-p5)) (- (* b2 k log-p5)) (- (* b3 k log-p5)) (- (* b4 k log-p5)) (* c log-s) (- (* k log-λ))) denominator))
        effort (Math/pow Math/E (/ (+ (- ( * log-a)) (- (* b1 log-b1)) (- (* b2 log-b2)) (- (* b3 log-b3)) (- (* b4 log-b4)) (- (* b5 log-b5)) (* b1 log-p1) (* b2 log-p2) (* b3 log-p3) (* b4 log-p4) (* b5 log-p5) (- (* b1 log-λ)) (- (* b2 log-λ)) (- (* b3 log-λ)) (- (* b4 log-λ)) (- (* b5 log-λ)) (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ))) denominator) (- (/ (* b1 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)))) denominator)) (- (/ (* b2 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)))) denominator)) (- (/ (* b3 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)))) denominator)) (- (/ (* b4 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)))) denominator)) (- (/ (* b5 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)))) denominator))) c))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5] include-pollutants?)]
    (if include-pollutants?
     {:output output
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs
      :pollutant-quantities pollutant-qs}
     {:output output
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs})))

(defn solution-6 [a s c k ps b λ p-i include-pollutants?]
  (let [[b1 b2 b3 b4 b5 b6] (flatten b)
        [p1 p2 p3 p4 p5 p6] (flatten ps)
        log-a (Math/log a)
        log-b1 (Math/log b1)
        log-b2 (Math/log b2)
        log-b3 (Math/log b3)
        log-b4 (Math/log b4)
        log-b5 (Math/log b5)
        log-b6 (Math/log b6)
        log-c (Math/log c)
        log-k (Math/log k)
        log-s (Math/log s)
        log-p1 (Math/log p1)
        log-p2 (Math/log p2)
        log-p3 (Math/log p3)
        log-p4 (Math/log p4)
        log-p5 (Math/log p5)
        log-p6 (Math/log p6)
        log-λ (Math/log λ)
        denominator (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6))
        k-log-a (- (* k log-a))
        b1-k-log-b1 (- (* b1 k log-b1))
        b2-k-log-b2 (- (* b2 k log-b2))
        b3-k-log-b3 (- (* b3 k log-b3))
        b4-k-log-b4 (- (* b4 k log-b4))
        b5-k-log-b5 (- (* b5 k log-b5))
        b6-k-log-b6 (- (* b6 k log-b6))
        b1-k-log-p1 (* b1 k log-p1)
        b2-k-log-p2 (* b2 k log-p2)
        b3-k-log-p3 (* b3 k log-p3)
        b4-k-log-p4 (* b4 k log-p4)
        b5-k-log-p5 (* b5 k log-p5)
        b6-k-log-p6 (* b6 k log-p6)
        c-log-c (- (* c log-c))
        c-log-k (* c log-k)
        c-log-s (* c log-s)
        k-log-λ (- (* k log-λ))
        output (Math/pow Math/E (- (/ (+ (* k log-a) (* b1 k log-b1) (* b2 k log-b2) (* b3 k log-b3) (* b4 k log-b4) (* b5 k log-b5) (* b6 k log-b6) (* c log-c) (- c-log-k) (- b1-k-log-p1) (- b2-k-log-p2) (- b3-k-log-p3) (- b4-k-log-p4) (- b5-k-log-p5) (- b6-k-log-p6)  (- c-log-s) (* c log-λ) (* b1 k log-λ) (* b2 k log-λ) (* b3 k log-λ) (* b4 k log-λ) (* b5 k log-λ) (* b6 k log-λ)) denominator)))
        x1 (Math/pow Math/E (/ (+ k-log-a (* c log-b1) (- (* k log-b1)) (* b2 k log-b1) (* b3 k log-b1) (* b4 k log-b1) (* b5 k log-b1) (* b6 k log-b1) b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 c-log-c c-log-k (- (* c log-p1)) (* k log-p1) (- (* b2 k log-p1)) (- (* b3 k log-p1)) (- (* b4 k log-p1)) (- (* b5 k log-p1)) (- (* b6 k log-p1)) b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 c-log-s k-log-λ) denominator))
        x2 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 (* c log-b2) (- (* k log-b2)) (* b1 k log-b2) (* b3 k log-b2) (* b4 k log-b2) (* b5 k log-b2) (* b6 k log-b2) b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 c-log-c c-log-k b1-k-log-p1 (- (* c log-p2)) (* k log-p2) (- (* b1 k log-p2)) (- (* b3 k log-p2)) (- (* b4 k log-p2)) (- (* b5 k log-p2)) (- (* b6 k log-p2)) b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 c-log-s k-log-λ) denominator))
        x3 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 (* c log-b3) (- (* k log-b3)) (* b1 k log-b3) (* b2 k log-b3) (* b4 k log-b3) (* b5 k log-b3) (* b6 k log-b3) b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 (- (* c log-p3)) (* k log-p3) (- (* b1 k log-p3)) (- (* b2 k log-p3)) (- (* b4 k log-p3)) (- (* b5 k log-p3)) (- (* b6 k log-p3)) b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 c-log-s k-log-λ) denominator))
        x4 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 (* c log-b4) (- (* k log-b4)) (* b1 k log-b4) (* b2 k log-b4) (* b3 k log-b4) (* b5 k log-b4) (* b6 k log-b4) b5-k-log-b5 b6-k-log-b6 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 (- (* c log-p4)) (* k log-p4) (- (* b1 k log-p4)) (- (* b2 k log-p4)) (- (* b3 k log-p4)) (- (* b5 k log-p4)) (- (* b6 k log-p4)) b5-k-log-p5 b6-k-log-p6 c-log-s k-log-λ) denominator))
        x5 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 (* c log-b5) (- (* k log-b5)) (* b1 k log-b5) (* b2 k log-b5) (* b3 k log-b5) (* b4 k log-b5) (* b6 k log-b5) b6-k-log-b6 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 (- (* c log-p5)) (* k log-p5) (- (* b1 k log-p5)) (- (* b2 k log-p5)) (- (* b3 k log-p5)) (- (* b4 k log-p5)) (- (* b6 k log-p5)) b6-k-log-p6 c-log-s k-log-λ) denominator))
        x6 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 (* c log-b6) (- (* k log-b6)) (* b1 k log-b6) (* b2 k log-b6) (* b3 k log-b6) (* b4 k log-b6) (* b5 k log-b6) c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (- (* c log-p6)) (* k log-p6) (- (* b1 k log-p6)) (- (* b2 k log-p6)) (- (* b3 k log-p6)) (- (* b4 k log-p6)) (- (* b5 k log-p6)) c-log-s k-log-λ) denominator))
        effort (Math/pow Math/E (/ (+ (- log-a) (- (* b1 log-b1)) (- (* b2 log-b2)) (- (* b3 log-b3)) (- (* b4 log-b4)) (- (* b5 log-b5)) (- (* b6 log-b6)) (- (* log-c)) (* b1 log-c) (* b2 log-c) (* b3 log-c) (* b4 log-c) (* b5 log-c) (* b6 log-c) (* log-k) (- (* b1 log-k)) (- (* b2 log-k)) (- (* b3 log-k)) (- (* b4 log-k)) (- (* b5 log-k)) (- (* b6 log-k)) (* b1 log-p1) (* b2 log-p2) (* b3 log-p3) (* b4 log-p4) (* b5 log-p5) (* b6 log-p6) (* log-s) (- (* b1 log-s)) (- (* b2 log-s)) (- (* b3 log-s)) (- (* b4 log-s)) (- (* b5 log-s)) (- (* b6 log-s)) (- (* log-λ))) denominator))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5 x6] include-pollutants?)]
    (if include-pollutants?
     {:output output
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs
      :pollutant-quantities pollutant-qs}
     {:output output
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs})))

(defn solution-7 [a s c k ps b λ p-i include-pollutants?]
  (let [[b1 b2 b3 b4 b5 b6 b7] (flatten b)
        [p1 p2 p3 p4 p5 p6 p7] (flatten ps)
        log-a (Math/log a)
        log-b1 (Math/log b1)
        log-b2 (Math/log b2)
        log-b3 (Math/log b3)
        log-b4 (Math/log b4)
        log-b5 (Math/log b5)
        log-b6 (Math/log b6)
        log-b7 (Math/log b7)
        log-c (Math/log c)
        log-k (Math/log k)
        log-s (Math/log s)
        log-p1 (Math/log p1)
        log-p2 (Math/log p2)
        log-p3 (Math/log p3)
        log-p4 (Math/log p4)
        log-p5 (Math/log p5)
        log-p6 (Math/log p6)
        log-p7 (Math/log p7)
        log-λ (Math/log λ)
        denominator (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7))
        k-log-a (- (* k log-a))
        b1-k-log-b1 (- (* b1 k log-b1))
        b2-k-log-b2 (- (* b2 k log-b2))
        b3-k-log-b3 (- (* b3 k log-b3))
        b4-k-log-b4 (- (* b4 k log-b4))
        b5-k-log-b5 (- (* b5 k log-b5))
        b6-k-log-b6 (- (* b6 k log-b6))
        b7-k-log-b7 (- (* b7 k log-b7))
        b1-k-log-p1 (* b1 k log-p1)
        b2-k-log-p2 (* b2 k log-p2)
        b3-k-log-p3 (* b3 k log-p3)
        b4-k-log-p4 (* b4 k log-p4)
        b5-k-log-p5 (* b5 k log-p5)
        b6-k-log-p6 (* b6 k log-p6)
        b7-k-log-p7 (* b7 k log-p7)
        c-log-c (- (* c log-c))
        c-log-k (* c log-k)
        c-log-s (* c log-s)
        k-log-λ (- (* k log-λ))
        output (Math/pow Math/E (/ (+ k-log-a c-log-k c-log-s (- (* c log-λ)) c-log-c b1-k-log-b1 b1-k-log-p1 (- (* b1 k log-λ)) b2-k-log-b2 b2-k-log-p2 (- (* b2 k log-λ)) b3-k-log-b3 b3-k-log-p3 (- (* b3 k log-λ)) b4-k-log-b4 b4-k-log-p4 (- (* b4 k log-λ)) b5-k-log-b5 b5-k-log-p5 (- (* b5 k log-λ)) b6-k-log-b6 b6-k-log-p6 (- (* b6 k log-λ)) b7-k-log-b7 b7-k-log-p7 (- (* b7 k log-λ))) denominator))
        x1 (Math/pow Math/E (/ (+ k-log-a (* b2 k log-b1) (* b3 k log-b1) (* b4 k log-b1) (* b5 k log-b1) (* b6 k log-b1) (* b7 k log-b1) (* c log-b1) (- (* k log-b1)) b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k (- (* b2 k log-p1)) (- (* b3 k log-p1)) (- (* b4 k log-p1)) (- (* b5 k log-p1)) (- (* b6 k log-p1)) (- (* b7 k log-p1)) (* k log-p1) (- (* c log-p1)) b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s k-log-λ) denominator))
        x2 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b2) (* b3 k log-b2) (* b4 k log-b2) (* b5 k log-b2) (* b6 k log-b2) (* b7 k log-b2) (* c log-b2) (- (* k log-b2)) b1-k-log-b1 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k (- (* b1 k log-p2)) (- (* b3 k log-p2)) (- (* b4 k log-p2)) (- (* b5 k log-p2)) (- (* b6 k log-p2)) (- (* b7 k log-p2)) (* k log-p2) (- (* c log-p2)) b1-k-log-p1 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s k-log-λ) denominator))
        x3 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b3) (* b2 k log-b3) (* b4 k log-b3) (* b5 k log-b3) (* b6 k log-b3) (* b7 k log-b3) (* c log-b3) (- (* k log-b3)) b1-k-log-b1 b2-k-log-b2 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k (- (* b1 k log-p3)) (- (* b2 k log-p3)) (- (* b4 k log-p3)) (- (* b5 k log-p3)) (- (* b6 k log-p3)) (- (* b7 k log-p3)) (* k log-p3) (- (* c log-p3)) b1-k-log-p1 b2-k-log-p2 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s k-log-λ) denominator))
        x4 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b4) (* b2 k log-b4) (* b3 k log-b4) (* b5 k log-b4) (* b6 k log-b4) (* b7 k log-b4) (* c log-b4) (- (* k log-b4)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k (- (* b1 k log-p4)) (- (* b2 k log-p4)) (- (* b3 k log-p4)) (- (* b5 k log-p4)) (- (* b6 k log-p4)) (- (* b7 k log-p4)) (* k log-p4) (- (* c log-p4)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s k-log-λ) denominator))
        x5 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b5) (* b2 k log-b5) (* b3 k log-b5) (* b4 k log-b5) (* b6 k log-b5) (* b7 k log-b5) (* c log-b5) (- (* k log-b5)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k (- (* b1 k log-p5)) (- (* b2 k log-p5)) (- (* b3 k log-p5)) (- (* b4 k log-p5)) (- (* b6 k log-p5)) (- (* b7 k log-p5)) (* k log-p5) (- (* c log-p5)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b6-k-log-p6 b7-k-log-p7 c-log-s k-log-λ) denominator))
        x6 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b6) (* b2 k log-b6) (* b3 k log-b6) (* b4 k log-b6) (* b5 k log-b6) (* b7 k log-b6) (* c log-b6) (- (* k log-b6)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b7-k-log-b7 c-log-c c-log-k (- (* b1 k log-p6)) (- (* b2 k log-p6)) (- (* b3 k log-p6)) (- (* b4 k log-p6)) (- (* b5 k log-p6)) (- (* b7 k log-p6)) (* k log-p6) (- (* c log-p6)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b7-k-log-p7 c-log-s k-log-λ) denominator))
        x7 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b7) (* b2 k log-b7) (* b3 k log-b7) (* b4 k log-b7) (* b5 k log-b7) (* b6 k log-b7) (* c log-b7) (- (* k log-b7)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 c-log-c c-log-k (- (* b1 k log-p7)) (- (* b2 k log-p7)) (- (* b3 k log-p7)) (- (* b4 k log-p7)) (- (* b5 k log-p7)) (- (* b6 k log-p7)) (* k log-p7) (- (* c log-p7)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 c-log-s k-log-λ) denominator))
        effort (Math/pow Math/E (/ (+ (- (* log-a)) (- (* b1 log-b1)) (- (* b2 log-b2)) (- (* b3 log-b3)) (- (* b4 log-b4)) (- (* b5 log-b5)) (- (* b6 log-b6)) (- (* b7 log-b7)) (* b1 log-p1) (* b2 log-p2) (* b3 log-p3) (* b4 log-p4) (* b5 log-p5) (* b6 log-p6) (* b7 log-p7) (- (* b1 log-λ)) (- (* b2 log-λ)) (- (* b3 log-λ)) (- (* b4 log-λ)) (- (* b5 log-λ)) (- (* b6 log-λ)) (- (* b7 log-λ)) (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ))) denominator) (- (/ (* b1 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)))) denominator)) (- (/ (* b2 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)))) denominator)) (- (/ (* b3 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)))) denominator)) (- (/ (* b4 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)))) denominator)) (- (/ (* b5 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)))) denominator)) (- (/ (* b6 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)))) denominator)) (- (/ (* b7 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)))) denominator))) c))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5 x6 x7] include-pollutants?)]
    (if include-pollutants?
     {:output output
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs
      :pollutant-quantities pollutant-qs}
     {:output output
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs})))

(defn solution-8 [a s c k ps b λ p-i include-pollutants?]
  (let [[b1 b2 b3 b4 b5 b6 b7 b8] (flatten b)
        [p1 p2 p3 p4 p5 p6 p7 p8] (flatten ps)
        log-a (Math/log a)
        log-b1 (Math/log b1)
        log-b2 (Math/log b2)
        log-b3 (Math/log b3)
        log-b4 (Math/log b4)
        log-b5 (Math/log b5)
        log-b6 (Math/log b6)
        log-b7 (Math/log b7)
        log-b8 (Math/log b8)
        log-c (Math/log c)
        log-k (Math/log k)
        log-s (Math/log s)
        log-p1 (Math/log p1)
        log-p2 (Math/log p2)
        log-p3 (Math/log p3)
        log-p4 (Math/log p4)
        log-p5 (Math/log p5)
        log-p6 (Math/log p6)
        log-p7 (Math/log p7)
        log-p8 (Math/log p8)
        log-λ (Math/log λ)
        denominator (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7) (* k b8))
        k-log-a (- (* k log-a))
        b1-k-log-b1 (- (* b1 k log-b1))
        b2-k-log-b2 (- (* b2 k log-b2))
        b3-k-log-b3 (- (* b3 k log-b3))
        b4-k-log-b4 (- (* b4 k log-b4))
        b5-k-log-b5 (- (* b5 k log-b5))
        b6-k-log-b6 (- (* b6 k log-b6))
        b7-k-log-b7 (- (* b7 k log-b7))
        b8-k-log-b8 (- (* b8 k log-b8))
        b1-k-log-p1 (* b1 k log-p1)
        b2-k-log-p2 (* b2 k log-p2)
        b3-k-log-p3 (* b3 k log-p3)
        b4-k-log-p4 (* b4 k log-p4)
        b5-k-log-p5 (* b5 k log-p5)
        b6-k-log-p6 (* b6 k log-p6)
        b7-k-log-p7 (* b7 k log-p7)
        b8-k-log-p8 (* b8 k log-p8)
        c-log-c (- (* c log-c))
        c-log-k (* c log-k)
        c-log-s (* c log-s)
        k-log-λ (- (* k log-λ))
        output (Math/pow Math/E (/ (+ k-log-a c-log-k c-log-s (- (* c log-λ)) c-log-c b1-k-log-b1 b1-k-log-p1 (- (* b1 k log-λ)) b2-k-log-b2 b2-k-log-p2 (- (* b2 k log-λ)) b3-k-log-b3 b3-k-log-p3 (- (* b3 k log-λ)) b4-k-log-b4 b4-k-log-p4 (- (* b4 k log-λ)) b5-k-log-b5 b5-k-log-p5 (- (* b5 k log-λ)) b6-k-log-b6 b6-k-log-p6 (- (* b6 k log-λ)) b7-k-log-b7 b7-k-log-p7 (- (* b7 k log-λ)) b8-k-log-b8 b8-k-log-p8 (- (* b8 k log-λ))) denominator))
        x1 (Math/pow Math/E (/ (+ k-log-a (* b2 k log-b1) (* b3 k log-b1) (* b4 k log-b1) (* b5 k log-b1) (* b6 k log-b1) (* b7 k log-b1) (* b8 k log-b1) (* c log-b1) (- (* k log-b1)) b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 c-log-c c-log-k (- (* b2 k log-p1)) (- (* b3 k log-p1)) (- (* b4 k log-p1)) (- (* b5 k log-p1)) (- (* b6 k log-p1)) (- (* b7 k log-p1)) (- (* b8 k log-p1)) (* k log-p1) (- (* c log-p1)) b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 c-log-s k-log-λ) denominator))
        x2 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b2) (* b3 k log-b2) (* b4 k log-b2) (* b5 k log-b2) (* b6 k log-b2) (* b7 k log-b2) (* b8 k log-b2) (* c log-b2) (- (* k log-b2)) b1-k-log-b1 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 c-log-c c-log-k (- (* b1 k log-p2)) (- (* b3 k log-p2)) (- (* b4 k log-p2)) (- (* b5 k log-p2)) (- (* b6 k log-p2)) (- (* b7 k log-p2)) (- (* b8 k log-p2)) (* k log-p2) (- (* c log-p2)) b1-k-log-p1 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 c-log-s k-log-λ) denominator))
        x3 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b3) (* b2 k log-b3) (* b4 k log-b3) (* b5 k log-b3) (* b6 k log-b3) (* b7 k log-b3) (* b8 k log-b3) (* c log-b3) (- (* k log-b3)) b1-k-log-b1 b2-k-log-b2 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 c-log-c c-log-k (- (* b1 k log-p3)) (- (* b2 k log-p3)) (- (* b4 k log-p3)) (- (* b5 k log-p3)) (- (* b6 k log-p3)) (- (* b7 k log-p3)) (- (* b8 k log-p3)) (* k log-p3) (- (* c log-p3)) b1-k-log-p1 b2-k-log-p2 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 c-log-s k-log-λ) denominator))
        x4 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b4) (* b2 k log-b4) (* b3 k log-b4) (* b5 k log-b4) (* b6 k log-b4) (* b7 k log-b4) (* b8 k log-b4) (* c log-b4) (- (* k log-b4)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 c-log-c c-log-k (- (* b1 k log-p4)) (- (* b2 k log-p4)) (- (* b3 k log-p4)) (- (* b5 k log-p4)) (- (* b6 k log-p4)) (- (* b7 k log-p4)) (- (* b8 k log-p4)) (* k log-p4) (- (* c log-p4)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 c-log-s k-log-λ) denominator))
        x5 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b5) (* b2 k log-b5) (* b3 k log-b5) (* b4 k log-b5) (* b6 k log-b5) (* b7 k log-b5) (* b8 k log-b5) (* c log-b5) (- (* k log-b5)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 c-log-c c-log-k (- (* b1 k log-p5)) (- (* b2 k log-p5)) (- (* b3 k log-p5)) (- (* b4 k log-p5)) (- (* b6 k log-p5)) (- (* b7 k log-p5)) (- (* b8 k log-p5)) (* k log-p5) (- (* c log-p5)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 c-log-s k-log-λ) denominator))
        x6 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b6) (* b2 k log-b6) (* b3 k log-b6) (* b4 k log-b6) (* b5 k log-b6) (* b7 k log-b6) (* b8 k log-b6) (* c log-b6) (- (* k log-b6)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b7-k-log-b7 b8-k-log-b8 c-log-c c-log-k (- (* b1 k log-p6)) (- (* b2 k log-p6)) (- (* b3 k log-p6)) (- (* b4 k log-p6)) (- (* b5 k log-p6)) (- (* b7 k log-p6)) (- (* b8 k log-p6)) (* k log-p6) (- (* c log-p6)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b7-k-log-p7 b8-k-log-p8 c-log-s k-log-λ) denominator))
        x7 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b7) (* b2 k log-b7) (* b3 k log-b7) (* b4 k log-b7) (* b5 k log-b7) (* b6 k log-b7) (* b8 k log-b7) (* c log-b7) (- (* k log-b7)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b8-k-log-b8 c-log-c c-log-k (- (* b1 k log-p7)) (- (* b2 k log-p7)) (- (* b3 k log-p7)) (- (* b4 k log-p7)) (- (* b5 k log-p7)) (- (* b6 k log-p7)) (- (* b8 k log-p7)) (* k log-p7) (- (* c log-p7)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b8-k-log-p8 c-log-s k-log-λ) denominator))
        x8 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b8) (* b2 k log-b8) (* b3 k log-b8) (* b4 k log-b8) (* b5 k log-b8) (* b6 k log-b8) (* b7 k log-b8) (* c log-b8) (- (* k log-b8)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k (- (* b1 k log-p8)) (- (* b2 k log-p8)) (- (* b3 k log-p8)) (- (* b4 k log-p8)) (- (* b5 k log-p8)) (- (* b6 k log-p8)) (- (* b7 k log-p8)) (* k log-p8) (- (* c log-p8)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s k-log-λ) denominator))
        effort (Math/pow Math/E (/ (+ (- (* log-a)) (- (* b1 log-b1)) (- (* b2 log-b2)) (- (* b3 log-b3)) (- (* b4 log-b4)) (- (* b5 log-b5)) (- (* b6 log-b6)) (- (* b7 log-b7)) (- (* b8 log-b8)) (- (* log-c)) (* b1 log-c) (* b2 log-c) (* b3 log-c) (* b4 log-c) (* b5 log-c) (* b6 log-c) (* b7 log-c) (* b8 log-c) (* log-k) (* b1 log-k) (* b2 log-k) (* b3 log-k) (* b4 log-k) (* b5 log-k) (* b6 log-k) (* b7 log-k) (* b8 log-k) (* b1 log-p1) (* b2 log-p2) (* b3 log-p3) (* b4 log-p4) (* b5 log-p5) (* b6 log-p6) (* b7 log-p7) (* b8 log-p8) (* log-s) (* b1 log-s) (* b2 log-s) (* b3 log-s) (* b4 log-s) (* b5 log-s) (* b6 log-s) (* b7 log-s) (* b8 log-s) (- (* log-λ))) denominator))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5 x6 x7 x8] include-pollutants?)]
    (if include-pollutants?
     {:output output
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs
      :pollutant-quantities pollutant-qs}
     {:output output
      :effort effort
      :intermediate-input-quantities intermediate-input-qs
      :nature-quantities nature-qs
      :labor-quantities labor-qs})))

(defn get-delta [price-delta price-delta-datum]
  (->> price-delta-datum
       (* price-delta)
       Math/abs
       (min price-delta)
       (max 0.001)))

(defn get-filtered-input-quantities [filter-factor m]
  (->> m
       vals
       (apply interleave)
       (partition 2)
       (filter (fn [[a _]] (= filter-factor (get a :coefficient))))
       (map last)))

(defn force-to-one [n]
  (let [cap 0.25]
    (if (or (> n cap) (< n (- cap))) cap (Math/abs n))))

(defn compute-surpluses-prices [wcs ccs natural-resources-supply labor-supply price-delta-data type-to-use price-datum]
  (let [id-to-use (:id price-datum)
        supply (condp = type-to-use
                           :private-goods (->> wcs
                                               (filter #(and (= 0 (get % :industry))
                                                             (= id-to-use
                                                                (get % :product))))
                                               (map :output)
                                               (reduce +))
                           :intermediate-inputs (->> wcs
                                                    (filter #(and (= 1 (get % :industry))
                                                                  (= id-to-use
                                                                     (get % :product))))
                                                    (mapv :output)
                                                    (reduce +))
                           :nature (nth natural-resources-supply (dec id-to-use))
                           :labor  (nth labor-supply (dec id-to-use))
                           :public-goods (->> wcs
                                              (filter #(and (= 2 (get % :industry))
                                                            (= id-to-use
                                                               (get % :product))))
                                              (map :output)
                                              (reduce +)) 
                           :pollutants (/ (->> ccs
                                               (map :pollutant-permissions)
                                               (map #(map (fn [x] (get-in x [:demand])) %))
                                               flatten
                                               (reduce +))
                                          (count ccs)))
        demand (condp = type-to-use
                           :private-goods (->> ccs
                                               (mapv :private-goods)
                                               (map #(map (fn [x] (get-in x [:demand])) %))
                                               flatten
                                               (reduce +))
                           :intermediate-inputs (->> wcs
                                                     (mapv #(select-keys % [:intermediate-inputs :intermediate-input-quantities]))
                                                     (filter (fn [x] (contains? (set (mapv :coefficient (:intermediate-inputs x))) id-to-use)))
                                                     (mapv (partial get-filtered-input-quantities id-to-use))
                                                     flatten
                                                     (reduce +)) 
                           :nature (->> wcs
                                        (mapv #(select-keys % [:nature :nature-quantities]))
                                        (filter (fn [x] (contains? (set (mapv :coefficient (:nature x))) id-to-use)))
                                        (mapv (partial get-filtered-input-quantities id-to-use))
                                        flatten
                                        (reduce +))
                           :labor (->> wcs
                                        (mapv #(select-keys % [:labor :labor-quantities]))
                                        (filter (fn [x] (contains? (set (mapv :coefficient (:labor x))) id-to-use)))
                                        (mapv (partial get-filtered-input-quantities id-to-use))
                                        flatten
                                        (reduce +))
                           :public-goods (/ (->> ccs
                                                 (map :public-goods)
                                                 (map #(map (fn [x] (get-in x [:demand])) %))
                                                 flatten
                                                 (reduce +))
                                            (count ccs))
                           :pollutants (->> wcs
                                            (mapv #(select-keys % [:pollutants :pollutant-quantities]))
                                            (filter (fn [x] (contains? (set (mapv :coefficient (:pollutants x))) id-to-use)))
                                            (mapv (partial get-filtered-input-quantities id-to-use))
                                            flatten
                                            (reduce +)))
        surplus (- supply demand)
        price-delta-to-use (- 1.05 (Math/pow 0.5 (/ (Math/abs (* 2 surplus)) (+ demand supply))))
        new-delta (force-to-one (get-delta price-delta-to-use (get price-delta-data type-to-use 1)))
        new-price (cond (pos? surplus) (* (- 1 new-delta) (:price price-datum))
                        (neg? surplus) (* (+ 1 new-delta) (:price price-datum))
                        :else (:price price-datum))]
    (assoc price-datum :pd new-delta :price new-price :surplus surplus :price-delta-to-use price-delta-to-use :supply supply :demand demand)))

(defn compute-percent-surplus [supply-list demand-list surplus-list]
  (let [averaged-s-and-d (->> (interleave (flatten supply-list)
                                          (flatten demand-list))
                              (partition 2)
                              (mapv mean))]
    (->> (interleave (flatten surplus-list) averaged-s-and-d)
         (partition 2)
         (mapv #(/ (first %) (last %)))
         (mapv force-to-one))))

(defn update-percent-surplus [supply-data demand-data surplus-data include-pollutants?]
  (let [categories (if include-pollutants?
                     [:private-goods :intermediate-inputs :nature :labor :public-goods :pollutants]
                     [:private-goods :intermediate-inputs :nature :labor :public-goods])
        updates-to-use (mapv (fn [cat-to-use] (compute-percent-surplus (get-in supply-data [cat-to-use])
                                                                       (get-in demand-data [cat-to-use])
                                                                       (get-in surplus-data [cat-to-use]))) categories)]
    (zipmap categories updates-to-use)))

#_(defn update-percent-surplus [supply-list demand-list surplus-list]
  (let [averaged-s-and-d (->> (interleave (flatten supply-list)
                                          (flatten demand-list))
                              (partition 2)
                              (mapv mean))]
    (->> (interleave (flatten surplus-list) averaged-s-and-d)
         (partition 2)
         (mapv #(/ (first %) (last %)))
         (mapv force-to-one))))

(defn update-surpluses-prices [wcs ccs natural-resources-supply labor-supply price-data price-delta-data include-pollutants?]
  (let [categories (if include-pollutants?
                     [:private-goods :intermediate-inputs :nature :labor :public-goods :pollutants]
                     [:private-goods :intermediate-inputs :nature :labor :public-goods])
        price-updates (mapv (fn [type-to-use] (mapv (partial compute-surpluses-prices wcs ccs natural-resources-supply labor-supply price-delta-data type-to-use) (get-in price-data [type-to-use]))) categories)]
     (zipmap categories price-updates)))

(defn compute-threshold [supply-list demand-list surplus-list]
  (->> (interleave (flatten surplus-list) (flatten demand-list) (flatten supply-list))
       (partition 3)
       (mapv #(* 100 (/ (Math/abs (* 2 (first %))) (+ (second %) (last %)))))))

(defn report-threshold [supply-data demand-data surplus-data include-pollutants?]
  (let [categories (if include-pollutants?
                     [:private-goods :intermediate-inputs :nature :labor :public-goods :pollutants]
                     [:private-goods :intermediate-inputs :nature :labor :public-goods])
        updates-to-use (mapv (fn [cat-to-use] (compute-threshold (get-in supply-data [cat-to-use])
                                                                 (get-in demand-data [cat-to-use])
                                                                 (get-in surplus-data [cat-to-use]))) categories)]
    (zipmap categories updates-to-use)))

(defn proposal [include-pollutants? prices wc]
  (letfn [(map-wc-values [w k]
            (let [cats-to-use (if include-pollutants?
                                [:intermediate-inputs :nature :labor :pollutants]
                                [:intermediate-inputs :nature :labor])]
              (->> cats-to-use
                   (mapv #(mapv k (get w %))))))
          (get-product-category-price [prices category product]
            (->> prices
                 category
                 (filterv #(= product (:id %)))
                 first
                 :price))
          (get-product-price [product prices]
            (->> prices
                 (filterv #(= product (:id %)))
                 first
                 :price))
          (get-lambda-o [w private-good-prices input-prices public-good-prices]
            (let [industry (:industry w)
                  product (:product w)]
              (cond (= 0 industry) (get-product-price product private-good-prices)
                    (= 1 industry) (get-product-price product input-prices)
                    (= 2 industry) (get-product-price product public-good-prices))))]
    (let [private-good-prices (:private-goods prices)
          input-prices (:intermediate-inputs prices)
          public-good-prices (:public-goods prices)
          input-count-r (if include-pollutants?
                          (+ (count (:intermediate-inputs wc))
                             (count (:labor wc))
                             (count (:nature wc))
                             (count (:pollutants wc)))
                          (+ (count (:intermediate-inputs wc))
                             (count (:labor wc))
                             (count (:nature wc))))
          total-factor-productivity (get wc :total-factor-productivity)
          effort-elasticity (get wc :effort-elasticity)
          disutility-of-effort-coefficient (get-in wc [:disutility-of-effort :coefficient])
          disutility-of-effort-exponent (get-in wc [:disutility-of-effort :exponent])
          p-i (map-wc-values wc :coefficient)
          ps (if include-pollutants?
               [(mapv (partial get-product-category-price prices :intermediate-inputs) (first p-i))
                (mapv (partial get-product-category-price prices :nature) (second p-i))
                (mapv (partial get-product-category-price prices :labor) (nth p-i 2))
                (mapv (partial get-product-category-price prices :pollutants) (last p-i))]
               [(mapv (partial get-product-category-price prices :intermediate-inputs) (first p-i))
                (mapv (partial get-product-category-price prices :nature) (second p-i))
                (mapv (partial get-product-category-price prices :labor) (nth p-i 2))])
          b (map-wc-values wc :exponent)
          λ (get-lambda-o wc private-good-prices input-prices public-good-prices)]
      (condp = input-count-r
        3 (merge wc (solution-3 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
        4 (merge wc (solution-4 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
        5 (merge wc (solution-5 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
        6 (merge wc (solution-6 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
        7 (merge wc (solution-7 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
        8 (merge wc (solution-8 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
        (str "unexpected input-count value: " input-count-r)))))

; NB: Watch for pollutant-prices and scaling effects -- i.e., does a price affect all CCs or just one CC?
; TODO: Rename demand as permission in :pollutant-permissions ?
(defn consume [include-pollutants? private-goods public-goods pollutants num-of-ccs price-data cc]
  (let [private-good-exponents (mapv :exponent (:private-goods cc))
        public-good-exponents (mapv :exponent (:public-goods cc))
        private-goods-in-cc (:private-goods cc)
        public-goods-in-cc (:public-goods cc)
        pollutant-prices (if include-pollutants? (:pollutants price-data) [])
        private-good-prices (:private-goods price-data)
        public-good-prices (:public-goods price-data)
        pollutant-permissions (:pollutant-permissions cc)
        pollutant-positive-utility-from-income (get-in cc [:pollutant-utilities :positive-utility-from-income]) 
        pollutant-negative-utility-from-exposure (get-in cc [:pollutant-utilities :negative-utility-from-exposure])
        updated-pollutant-permissions (mapv (fn [pollutant]
                                              (let [p (:price (first (filter #(= pollutant (:id %)) pollutant-prices)))
                                                    previous-permission (first (filter #(= pollutant (:id %)) pollutant-permissions)) 
                                                    k pollutant-negative-utility-from-exposure
                                                    j pollutant-positive-utility-from-income
                                                    pollutant-permission (* (Math/pow 5 (/ 1 (- k j)))
                                                                            (Math/pow (/ (* j (Math/pow p j)) k) (/ 1 (- k j))))]
                                          (assoc previous-permission :demand pollutant-permission)))
                                        pollutants)
        income (if include-pollutants? 
                 (apply + (cc :income) (mapv :demand updated-pollutant-permissions))
                 (cc :income))
        updated-private-goods (mapv
                                (fn [private-good]
                                  (let [private-good-price (:price (first (filter #(= private-good (:id %)) private-good-prices)))
                                        previous-private-good (first (filter #(= private-good (:id %)) private-goods-in-cc))
                                        private-good-exponent (:exponent previous-private-good)
                                        updated-demand     (/ (* income private-good-exponent)
                                                              (* (apply + (concat private-good-exponents public-good-exponents))
                                                                 private-good-price))]
                                    (assoc previous-private-good :demand updated-demand)))
                               private-goods)
        updated-public-goods (mapv (fn [public-good]
                                     (let [public-good-price (:price (first (filter #(= public-good (:id %)) public-good-prices)))
                                           previous-public-good (first (filter #(= public-good (:id %)) public-goods-in-cc))
                                           public-good-exponent (:exponent previous-public-good)
                                           updated-demand       (/ (* income public-good-exponent)
                                                                   (* (apply + (concat private-good-exponents public-good-exponents))
                                                                      (/ public-good-price num-of-ccs)))]
                                   (assoc previous-public-good :demand updated-demand)))
                              public-goods)]
    (if include-pollutants?
      (assoc cc :private-goods updated-private-goods
                :public-goods updated-public-goods
                :pollutant-permissions updated-pollutant-permissions
                :income income)
      (assoc cc :private-goods updated-private-goods
                :public-goods updated-public-goods
                :income income))))

(defn get-pricing-data [price-data pricing-cat include-pollutants?]
  (let [categories (if include-pollutants?
                     [:private-goods :intermediate-inputs :nature :labor :public-goods :pollutants]
                     [:private-goods :intermediate-inputs :nature :labor :public-goods])
        data-to-get (mapv (fn [type-to-use] (mapv pricing-cat (get-in price-data [type-to-use]))) categories)]
    (zipmap categories data-to-get)))

(defn calculate-price-deltas [supply-list demand-list surplus-list]
  (let [surplus-list-means (mean surplus-list)
        averaged-s-and-d (mean [(mean supply-list) (mean demand-list)])]
        (Math/abs (/ surplus-list-means averaged-s-and-d))))

(defn update-price-deltas [supply-data demand-data surplus-data include-pollutants?]
  (let [categories (if include-pollutants?
                     [:private-goods :intermediate-inputs :nature :labor :public-goods :pollutants]
                     [:private-goods :intermediate-inputs :nature :labor :public-goods])
        data-to-get (mapv (fn [type-to-use] (calculate-price-deltas (get-in supply-data [type-to-use]) (get-in demand-data [type-to-use]) (get-in surplus-data [type-to-use]))) categories)]
    (zipmap categories data-to-get)))

#_(defn iterate-plan [t]
  (let [wcs (mapv (partial proposal (:price-data t)) (:wcs t))
        ccs (mapv (partial consume (t :private-goods) (t :public-good-types) (t :pollutant-types) (count (t :ccs)) (get-in t [:price-data])) (t :ccs))
        price-data (update-surpluses-prices wcs ccs (:price-data t))
        surplus-data (get-pricing-data price-data :surplus)
        supply-data (get-pricing-data price-data :supply)
        demand-data (get-pricing-data price-data :demand)
        price-deltas (get-pricing-data price-data :price-delta-to-use)
        pd-list (get-pricing-data price-data :pd)
        percent-surplus (update-percent-surplus supply-data demand-data surplus-data)
        threshold-report (report-threshold supply-data demand-data surplus-data)
        t2 (assoc t :wcs wcs
                    :ccs ccs
                    :price-data price-data
                    :surplus-data surplus-data
                    :supply-data supply-data
                    :demand-data demand-data
                    :price-delta-data price-deltas
                    :pd-data pd-list
                    :percent-surplus percent-surplus
                    :threshold-report threshold-report
                    :iteration (inc (:iteration t)))]
    t2))
; ---

(defn individual-augment [set-to-use]
  (mapv (fn [e] (assoc e :exponent (+ (get e :augment) (get e :exponent)))) set-to-use))

(defn augment-wc [wc]
  (assoc wc :intermediate-inputs (individual-augment (:intermediate-inputs wc))
            :nature (individual-augment (:nature wc))
            :labor (individual-augment (:labor wc))
            :pollutants (individual-augment (:pollutants wc))))

(defn augment-cc [cc]
  (assoc cc :public-goods (individual-augment (:public-goods cc))
            :private-goods (individual-augment (:private-goods cc))
            :pollutant-permissions (individual-augment (:pollutant-permissions cc))))

(defn augmented-reset [t]
  (assoc t :iteration 0
           :ccs (mapv augment-cc (get t :ccs))
           :wcs (mapv augment-wc (get t :wcs))))
