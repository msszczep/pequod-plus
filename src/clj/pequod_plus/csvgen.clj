(ns pequod-plus.csvgen
   (:require [pequod-plus.util :as util]
             [clojure.edn :as edn]
             [next.jdbc :as jdbc]
             [next.jdbc.result-set :as result-set]
             [clojure.java.io :as io]))

(def ds (jdbc/get-datasource {:dbtype "sqlite" :dbname "pequod-csv-test.db"}))

(def include-pollutants? true)

(def iteration-count (atom 0))

(def final-results (atom []))

(def price-data (atom []))

(def price-delta-data
  (atom {:private-goods 0.05
         :intermediate-inputs 0.05
         :nature 0.05
         :labor 0.05
         :public-goods 0.05
         :pollutants 0.05}))

(defn compute-gdp [supply-list private-good-prices public-good-prices]
  (let [[private-good-supply _ _ _ public-good-supply] supply-list]
    (->> public-good-prices
         (concat private-good-prices)
         (interleave (concat private-good-supply public-good-supply))
         (partition 2)
         (map (fn [[a b]] (* a b)))
         (apply +))))

(defn show-color [tre]
  (cond (empty? tre) :red
        (every? #(< % 3) tre) :blue
        (every? #(< % 5) tre) :green
        (every? #(< % 10) tre) :yellow
        (every? #(< % 20) tre) :orange
        :else :red))

(defn create-final-output [es]
  (for [e es]
    (let [k (first e)
          ns (second e)]
      (vector k (show-color ns) (util/mean ns) ns))))

#_(defn iterate-plan [t _]
  (let [include-pollutants? (:include-pollutants? t)
        wcs (mapv (partial util/proposal include-pollutants? (:price-data t)) (:wcs t))
        ccs (mapv (partial util/consume include-pollutants? (t :private-goods) (t :public-good-types) (t :pollutant-types) (count (t :ccs)) (get-in t [:price-data])) (t :ccs))
        price-data (util/update-surpluses-prices wcs ccs (:natural-resources-supply t) (:labor-supply t) (:price-data t) (:price-delta-data t) include-pollutants?)
        surplus-data (util/get-pricing-data price-data :surplus include-pollutants?)
        supply-data (util/get-pricing-data price-data :supply include-pollutants?)
        demand-data (util/get-pricing-data price-data :demand include-pollutants?)
        price-delta-data (util/update-price-deltas supply-data demand-data surplus-data include-pollutants?)
        pd-data (util/update-percent-surplus supply-data demand-data surplus-data include-pollutants?)
        threshold-report (util/report-threshold supply-data demand-data surplus-data include-pollutants?)
        color (zipmap (keys threshold-report) (map show-color (vals threshold-report)))
        t2 (assoc t :wcs wcs
                    :ccs ccs
                    :price-data price-data
                    :surplus-data surplus-data
                    :supply-data supply-data
                    :demand-data demand-data
                    :price-delta-data price-delta-data
                    :pd-data pd-data
                    :threshold-report threshold-report
                    :iteration (inc (:iteration t))
                    :color color)]
    t2))

(defn allot-production-quantities [production-inputs xs include-pollutants?]
  (let [[I N L P] production-inputs
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

(defn update-output-effort [ds output effort wc-id]
  (jdbc/execute! ds ["UPDATE wcs SET output = ?, effort = ? WHERE id = ?;" output effort wc-id]))

(defn update-production-quantities [ds table field wc-id production-quantities]
  (let [n (count production-quantities)]
    (doseq [i (range 1 (inc n))]
      (let [s [(str "UPDATE " table " SET quantity = ? where wc_id = ? AND " field " = ?")
               (nth production-quantities (dec i)) wc-id i]]
        (jdbc/execute! ds s)))))

#_(do
      (time (update-output-effort ds output effort wc-id))
      (time (update-production-quantities ds "intermediate_inputs" "intermediate_input_id" wc-id intermediate-input-qs))
      (time (update-production-quantities ds "nature" "nature_id" wc-id nature-qs))
      (time (update-production-quantities ds "labor" "labor_id" wc-id labor-qs))
      (if include-pollutants?
        (update-production-quantities ds "pollutant_demands" "pollutant_id" wc-id pollutant-qs)))

(defn solution-3 [{:keys [a s c k ps b λ p-i include-pollutants? wc-id]}]
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
    {:wc-id wc-id
     :output output
     :effort effort
     :intermediate-inputs intermediate-input-qs
     :nature nature-qs
     :labor labor-qs
     :pollutants pollutant-qs}))

(defn solution-4 [{:keys [a s c k ps b λ p-i include-pollutants? wc-id]}]
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
        k-log-λ (* k log-λ)
        k-log-p1 (* k log-p1)
        k-log-p2 (* k log-p2)
        k-log-p3 (* k log-p3)
        k-log-p4 (* k log-p4)
        output (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 (* c log-s) (- (* c log-λ)) (- (* b1 k-log-λ)) (- (* b2 k-log-λ)) (- (* b3 k-log-λ)) (- (* b4 k-log-λ))) denominator))
        x1 (Math/pow Math/E (/ (+ k-log-a (* c log-b1) (- (* k log-b1)) (* b2 k log-b1) (* b3 k log-b1) (* b4 k log-b1) b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 c-log-c c-log-k (- (* c log-p1)) k-log-p1 (- (* b2 k-log-p1)) (- (* b3 k-log-p1)) (- (* b4 k-log-p1)) b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 (* c log-s) (- k-log-λ)) denominator))
        x2 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 (* c log-b2) (- (* k log-b2)) (* b1 k log-b2) (* b3 k log-b2) (* b4 k log-b2) b3-k-log-b3 b4-k-log-b4 c-log-c c-log-k b1-k-log-p1 (- (* c log-p2)) k-log-p2 (- (* b1 k-log-p2)) (- (* b3 k-log-p2)) (- (* b4 k-log-p2)) b3-k-log-p3 b4-k-log-p4 (* c log-s) (- k-log-λ)) denominator))
        x3 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 (* c log-b3) (- (* k log-b3)) (* b1 k log-b3) (* b2 k log-b3) (* b4 k log-b3) b4-k-log-b4 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 (- (* c log-p3)) k-log-p3 (- (* b1 k-log-p3)) (- (* b2 k-log-p3)) (- (* b4 k-log-p3)) b4-k-log-p4 (* c log-s) (- k-log-λ)) denominator))
        x4 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 (* c log-b4) (- (* k log-b4)) (* b1 k log-b4) (* b2 k log-b4) (* b3 k log-b4) c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 (- (* c log-p4)) k-log-p4 (- (* b1 k-log-p4)) (- (* b2 k-log-p4)) (- (* b3 k-log-p4)) (* c log-s) (- k-log-λ)) denominator))
        effort (Math/pow Math/E (/ (+ (- (* log-a)) (- (* b1 log-b1)) (- (* b2 log-b2)) (- (* b3 log-b3)) (- (* b4 log-b4)) (- (* log-c)) (* b1 log-c) (* b2 log-c) (* b3 log-c) (* b4 log-c) (* log-k) (- (* b1 log-k)) (- (* b2 log-k)) (- (* b3 log-k)) (- (* b4 log-k)) (* b1 log-p1) (* b2 log-p2) (* b3 log-p3) (* b4 log-p4) log-s (- (* b1 log-s)) (- (* b2 log-s)) (- (* b3 log-s)) (- (* b4 log-s)) (- log-λ)) denominator))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4] include-pollutants?)]
    {:wc-id wc-id
     :output output
     :effort effort
     :intermediate-inputs intermediate-input-qs
     :nature nature-qs
     :labor labor-qs
     :pollutants pollutant-qs}))

(defn solution-5 [{:keys [a s c k ps b λ p-i include-pollutants? wc-id]}]
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
        k-log-λ (* k log-λ)
        k-log-p1 (* k log-p1)
        k-log-p2 (* k log-p2)
        k-log-p3 (* k log-p3)
        k-log-p4 (* k log-p4)
        k-log-p5 (* k log-p5)
        output (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k-log-λ)) (- (* b2 k-log-λ)) (- (* b3 k-log-λ)) (- (* b4 k-log-λ)) (- (* b5 k-log-λ))) denominator))
        x1 (Math/pow Math/E (/ (+ k-log-a (* c log-b1) (- (* k log-b1)) (* b2 k log-b1) (* b3 k log-b1) (* b4 k log-b1) (* b5 k log-b1) b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k (- (* c log-p1)) k-log-p1 (- (* b2 k-log-p1)) (- (* b3 k-log-p1)) (- (* b4 k-log-p1)) (- (* b5 k-log-p1)) b2-k-log-p2 (* b3 k log-p2) b4-k-log-p4 b5-k-log-p5 (* c log-s) (- k-log-λ)) denominator))
        x2 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 (* c log-b2) (- (* k log-b2)) (* b1 k log-b2) (* b3 k log-b2) (* b4 k log-b2) (* b5 k log-b2) b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 (- (* c log-p2)) k-log-p2 (- (* b1 k-log-p2)) (- (* b3 k-log-p2)) (- (* b4 k-log-p2)) (- (* b5 k-log-p2)) b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- k-log-λ)) denominator))
        x3 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 (* c log-b3) (- (* k log-b3)) (* b1 k log-b3) (* b2 k log-b3) (* b4 k log-b3) (* b5 k log-b3) b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 (- (* c log-p3)) k-log-p3 (- (* b1 k-log-p3)) (- (* b2 k-log-p3)) (- (* b4 k-log-p3)) (- (* b5 k-log-p3)) b4-k-log-p4 b5-k-log-p5 (* c log-s) (- k-log-λ)) denominator))
        x4 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 (* c log-b4) (- (* k log-b4)) (* b1 k log-b4) (* b2 k log-b4) (* b3 k log-b4) (* b5 k log-b4) b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 (- (* c log-p4)) k-log-p4 (- (* b1 k-log-p4)) (- (* b2 k-log-p4)) (- (* b3 k-log-p4)) (- (* b5 k-log-p4)) b5-k-log-p5 (* c log-s) (- k-log-λ)) denominator))
        x5 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 (* c log-b5) (- (* k log-b5)) (* b1 k log-b5) (* b2 k log-b5) (* b3 k log-b5) (* b4 k log-b5) c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 (- (* c log-p5)) k-log-p5 (- (* b1 k-log-p5)) (- (* b2 k-log-p5)) (- (* b3 k-log-p5)) (- (* b4 k-log-p5)) (* c log-s) (- k-log-λ)) denominator))
        effort (Math/pow Math/E (/ (+ (- ( * log-a)) (- (* b1 log-b1)) (- (* b2 log-b2)) (- (* b3 log-b3)) (- (* b4 log-b4)) (- (* b5 log-b5)) (* b1 log-p1) (* b2 log-p2) (* b3 log-p3) (* b4 log-p4) (* b5 log-p5) (- (* b1 log-λ)) (- (* b2 log-λ)) (- (* b3 log-λ)) (- (* b4 log-λ)) (- (* b5 log-λ)) (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ))) denominator) (- (/ (* b1 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)))) denominator)) (- (/ (* b2 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)))) denominator)) (- (/ (* b3 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)))) denominator)) (- (/ (* b4 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)))) denominator)) (- (/ (* b5 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (* c log-s) (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)))) denominator))) c))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5] include-pollutants?)]
    {:wc-id wc-id
     :output output
     :effort effort
     :intermediate-inputs intermediate-input-qs
     :nature nature-qs
     :labor labor-qs
     :pollutants pollutant-qs}))

(defn solution-6 [{:keys [a s c k ps b λ p-i include-pollutants? wc-id]}]
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
        k-log-p1 (* k log-p1)
        k-log-p2 (* k log-p2)
        k-log-p3 (* k log-p3)
        k-log-p4 (* k log-p4)
        k-log-p5 (* k log-p5)
        k-log-p6 (* k log-p6)
        output (Math/pow Math/E (- (/ (+ (* k log-a) (* b1 k log-b1) (* b2 k log-b2) (* b3 k log-b3) (* b4 k log-b4) (* b5 k log-b5) (* b6 k log-b6) (* c log-c) (- c-log-k) (- b1-k-log-p1) (- b2-k-log-p2) (- b3-k-log-p3) (- b4-k-log-p4) (- b5-k-log-p5) (- b6-k-log-p6)  (- c-log-s) (* c log-λ) (* b1 k log-λ) (* b2 k log-λ) (* b3 k log-λ) (* b4 k log-λ) (* b5 k log-λ) (* b6 k log-λ)) denominator)))
        x1 (Math/pow Math/E (/ (+ k-log-a (* c log-b1) (- (* k log-b1)) (* b2 k log-b1) (* b3 k log-b1) (* b4 k log-b1) (* b5 k log-b1) (* b6 k log-b1) b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 c-log-c c-log-k (- (* c log-p1)) k-log-p1 (- (* b2 k-log-p1)) (- (* b3 k-log-p1)) (- (* b4 k-log-p1)) (- (* b5 k-log-p1)) (- (* b6 k log-p1)) b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 c-log-s k-log-λ) denominator))
        x2 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 (* c log-b2) (- (* k log-b2)) (* b1 k log-b2) (* b3 k log-b2) (* b4 k log-b2) (* b5 k log-b2) (* b6 k log-b2) b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 c-log-c c-log-k b1-k-log-p1 (- (* c log-p2)) k-log-p2 (- (* b1 k-log-p2)) (- (* b3 k-log-p2)) (- (* b4 k-log-p2)) (- (* b5 k-log-p2)) (- (* b6 k-log-p2)) b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 c-log-s k-log-λ) denominator))
        x3 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 (* c log-b3) (- (* k log-b3)) (* b1 k log-b3) (* b2 k log-b3) (* b4 k log-b3) (* b5 k log-b3) (* b6 k log-b3) b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 (- (* c log-p3)) k-log-p3 (- (* b1 k-log-p3)) (- (* b2 k-log-p3)) (- (* b4 k-log-p3)) (- (* b5 k-log-p3)) (- (* b6 k-log-p3)) b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 c-log-s k-log-λ) denominator))
        x4 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 (* c log-b4) (- (* k log-b4)) (* b1 k log-b4) (* b2 k log-b4) (* b3 k log-b4) (* b5 k log-b4) (* b6 k log-b4) b5-k-log-b5 b6-k-log-b6 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 (- (* c log-p4)) k-log-p4 (- (* b1 k-log-p4)) (- (* b2 k-log-p4)) (- (* b3 k-log-p4)) (- (* b5 k-log-p4)) (- (* b6 k-log-p4)) b5-k-log-p5 b6-k-log-p6 c-log-s k-log-λ) denominator))
        x5 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 (* c log-b5) (- (* k log-b5)) (* b1 k log-b5) (* b2 k log-b5) (* b3 k log-b5) (* b4 k log-b5) (* b6 k log-b5) b6-k-log-b6 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 (- (* c log-p5)) k-log-p5 (- (* b1 k-log-p5)) (- (* b2 k-log-p5)) (- (* b3 k-log-p5)) (- (* b4 k-log-p5)) (- (* b6 k-log-p5)) b6-k-log-p6 c-log-s k-log-λ) denominator))
        x6 (Math/pow Math/E (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 (* c log-b6) (- (* k log-b6)) (* b1 k log-b6) (* b2 k log-b6) (* b3 k log-b6) (* b4 k log-b6) (* b5 k log-b6) c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 (- (* c log-p6)) k-log-p6 (- (* b1 k-log-p6)) (- (* b2 k-log-p6)) (- (* b3 k-log-p6)) (- (* b4 k-log-p6)) (- (* b5 k-log-p6)) c-log-s k-log-λ) denominator))
        effort (Math/pow Math/E (/ (+ (- log-a) (- (* b1 log-b1)) (- (* b2 log-b2)) (- (* b3 log-b3)) (- (* b4 log-b4)) (- (* b5 log-b5)) (- (* b6 log-b6)) (- (* log-c)) (* b1 log-c) (* b2 log-c) (* b3 log-c) (* b4 log-c) (* b5 log-c) (* b6 log-c) (* log-k) (- (* b1 log-k)) (- (* b2 log-k)) (- (* b3 log-k)) (- (* b4 log-k)) (- (* b5 log-k)) (- (* b6 log-k)) (* b1 log-p1) (* b2 log-p2) (* b3 log-p3) (* b4 log-p4) (* b5 log-p5) (* b6 log-p6) log-s (- (* b1 log-s)) (- (* b2 log-s)) (- (* b3 log-s)) (- (* b4 log-s)) (- (* b5 log-s)) (- (* b6 log-s)) (- log-λ)) denominator))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5 x6] include-pollutants?)]
    {:wc-id wc-id
     :output output
     :effort effort
     :intermediate-inputs intermediate-input-qs
     :nature nature-qs
     :labor labor-qs
     :pollutants pollutant-qs}))

(defn solution-7 [{:keys [a s c k ps b λ p-i include-pollutants? wc-id]}]
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
        minus-k-log-λ (- (* k log-λ))
        k-log-λ (* k log-λ)
        k-log-p1 (* k log-p1)
        k-log-p2 (* k log-p2)
        k-log-p3 (* k log-p3)
        k-log-p4 (* k log-p4)
        k-log-p5 (* k log-p5)
        k-log-p6 (* k log-p6)
        k-log-p7 (* k log-p7)
        output (Math/pow Math/E (/ (+ k-log-a c-log-k c-log-s (- (* c log-λ)) c-log-c b1-k-log-b1 b1-k-log-p1 (- (* b1 k-log-λ)) b2-k-log-b2 b2-k-log-p2 (- (* b2 k-log-λ)) b3-k-log-b3 b3-k-log-p3 (- (* b3 k-log-λ)) b4-k-log-b4 b4-k-log-p4 (- (* b4 k-log-λ)) b5-k-log-b5 b5-k-log-p5 (- (* b5 k-log-λ)) b6-k-log-b6 b6-k-log-p6 (- (* b6 k-log-λ)) b7-k-log-b7 b7-k-log-p7 (- (* b7 k-log-λ))) denominator))
        x1 (Math/pow Math/E (/ (+ k-log-a (* b2 k log-b1) (* b3 k log-b1) (* b4 k log-b1) (* b5 k log-b1) (* b6 k log-b1) (* b7 k log-b1) (* c log-b1) (- (* k log-b1)) b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k (- (* b2 k-log-p1)) (- (* b3 k-log-p1)) (- (* b4 k-log-p1)) (- (* b5 k-log-p1)) (- (* b6 k-log-p1)) (- (* b7 k-log-p1)) k-log-p1 (- (* c log-p1)) b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s minus-k-log-λ) denominator))
        x2 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b2) (* b3 k log-b2) (* b4 k log-b2) (* b5 k log-b2) (* b6 k log-b2) (* b7 k log-b2) (* c log-b2) (- (* k log-b2)) b1-k-log-b1 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k (- (* b1 k-log-p2)) (- (* b3 k-log-p2)) (- (* b4 k-log-p2)) (- (* b5 k-log-p2)) (- (* b6 k-log-p2)) (- (* b7 k-log-p2)) k-log-p2 (- (* c log-p2)) b1-k-log-p1 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s minus-k-log-λ) denominator))
        x3 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b3) (* b2 k log-b3) (* b4 k log-b3) (* b5 k log-b3) (* b6 k log-b3) (* b7 k log-b3) (* c log-b3) (- (* k log-b3)) b1-k-log-b1 b2-k-log-b2 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k (- (* b1 k-log-p3)) (- (* b2 k-log-p3)) (- (* b4 k-log-p3)) (- (* b5 k-log-p3)) (- (* b6 k-log-p3)) (- (* b7 k-log-p3)) k-log-p3 (- (* c log-p3)) b1-k-log-p1 b2-k-log-p2 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s minus-k-log-λ) denominator))
        x4 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b4) (* b2 k log-b4) (* b3 k log-b4) (* b5 k log-b4) (* b6 k log-b4) (* b7 k log-b4) (* c log-b4) (- (* k log-b4)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k (- (* b1 k-log-p4)) (- (* b2 k-log-p4)) (- (* b3 k-log-p4)) (- (* b5 k-log-p4)) (- (* b6 k-log-p4)) (- (* b7 k-log-p4)) k-log-p4 (- (* c log-p4)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s minus-k-log-λ) denominator))
        x5 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b5) (* b2 k log-b5) (* b3 k log-b5) (* b4 k log-b5) (* b6 k log-b5) (* b7 k log-b5) (* c log-b5) (- (* k log-b5)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k (- (* b1 k-log-p5)) (- (* b2 k-log-p5)) (- (* b3 k-log-p5)) (- (* b4 k-log-p5)) (- (* b6 k-log-p5)) (- (* b7 k-log-p5)) k-log-p5 (- (* c log-p5)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b6-k-log-p6 b7-k-log-p7 c-log-s minus-k-log-λ) denominator))
        x6 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b6) (* b2 k log-b6) (* b3 k log-b6) (* b4 k log-b6) (* b5 k log-b6) (* b7 k log-b6) (* c log-b6) (- (* k log-b6)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b7-k-log-b7 c-log-c c-log-k (- (* b1 k-log-p6)) (- (* b2 k-log-p6)) (- (* b3 k-log-p6)) (- (* b4 k-log-p6)) (- (* b5 k-log-p6)) (- (* b7 k-log-p6)) k-log-p6 (- (* c log-p6)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b7-k-log-p7 c-log-s minus-k-log-λ) denominator))
        x7 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b7) (* b2 k log-b7) (* b3 k log-b7) (* b4 k log-b7) (* b5 k log-b7) (* b6 k log-b7) (* c log-b7) (- (* k log-b7)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 c-log-c c-log-k (- (* b1 k-log-p7)) (- (* b2 k-log-p7)) (- (* b3 k-log-p7)) (- (* b4 k-log-p7)) (- (* b5 k-log-p7)) (- (* b6 k-log-p7)) k-log-p7 (- (* c log-p7)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 c-log-s minus-k-log-λ) denominator))
        effort (Math/pow Math/E (/ (+ (- (* log-a)) (- (* b1 log-b1)) (- (* b2 log-b2)) (- (* b3 log-b3)) (- (* b4 log-b4)) (- (* b5 log-b5)) (- (* b6 log-b6)) (- (* b7 log-b7)) (* b1 log-p1) (* b2 log-p2) (* b3 log-p3) (* b4 log-p4) (* b5 log-p5) (* b6 log-p6) (* b7 log-p7) (- (* b1 log-λ)) (- (* b2 log-λ)) (- (* b3 log-λ)) (- (* b4 log-λ)) (- (* b5 log-λ)) (- (* b6 log-λ)) (- (* b7 log-λ)) (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k-log-λ)) (- (* b2 k-log-λ)) (- (* b3 k-log-λ)) (- (* b4 k-log-λ)) (- (* b5 k-log-λ)) (- (* b6 k-log-λ)) (- (* b7 k-log-λ))) denominator) (- (/ (* b1 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k-log-λ)) (- (* b2 k-log-λ)) (- (* b3 k-log-λ)) (- (* b4 k-log-λ)) (- (* b5 k-log-λ)) (- (* b6 k-log-λ)) (- (* b7 k-log-λ)))) denominator)) (- (/ (* b2 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k-log-λ)) (- (* b2 k-log-λ)) (- (* b3 k-log-λ)) (- (* b4 k-log-λ)) (- (* b5 k-log-λ)) (- (* b6 k-log-λ)) (- (* b7 k-log-λ)))) denominator)) (- (/ (* b3 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k-log-λ)) (- (* b2 k-log-λ)) (- (* b3 k-log-λ)) (- (* b4 k-log-λ)) (- (* b5 k-log-λ)) (- (* b6 k-log-λ)) (- (* b7 k-log-λ)))) denominator)) (- (/ (* b4 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k-log-λ)) (- (* b2 k-log-λ)) (- (* b3 k-log-λ)) (- (* b4 k-log-λ)) (- (* b5 k-log-λ)) (- (* b6 k-log-λ)) (- (* b7 k-log-λ)))) denominator)) (- (/ (* b5 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k-log-λ)) (- (* b2 k-log-λ)) (- (* b3 k-log-λ)) (- (* b4 k-log-λ)) (- (* b5 k-log-λ)) (- (* b6 k-log-λ)) (- (* b7 k-log-λ)))) denominator)) (- (/ (* b6 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k-log-λ)) (- (* b2 k-log-λ)) (- (* b3 k-log-λ)) (- (* b4 k-log-λ)) (- (* b5 k-log-λ)) (- (* b6 k-log-λ)) (- (* b7 k-log-λ)))) denominator)) (- (/ (* b7 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s (- (* c log-λ)) (- (* b1 k-log-λ)) (- (* b2 k-log-λ)) (- (* b3 k-log-λ)) (- (* b4 k-log-λ)) (- (* b5 k-log-λ)) (- (* b6 k-log-λ)) (- (* b7 k-log-λ)))) denominator))) c))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5 x6 x7] include-pollutants?)]
    {:wc-id wc-id
     :output output
     :effort effort
     :intermediate-inputs intermediate-input-qs
     :nature nature-qs
     :labor labor-qs
     :pollutants pollutant-qs}))

(defn solution-8 [{:keys [a s c k ps b λ p-i include-pollutants? wc-id]}]
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
        minus-k-log-λ (- (* k log-λ))
        k-log-λ (* k log-λ)
        k-log-p1 (* k log-p1)
        k-log-p2 (* k log-p2)
        k-log-p3 (* k log-p3)
        k-log-p4 (* k log-p4)
        k-log-p5 (* k log-p5)
        k-log-p6 (* k log-p6)
        k-log-p7 (* k log-p7)
        k-log-p8 (* k log-p8)
        output (Math/pow Math/E (/ (+ k-log-a c-log-k c-log-s (- (* c log-λ)) c-log-c b1-k-log-b1 b1-k-log-p1 (- (* b1 k log-λ)) b2-k-log-b2 b2-k-log-p2 (- (* b2 k log-λ)) b3-k-log-b3 b3-k-log-p3 (- (* b3 k log-λ)) b4-k-log-b4 b4-k-log-p4 (- (* b4 k log-λ)) b5-k-log-b5 b5-k-log-p5 (- (* b5 k log-λ)) b6-k-log-b6 b6-k-log-p6 (- (* b6 k log-λ)) b7-k-log-b7 b7-k-log-p7 (- (* b7 k log-λ)) b8-k-log-b8 b8-k-log-p8 (- (* b8 k log-λ))) denominator))
        x1 (Math/pow Math/E (/ (+ k-log-a (* b2 k log-b1) (* b3 k log-b1) (* b4 k log-b1) (* b5 k log-b1) (* b6 k log-b1) (* b7 k log-b1) (* b8 k log-b1) (* c log-b1) (- (* k log-b1)) b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 c-log-c c-log-k (- (* b2 k-log-p1)) (- (* b3 k-log-p1)) (- (* b4 k-log-p1)) (- (* b5 k-log-p1)) (- (* b6 k-log-p1)) (- (* b7 k-log-p1)) (- (* b8 k-log-p1)) k-log-p1 (- (* c log-p1)) b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 c-log-s minus-k-log-λ) denominator))
        x2 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b2) (* b3 k log-b2) (* b4 k log-b2) (* b5 k log-b2) (* b6 k log-b2) (* b7 k log-b2) (* b8 k log-b2) (* c log-b2) (- (* k log-b2)) b1-k-log-b1 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 c-log-c c-log-k (- (* b1 k-log-p2)) (- (* b3 k-log-p2)) (- (* b4 k-log-p2)) (- (* b5 k-log-p2)) (- (* b6 k-log-p2)) (- (* b7 k-log-p2)) (- (* b8 k-log-p2)) k-log-p2 (- (* c log-p2)) b1-k-log-p1 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 c-log-s minus-k-log-λ) denominator))
        x3 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b3) (* b2 k log-b3) (* b4 k log-b3) (* b5 k log-b3) (* b6 k log-b3) (* b7 k log-b3) (* b8 k log-b3) (* c log-b3) (- (* k log-b3)) b1-k-log-b1 b2-k-log-b2 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 c-log-c c-log-k (- (* b1 k-log-p3)) (- (* b2 k-log-p3)) (- (* b4 k-log-p3)) (- (* b5 k-log-p3)) (- (* b6 k-log-p3)) (- (* b7 k-log-p3)) (- (* b8 k-log-p3)) k-log-p3 (- (* c log-p3)) b1-k-log-p1 b2-k-log-p2 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 c-log-s minus-k-log-λ) denominator))
        x4 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b4) (* b2 k log-b4) (* b3 k log-b4) (* b5 k log-b4) (* b6 k log-b4) (* b7 k log-b4) (* b8 k log-b4) (* c log-b4) (- (* k log-b4)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 c-log-c c-log-k (- (* b1 k-log-p4)) (- (* b2 k-log-p4)) (- (* b3 k-log-p4)) (- (* b5 k-log-p4)) (- (* b6 k-log-p4)) (- (* b7 k-log-p4)) (- (* b8 k-log-p4)) k-log-p4 (- (* c log-p4)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 c-log-s minus-k-log-λ) denominator))
        x5 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b5) (* b2 k log-b5) (* b3 k log-b5) (* b4 k log-b5) (* b6 k log-b5) (* b7 k log-b5) (* b8 k log-b5) (* c log-b5) (- (* k log-b5)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 c-log-c c-log-k (- (* b1 k-log-p5)) (- (* b2 k-log-p5)) (- (* b3 k-log-p5)) (- (* b4 k-log-p5)) (- (* b6 k-log-p5)) (- (* b7 k-log-p5)) (- (* b8 k-log-p5)) k-log-p5 (- (* c log-p5)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 c-log-s minus-k-log-λ) denominator))
        x6 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b6) (* b2 k log-b6) (* b3 k log-b6) (* b4 k log-b6) (* b5 k log-b6) (* b7 k log-b6) (* b8 k log-b6) (* c log-b6) (- (* k log-b6)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b7-k-log-b7 b8-k-log-b8 c-log-c c-log-k (- (* b1 k-log-p6)) (- (* b2 k-log-p6)) (- (* b3 k-log-p6)) (- (* b4 k-log-p6)) (- (* b5 k-log-p6)) (- (* b7 k-log-p6)) (- (* b8 k-log-p6)) k-log-p6 (- (* c log-p6)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b7-k-log-p7 b8-k-log-p8 c-log-s minus-k-log-λ) denominator))
        x7 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b7) (* b2 k log-b7) (* b3 k log-b7) (* b4 k log-b7) (* b5 k log-b7) (* b6 k log-b7) (* b8 k log-b7) (* c log-b7) (- (* k log-b7)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b8-k-log-b8 c-log-c c-log-k (- (* b1 k-log-p7)) (- (* b2 k-log-p7)) (- (* b3 k-log-p7)) (- (* b4 k-log-p7)) (- (* b5 k-log-p7)) (- (* b6 k-log-p7)) (- (* b8 k-log-p7)) k-log-p7 (- (* c log-p7)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b8-k-log-p8 c-log-s minus-k-log-λ) denominator))
        x8 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b8) (* b2 k log-b8) (* b3 k log-b8) (* b4 k log-b8) (* b5 k log-b8) (* b6 k log-b8) (* b7 k log-b8) (* c log-b8) (- (* k log-b8)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 c-log-c c-log-k (- (* b1 k-log-p8)) (- (* b2 k-log-p8)) (- (* b3 k-log-p8)) (- (* b4 k-log-p8)) (- (* b5 k-log-p8)) (- (* b6 k-log-p8)) (- (* b7 k-log-p8)) k-log-p8 (- (* c log-p8)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 c-log-s minus-k-log-λ) denominator))
        effort (Math/pow Math/E (/ (+ (- (* log-a)) (- (* b1 log-b1)) (- (* b2 log-b2)) (- (* b3 log-b3)) (- (* b4 log-b4)) (- (* b5 log-b5)) (- (* b6 log-b6)) (- (* b7 log-b7)) (- (* b8 log-b8)) (- (* log-c)) (* b1 log-c) (* b2 log-c) (* b3 log-c) (* b4 log-c) (* b5 log-c) (* b6 log-c) (* b7 log-c) (* b8 log-c) (* log-k) (* b1 log-k) (* b2 log-k) (* b3 log-k) (* b4 log-k) (* b5 log-k) (* b6 log-k) (* b7 log-k) (* b8 log-k) (* b1 log-p1) (* b2 log-p2) (* b3 log-p3) (* b4 log-p4) (* b5 log-p5) (* b6 log-p6) (* b7 log-p7) (* b8 log-p8) (* log-s) (* b1 log-s) (* b2 log-s) (* b3 log-s) (* b4 log-s) (* b5 log-s) (* b6 log-s) (* b7 log-s) (* b8 log-s) (- log-λ)) denominator))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5 x6 x7 x8] include-pollutants?)]
    {:wc-id wc-id
     :output output
     :effort effort
     :intermediate-inputs intermediate-input-qs
     :nature nature-qs
     :labor labor-qs
     :pollutants pollutant-qs}))

(defn solution-9 [{:keys [a s c k ps b λ p-i include-pollutants? wc-id]}]
  (let [[b1 b2 b3 b4 b5 b6 b7 b8 b9] (flatten b)
        [p1 p2 p3 p4 p5 p6 p7 p8 p9] (flatten ps)
        log-a (Math/log a)
        log-b1 (Math/log b1)
        log-b2 (Math/log b2)
        log-b3 (Math/log b3)
        log-b4 (Math/log b4)
        log-b5 (Math/log b5)
        log-b6 (Math/log b6)
        log-b7 (Math/log b7)
        log-b8 (Math/log b8)
        log-b9 (Math/log b9)
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
        log-p9 (Math/log p9)
        log-λ (Math/log λ)
        denominator (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7) (* k b8) (* k b9))
        k-log-a (- (* k log-a))
        b1-k-log-b1 (- (* b1 k log-b1))
        b2-k-log-b2 (- (* b2 k log-b2))
        b3-k-log-b3 (- (* b3 k log-b3))
        b4-k-log-b4 (- (* b4 k log-b4))
        b5-k-log-b5 (- (* b5 k log-b5))
        b6-k-log-b6 (- (* b6 k log-b6))
        b7-k-log-b7 (- (* b7 k log-b7))
        b8-k-log-b8 (- (* b8 k log-b8))
        b9-k-log-b9 (- (* b9 k log-b9))
        b1-k-log-p1 (* b1 k log-p1)
        b2-k-log-p2 (* b2 k log-p2)
        b3-k-log-p3 (* b3 k log-p3)
        b4-k-log-p4 (* b4 k log-p4)
        b5-k-log-p5 (* b5 k log-p5)
        b6-k-log-p6 (* b6 k log-p6)
        b7-k-log-p7 (* b7 k log-p7)
        b8-k-log-p8 (* b8 k log-p8)
        b9-k-log-p9 (* b9 k log-p9)
        c-log-c (- (* c log-c))
        c-log-k (* c log-k)
        c-log-s (* c log-s)
        minus-k-log-λ (- (* k log-λ))
        k-log-λ (* k log-λ)
        k-log-p1 (* k log-p1)
        k-log-p2 (* k log-p2)
        k-log-p3 (* k log-p3)
        k-log-p4 (* k log-p4)
        k-log-p5 (* k log-p5)
        k-log-p6 (* k log-p6)
        k-log-p7 (* k log-p7)
        k-log-p8 (* k log-p8)
        k-log-p9 (* k log-p9)
        output (Math/pow Math/E (/ (+ k-log-a c-log-k c-log-s (- (* c log-λ)) c-log-c b1-k-log-b1 b1-k-log-p1 (- (* b1 k log-λ)) b2-k-log-b2 b2-k-log-p2 (- (* b2 k log-λ)) b3-k-log-b3 b3-k-log-p3 (- (* b3 k log-λ)) b4-k-log-b4 b4-k-log-p4 (- (* b4 k log-λ)) b5-k-log-b5 b5-k-log-p5 (- (* b5 k log-λ)) b6-k-log-b6 b6-k-log-p6 (- (* b6 k log-λ)) b7-k-log-b7 b7-k-log-p7 (- (* b7 k log-λ)) b8-k-log-b8 b8-k-log-p8 (- (* b8 k log-λ)) b9-k-log-b9 b9-k-log-p9 (- (* b9 k log-λ))) denominator))
        x1 (Math/pow Math/E (/ (+ k-log-a (* b2 k log-b1) (* b3 k log-b1) (* b4 k log-b1) (* b5 k log-b1) (* b6 k log-b1) (* b7 k log-b1) (* b8 k log-b1) (* b9 k log-b1) (* c log-b1) (- (* k log-b1)) b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k (- (* b2 k-log-p1)) (- (* b3 k-log-p1)) (- (* b4 k-log-p1)) (- (* b5 k-log-p1)) (- (* b6 k-log-p1)) (- (* b7 k-log-p1)) (- (* b8 k-log-p1)) (- (* b9 k-log-p1)) k-log-p1 (- (* c log-p1)) b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s minus-k-log-λ) denominator))
        x2 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b2) (* b3 k log-b2) (* b4 k log-b2) (* b5 k log-b2) (* b6 k log-b2) (* b7 k log-b2) (* b8 k log-b2) (* b9 k log-b2) (* c log-b2) (- (* k log-b2)) b1-k-log-b1 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k (- (* b1 k-log-p2)) (- (* b3 k-log-p2)) (- (* b4 k-log-p2)) (- (* b5 k-log-p2)) (- (* b6 k-log-p2)) (- (* b7 k-log-p2)) (- (* b8 k-log-p2)) (- (* b9 k-log-p2)) (* k-log-p2) (- (* c log-p2)) b1-k-log-p1 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s minus-k-log-λ) denominator))
        x3 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b3) (* b2 k log-b3) (* b4 k log-b3) (* b5 k log-b3) (* b6 k log-b3) (* b7 k log-b3) (* b8 k log-b3) (* b9 k log-b3) (* c log-b3) (- (* k log-b3)) b1-k-log-b1 b2-k-log-b2 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k (- (* b1 k-log-p3)) (- (* b2 k-log-p3)) (- (* b4 k-log-p3)) (- (* b5 k-log-p3)) (- (* b6 k-log-p3)) (- (* b7 k-log-p3)) (- (* b8 k-log-p3)) (- (* b9 k-log-p3)) (* k-log-p3) (- (* c log-p3)) b1-k-log-p1 b2-k-log-p2 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s minus-k-log-λ) denominator))
        x4 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b4) (* b2 k log-b4) (* b3 k log-b4) (* b5 k log-b4) (* b6 k log-b4) (* b7 k log-b4) (* b8 k log-b4) (* b9 k log-b4) (* c log-b4) (- (* k log-b4)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k (- (* b1 k-log-p4)) (- (* b2 k-log-p4)) (- (* b3 k-log-p4)) (- (* b5 k-log-p4)) (- (* b6 k-log-p4)) (- (* b7 k-log-p4)) (- (* b8 k-log-p4)) (- (* b9 k-log-p4)) (* k-log-p4) (- (* c log-p4)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s minus-k-log-λ) denominator))
        x5 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b5) (* b2 k log-b5) (* b3 k log-b5) (* b4 k log-b5) (* b6 k log-b5) (* b7 k log-b5) (* b8 k log-b5) (* b9 k log-b5) (* c log-b5) (- (* k log-b5)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k (- (* b1 k-log-p5)) (- (* b2 k-log-p5)) (- (* b3 k-log-p5)) (- (* b4 k-log-p5)) (- (* b6 k-log-p5)) (- (* b7 k-log-p5)) (- (* b8 k-log-p5)) (- (* b9 k-log-p5)) (* k-log-p5) (- (* c log-p5)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s minus-k-log-λ) denominator))
        x6 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b6) (* b2 k log-b6) (* b3 k log-b6) (* b4 k log-b6) (* b5 k log-b6) (* b7 k log-b6) (* b8 k log-b6) (* b9 k log-b6) (* c log-b6) (- (* k log-b6)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k (- (* b1 k-log-p6)) (- (* b2 k-log-p6)) (- (* b3 k-log-p6)) (- (* b4 k-log-p6)) (- (* b5 k-log-p6)) (- (* b7 k-log-p6)) (- (* b8 k-log-p6)) (- (* b9 k-log-p6)) (* k-log-p6) (- (* c log-p6)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s minus-k-log-λ) denominator))
        x7 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b7) (* b2 k log-b7) (* b3 k log-b7) (* b4 k log-b7) (* b5 k log-b7) (* b6 k log-b7) (* b8 k log-b7) (* b9 k log-b7) (* c log-b7) (- (* k log-b7)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k (- (* b1 k-log-p7)) (- (* b2 k-log-p7)) (- (* b3 k-log-p7)) (- (* b4 k-log-p7)) (- (* b5 k-log-p7)) (- (* b6 k-log-p7)) (- (* b8 k-log-p7)) (- (* b9 k-log-p7)) (* k-log-p7) (- (* c log-p7)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b8-k-log-p8 b9-k-log-p9 c-log-s minus-k-log-λ) denominator))
        x8 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b8) (* b2 k log-b8) (* b3 k log-b8) (* b4 k log-b8) (* b5 k log-b8) (* b6 k log-b8) (* b7 k log-b8) (* b9 k log-b8) (* c log-b8) (- (* k log-b8)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b9-k-log-b9 c-log-c c-log-k (- (* b1 k-log-p8)) (- (* b2 k-log-p8)) (- (* b3 k-log-p8)) (- (* b4 k-log-p8)) (- (* b5 k-log-p8)) (- (* b6 k-log-p8)) (- (* b7 k-log-p8)) (- (* b9 k-log-p8)) (* k-log-p8) (- (* c log-p8)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b9-k-log-p9 c-log-s minus-k-log-λ) denominator))
        x9 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b9) (* b2 k log-b9) (* b3 k log-b9) (* b4 k log-b9) (* b5 k log-b9) (* b6 k log-b9) (* b7 k log-b9) (* b8 k log-b9) (* c log-b9) (- (* k log-b9)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 c-log-c c-log-k (- (* b1 k-log-p9)) (- (* b2 k-log-p9)) (- (* b3 k-log-p9)) (- (* b4 k-log-p9)) (- (* b5 k-log-p9)) (- (* b6 k-log-p9)) (- (* b7 k-log-p9)) (- (* b8 k-log-p9)) (* k-log-p9) (- (* c log-p9)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 c-log-s minus-k-log-λ) denominator)) 
        effort (Math/pow Math/E (/ (+ (- (* log-a)) (- (* b1 log-b1)) (- (* b2 log-b2)) (- (* b3 log-b3)) (- (* b4 log-b4)) (- (* b5 log-b5)) (- (* b6 log-b6)) (- (* b7 log-b7)) (- (* b8 log-b8)) (- (* b9 log-b9)) (* b1 log-p1) (* b2 log-p2) (* b3 log-p3) (* b4 log-p4) (* b5 log-p5) (* b6 log-p6) (* b7 log-p7) (* b8 log-p8) (* b9 log-p9) (- (* b1 log-λ)) (- (* b2 log-λ)) (- (* b3 log-λ)) (- (* b4 log-λ)) (- (* b5 log-λ)) (- (* b6 log-λ)) (- (* b7 log-λ)) (- (* b8 log-λ)) (- (* b9 log-λ)) (/ (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)) (- (* b8 k log-λ)) (- (* b9 k log-λ))) denominator) (- (/ (* b1 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)) (- (* b8 k log-λ)) (- (* b9 k log-λ)))) denominator)) (- (/ (* b2 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)) (- (* b8 k log-λ)) (- (* b9 k log-λ)))) denominator)) (- (/ (* b3 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)) (- (* b8 k log-λ)) (- (* b9 k log-λ)))) denominator)) (- (/ (* b4 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)) (- (* b8 k log-λ)) (- (* b9 k log-λ)))) denominator)) (- (/ (* b5 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)) (- (* b8 k log-λ)) (- (* b9 k log-λ)))) denominator)) (- (/ (* b6 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)) (- (* b8 k log-λ)) (- (* b9 k log-λ)))) denominator)) (- (/ (* b7 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)) (- (* b8 k log-λ)) (- (* b9 k log-λ)))) denominator)) (- (/ (* b8 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)) (- (* b8 k log-λ)) (- (* b9 k log-λ)))) denominator)) (- (/ (* b9 (+ k-log-a b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s (- (* c log-λ)) (- (* b1 k log-λ)) (- (* b2 k log-λ)) (- (* b3 k log-λ)) (- (* b4 k log-λ)) (- (* b5 k log-λ)) (- (* b6 k log-λ)) (- (* b7 k log-λ)) (- (* b8 k log-λ)) (- (* b9 k log-λ)))) denominator))) c))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5 x6 x7 x8 x9] include-pollutants?)]
    {:wc-id wc-id
     :output output
     :effort effort
     :intermediate-inputs intermediate-input-qs
     :nature nature-qs
     :labor labor-qs
     :pollutants pollutant-qs}))

(defn solution-10 [{:keys [a s c k ps b λ p-i include-pollutants? wc-id]}]
  (let [[b1 b2 b3 b4 b5 b6 b7 b8 b9 b10] (flatten b)
        [p1 p2 p3 p4 p5 p6 p7 p8 p9 p10] (flatten ps)
        log-a (Math/log a)
        log-b1 (Math/log b1)
        log-b2 (Math/log b2)
        log-b3 (Math/log b3)
        log-b4 (Math/log b4)
        log-b5 (Math/log b5)
        log-b6 (Math/log b6)
        log-b7 (Math/log b7)
        log-b8 (Math/log b8)
        log-b9 (Math/log b9)
        log-b10 (Math/log b10)
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
        log-p9 (Math/log p9)
        log-p10 (Math/log p10)
        log-λ (Math/log λ)
        denominator (+ c (- k) (* k b1) (* k b2) (* k b3) (* k b4) (* k b5) (* k b6) (* k b7) (* k b8) (* k b9) (* k b10))
        k-log-a (- (* k log-a))
        b1-k-log-b1 (- (* b1 k log-b1))
        b2-k-log-b2 (- (* b2 k log-b2))
        b3-k-log-b3 (- (* b3 k log-b3))
        b4-k-log-b4 (- (* b4 k log-b4))
        b5-k-log-b5 (- (* b5 k log-b5))
        b6-k-log-b6 (- (* b6 k log-b6))
        b7-k-log-b7 (- (* b7 k log-b7))
        b8-k-log-b8 (- (* b8 k log-b8))
        b9-k-log-b9 (- (* b9 k log-b9))
        b10-k-log-b10 (- (* b10 k log-b10))
        b1-k-log-p1 (* b1 k log-p1)
        b2-k-log-p2 (* b2 k log-p2)
        b3-k-log-p3 (* b3 k log-p3)
        b4-k-log-p4 (* b4 k log-p4)
        b5-k-log-p5 (* b5 k log-p5)
        b6-k-log-p6 (* b6 k log-p6)
        b7-k-log-p7 (* b7 k log-p7)
        b8-k-log-p8 (* b8 k log-p8)
        b9-k-log-p9 (* b9 k log-p9)
        b10-k-log-p10 (* b10 k log-p10)
        c-log-c (- (* c log-c))
        c-log-k (* c log-k)
        c-log-s (* c log-s)
        minus-k-log-λ (- (* k log-λ))
        k-log-λ (* k log-λ)
        output (Math/pow Math/E (/ (+ k-log-a c-log-k c-log-s (- (* c log-λ)) c-log-c b1-k-log-b1 b1-k-log-p1 (- (* b1 k log-λ)) b2-k-log-b2 b2-k-log-p2 (- (* b2 k log-λ)) b3-k-log-b3 b3-k-log-p3 (- (* b3 k log-λ)) b4-k-log-b4 b4-k-log-p4 (- (* b4 k log-λ)) b5-k-log-b5 b5-k-log-p5 (- (* b5 k log-λ)) b6-k-log-b6 b6-k-log-p6 (- (* b6 k log-λ)) b7-k-log-b7 b7-k-log-p7 (- (* b7 k log-λ)) b8-k-log-b8 b8-k-log-p8 (- (* b8 k log-λ)) b9-k-log-b9 b9-k-log-p9 (- (* b9 k log-λ)) b10-k-log-b10 b10-k-log-p10 (- (* b10 k log-λ))) denominator))
        x1 (Math/pow Math/E (/ (+ k-log-a (* b2 k log-b1) (* b3 k log-b1) (* b4 k log-b1) (* b5 k log-b1) (* b6 k log-b1) (* b7 k log-b1) (* b8 k log-b1) (* b9 k log-b1) (* b10 k log-b1) (* c log-b1) (- (* k log-b1)) b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 b10-k-log-b10 c-log-c c-log-k (- (* b2 k log-p1)) (- (* b3 k log-p1)) (- (* b4 k log-p1)) (- (* b5 k log-p1)) (- (* b6 k log-p1)) (- (* b7 k log-p1)) (- (* b8 k log-p1)) (- (* b9 k log-p1)) (- (* b10 k log-p1)) (* k log-p1) (- (* c log-p1)) b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 b10-k-log-p10 c-log-s minus-k-log-λ) denominator))
        x2 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b2) (* b3 k log-b2) (* b4 k log-b2) (* b5 k log-b2) (* b6 k log-b2) (* b7 k log-b2) (* b8 k log-b2) (* b9 k log-b2) (* b10 k log-b2) (* c log-b2) (- (* k log-b2)) b1-k-log-b1 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 b10-k-log-b10 c-log-c c-log-k (- (* b1 k log-p2)) (- (* b3 k log-p2)) (- (* b4 k log-p2)) (- (* b5 k log-p2)) (- (* b6 k log-p2)) (- (* b7 k log-p2)) (- (* b8 k log-p2)) (- (* b9 k log-p2)) (- (* b10 k log-p2)) (* k log-p2) (- (* c log-p2)) b1-k-log-p1 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 b10-k-log-p10 c-log-s minus-k-log-λ) denominator))
        x3 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b3) (* b2 k log-b3) (* b4 k log-b3) (* b5 k log-b3) (* b6 k log-b3) (* b7 k log-b3) (* b8 k log-b3) (* b9 k log-b3) (* b10 k log-b3) (* c log-b3) (- (* k log-b3)) b1-k-log-b1 b2-k-log-b2 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 b10-k-log-b10 c-log-c c-log-k (- (* b1 k log-p3)) (- (* b2 k log-p3)) (- (* b4 k log-p3)) (- (* b5 k log-p3)) (- (* b6 k log-p3)) (- (* b7 k log-p3)) (- (* b8 k log-p3)) (- (* b9 k log-p3)) (- (* b10 k log-p3)) (* k log-p3) (- (* c log-p3)) b1-k-log-p1 b2-k-log-p2 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 b10-k-log-p10 c-log-s minus-k-log-λ) denominator))
        x4 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b4) (* b2 k log-b4) (* b3 k log-b4) (* b5 k log-b4) (* b6 k log-b4) (* b7 k log-b4) (* b8 k log-b4) (* b9 k log-b4) (* b10 k log-b4) (* c log-b4) (- (* k log-b4)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 b10-k-log-b10 c-log-c c-log-k (- (* b1 k log-p4)) (- (* b2 k log-p4)) (- (* b3 k log-p4)) (- (* b5 k log-p4)) (- (* b6 k log-p4)) (- (* b7 k log-p4)) (- (* b8 k log-p4)) (- (* b9 k log-p4)) (- (* b10 k log-p4)) (* k log-p4) (- (* c log-p4)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 b10-k-log-p10 c-log-s minus-k-log-λ) denominator))
        x5 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b5) (* b2 k log-b5) (* b3 k log-b5) (* b4 k log-b5) (* b6 k log-b5) (* b7 k log-b5) (* b8 k log-b5) (* b9 k log-b5) (* b10 k log-b5) (* c log-b5) (- (* k log-b5)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 b10-k-log-b10 c-log-c c-log-k (- (* b1 k log-p5)) (- (* b2 k log-p5)) (- (* b3 k log-p5)) (- (* b4 k log-p5)) (- (* b6 k log-p5)) (- (* b7 k log-p5)) (- (* b8 k log-p5)) (- (* b9 k log-p5)) (- (* b10 k log-p5)) (* k log-p5) (- (* c log-p5)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 b10-k-log-p10 c-log-s minus-k-log-λ) denominator))
        x6 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b6) (* b2 k log-b6) (* b3 k log-b6) (* b4 k log-b6) (* b5 k log-b6) (* b7 k log-b6) (* b8 k log-b6) (* b9 k log-b6) (* b10 k log-b6) (* c log-b6) (- (* k log-b6)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 b10-k-log-b10 c-log-c c-log-k (- (* b1 k log-p6)) (- (* b2 k log-p6)) (- (* b3 k log-p6)) (- (* b4 k log-p6)) (- (* b5 k log-p6)) (- (* b7 k log-p6)) (- (* b8 k log-p6)) (- (* b9 k log-p6)) (- (* b10 k log-p6)) (* k log-p6) (- (* c log-p6)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 b10-k-log-p10 c-log-s minus-k-log-λ) denominator))
        x7 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b7) (* b2 k log-b7) (* b3 k log-b7) (* b4 k log-b7) (* b5 k log-b7) (* b6 k log-b7) (* b8 k log-b7) (* b9 k log-b7) (* b10 k log-b7) (* c log-b7) (- (* k log-b7)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b8-k-log-b8 b9-k-log-b9 b10-k-log-b10 c-log-c c-log-k (- (* b1 k log-p7)) (- (* b2 k log-p7)) (- (* b3 k log-p7)) (- (* b4 k log-p7)) (- (* b5 k log-p7)) (- (* b6 k log-p7)) (- (* b8 k log-p7)) (- (* b9 k log-p7)) (- (* b10 k log-p7)) (* k log-p7) (- (* c log-p7)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b8-k-log-p8 b9-k-log-p9 b10-k-log-p10 c-log-s minus-k-log-λ) denominator))
        x8 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b8) (* b2 k log-b8) (* b3 k log-b8) (* b4 k log-b8) (* b5 k log-b8) (* b6 k log-b8) (* b7 k log-b8) (* b9 k log-b8) (* b10 k log-b8) (* c log-b8) (- (* k log-b8)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b9-k-log-b9 b10-k-log-b10 c-log-c c-log-k (- (* b1 k log-p8)) (- (* b2 k log-p8)) (- (* b3 k log-p8)) (- (* b4 k log-p8)) (- (* b5 k log-p8)) (- (* b6 k log-p8)) (- (* b7 k log-p8)) (- (* b9 k log-p8)) (- (* b10 k log-p8)) (* k log-p8) (- (* c log-p8)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b9-k-log-p9 b10-k-log-p10 c-log-s minus-k-log-λ) denominator))
        x9 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b9) (* b2 k log-b9) (* b3 k log-b9) (* b4 k log-b9) (* b5 k log-b9) (* b6 k log-b9) (* b7 k log-b9) (* b8 k log-b9) (* b10 k log-b9) (* c log-b9) (- (* k log-b9)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b10-k-log-b10 c-log-c c-log-k (- (* b1 k log-p9)) (- (* b2 k log-p9)) (- (* b3 k log-p9)) (- (* b4 k log-p9)) (- (* b5 k log-p9)) (- (* b6 k log-p9)) (- (* b7 k log-p9)) (- (* b8 k log-p9)) (- (* b10 k log-p9)) (* k log-p9) (- (* c log-p9)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b10-k-log-p10 c-log-s minus-k-log-λ) denominator))
        x10 (Math/pow Math/E (/ (+ k-log-a (* b1 k log-b10) (* b2 k log-b10) (* b3 k log-b10) (* b4 k log-b10) (* b5 k log-b10) (* b6 k log-b10) (* b7 k log-b10) (* b8 k log-b10) (* b9 k log-b10) (* c log-b10) (- (* k log-b10)) b1-k-log-b1 b2-k-log-b2 b3-k-log-b3 b4-k-log-b4 b5-k-log-b5 b6-k-log-b6 b7-k-log-b7 b8-k-log-b8 b9-k-log-b9 c-log-c c-log-k (- (* b1 k log-p10)) (- (* b2 k log-p10)) (- (* b3 k log-p10)) (- (* b4 k log-p10)) (- (* b5 k log-p10)) (- (* b6 k log-p10)) (- (* b7 k log-p10)) (- (* b8 k log-p10)) (- (* b9 k log-p10)) (* k log-p10) (- (* c log-p10)) b1-k-log-p1 b2-k-log-p2 b3-k-log-p3 b4-k-log-p4 b5-k-log-p5 b6-k-log-p6 b7-k-log-p7 b8-k-log-p8 b9-k-log-p9 c-log-s minus-k-log-λ) denominator))
        effort (Math/pow Math/E (/ (+ (- log-a) (- (* b1 log-b1)) (- (* b2 log-b2)) (- (* b3 log-b3)) (- (* b4 log-b4)) (- (* b5 log-b5)) (- (* b6 log-b6)) (- (* b7 log-b7)) (- (* b8 log-b8)) (- (* b9 log-b9)) (- (* b10 log-b10)) (- (* log-c)) (* b1 log-c) (* b2 log-c) (* b3 log-c) (* b4 log-c) (* b5 log-c) (* b6 log-c) (* b7 log-c) (* b8 log-c) (* b9 log-c) (* b10 log-c) (* log-k) (* b1 log-k) (* b2 log-k) (* b3 log-k) (* b4 log-k) (* b5 log-k) (* b6 log-k) (* b7 log-k) (* b8 log-k) (* b9 log-k) (* b10 log-k) (* b1 log-p1) (* b2 log-p2) (* b3 log-p3) (* b4 log-p4) (* b5 log-p5) (* b6 log-p6) (* b7 log-p7) (* b8 log-p8) (* b9 log-p9) (* b10 log-p10) (* log-s) (* b1 log-s) (* b2 log-s) (* b3 log-s) (* b4 log-s) (* b5 log-s) (* b6 log-s) (* b7 log-s) (* b8 log-s) (* b9 log-s) (* b10 log-s) (- log-λ)) denominator))
        [intermediate-input-qs nature-qs labor-qs pollutant-qs] (allot-production-quantities p-i [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10] include-pollutants?)]
    {:wc-id wc-id
     :output output
     :effort effort
     :intermediate-inputs intermediate-input-qs
     :nature nature-qs
     :labor labor-qs
     :pollutants pollutant-qs}))

#_(defn get-prices [ds ids table-name]          
  (let [placeholders (clojure.string/join "," (repeat (count ids) "?"))
        sql (str "select price from " table-name
                 " where id in (" placeholders ") "
                 "order by id")]
    (->> {:builder-fn result-set/as-unqualified-lower-maps}
         (jdbc/execute! ds (into [sql] ids))
         (mapv :price))))

(defn get-lambda-o [wc private-good-prices intermediate-input-prices public-good-prices]
  (let [industry (:industry wc)
        product (:product wc)]
    (cond (= 0 industry) (get private-good-prices product) 
          (= 1 industry) (get intermediate-input-prices product)
          (= 2 industry) (get public-good-prices product))))

(defn process-wc-cats [[objs wc-id]]
  (let [c (count objs)]
    (partition 3 (interleave objs 
                             (range 1 (inc c)) 
                             (repeat c wc-id)))))

(defn process-wc-db-calls [data include-pollutants?]
  (let [ds (jdbc/get-datasource {:dbtype "sqlite" :dbname "pequod-csv-test.db"})
        wc-updates (mapv (juxt :output :effort :wc-id) data)
        ii-updates (apply concat (mapv process-wc-cats (map (juxt :intermediate-inputs :wc-id) data)))
        nature-updates (apply concat (mapv process-wc-cats (map (juxt :nature :wc-id) data)))
        labor-updates (apply concat (mapv process-wc-cats (map (juxt :labor :wc-id) data)))
        pollutant-updates (when include-pollutants? (apply concat (mapv process-wc-cats (map (juxt :pollutants :wc-id) data))))]
    (jdbc/with-transaction [tx ds]
      (jdbc/execute-batch! tx "update wcs set output = ?, effort = ? where id = ?" wc-updates {})
      (jdbc/execute-batch! tx "update intermediate_inputs set quantity = ? where intermediate_input_id = ? and wc_id = ?" ii-updates {})
      (jdbc/execute-batch! tx "update nature set quantity = ? where nature_id = ? and wc_id = ?" nature-updates {})
      (jdbc/execute-batch! tx "update labor set quantity = ? where labor_id = ? and wc_id = ?" labor-updates {})
      (when include-pollutants?
        (jdbc/execute-batch! tx "update pollutant_demands set quantity = ? where pollutant_id = ? and wc_id = ?" pollutant-updates {})))))

(defn process-wc [include-pollutants? intermediate-inputs-by-wc nature-by-wc labor-by-wc pollutant-demands-by-wc intermediate-input-prices labor-prices nature-prices pollutant-prices private-good-prices public-good-prices wc]
  (let [wc-id (:id wc)
        intermediate-inputs (get intermediate-inputs-by-wc wc-id [])
        nature (get nature-by-wc wc-id [])
        labor (get labor-by-wc wc-id [])
        pollutant-demands (if include-pollutants?
                            (get pollutant-demands-by-wc wc-id [])
                            [])
        all-goods (if include-pollutants?
                    (concat intermediate-inputs nature labor pollutant-demands)
                    (concat intermediate-inputs nature labor))
        input-count-r (count all-goods)
                                        ; p-i (mapv :coefficient all-goods)
        p-i (map count (vector intermediate-inputs nature labor pollutant-demands))
        b (mapv :exponent all-goods)
        intermediate-input-prices-to-use (map #(get intermediate-input-prices %) (map :coefficient intermediate-inputs))
        nature-prices-to-use (map #(get nature-prices %) (map :coefficient nature))
        labor-prices-to-use (map #(get labor-prices %) (map :coefficient labor))
        pollutant-prices-to-use (map #(get pollutant-prices %) (map :coefficient pollutant-demands))
        ps (if include-pollutants?
             [intermediate-input-prices-to-use nature-prices-to-use labor-prices-to-use pollutant-prices-to-use]
             [intermediate-input-prices-to-use nature-prices-to-use labor-prices-to-use])
        λ (get-lambda-o wc private-good-prices intermediate-input-prices public-good-prices)
        solution-input (hash-map :a (:total_factor_productivity wc)
                                 :s (:disutility_of_effort_coefficient wc)
                                 :c (:effort_elasticity wc)
                                 :k (:disutility_of_effort_exponent wc)
                                 :ps ps
                                 :b b
                                 :λ λ
                                 :p-i p-i
                                 :include-pollutants? include-pollutants?
                                 :wc-id wc-id)
        solution (condp = input-count-r
                   3 (solution-3 solution-input)
                   4 (solution-4 solution-input)
                   5 (solution-5 solution-input)
                   6 (solution-6 solution-input)
                   7 (solution-7 solution-input)
                   8 (solution-8 solution-input)
                   9 (solution-9 solution-input)
                   10 (solution-10 solution-input)
                   (str "unexpected input-count value: " input-count-r))]
    solution))

(defn proposal-db [include-pollutants?]
  (let [ds (jdbc/get-datasource {:dbtype "sqlite" :dbname "pequod-csv-test.db"})
        builder-fn-map {:builder-fn result-set/as-unqualified-lower-maps}
        wcs (jdbc/execute! ds
              ["select id, industry, product, total_factor_productivity, effort_elasticity, 
                disutility_of_effort_coefficient, disutility_of_effort_exponent
                from wcs order by id"] builder-fn-map)
        intermediate-inputs-by-wc (group-by :wc_id (jdbc/execute! ds ["select * from intermediate_inputs"] builder-fn-map))
        nature-by-wc (group-by :wc_id (jdbc/execute! ds ["select * from nature"] builder-fn-map))
        labor-by-wc (group-by :wc_id (jdbc/execute! ds ["select * from labor"] builder-fn-map))
        pollutant-demands-by-wc (when include-pollutants? (group-by :wc_id (jdbc/execute! ds ["select * from pollutant_demands"] builder-fn-map)))
        intermediate-input-prices (into {} (map (juxt :id :price) (jdbc/execute! ds ["select id, price from intermediate_input_prices"] builder-fn-map)))
        labor-prices (into {} (map (juxt :id :price) (jdbc/execute! ds ["select id, price from labor_prices"] builder-fn-map)))
        nature-prices (into {} (map (juxt :id :price) (jdbc/execute! ds ["select id, price from nature_prices"] builder-fn-map)))
        pollutant-prices (into {} (map (juxt :id :price) (jdbc/execute! ds ["select id, price from pollutant_prices"] builder-fn-map)))
        private-good-prices (into {} (map (juxt :id :price) (jdbc/execute! ds ["select id, price from private_good_prices"] builder-fn-map)))
        public-good-prices (into {} (map (juxt :id :price) (jdbc/execute! ds ["select id, price from public_good_prices"] builder-fn-map)))
        ]
    (mapv (partial process-wc include-pollutants? intermediate-inputs-by-wc nature-by-wc labor-by-wc pollutant-demands-by-wc intermediate-input-prices labor-prices nature-prices pollutant-prices private-good-prices public-good-prices) wcs)))

#_(defn proposal-db-take1 [include-pollutants?]
  (let [ds (jdbc/get-datasource {:dbtype "sqlite" :dbname "pequod-csv-test.db"})
        builder-fn-map {:builder-fn result-set/as-unqualified-lower-maps}
        wc-ids (mapv :id (jdbc/execute! ds ["select distinct id from wcs order by id" ] builder-fn-map))]
    (loop [w wc-ids]
      (if (empty? w)
        (println "PROPOSAL-DB/finished!")
        (let [wc-id (first w)
              wc (jdbc/execute-one! ds ["select industry, product, total_factor_productivity, effort_elasticity, disutility_of_effort_coefficient, disutility_of_effort_exponent from wcs where id = ?" wc-id] builder-fn-map)
              intermediate-inputs (jdbc/execute! ds ["select * from intermediate_inputs where wc_id = ?" wc-id] builder-fn-map)
              nature (jdbc/execute! ds ["select * from nature where wc_id = ?" wc-id] builder-fn-map)
              labor (jdbc/execute! ds ["select * from labor where wc_id = ?" wc-id] builder-fn-map)
              pollutant-demands (jdbc/execute! ds ["select * from pollutant_demands where wc_id = ?" wc-id] builder-fn-map)
              total-factor-productivity (get wc :total_factor_productivity)
              effort-elasticity (get wc :effort_elasticity)
              disutility-of-effort-coefficient (get wc :disutility_of_effort_coefficient)
o              disutility-of-effort-exponent (get wc :disutility_of_effort_exponent)
              all-goods (if include-pollutants?
                          (concat intermediate-inputs nature labor pollutant-demands)
                          (concat intermediate-inputs nature labor))
              input-count-r (count all-goods)
              p-i (mapv :coefficent all-goods)
              intermediate-input-prices-to-use (get-prices ds (map :coefficient intermediate-inputs) "intermediate_input_prices")
              nature-prices-to-use (get-prices ds (map :coefficient nature) "nature_prices")
              labor-prices-to-use (get-prices ds (map :coefficient labor) "labor_prices")
              pollutant-prices-to-use (get-prices ds [1] "pollutant_prices")
              ps (if include-pollutants?
                   [intermediate-input-prices-to-use nature-prices-to-use labor-prices-to-use pollutant-prices-to-use]
                   [intermediate-input-prices-to-use nature-prices-to-use labor-prices-to-use])
              b (mapv :exponent all-goods)
              λ (get-lambda-o ds wc)]
          (condp = input-count-r
            3 (solution-db-3 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants? wc-id ds)
            4 (solution-db-4 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants? wc-id ds)
            5 (solution-db-5 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants? wc-id ds)
            6 (solution-db-6 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants? wc-id ds)
            7 (solution-db-7 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants? wc-id ds)
            8 (solution-db-8 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants? wc-id ds)
            9 (solution-db-9 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants? wc-id ds)
            10 (solution-db-10 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants? wc-id ds)
            (str "unexpected input-count value: " input-count-r))
            (recur (rest w)))))))

(defn get-demand-sum [ds table-name]
  (let [q (case table-name
            :pollutant-permissions "select sum(demand) as s from pollutant_permissions"
            :private-goods "select sum(demand) as s from private_goods"
            :public-goods "select sum(demand) as s from public_goods"
            "dunno")]
    (->> {:builder-fn result-set/as-unqualified-lower-maps}
         (jdbc/execute-one! ds [q])
         :s)))

(defn iterate-plan-improved []
  (let [solutions-to-use (proposal-db include-pollutants?)
        _ (process-wc-db-calls solutions-to-use include-pollutants?)
        _ (util/consume-process-all-in-db ds include-pollutants?)
        pollutants-demand-sum (get-demand-sum ds :pollutant-permissions)
        private-goods-demand-sum (get-demand-sum ds :private-goods)
        public-goods-demand-sum (get-demand-sum ds :public-goods)
        previous-price-delta-data @price-delta-data
        _ (util/update-surpluses-prices-improved ds private-goods-demand-sum public-goods-demand-sum pollutants-demand-sum previous-price-delta-data include-pollutants?)
        new-price-delta-data (util/update-price-deltas-db ds include-pollutants?)
        ; figure out how to include pollutant-prices
        all-price-data (jdbc/execute!
                         ds
                         ["SELECT id, 'intermediate-inputs' as type, price, price_delta, pd, supply, demand, surplus FROM intermediate_input_prices
                           UNION
                           SELECT id, 'private-goods' as type, price, price_delta, pd, supply, demand, surplus FROM private_good_prices
                           UNION
                           SELECT id, 'public-goods' as type, price, price_delta, pd, supply, demand, surplus FROM public_good_prices
                           UNION
                           SELECT id, 'nature' as type, price, price_delta, pd, supply, demand, surplus FROM nature_prices
                           UNION
                           SELECT id, 'labor' as type, price, price_delta, pd, supply, demand, surplus FROM labor_prices
                           UNION
                           SELECT id, 'pollutants' as type, price, price_delta, pd, supply, demand, surplus FROM pollutant_prices
                           "]
                         {:builder-fn result-set/as-unqualified-lower-maps})
        threshold-report (map util/compute-threshold-improved all-price-data)
        threshold-baked (mapv (fn [x] (vector (keyword (first x)) (mapv :threshold (val x)))) (group-by :type threshold-report))
        final-output (create-final-output threshold-baked)]
    (do 
      (swap! iteration-count inc)
      (reset! price-delta-data new-price-delta-data)
      (reset! price-data all-price-data)
      (reset! final-results final-output))))

(defn print-csv [args-to-print data]
  (let [all-args (flatten (mapv (partial get data) args-to-print))]
    (clojure.string/join "|" all-args)))

(defn augmented-reset [t _]
  (assoc t :iteration 0
           :ccs (mapv util/augment-cc (get t :ccs))
           :wcs (mapv util/augment-wc (get t :wcs))))

(defn augmented-reset-improved [t _]
  (do
    (util/augment-cc-in-db (:ds t) (:include-pollutants? t))
    (assoc t :iteration 0
             :wcs (mapv util/augment-wc (get t :wcs)))))

(defn update-aggregated-demand [value-to-use m]
  (->> m
       value-to-use
       (map (fn [x] (get-in x [:demand])))
       flatten
       (reduce +)))

(defn read-stream-ccs [ds resource-path]
  (with-open [r (-> resource-path io/resource io/reader java.io.PushbackReader.)]
    (loop [num-of-ccs 1
           pollutants-demand-sum 0
           private-goods-demand-sum 0
           public-goods-demand-sum 0]
      (let [form (edn/read {:eof ::eof} r)
            _ (if (not= form ::eof) (jdbc/execute! ds ["insert into ccs (id, cc) values (?, ?)" num-of-ccs form]))]
        (if (= form ::eof)
          [num-of-ccs pollutants-demand-sum private-goods-demand-sum public-goods-demand-sum]
          (recur (inc num-of-ccs)
                 (+ pollutants-demand-sum (update-aggregated-demand :pollutant-permissions form))
                 (+ private-goods-demand-sum (update-aggregated-demand :private-goods form))
                 (+ public-goods-demand-sum (update-aggregated-demand :public-goods form))))))))

(defn read-stream-ccs-to-normalized-db [ds resource-path]
  (with-open [r (-> resource-path io/resource io/reader java.io.PushbackReader.)]
    (loop [num-of-ccs 1]
      (let [form (edn/read {:eof ::eof} r)
            cohort-region (get-in form [:cohort :region])
            income (get-in form [:income])
            ; _ (if (zero? (mod num-of-ccs 1000)) (println "ccs: " num-of-ccs))
            positive-utility-from-income (get-in form [:pollutant-utilities :positive-utility-from-income])
            negative-utility-from-exposure (get-in form [:pollutant-utilities :negative-utility-from-exposure])
            private-goods (get-in form [:private-goods])
            public-goods (get-in form [:public-goods])
            pollutant-permissions (get-in form [:pollutant-permissions])
            _ (if (not= form ::eof)
                (do
                  (jdbc/execute! ds ["INSERT into ccs (id, cohort_region, income, positive_utility_from_income, negative_utility_from_exposure) values (?, ?, ?, ?, ?);" num-of-ccs cohort-region income positive-utility-from-income negative-utility-from-exposure])
                  (doseq [e private-goods]
                     (jdbc/execute! ds ["INSERT into private_goods (cc_id, good_id, exponent, augment, demand) values (?, ?, ?, ?, ?);" num-of-ccs (get e :id) (get e :exponent) (get e :augment) (get e :demand)]))
                  (doseq [e public-goods]
                     (jdbc/execute! ds ["INSERT into public_goods (cc_id, good_id, exponent, augment, demand) values (?, ?, ?, ?, ?);" num-of-ccs (get e :id) (get e :exponent) (get e :augment) (get e :demand)]))
                  (doseq [e pollutant-permissions]
                     (jdbc/execute! ds ["INSERT into pollutant_permissions (cc_id, pollutant_id, exponent, augment, demand) values (?, ?, ?, ?, ?);" num-of-ccs (get e :id) (get e :exponent) (get e :augment) (get e :demand)]))
))]
        (if (= form ::eof)
          num-of-ccs
          (recur (inc num-of-ccs)))))))

(defn read-stream-wcs [resource-path]
  (with-open [r (-> resource-path io/resource io/reader java.io.PushbackReader.)]
    (loop [wcs-to-return []]
      (let [form (edn/read {:eof ::eof} r)]
        (if (= form ::eof)
          wcs-to-return
          (recur (conj wcs-to-return form)))))))

(defn create-normalized-ccs-tables [ds]
  (let [ccs-table "CREATE TABLE ccs (
                     id INTEGER PRIMARY KEY,
                     cohort_region INTEGER,
                     income REAL,
                     positive_utility_from_income REAL,
                     negative_utility_from_exposure REAL
                   );"
        private-goods-table "CREATE TABLE private_goods (
                               cc_id INTEGER,
                               good_id INTEGER,
                               exponent REAL,
                               augment REAL,
                               demand REAL,
                               PRIMARY KEY (cc_id, good_id)
                             );"
        public-goods-table "CREATE TABLE public_goods (
                              cc_id INTEGER,
                              good_id INTEGER,
                              exponent REAL,
                              augment REAL,
                              demand REAL,
                              PRIMARY KEY (cc_id, good_id)
                             );"
        pollutant-permissions-table "CREATE TABLE pollutant_permissions (
                                     cc_id INTEGER,
                                     pollutant_id INTEGER,
                                     exponent REAL,
                                     augment REAL,
                                     demand REAL
                                    );"
        private-good-prices-table "CREATE TABLE private_good_prices (
                                     id INTEGER,
                                     price REAL,
                                     price_delta REAL,
                                     price_delta_to_use REAL,
                                     pd REAL,
                                     supply REAL,
                                     demand REAL,
                                     surplus REAL
                                    );"
        public-good-prices-table "CREATE TABLE public_good_prices (
                                     id INTEGER,
                                     price REAL,
                                     price_delta REAL,
                                     price_delta_to_use REAL,
                                     pd REAL,
                                     supply REAL,
                                     demand REAL,
                                     surplus REAL
                                    );"
        pollutant-prices-table "CREATE TABLE pollutant_prices (
                                     id INTEGER,
                                     price REAL,
                                     price_delta REAL,
                                     price_delta_to_use REAL,
                                     pd REAL,
                                     supply REAL,
                                     demand REAL,
                                     surplus REAL
                                    );"
         private-good-prices-id-index "CREATE INDEX id_idx_private_good_prices ON private_good_prices(id)"
         public-good-prices-index "CREATE INDEX idx_public_good_prices ON public_good_prices(id)"
         pollutant-prices-index "CREATE INDEX idx_pollutant_prices ON pollutant_prices(id)"
         private-goods-cc-id-index "CREATE INDEX idx_private_cc_id ON private_goods(cc_id);"
         private-goods-good-id-index "CREATE INDEX idx_private_good_id ON private_goods(good_id);"
         public-goods-cc-id-index "CREATE INDEX idx_public_cc ON public_goods(cc_id);"
         public-goods-good-id-index "CREATE INDEX idx_public_good_id ON public_goods(good_id);"
         pollutant-permissions-cc-id-index "CREATE INDEX idx_pp_cc_id ON pollutant_permissions(cc_id);"
         pollutant-permissions-pollutant-id-index "CREATE INDEX idx_pp_pollutant_id ON pollutant_permissions(pollutant_id);"
         ccs-index "CREATE INDEX idx_ccs ON ccs(id);"
        ]
    (do
      (jdbc/execute! ds [ccs-table])
      (jdbc/execute! ds [private-goods-table])
      (jdbc/execute! ds [public-goods-table])
      (jdbc/execute! ds [pollutant-permissions-table])
      (jdbc/execute! ds [private-good-prices-table])
      (jdbc/execute! ds [public-good-prices-table])
      (jdbc/execute! ds [pollutant-prices-table])
      (jdbc/execute! ds [private-good-prices-id-index])
      (jdbc/execute! ds [public-good-prices-index])
      (jdbc/execute! ds [pollutant-prices-index])
      (jdbc/execute! ds [private-goods-cc-id-index])
      (jdbc/execute! ds [private-goods-good-id-index])
      (jdbc/execute! ds [public-goods-cc-id-index])
      (jdbc/execute! ds [public-goods-good-id-index])
      (jdbc/execute! ds [ccs-index])
    )))

; after moving stuff to the database, this is now unnecessary, I think
(defn setup-improved [t _ experiment]
  (let [intermediate-inputs (vec (range 1 (inc (t :intermediate-inputs))))
        nature-types (vec (range 1 (inc (t :resources))))
        labor-types (vec (range 1 (inc (t :labors))))
        private-goods (vec (range 1 (inc (t :private-goods))))
        public-good-types (vec (range 1 (inc (t :public-goods))))
        pollutant-types (if (:include-pollutants? t)
                          (vec (range 1 (inc (t :pollutants))))
                          [])
        ds (t :ds)
        ; _ (create-normalized-ccs-tables ds)
        ; num-of-ccs (read-stream-ccs-to-normalized-db ds "ppex001-ccs.edn")
        ; [num-of-ccs pollutants-demand-sum private-goods-demand-sum public-goods-demand-sum] (read-stream-ccs ds "ppex001-ccs.edn")
        wcs (read-stream-wcs "ppex001-wcs.edn")]
    (-> t
        util/initialize-prices-db
        (assoc :natural-resources-supply (repeat (t :resources) 1000)
               :labor-supply (repeat (t :labors) 1000)
               :private-goods private-goods
               :intermediate-inputs intermediate-inputs
               :nature-types nature-types
               :labor-types labor-types
               :public-good-types public-good-types
               :pollutant-types pollutant-types
               :num-of-ccs 30000
;               :ccs (util/add-ids ccs)
               :wcs (util/add-ids wcs)
))))

#_(defn setup [t _ experiment]
  (let [intermediate-inputs (vec (range 1 (inc (t :intermediate-inputs))))
        nature-types (vec (range 1 (inc (t :resources))))
        labor-types (vec (range 1 (inc (t :labors))))
        private-goods (vec (range 1 (inc (t :private-goods))))
        public-good-types (vec (range 1 (inc (t :public-goods))))
        pollutant-types (if (:include-pollutants? t)
                          (vec (range 1 (inc (t :pollutants))))
                          [])
        [num-of-ccs pollutants-demand-sum private-goods-demand-sum public-goods-demand-sum ccs] (read-stream-ccs "ppex001-ccs.edn")
        ; wcs (read-stream "ppex001-wcs.edn")
        
]
    (-> t
        util/initialize-prices
        (assoc :natural-resources-supply (repeat (t :resources) 1000)
               :labor-supply (repeat (t :labors) 1000)
               :private-goods private-goods
               :intermediate-inputs intermediate-inputs
               :nature-types nature-types
               :labor-types labor-types
               :public-good-types public-good-types
               :pollutant-types pollutant-types
;               :num-of-ccs num-of-ccs
;               :pollutants-demand-sum pollutants-demand-sum
;               :private-goods-demand-sum private-goods-demand-sum
;               :public-goods-demand-sum public-goods-demand-sum
;               :ccs ccs
;               :wcs (util/add-ids wcs)
))))

; [:iteration :color :price-data :price-delta-data :pd-data :supply-data :demand-data :surplus-data :threshold-report]

(defn format-final-results [d]
  (mapv (fn [e] (vector (first e) (second e) (nth e 2))) d))

(defn -main [& ns-to-use]
  (do 
    (iterate-plan-improved)
    (println "ITERATION: " @iteration-count)
    (println (format-final-results @final-results))
    (println "=========")
    (while (and (or (some #(> % 5) (flatten (map last @final-results))))
                (> 100 @iteration-count))
      (do
        (iterate-plan-improved)
        (println "ITERATION: " @iteration-count)
        (println (format-final-results @final-results))
        (println "=========")))))
