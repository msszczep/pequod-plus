(ns pequod-plus.csvgen
   (:require [pequod-plus.ppex004 :as ppex004]
             [pequod-plus.util :as util]))

(def globals
  (atom {:init-private-good-price 700
         :init-intermediate-price 700
         :init-labor-price        700
         :init-nature-price       700
         :init-public-good-price  700
         :init-pollutant-price    700
         :private-goods           100
         :intermediate-inputs     100
         :resources               100
         :labors                  100
         :public-goods            100
         :pollutants                1
         :price-data               {}
         :price-delta-data         {}
         :surplus-data             {}
         :supply-data              {}
         :demand-data              {}
         :threshold-report         []
         :wcs                      []
         :ccs                      []
         :iteration                0
         :include-pollutants?      true}))

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

(defn iterate-plan [t _]
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

(defn print-csv [args-to-print data]
  (let [all-args (flatten (mapv (partial get data) args-to-print))]
    (clojure.string/join "|" all-args)))

(defn augmented-reset [t _]
  (assoc t :iteration 0
           :ccs (mapv util/augment-cc (get t :ccs))
           :wcs (mapv util/augment-wc (get t :wcs))))

(defn setup [t _ experiment]
  (let [intermediate-inputs (vec (range 1 (inc (t :intermediate-inputs))))
        nature-types (vec (range 1 (inc (t :resources))))
        labor-types (vec (range 1 (inc (t :labors))))
        private-goods (vec (range 1 (inc (t :private-goods))))
        public-good-types (vec (range 1 (inc (t :public-goods))))
        pollutant-types (if (:include-pollutants? t)
                          (vec (range 1 (inc (t :pollutants))))
                          [])]
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
               :ccs (util/add-ids ppex004/ccs)
               :wcs (util/add-ids ppex004/wcs)))))

; [:iteration :color :price-data :price-delta-data :pd-data :supply-data :demand-data :surplus-data :threshold-report]

(defn -main [& [ns-to-use]]
  (let [keys-to-print [:iteration :color :threshold-report]]
    (do
      (swap! globals setup globals ns-to-use)
      (println (clojure.string/join "|" keys-to-print))
      (println (print-csv keys-to-print @globals))
      (while (and (or (empty? (flatten (vals (get @globals :threshold-report))))
                      (some #(> % 5) (flatten (vals (get @globals :threshold-report)))))
                  (> 200 (get @globals :iteration)))
        (do
          (swap! globals iterate-plan globals) 
          (println (print-csv keys-to-print @globals))))
      (swap! globals augmented-reset globals)
      (println "AUGMENTED_RESET")
      (do
        (swap! globals iterate-plan globals)
        (println (print-csv keys-to-print @globals)))
      (while (and (some #(> % 5) (flatten (vals (get @globals :threshold-report))))
                  (> 200 (get @globals :iteration)))
        (do
          (swap! globals iterate-plan globals) 
          (println (print-csv keys-to-print @globals))))
      )))

