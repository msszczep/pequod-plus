(ns pequod-plus.csvgen
   (:require [pequod-plus.util :as util]
             [clojure.edn :as edn]
             [next.jdbc :as jdbc]
             [clojure.java.io :as io]))

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
         :include-pollutants?      true
         :ds                       (jdbc/get-datasource {:dbtype "sqlite" :dbname "pequod.db"})}))

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

(defn iterate-plan-improved [t _]
  (let [include-pollutants? (:include-pollutants? t)
        ds (:ds t)
        wcs (mapv (partial util/proposal include-pollutants? (:price-data t)) (:wcs t))
        _ (util/consume-improved ds include-pollutants? (t :private-goods) (t :public-good-types) (t :pollutant-types) (t :num-of-ccs) (get-in t [:price-data]))
        price-data (util/update-surpluses-prices-improved wcs (:num-of-ccs t) (:pollutants-demand-sum t) (:private-goods-demand-sum t) (:public-goods-demand-sum t) (:natural-resources-supply t) (:labor-supply t) (:price-data t) (:price-delta-data t) include-pollutants?)
        surplus-data (util/get-pricing-data price-data :surplus include-pollutants?)
        supply-data (util/get-pricing-data price-data :supply include-pollutants?)
        demand-data (util/get-pricing-data price-data :demand include-pollutants?)
        price-delta-data (util/update-price-deltas supply-data demand-data surplus-data include-pollutants?)
        pd-data (util/update-percent-surplus supply-data demand-data surplus-data include-pollutants?)
        threshold-report (util/report-threshold supply-data demand-data surplus-data include-pollutants?)
        color (zipmap (keys threshold-report) (map show-color (vals threshold-report)))
        t2 (assoc t :wcs wcs
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
            _ (if (zero? (mod num-of-ccs 1000)) (println "ccs: " num-of-ccs))
            positive-utility-from-income (get-in form [:pollutant-utilities :positive-utility-from-income])
            negative-utility-from-exposure (get-in form [:pollutant-utilities :negative-utility-from-exposure])
            private-goods (get-in form [:private-goods])
            public-goods (get-in form [:public-goods])
            _ (if (not= form ::eof)
                (do
                  (jdbc/execute! ds ["insert into ccs (id, cohort_region, income, positive_utility_from_income, negative_utility_from_exposure) values (?, ?, ?, ?, ?);" num-of-ccs cohort-region income positive-utility-from-income negative-utility-from-exposure])
                  (doseq [e private-goods]
                     (jdbc/execute! ds ["insert into private_goods (cc_id, good_id, exponent, augment, demand) values (?, ?, ?, ?, ?);" num-of-ccs (get e :id) (get e :exponent) (get e :augment) (get e :demand)]))
                  (doseq [e public-goods]
                     (jdbc/execute! ds ["insert into public_goods (cc_id, good_id, exponent, augment, demand) values (?, ?, ?, ?, ?);" num-of-ccs (get e :id) (get e :exponent) (get e :augment) (get e :demand)]))))]
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
         private-goods-index "CREATE INDEX idx_private_cc ON private_goods(cc_id);"
         public-goods-index "CREATE INDEX idx_public_cc ON public_goods(cc_id);"
        ]
    (do
      (jdbc/execute! ds [ccs-table])
      (jdbc/execute! ds [private-goods-table])
      (jdbc/execute! ds [public-goods-table])
      (jdbc/execute! ds [pollutant-permissions-table])
      (jdbc/execute! ds [private-goods-index])
      (jdbc/execute! ds [public-goods-index]))))

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
        _ (create-normalized-ccs-tables ds)
        num-of-ccs (read-stream-ccs-to-normalized-db ds "ppex001-ccs.edn")
        ; [num-of-ccs pollutants-demand-sum private-goods-demand-sum public-goods-demand-sum] (read-stream-ccs ds "ppex001-ccs.edn")
         wcs (read-stream-wcs "ppex001-wcs.edn")]
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
               :num-of-ccs 30000 ; TEMP hard coded
               :pollutants-demand-sum  0 ; TEMP
               :private-goods-demand-sum 0 ; TEMP
               :public-goods-demand-sum 0 ; TEMP
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

(defn -main [& ns-to-use]
  (let [keys-to-print [:iteration :color :threshold-report]]
    (do
      (swap! globals setup-improved globals ns-to-use)
      #_(println (clojure.string/join "|" keys-to-print))
      #_(println (print-csv keys-to-print @globals))
      #_(while (and (or (empty? (flatten (vals (get @globals :threshold-report))))
                      (some #(> % 5) (flatten (vals (get @globals :threshold-report)))))
                  (> 200 (get @globals :iteration)))
        (do
          (swap! globals iterate-plan-improved globals) 
          (println (print-csv keys-to-print @globals))))
      #_(swap! globals augmented-reset globals)
      #_(println "AUGMENTED_RESET")
      #_(do
        (swap! globals iterate-plan globals)
        (println (print-csv keys-to-print @globals)))
      #_(while (and (some #(> % 5) (flatten (vals (get @globals :threshold-report))))
                  (> 200 (get @globals :iteration)))
        (do
          (swap! globals iterate-plan globals) 
          (println (print-csv keys-to-print @globals))))
      )))

