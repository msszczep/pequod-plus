(ns pequod-plus.csvgen
   (:require [pequod-plus.util :as util]
             [clojure.edn :as edn]
             [next.jdbc :as jdbc]
             [next.jdbc.result-set :as result-set]
             [clojure.java.io :as io]))

(def globals
  (atom {:price-data               {}
         :price-delta-data         {}
         :surplus-data             {}
         :supply-data              {}
         :demand-data              {}
         :threshold-report         []
         :wcs                      []
         :ccs                      []
         :iteration                0
         :include-pollutants?      false
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

(defn get-demand-sum [ds table-name]
  (let [q (case table-name
            :pollutant-permissions "select sum(demand) as s from pollutant_permissions"
            :private-goods "select sum(demand) as s from private_goods"
            :public-goods "select sum(demand) as s from public_goods"
            "dunno")]
    (->> {:builder-fn result-set/as-unqualified-lower-maps}
         (jdbc/execute-one! ds [q])
         :s)))

(defn iterate-plan-improved [t _]
  (let [include-pollutants? (:include-pollutants? t)
        ds (:ds t)
        ; _ (println "price-data/keys: " (keys (:price-data t)))
        wc-ids (mapv :id (jdbc/execute! ds ["select distinct id from wcs order by id" ] {:builder-fn result-set/as-unqualified-lower-maps}))
        _ (map (partial util/proposal-db ds include-pollutants?) wc-ids)
        ; _ (println "wcs loaded")
        _ (util/consume-process-all-in-db ds include-pollutants?)
        ; _ (util/consume-from-db ds include-pollutants? (t :private-goods) (t :public-good-types) (t :pollutant-types) (t :num-of-ccs) (get-in t [:price-data]))
        ; _ (println "consume-from-db complete")
        pollutants-demand-sum (get-demand-sum ds :pollutant-permissions)
        private-goods-demand-sum (get-demand-sum ds :private-goods)
        public-goods-demand-sum (get-demand-sum ds :public-goods)
        ; _ (println "pollutants-demand-sum: " pollutants-demand-sum)
        ; _ (println "private-goods-demand-sum: " private-goods-demand-sum)
        ; _ (println "public-goods-demand-sum: " public-goods-demand-sum)
        price-data (util/update-surpluses-prices-improved ds wcs (:num-of-ccs t) pollutants-demand-sum private-goods-demand-sum public-goods-demand-sum (:natural-resources-supply t) (:labor-supply t) (:price-data t) (:price-delta-data t) include-pollutants?)
        ; _ (println "price-data updated")
        ; _ (println "price-data: " price-data)
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

(defn -main [& ns-to-use]
  (let [keys-to-print [:iteration :color :threshold-report]]
    (do
      #_(swap! globals setup-improved globals ns-to-use)
      (println (clojure.string/join "|" keys-to-print))
      (println (print-csv keys-to-print @globals))
      (while (and (or (empty? (flatten (vals (get @globals :threshold-report))))
                      (some #(> % 5) (flatten (vals (get @globals :threshold-report)))))
                  (> 200 (get @globals :iteration)))
        (do
          (swap! globals iterate-plan-improved globals)
          (println (print-csv keys-to-print @globals))))
      (swap! globals augmented-reset-improved globals)
      (println "AUGMENTED_RESET")
      (do
          (swap! globals iterate-plan-improved globals)
          (println (print-csv keys-to-print @globals)))
      (while (and (some #(> % 5) (flatten (vals (get @globals :threshold-report))))
                    (> 200 (get @globals :iteration)))
          (do
            (swap! globals iterate-plan-improved globals)
            (println (print-csv keys-to-print @globals))))
      )))

