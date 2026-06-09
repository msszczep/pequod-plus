(ns pequod-plus.ccs-csv
  (:require
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]
    [pequod-plus.util :as util]))

; private_goods / public_goods / pollutant_permissions tables: cc_id, good_id, exponent, augment, demand 
(defn create-goods-in-csv [num-of-consumer-councils num-goods max-exponent-threshold]     
  (for [cc-id (range 1 (inc num-of-consumer-councils))
        good-id (range 1 (inc num-goods))]
    (vector cc-id
            good-id
            (+ max-exponent-threshold (rand max-exponent-threshold))
            (rand-nth [(- 0.002) (- 0.001) 0 0.001 0.002])
            0)))

; ccs table: id, cohort_region, income, positive_utility_from_income
(defn create-ccs-in-csv [num-of-consumer-councils]       
    (mapv #(vector % 1 5000 (rand-nth [0.11 0.13 0.15 0.17 0.19])         
                            (rand-nth [0.11 0.13 0.15 0.17 0.19]))
          (range 1 (inc num-of-consumer-councils))))

; prices tables: id, price, price_delta, price_delta_to_use, pd, supply, demand, surplus
(defn create-prices-in-csv [num-goods]
  (let [price 700
        price-delta-to-use 0.05
        pd-to-use 0.25]
    (mapv #(vector % price price-delta-to-use nil pd-to-use nil nil nil)
          (range 1 (inc num-goods)))))

(defn create-csv-file [file-name data]
  (with-open [writer (io/writer (str "resources/" file-name))]
    (csv/write-csv writer data)))

(defn create-all-ccs-csv-files []
  (let [num-of-consumer-councils 30000
        num-goods 100
        max-exponent-threshold 0.005]
    (do
      (create-csv-file "ccs.csv" (create-ccs-in-csv num-of-consumer-councils))
      (create-csv-file "private_goods.csv" (create-goods-in-csv num-of-consumer-councils num-goods max-exponent-threshold))
      (create-csv-file "public_goods.csv" (create-goods-in-csv num-of-consumer-councils num-goods max-exponent-threshold))
      (create-csv-file "pollutant_permissions.csv" (create-goods-in-csv num-of-consumer-councils num-goods max-exponent-threshold))
      (create-csv-file "private_good_prices.csv" (create-prices-in-csv num-goods))
      (create-csv-file "public_good_prices.csv" (create-prices-in-csv num-goods))
      (create-csv-file "pollutant_prices.csv" (create-prices-in-csv 1)))))

; (pprint (create-ccs-bulk 30 10 1 1))
