(ns pequod-plus.populate-from-clj-file
  (:require
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]))

; Instructions:
; 1. Open a repl
; 2a. (require '[clojure.data.csv :as csv])
; 2b. (require '[clojure.java.io :as io])
; 3. Load the source code
; 4. Load the load-file statement below
; 5. (create-all-wcs-csv-files wcs)
; 6. (create-all-ccs-csv-files ccs)
; 7. lein run -m pequod-plus.datasource-from-clj-file

(load-file "/home/pequod-plus/resources/dep1ex01.clj")

(defn input-c-and-e [wc-id category coefficients exponents]
  (->> coefficients
       count
       inc
       (range 1)
       (interleave coefficients exponents)
       (partition 3)
       (mapv (fn [[coefficient exponent good-id]]
               (vector category
                       wc-id
                       good-id
                       exponent
                       coefficient
                       (rand-nth [0 0.001 0.002 0.003 0.004]))))))

(defn retrieve-wc-metadata [[wc id]]
  (let [intermediate-inputs (input-c-and-e id 
                                           :intermediate-inputs 
                                           (first (:production-inputs wc))
                                           (:input-exponents wc))
        nature (input-c-and-e id 
                              :nature
                              (second (:production-inputs wc))
                              (:nature-exponents wc))
        labor (input-c-and-e id 
                             :labor
                             (last (:production-inputs wc))
                             (:labor-exponents wc))]
    (apply concat (concat (vector intermediate-inputs nature labor)))))

; id effort industry product output effort_elasticity
; total_factor_productivity  disutility_of_effort_coefficient disutility_of_effort_exponent
(defn create-wcs-in-csv [wcs-via-file num-goods]
  (let [wcs-to-use (for [w wcs-via-file]
                     (vector 
                        0 ; effort 
                        (:industry w) 
                        (:product w) 
                        0 ; output
                        (:c w) ; effort elasticity
                        (:a w) ; total factor productivity
                        (:s w) ; d-of-e coefficient
                        (:du w) ; d-of-e exponent
                     ))]
    (->> wcs-to-use
         (interleave (range 1 30001))
         (partition 2)
         (mapv flatten))))

; private_goods / public_goods / pollutant_permissions tables: cc_id, good_id, exponent, augment, demand 
(defn create-goods-in-csv [[cc id]]
  (letfn [(create-goods-intermediate [id-to-use type-to-use indexed-exponents]
            (for [e indexed-exponents]
              (vector type-to-use
                      id-to-use
                      (inc (first e))
                      (last e)
                      (rand-nth [(- 0.002) (- 0.001) 0 0.001 0.002])
                      0)))]
    (let [private-goods (create-goods-intermediate id :private-goods (map-indexed vector (:utility-exponents cc)))
          public-goods (create-goods-intermediate id :public-goods (map-indexed vector (:public-good-exponents cc)))]
      (concat private-goods public-goods))))

; ccs table: id, cohort_region, income, positive_utility_from_income, negative_utility_from_exposure
(defn create-ccs-in-csv [num-of-consumer-councils]
    (mapv #(vector % 1 5000 (rand-nth [1.11 1.13 1.15 1.17 1.19])
                            (rand-nth [0.11 0.13 0.15 0.17 0.19]))
          (range 1 (inc num-of-consumer-councils))))

; prices tables: id, price, price_delta, price_delta_to_use, pd, supply, demand, surplus
(defn create-prices [num-goods]
  (let [price 700
        price-delta-to-use 0.05
        pd-to-use 0.25]
    (mapv #(vector % price price-delta-to-use nil pd-to-use nil nil nil)
          (range 1 (inc num-goods)))))

; supplies: id, supply
(defn create-supplies [num-goods]
  (let [supply 1000]
    (mapv #(vector % supply)
          (range 1 (inc num-goods)))))

(defn create-file [file-name data]
  (with-open [writer (io/writer (str "resources/" file-name))]
    (csv/write-csv writer data)))

(defn filter-metadata-by-category [metadata category]
  (->> metadata
       (apply concat)
       (filter #(= (first %) category))
       (map rest)))

(defn create-all-ccs-csv-files [ccs]
  (let [num-goods 100
        max-exponent-threshold 0.005
        num-of-consumer-councils (count ccs)
        goods-to-use (map create-goods-in-csv (partition 2 (interleave ccs (range 1 (inc num-of-consumer-councils)))))]
    (do
      (create-file "ccs.csv" (create-ccs-in-csv num-of-consumer-councils))
      (create-file "private_goods.csv" (filter-metadata-by-category goods-to-use :private-goods))
      (create-file "public_goods.csv" (filter-metadata-by-category goods-to-use :public-goods))
      (create-file "private_good_prices.csv" (create-prices num-goods))
      (create-file "public_good_prices.csv" (create-prices num-goods)))))

; Note for 10/13: resume with wc-metadata

(defn create-all-wcs-csv-files [wcs]
  (let [num-of-worker-councils 30000
        num-goods 100
        wc-metadata (map retrieve-wc-metadata (partition 2 (interleave wcs (range 1 (inc num-of-worker-councils)))))
        intermediate-inputs (filter-metadata-by-category wc-metadata :intermediate-inputs)
        nature (filter-metadata-by-category wc-metadata :nature)
        labor (filter-metadata-by-category wc-metadata :labor)
        nature-supplies (create-supplies num-goods)
        labor-supplies (create-supplies num-goods)]
    (do
      (create-file "wcs.csv" (create-wcs-in-csv wcs num-goods))
      (create-file "intermediate_inputs.csv" intermediate-inputs)
      (create-file "nature.csv" nature)
      (create-file "labor.csv" labor)
      (create-file "nature_supply.csv" nature-supplies)
      (create-file "labor_supply.csv" labor-supplies)
      (create-file "nature_prices.csv" (create-prices num-goods))
      (create-file "labor_prices.csv" (create-prices num-goods))
      (create-file "intermediate_input_prices.csv" (create-prices num-goods))
    )))
