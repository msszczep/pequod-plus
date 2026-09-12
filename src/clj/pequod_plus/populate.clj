(ns pequod-plus.populate
  (:require
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]))

(defn rand-range [start end]
  (+ start (clojure.core/rand (- end start))))

(defn generate-wc-metadata [num-goods wc-id]
  (letfn [(get-random-subsets []
            (let [s (->> [1 1 1 1 2 2 3 3 4 4]
                         shuffle
                         (take (inc (rand-int 4)))
                         frequencies)]
              (->> [1 2 3 4]
                   (map #(get s % 0))
                   (mapv inc))))
          (generate-exponents [n inputs-to-use]
            (->> #(rand-range (/ 0.75 n) (/ 0.85 n))
                 repeatedly
                 (take (count inputs-to-use))
                 vec))
          (determine-production-input [subset types]
            (->> types
                 shuffle
                 (take subset)
                 sort))
          (populate-c-and-e [wc-id category coefficients exponents]
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
                                  (rand-nth [0 0.001 0.002 0.003 0.004]) ;augment
                          )))))]
    (let [[intermediate-inputs-subset
           nature-subset
           labor-subset
           pollutant-subset] (get-random-subsets)
          intermediate-input-types (vec (range 1 (inc num-goods)))
          nature-types (vec (range 1 (inc num-goods)))
          labor-types (vec (range 1 (inc num-goods)))
          pollutants (vec (range 1 2))
          production-inputs-intermediate-input (determine-production-input intermediate-inputs-subset intermediate-input-types)
          production-inputs-nature (determine-production-input nature-subset nature-types)
          production-inputs-labor (determine-production-input labor-subset labor-types)
          production-inputs-pollutants (determine-production-input pollutant-subset pollutants)
          production-inputs-count (+ intermediate-inputs-subset nature-subset labor-subset pollutant-subset)
          intermediate-input-exponents (generate-exponents production-inputs-count production-inputs-intermediate-input)
          nature-exponents (generate-exponents production-inputs-count production-inputs-nature)
          labor-exponents (generate-exponents production-inputs-count production-inputs-labor)
          pollutant-exponents (generate-exponents production-inputs-count production-inputs-pollutants)
          intermediate-inputs (populate-c-and-e wc-id :intermediate-inputs production-inputs-intermediate-input intermediate-input-exponents)
          nature (populate-c-and-e wc-id :nature production-inputs-nature nature-exponents)
          labor (populate-c-and-e wc-id :labor production-inputs-labor labor-exponents)
          pollutant-demands (populate-c-and-e wc-id :pollutant-demands production-inputs-pollutants pollutant-exponents)]
            (apply concat (concat (vector intermediate-inputs nature labor pollutant-demands))))))

; id effort industry product output effort_elasticity
; total_factor_productivity  disutility_of_effort_coefficient disutility_of_effort_exponent
(defn create-wcs-in-csv [num-councils num-goods]
  (let [wcs (for [multiplying-factor (range 1 (inc (/ num-councils num-goods 3)))
                  industry (range 3)
                  product (range 1 (inc num-goods))]
              (vector 0 industry product 0
                      (rand-range 0.05 0.1)
                      (rand-range 4 6)
                      1
                      (rand-range 3 4)))]
    (->> wcs
         (interleave (range 1 (inc num-councils)))
         (partition 2)
         (mapv flatten))))

; private_goods / public_goods / pollutant_permissions tables: cc_id, good_id, exponent, augment, demand 
(defn create-goods-in-csv [num-of-consumer-councils num-goods max-exponent-threshold]
  (for [cc-id (range 1 (inc num-of-consumer-councils))
        good-id (range 1 (inc num-goods))]
    (vector cc-id
            good-id
            (+ max-exponent-threshold (rand max-exponent-threshold))
            (rand-nth [(- 0.002) (- 0.001) 0 0.001 0.002])
            0)))

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

(defn create-all-ccs-csv-files []
  (let [num-of-consumer-councils 30000
        num-goods 100
        max-exponent-threshold 0.005]
    (do
      (create-file "ccs.csv" (create-ccs-in-csv num-of-consumer-councils))
      (create-file "private_goods.csv" (create-goods-in-csv num-of-consumer-councils num-goods max-exponent-threshold))
      (create-file "public_goods.csv" (create-goods-in-csv num-of-consumer-councils num-goods max-exponent-threshold))
      (create-file "pollutant_permissions.csv" (create-goods-in-csv num-of-consumer-councils num-goods max-exponent-threshold))
      (create-file "private_good_prices.csv" (create-prices num-goods))
      (create-file "public_good_prices.csv" (create-prices num-goods))
      (create-file "pollutant_prices.csv" (create-prices 1)))))

(defn filter-wc-metadata-by-category [wc-metadata category]
  (->> wc-metadata
       (filter #(= (first %) category))
       (map rest)))

(defn create-all-wcs-csv-files []
  (let [num-of-consumer-councils 30000
        num-goods 100
        wc-metadata (->> num-of-consumer-councils
                         inc
                         (range 1)
                         (map (partial generate-wc-metadata 100))
                         flatten
                         (partition 6))
        intermediate-inputs (filter-wc-metadata-by-category wc-metadata :intermediate-inputs)
        nature (filter-wc-metadata-by-category wc-metadata :nature)
        labor (filter-wc-metadata-by-category wc-metadata :labor)
        pollutant-demands (filter-wc-metadata-by-category wc-metadata :pollutant-demands)
        nature-supplies (create-supplies num-goods)
        labor-supplies (create-supplies num-goods)]
    (do
      (create-file "wcs.csv" (create-wcs-in-csv num-of-consumer-councils num-goods))
      (create-file "intermediate_inputs.csv" intermediate-inputs)
      (create-file "nature.csv" nature)
      (create-file "labor.csv" labor)
      (create-file "nature_supply.csv" nature-supplies)
      (create-file "labor_supply.csv" labor-supplies)
      (create-file "pollutant_demands.csv" pollutant-demands)
      (create-file "nature_prices.csv" (create-prices num-goods))
      (create-file "labor_prices.csv" (create-prices num-goods))
      (create-file "intermediate_input_prices.csv" (create-prices num-goods))
      )))

; (pprint (create-ccs-bulk 30 10 1 1))
