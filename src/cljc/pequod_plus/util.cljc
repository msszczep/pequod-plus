(ns pequod-plus.util
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as result-set]))

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

(defn initialize-prices-db [t]
  (let [num-private-goods (t :private-goods)
        num-im-inputs (t :intermediate-inputs)
        num-resources (t :resources)
        num-labor (t :labors)
        num-public-goods (t :public-goods)
        num-pollutants (t :pollutants)
        price-delta-to-use 0.05
        pd-to-use 0.25
        ds (t :ds)]
    (do
      #_(doseq [n (range 1 (inc num-private-goods))]
         (jdbc/execute! ds ["INSERT into private_good_prices (id, price, price_delta, pd) values (?, ?, ?, ?);" n (t :init-private-good-price) price-delta-to-use pd-to-use]))
      #_(doseq [n (range 1 (inc num-public-goods))]
         (jdbc/execute! ds ["INSERT into public_good_prices (id, price, price_delta, pd) values (?, ?, ?, ?);" n (t :init-public-good-price) price-delta-to-use pd-to-use]))
      #_(doseq [n (range 1 (inc num-pollutants))]
         (jdbc/execute! ds ["INSERT into pollutant_prices (id, price, price_delta, pd) values (?, ?, ?, ?);" n (t :init-pollutant-price) price-delta-to-use pd-to-use]))
      (assoc t
             :price-data {:intermediate-inputs (make-price-maps (t :init-intermediate-price) num-im-inputs price-delta-to-use pd-to-use)
                          :nature (make-price-maps (t :init-nature-price) num-resources price-delta-to-use pd-to-use)
                          :labor (make-price-maps (t :init-labor-price) num-labor price-delta-to-use pd-to-use)
                          :private-goods (make-price-maps (t :init-private-good-price) num-private-goods price-delta-to-use pd-to-use)
                          :public-goods (make-price-maps (t :init-public-good-price) num-public-goods price-delta-to-use pd-to-use)
                          :pollutants (make-price-maps (t :init-pollutant-price) num-pollutants price-delta-to-use pd-to-use)
                          }))))

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
        newly-computed-price-delta (- 1.05 (Math/pow 0.5 (/ (Math/abs (* 2 surplus)) (+ demand supply))))
        new-delta (force-to-one (get-delta newly-computed-price-delta (get price-delta-data type-to-use 1)))
        new-price (cond (pos? surplus) (* (- 1 new-delta) (:price price-datum))
                        (neg? surplus) (* (+ 1 new-delta) (:price price-datum))
                        :else (:price price-datum))]
    (assoc price-datum :pd new-delta :price new-price :surplus surplus :price-delta-to-use newly-computed-price-delta :supply supply :demand demand)))

(defn get-price-table-name [cat]
  (let [d {:public-goods "public_good_prices"
           :private-goods "private_good_prices"
           :nature "nature_prices"
           :labor "labor_prices"
           :intermediate-inputs "intermediate_input_prices"
           :pollutants "pollutant_prices"}]
    (get d cat)))

(defn run-query [ds q-array k]
  (->> {:builder-fn result-set/as-unqualified-lower-maps}
       (jdbc/execute-one! ds q-array)
       k))

(defn compute-surpluses-prices-improved [ds private-goods-demand-sum public-goods-demand-sum pollutants-demand-sum price-delta-data id-to-use type-to-use]
  (let [{:keys [price pd]} (jdbc/execute-one! ds [(str "select price from " (get-price-table-name type-to-use) " where id = ?") id-to-use] {:builder-fn result-set/as-unqualified-lower-maps})
        num-of-ccs (run-query ds ["select count(*) as num from ccs"] :num)
        supply (condp = type-to-use
                           :private-goods
                             (run-query ds ["select sum(output) as sum from wcs where industry = 0 and product = ?" id-to-use] :sum)
                           :intermediate-inputs
                             (run-query ds ["select sum(output) as sum from wcs where industry = 1 and product = ?" id-to-use] :sum)
                           :nature
                             (run-query ds ["select natural_resource_supply as supply from natural_resources_supply where id = ?" id-to-use] :supply)
                           :labor
                             (run-query ds ["select labor_supply as supply from labor_supply where id = ?" id-to-use] :supply)
                           :public-goods
                             (run-query ds ["select sum(output) as sum from wcs where industry = 2 and product = ?" id-to-use] :sum)
                           :pollutants (/ pollutants-demand-sum num-of-ccs))
        demand (condp = type-to-use
                           :private-goods private-goods-demand-sum
                           :intermediate-inputs
                             (run-query ds ["select sum(quantity) as sum from intermediate_inputs where coefficient = ?" id-to-use] :sum)
                           :nature
                             (run-query ds ["select sum(quantity) as sum from nature where coefficient = ?" id-to-use] :sum)
                           :labor
                             (run-query ds ["select sum(quantity) as sum from labor where coefficient = ?" id-to-use] :sum)
                           :public-goods (/ public-goods-demand-sum num-of-ccs)
                           :pollutants
                             (run-query ds ["select sum(quantity) as sum from pollutant_demands where coefficient = ?" id-to-use] :sum))
        surplus (- supply demand)
        newly-computed-price-delta (- 1.05 (Math/pow 0.5 (/ (Math/abs (* 2 surplus)) (+ demand supply))))
        new-delta (force-to-one (get-delta newly-computed-price-delta (get price-delta-data type-to-use 1)))
        new-price (cond (pos? surplus) (* (- 1 new-delta) price)
                        (neg? surplus) (* (+ 1 new-delta) price)
                        :else price)]
    (case type-to-use
      :private-goods
        (jdbc/execute! ds ["UPDATE private_good_prices SET pd = ?, price = ?, price_delta_to_use = ?, supply = ?, demand = ?, surplus = ? WHERE id = ?;" 
                            new-delta new-price newly-computed-price-delta supply demand surplus id-to-use])
      :intermediate-inputs
        (jdbc/execute! ds ["UPDATE intermediate_input_prices SET pd = ?, price = ?, price_delta_to_use = ?, supply = ?, demand = ?, surplus = ? WHERE id = ?;" 
                            new-delta new-price newly-computed-price-delta supply demand surplus id-to-use])
      :nature
        (jdbc/execute! ds ["UPDATE nature_prices SET pd = ?, price = ?, price_delta_to_use = ?, supply = ?, demand = ?, surplus = ? WHERE id = ?;" 
                            new-delta new-price newly-computed-price-delta supply demand surplus id-to-use])
      :labor
        (jdbc/execute! ds ["UPDATE labor_prices SET pd = ?, price = ?, price_delta_to_use = ?, supply = ?, demand = ?, surplus = ? WHERE id = ?;" 
                            new-delta new-price newly-computed-price-delta supply demand surplus id-to-use])
      :public-goods
        (jdbc/execute! ds ["UPDATE public_good_prices SET pd = ?, price = ?, price_delta_to_use = ?, supply = ?, demand = ?, surplus = ? WHERE id = ?;" 
                            new-delta new-price newly-computed-price-delta supply demand surplus id-to-use])
      :pollutants
         (jdbc/execute! ds ["UPDATE pollutant_prices SET pd = ?, price = ?, price_delta_to_use = ?, supply = ?, demand = ?, surplus = ? WHERE id = ?;" 
                             new-delta new-price newly-computed-price-delta supply demand surplus id-to-use])
      )))

(defn calculate-price-deltas [supply-list demand-list surplus-list]
  (let [surplus-list-means (mean surplus-list)
        averaged-s-and-d (mean [(mean supply-list) (mean demand-list)])]
        (Math/abs (/ surplus-list-means averaged-s-and-d))))

(defn update-price-deltas-db [ds include-pollutants?]
  (let [categories
        (if include-pollutants?
          [:private-goods :intermediate-inputs :nature :labor :public-goods :pollutants]
          [:private-goods :intermediate-inputs :nature :labor :public-goods])

        data
        (into {}
              (for [category categories]
                (let [rows
                      (jdbc/execute! ds [(str "SELECT supply, demand, surplus FROM " (get-price-table-name category))]
                                     {:builder-fn result-set/as-unqualified-lower-maps})
                      supply-list (map :supply rows)
                      demand-list (map :demand rows)
                      surplus-list (map :surplus rows)]
                  [category (calculate-price-deltas supply-list demand-list surplus-list)])))]

    (doseq [[category price-delta] data]
      (jdbc/execute! ds ["UPDATE price_delta_data SET price_delta = ? WHERE type = ?" price-delta (get-price-table-name category)]))

    data))

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

(defn update-surpluses-prices [wcs ccs natural-resources-supply labor-supply price-data price-delta-data include-pollutants?]
  (let [categories (if include-pollutants?
                     [:private-goods :intermediate-inputs :nature :labor :public-goods :pollutants]
                     [:private-goods :intermediate-inputs :nature :labor :public-goods])
        price-updates (mapv (fn [type-to-use] (mapv (partial compute-surpluses-prices wcs ccs natural-resources-supply labor-supply price-delta-data type-to-use) (get-in price-data [type-to-use]))) categories)]
     (zipmap categories price-updates)))

(defn update-surpluses-prices-improved [ds private-goods-demand-sum public-goods-demand-sum pollutants-demand-sum price-delta-data include-pollutants?]
  (doseq [c (if include-pollutants?
              [:private-goods :intermediate-inputs :nature :labor :public-goods :pollutants]
              [:private-goods :intermediate-inputs :nature :labor :public-goods])
          n (range 1 (inc (if (= c :pollutants) 1 100)))]
    (compute-surpluses-prices-improved ds private-goods-demand-sum public-goods-demand-sum pollutants-demand-sum price-delta-data n c)))

(defn compute-threshold [supply-list demand-list surplus-list]
  (->> (interleave (flatten surplus-list) (flatten demand-list) (flatten supply-list))
       (partition 3)
       (mapv #(* 100 (/ (Math/abs (* 2 (first %))) (+ (second %) (last %)))))))

(defn compute-threshold-improved [{:keys [id type supply demand surplus]}]
  (let [threshold (if (or (and (nil? demand) (nil? surplus))
                          (and (zero? demand) (zero? surplus)))
                      0
                      (* 100 (/ (Math/abs (* 2 surplus)) (+ demand supply))))]
    (hash-map :id id :type type :threshold threshold)))

(defn report-threshold [supply-data demand-data surplus-data include-pollutants?]
  (let [categories (if include-pollutants?
                     [:private-goods :intermediate-inputs :nature :labor :public-goods :pollutants]
                     [:private-goods :intermediate-inputs :nature :labor :public-goods])
        updates-to-use (mapv (fn [cat-to-use] (compute-threshold (get-in supply-data [cat-to-use])
                                                                 (get-in demand-data [cat-to-use])
                                                                 (get-in surplus-data [cat-to-use]))) categories)]
    (zipmap categories updates-to-use)))

#_(defn proposal [ds include-pollutants? prices wc]
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
          (get-product-price-db [product select-statement]
            (->> {:builder-fn result-set/as-unqualified-lower-maps}
                 (jdbc/execute! ds [select-statement product])
                 first
                 :price))
          (get-lambda-o [w input-prices]
            (let [industry (:industry w)
                  product (:product w)]
              (cond (= 0 industry) (get-product-price-db product "select price from private_good_prices where id = ?")
                    (= 1 industry) (get-product-price product input-prices)
                    (= 2 industry) (get-product-price-db product "select price from public_good_prices where id = ?"))))]
    (let [input-prices (:intermediate-inputs prices)
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
          ; TODO: Fix pollutant-prices-to-use to scale up with an IN or ANY statement
          pollutant-prices-to-use (->> {:builder-fn result-set/as-unqualified-lower-maps}
                                       (jdbc/execute! ds ["select price from pollutant_prices where id = 1"])
                                       first
                                       :price)
          ps (if include-pollutants?
               [(mapv (partial get-product-category-price prices :intermediate-inputs) (first p-i))
                (mapv (partial get-product-category-price prices :nature) (second p-i))
                (mapv (partial get-product-category-price prices :labor) (nth p-i 2))
                [pollutant-prices-to-use]]
               [(mapv (partial get-product-category-price prices :intermediate-inputs) (first p-i))
                (mapv (partial get-product-category-price prices :nature) (second p-i))
                (mapv (partial get-product-category-price prices :labor) (nth p-i 2))])
          b (map-wc-values wc :exponent)
          λ (get-lambda-o wc input-prices)]
      (condp = input-count-r
        3 (merge wc (solution-3 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
        4 (merge wc (solution-4 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
        5 (merge wc (solution-5 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
        6 (merge wc (solution-6 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
        7 (merge wc (solution-7 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
        8 (merge wc (solution-8 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
        9 (merge wc (solution-9 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
        10 (merge wc (solution-10 total-factor-productivity disutility-of-effort-coefficient effort-elasticity disutility-of-effort-exponent ps b λ p-i include-pollutants?))
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

(defn consume-from-db [ds include-pollutants? private-goods public-goods pollutants num-of-ccs price-data]
  (let [builder-fn-map {:builder-fn result-set/as-unqualified-lower-maps}
        ccs (jdbc/execute! ds ["select id, income, positive_utility_from_income, negative_utility_from_exposure from ccs"] builder-fn-map)
        private-good-prices (:private-goods price-data)
        public-good-prices (:public-goods price-data)
        pollutant-prices (if include-pollutants? (:pollutants price-data) [])]
    (doseq [cc ccs]
      (let [cc-id (:id cc)
            income (:income cc)
            private-good-exponents-sum (->> builder-fn-map
                                            (jdbc/execute-one! ds ["select sum(exponent) as sum_exponent from private_goods where cc_id = ?" cc-id])
                                            :sum_exponent)
            public-good-exponents-sum (->> builder-fn-map
                                            (jdbc/execute-one! ds ["select sum(exponent) as sum_exponent from public_goods where cc_id = ?" cc-id])
                                            :sum_exponent)
            pollutant-positive-utility-from-income (get-in cc [:positive_utility_from_income])
            pollutant-negative-utility-from-exposure (get-in cc [:negative_utility_from_exposure])]
       (do
         (doseq [private-good private-goods]
           (let [private-good-price (->> private-good-prices
                                         (filter #(= private-good (:id %)))
                                         first
                                         :price)
                 private-good-exponent (:exponent (jdbc/execute-one! ds ["select exponent from private_goods where cc_id = ? and good_id = ?" cc-id private-good] builder-fn-map))
                 updated-demand (/ (* income private-good-exponent)
                                   (* (+ private-good-exponents-sum public-good-exponents-sum)
                                      private-good-price))]
             (jdbc/execute-one! ds ["update private_goods set demand = ? where cc_id = ? and good_id = ?" updated-demand cc-id private-good])))
         (doseq [public-good public-goods]
              (let [public-good-price (->> public-good-prices
                                           (filter #(= public-good (:id %)))
                                           first
                                           :price)
                    public-good-exponent (:exponent (jdbc/execute-one! ds ["select exponent from public_goods where cc_id = ? and good_id = ?" cc-id public-good] builder-fn-map))
                    updated-demand (/ (* income public-good-exponent)
                                      (* (+ private-good-exponents-sum public-good-exponents-sum)
                                         public-good-price))]
                (jdbc/execute-one! ds ["update public_goods set demand = ? where cc_id = ? and good_id = ?" updated-demand cc-id public-good])))
         (if include-pollutants?
           (do
             (doseq [pollutant pollutants]
                  (let [p (->> pollutant-prices
                               (filter #(= pollutant (:id %)))
                               first
                               :price)
                        k pollutant-negative-utility-from-exposure
                        j pollutant-positive-utility-from-income
                        pollutant-permission (* (Math/pow 5 (/ 1 (- k j)))
                                                (Math/pow (/ (* j (Math/pow p j)) k) (/ 1 (- k j))))]
                    (jdbc/execute-one! ds ["update pollutant_permissions set demand = ? where cc_id = ? and good_id = ?" pollutant-permission cc-id pollutant])))
             (let [updated-income (->> builder-fn-map
                                       (jdbc/execute-one! ds ["select sum(demand) as sum_demand from pollutant_permissions where cc_id = ?" cc-id])
                                       :sum_demand)]
               (jdbc/execute-one! ds ["update ccs set income = ? where id = ?" (+ income updated-income) cc-id])))))))))

(defn process-batch [tx rows include-pollutants? private-goods public-goods pollutants num-of-ccs price-data]
  (let [updates
        (mapv (fn [e]
                (let [cc-map (clojure.edn/read-string (get-in e [:ccs/cc]))
                      id (get-in e [:ccs/id])
                      _ (println "process-batch/id: " id)
                      updated (consume include-pollutants? private-goods public-goods pollutants num-of-ccs price-data cc-map)]
                  [(pr-str updated) id]))
              rows)]
    (jdbc/execute-batch!
      tx
      "update ccs set cc = ? where id = ?"
      updates)))

(defn consume-process-all-in-db [ds include-pollutants?]
  (jdbc/with-transaction [tx ds]
    (jdbc/execute! tx ["CREATE TEMP TABLE exponent_sums AS
      SELECT
        c.id AS cc_id,
        COALESCE(pg_sum.sum_exp, 0) AS private_sum,
        COALESCE(pub_sum.sum_exp, 0) AS public_sum,
        COALESCE(pg_sum.sum_exp, 0) + COALESCE(pub_sum.sum_exp, 0) AS total_sum
      FROM ccs c
      LEFT JOIN (
        SELECT cc_id, SUM(exponent) AS sum_exp
        FROM private_goods GROUP BY cc_id
      ) pg_sum ON pg_sum.cc_id = c.id
      LEFT JOIN (
        SELECT cc_id, SUM(exponent) AS sum_exp
        FROM public_goods GROUP BY cc_id
      ) pub_sum ON pub_sum.cc_id = c.id;"])
    (jdbc/execute! tx ["CREATE INDEX temp.idx_exponent_sums ON exponent_sums(cc_id);"])
    (jdbc/execute! tx ["UPDATE private_goods SET demand = (
        (SELECT income FROM ccs WHERE id = private_goods.cc_id)
        * exponent
      ) / (
        (SELECT total_sum FROM exponent_sums WHERE cc_id = private_goods.cc_id)
        * (SELECT price FROM private_good_prices WHERE id = private_goods.good_id)
      );"])
    (jdbc/execute! tx ["UPDATE public_goods SET demand = (
        (SELECT income FROM ccs WHERE id = public_goods.cc_id)
        * exponent
      ) / (
        (SELECT total_sum FROM exponent_sums WHERE cc_id = public_goods.cc_id)
        * (SELECT price FROM public_good_prices WHERE id = public_goods.good_id)
      );"])
    (when include-pollutants?
      (do
        (jdbc/execute! tx ["UPDATE pollutant_permissions
        SET demand =
          POW(5, 1.0 / (ccs.negative_utility_from_exposure - ccs.positive_utility_from_income))
          *
          POW(
            (
              ccs.positive_utility_from_income *
              POW(pollutant_prices.price, ccs.positive_utility_from_income)
            ) / ccs.negative_utility_from_exposure,
            1.0 / (ccs.negative_utility_from_exposure - ccs.positive_utility_from_income)
           )
          FROM ccs, pollutant_prices
          WHERE pollutant_permissions.cc_id = ccs.id
          AND pollutant_prices.id = pollutant_permissions.pollutant_id;"])
        (jdbc/execute! tx ["UPDATE ccs
        SET income = income + (
          SELECT COALESCE(SUM(demand), 0)
          FROM pollutant_permissions
          WHERE pollutant_permissions.cc_id = ccs.id
        );"])))
    (jdbc/execute! tx ["DROP TABLE exponent_sums;"])))

(defn consume-improved [ds include-pollutants? private-goods public-goods pollutants num-of-ccs price-data]
  (jdbc/with-transaction [tx ds]
    (loop [offset 0
           batch-size 10]
      (let [rows (jdbc/execute! tx ["select id, cc from ccs limit ? offset ?" batch-size offset])]
        (when (seq rows)
          (process-batch tx rows include-pollutants? private-goods public-goods pollutants num-of-ccs price-data)
          (recur (+ offset batch-size) batch-size))))))

(defn consume-improved-prototype [include-pollutants? private-goods public-goods pollutants num-of-ccs price-data]
  (let [ds (jdbc/get-datasource {:dbtype "sqlite" :dbname "pequod.db"})]
    (dotimes [i-to-use num-of-ccs]
      (let [i (inc i-to-use)
            cc (->> ["select * from ccs where id = ?" i]
                    (jdbc/execute-one! ds)
                    second
                    second
                    read-string)]
        (if (zero? (mod i 1000)) (println (str "consume-improved cc: " i " =>" cc)))))))

(defn get-pricing-data [price-data pricing-cat include-pollutants?]
  (let [categories (if include-pollutants?
                     [:private-goods :intermediate-inputs :nature :labor :public-goods :pollutants]
                     [:private-goods :intermediate-inputs :nature :labor :public-goods])
        data-to-get (mapv (fn [type-to-use] (mapv pricing-cat (get-in price-data [type-to-use]))) categories)]
    (zipmap categories data-to-get)))

(defn update-price-deltas [supply-data demand-data surplus-data include-pollutants?]
  (let [categories (if include-pollutants?
                     [:private-goods :intermediate-inputs :nature :labor :public-goods :pollutants]
                     [:private-goods :intermediate-inputs :nature :labor :public-goods])
        data-to-get (mapv (fn [type-to-use] (calculate-price-deltas (get-in supply-data [type-to-use]) (get-in demand-data [type-to-use]) (get-in surplus-data [type-to-use]))) categories)]
    (zipmap categories data-to-get)))

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

(defn augment-cc-in-db [ds include-pollutants?]
  (jdbc/with-transaction [tx ds]
    (jdbc/execute! tx ["UPDATE private_goods SET exponent = augment + exponent;"])
    (jdbc/execute! tx ["UPDATE public_goods SET exponent = augment + exponent;"])
    (when include-pollutants?
      (jdbc/execute! tx ["UPDATE pollutant_permissions SET exponent = augment + exponent;"]))))

(defn augmented-reset [t]
  (assoc t :iteration 0
           :ccs (mapv augment-cc (get t :ccs))
           :wcs (mapv augment-wc (get t :wcs))))
