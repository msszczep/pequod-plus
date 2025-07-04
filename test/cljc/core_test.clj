(ns ppc.core-test
  (:require [clojure.test :refer :all]
            [ppc.core :refer :all]))

(deftest assorted-core-tests
  (testing "Testing core"
    (let [g2 (setup globals)
          price-data (:price-data g2)
          proposal-test-partial (->> g2
                                    :wcs
                                    (mapv (partial proposal-revised price-data)))
          consume-test-partial (->> g2
                                    :ccs
                                    (mapv (partial consume-revised (g2 :private-goods) (g2 :public-good-types) (g2 :pollutant-types) (count (g2 :ccs)) (get-in g2 [:price-data]))))
          consume-test-partial-singleton (->> g2
                                              :ccs
                                              first
                                              (consume-revised (g2 :private-goods) (g2 :public-good-types) (g2 :pollutant-types) (count (g2 :ccs)) (get-in g2 [:price-data])))
          iterate-plan-result (iterate-plan (assoc g2 :wcs proposal-test-partial :ccs consume-test-partial))
          proposal-test-result (->> proposal-test-partial
                                    (map keys)
                                    flatten
                                    frequencies)
          uspr (update-surpluses-prices-revised (assoc g2 :wcs proposal-test-partial))
          ]
      (is (false? (empty? globals)))

      (is (= 30 (count (get-in g2 [:wcs]))))
      (is (= 30 (count (get-in g2 [:ccs]))))

      (is (not= (:wcs g2) (:wcs iterate-plan-result)))
      (is (not= (:ccs g2) (:ccs iterate-plan-result)))
      (is (not= (:price-data g2) (:price-data iterate-plan-result)))
      
      (is (not (empty? (:surplus-data iterate-plan-result))))
      (is (not (empty? (:supply-data iterate-plan-result))))
      (is (not (empty? (:demand-data iterate-plan-result))))
      (is (not (empty? (:price-delta-data iterate-plan-result))))
      (is (not (empty? (:pd-data iterate-plan-result))))
      (is (not (empty? (:percent-surplus iterate-plan-result))))
      (is (not (empty? (:threshold-report iterate-plan-result))))
      (is (pos? (:iteration iterate-plan-result)))

      (is (= [:ccs :demand-data :init-intermediate-price :init-labor-price :init-nature-price :init-pollutant-price :init-private-good-price :init-public-good-price :intermediate-inputs :iteration :labor-supply :labor-types :labors :natural-resources-supply :nature-types :pollutant-types :pollutants :price-data :private-goods :public-good-types :public-goods :resources :supply-data :surplus-data :threshold-report :wcs] (sort (keys g2))))
      (is (= [:disutility-of-effort :effort :effort-elasticity :id :industry :intermediate-input-quantities :intermediate-inputs :labor :labor-quantities :nature :nature-quantities :output :pollutant-quantities :pollutants :product :total-factor-productivity] (sort (keys proposal-test-result))))

      (is (= [30] (distinct (vals proposal-test-result))))

      (is (= [10 10 10 10 1 1] (mapv count (vals price-data))))
      (is (= [:intermediate-goods :labor :nature :pollutants :private-goods :public-goods] (sort (keys price-data))))
      (is (= [[:disutility-of-effort :effort :effort-elasticity :id :industry :intermediate-input-quantities :intermediate-inputs :labor :labor-quantities :nature :nature-quantities :output :pollutant-quantities :pollutants :product :total-factor-productivity]] (distinct (mapv (comp sort keys) proposal-test-partial))))
      (is (< 0 (apply + (map :output proposal-test-partial))))
      (is (= 30 (count proposal-test-partial)))
      (is (= [:cohort :id :income :pollutant-permissions :pollutant-utilities :private-goods :public-goods] (sort (keys consume-test-partial-singleton))))
      (is (= [10 1 1] (map count (vals (select-keys consume-test-partial-singleton [:private-goods :public-goods :pollutant-permissions])))))
;       (is (= [] uspr))
)))


