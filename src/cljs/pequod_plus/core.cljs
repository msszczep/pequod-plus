(ns pequod-plus.core
  (:require
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [reagent.session :as session]
   [reitit.frontend :as reitit]
   [clerk.core :as clerk]
   [accountant.core :as accountant]
   [goog.string :as gstring]
   [pequod-plus.util :as util]
   [pequod-plus.ppex001 :as ppex001]))

;; -----
;; Pequod Proper

; TODO : De-atom-ize this
; TODO : Add time-travelling

(def globals
  (atom {:init-private-good-price 700
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
         :iteration                0}))

(defn iterate-plan [t]
  (let [wcs (mapv (partial util/proposal (:price-data t)) (:wcs t))
        ccs (mapv (partial util/consume (t :private-goods) (t :public-good-types) (t :pollutant-types) (count (t :ccs)) (get-in t [:price-data])) (t :ccs))
        price-data (util/update-surpluses-prices wcs ccs (:natural-resources-supply t) (:labor-supply t) (:price-data t))
        surplus-data (util/get-pricing-data price-data :surplus)
        supply-data (util/get-pricing-data price-data :supply)
        demand-data (util/get-pricing-data price-data :demand)
        price-deltas (util/get-pricing-data price-data :price-delta-to-use)
        pd-list (util/get-pricing-data price-data :pd)
        percent-surplus (util/update-percent-surplus supply-data demand-data surplus-data)
        threshold-report (util/report-threshold supply-data demand-data surplus-data)
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

; TODO feed "experiment" argument into :ccs and :wcs
(defn setup [t _ experiment]
  (let [intermediate-inputs (vec (range 1 (inc (t :intermediate-inputs))))
        nature-types (vec (range 1 (inc (t :resources))))
        labor-types (vec (range 1 (inc (t :labors))))
        private-goods (vec (range 1 (inc (t :private-goods))))
        public-good-types (vec (range 1 (inc (t :public-goods))))
        pollutant-types (vec (range 1 (inc (t :pollutants))))]
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
               :ccs (util/add-ids
                     (case @experiment
                       "ppex001" ppex001/ccs))
               :wcs (util/add-ids
                     (case @experiment
                       "ppex001" ppex001/wcs))))))

(defn truncate-number [n]
  (if (nil? n)
    "0.000"
    (gstring/format "%.3f" n)))

(defn show-color [threshold-report-excerpt]
  (let [tre threshold-report-excerpt
        red "#ff4d4d"]
    (cond (empty? tre) red
          (every? #(< % 3) tre) "#4dd2ff"
          (every? #(< % 5) tre) "lawngreen"
          (every? #(< % 10) tre) "gold"
          (every? #(< % 20) tre) "darkorange"
          :else red)))

(defn all-buttons []
  (let [experiment-to-use (atom "ppex001")]
    [:div
     [:table
       [:tr
         [:td [:select {:field :list
               :id :experiment
               :on-change #(reset! experiment-to-use (-> % .-target .-value))}
          [:option {:key :ppex001} "ppex001"]
          ]]
         [:td [:input {:type "button" :value "Setup"
              :on-click #(swap! globals setup globals experiment-to-use)}]]
         [:td [:input {:type "button" :value "Iterate"
           :on-click #(swap! globals iterate-plan globals)}]]
         [:td [:input {:type "button" :value "Augmented reset"
           :on-click #(swap! globals util/augmented-reset globals)}]]
         ]
        [:tr
         [:td (str "WCs: " (count (get @globals :wcs)))]
         [:td (str "CCs: " (count (get @globals :ccs)))]]]]))

(defn show-globals []
    (let [td-cell-style {:border "1px solid #ddd" :text-align "center" :vertical-align "middle" :padding "8px"}
          threshold-to-use (get @globals :threshold-report)
          percent-surplus (get @globals :percent-surplus)
          price-data (get-in @globals [:price-data])]
     [:div [:h4 "Welcome to pequod-plus"]
           (all-buttons)
           [:p]
           [:table {:style {:width "100%" :padding "8px" :border "1px solid #ddd"}}
             [:tr
               [:th {:style td-cell-style} "Iteration: " (get @globals :iteration)]
               [:th {:style td-cell-style} "Private Goods"]
               [:th {:style td-cell-style} "Intermediate Inputs"]
               [:th {:style td-cell-style} "Nature"]
               [:th {:style td-cell-style} "Labor"]
               [:th {:style td-cell-style} "Public Goods"]
               [:th {:style td-cell-style} "Pollutants"]
             ]
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Prices"]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :price) (:private-goods price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :price) (:intermediate-inputs price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :price) (:nature price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :price) (:labor price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :price) (:public-goods price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :price) (:pollutants price-data))) "")]
             ]
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Price Deltas"]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :price-delta) (:private-goods price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :price-delta) (:intermediate-inputs price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :price-delta) (:nature price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :price-delta) (:labor price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :price-delta) (:public-goods price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :price-delta) (:pollutants price-data))) "")]
             ]

             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Supply"]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :supply) (:private-goods price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :supply) (:intermediate-inputs price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :supply) (:nature price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :supply) (:labor price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :supply) (:public-goods price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :supply) (:pollutants price-data))) "")]
             ]
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Demand"]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :demand) (:private-goods price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :demand) (:intermediate-inputs price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :demand) (:nature price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :demand) (:labor price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :demand) (:public-goods price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :demand) (:pollutants price-data))) "")]
             ]
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Surplus"]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :surplus) (:private-goods price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :surplus) (:intermediate-inputs price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :surplus) (:nature price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :surplus) (:labor price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :surplus) (:public-goods price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv (comp truncate-number :surplus) (:pollutants price-data))) "")]
             ]
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Percent Surplus / Threshold Met?"]
              [:td {:style (assoc td-cell-style :background (show-color (:private-goods threshold-to-use)))}
                   (str (:private-goods threshold-to-use))]
              [:td {:style (assoc td-cell-style :background (show-color (:intermediate-inputs threshold-to-use)))}
                   (str (:intermediate-inputs threshold-to-use))]
              [:td {:style (assoc td-cell-style :background (show-color (:nature threshold-to-use)))}
                   (str (:nature threshold-to-use))]
              [:td {:style (assoc td-cell-style :background (show-color (:labor threshold-to-use)))}
                   (str (:labor threshold-to-use))]
              [:td {:style (assoc td-cell-style :background (show-color (:public-goods threshold-to-use)))}
                   (str (:public-goods threshold-to-use))]
              [:td {:style (assoc td-cell-style :background (show-color (:pollutants threshold-to-use)))}
                   (str (:pollutants threshold-to-use))]]
]]))

;; -------------------------
;; Routes

(def router
  (reitit/router
   [["/" :index]]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

;; -------------------------
;; Page components

(defn home-page []
  (show-globals))

;; -------------------------
;; Translate routes -> page components

(defn page-for [route]
  (case route
    :index #'home-page))

;; -------------------------
;; Page mounting component

(defn current-page []
  (fn []
    (let [page (:current-page (session/get :route))]
      [:div
       [page]
       [:footer
        [:p "pequod-plus was generated by the "
         [:a {:href "https://github.com/reagent-project/reagent-template"} "Reagent Template"] "."]]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (rdom/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (clerk/initialize!)
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (let [match (reitit/match-by-path router path)
            current-page (:name (:data  match))
            route-params (:path-params match)]
        (reagent/after-render clerk/after-render!)
        (session/put! :route {:current-page (page-for current-page)
                              :route-params route-params})
        (clerk/navigate-page! path)
        ))
    :path-exists?
    (fn [path]
      (boolean (reitit/match-by-path router path)))})
  (accountant/dispatch-current!)
  (mount-root))

