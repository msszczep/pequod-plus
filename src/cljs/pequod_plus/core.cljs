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
   [pequod-plus.ppex001 :as ppex001]
   [pequod-plus.ppex002 :as ppex002]))

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

         :private-goods             10
         :intermediate-inputs       10
         :resources                 10
         :labors                    10
         :public-goods              1
         :pollutants                1

         :private-good-prices      []
         :intermediate-good-prices []
         :labor-prices             []
         :nature-prices            []
         :public-good-prices       []
         :pollutant-prices         []

         :private-good-surpluses   []
         :intermediate-input-surpluses []
         :nature-surpluses         []
         :labor-surpluses          []
         :public-good-surpluses    []
         :pollutant-surpluses      []

         :threshold-report         []
         :price-deltas             []
         :wcs                      []
         :ccs                      []
         :iteration                0
         :natural-resources-supply 0
         :labor-supply             0}))

(defn iterate-plan [t]
  (let [t2 (assoc t :ccs (map (partial util/consume (t :private-goods) (t :private-good-prices) (t :public-good-types) (t :public-good-prices) (t :pollutant-types) (t :pollutant-prices) (count (t :ccs))) (t :ccs))
                    :wcs (map (partial util/proposal (t :private-good-prices) (t :intermediate-good-prices) (t :nature-prices) (t :labor-prices) (t :public-good-prices) (t :pollutant-prices)) (t :wcs)))
        {private-good-prices :prices, private-good-surpluses :surpluses, private-good-new-deltas :new-deltas} (util/update-surpluses-prices "private-goods" (t2 :private-goods) (t2 :private-good-prices) (t2 :wcs) (t2 :ccs) (t2 :natural-resources-supply) (t2 :labor-supply) (t2 :pdlist) (last (t2 :private-goods)) (last (t2 :intermediate-inputs)) (t2 :resources) (t2 :labors) (t2 :pollutants))
        {intermediate-good-prices :prices, intermediate-good-surpluses :surpluses, intermediate-good-new-deltas :new-deltas} (util/update-surpluses-prices "intermediate" (t2 :intermediate-inputs) (t2 :intermediate-good-prices) (t2 :wcs) (t2 :ccs) (t2 :natural-resources-supply) (t2 :labor-supply) (t2 :pdlist) (last (t2 :private-goods)) (last (t2 :intermediate-inputs)) (t2 :resources) (t2 :labors) (t2 :pollutants))
        {nature-prices :prices, nature-surpluses :surpluses, nature-new-deltas :new-deltas} (util/update-surpluses-prices "nature" (t2 :nature-types) (t2 :nature-prices) (t2 :wcs) (t2 :ccs) (t2 :natural-resources-supply) (t2 :labor-supply) (t2 :pdlist) (last (t2 :private-goods)) (last (t2 :intermediate-inputs)) (t2 :resources) (t2 :labors) (t2 :pollutants))
        {labor-prices :prices, labor-surpluses :surpluses, labor-new-deltas :new-deltas} (util/update-surpluses-prices "labor" (t2 :labor-types) (t2 :labor-prices) (t2 :wcs) (t2 :ccs) (t2 :natural-resources-supply) (t2 :labor-supply) (t2 :pdlist) (last (t2 :private-goods)) (last (t2 :intermediate-inputs)) (t2 :resources) (t2 :labors) (t2 :pollutants))
        {public-good-prices :prices, public-good-surpluses :surpluses, public-good-new-deltas :new-deltas} (util/update-surpluses-prices "public-goods" (t2 :public-good-types) (t2 :public-good-prices) (t2 :wcs) (t2 :ccs) (t2 :natural-resources-supply) (t2 :labor-supply) (t2 :pdlist) (last (t2 :private-goods)) (last (t2 :intermediate-inputs)) (t2 :resources) (t2 :labors) (t2 :pollutants))
        {pollutant-prices :prices, pollutant-surpluses :surpluses, pollutant-new-deltas :new-deltas} (util/update-surpluses-prices "pollutants" (t2 :pollutant-types) (t2 :pollutant-prices) (t2 :wcs) (t2 :ccs) (t2 :natural-resources-supply) (t2 :labor-supply) (t2 :pdlist) (last (t2 :private-goods)) (last (t2 :intermediate-inputs)) (t2 :resources) (t2 :labors) (t2 :pollutants))
        surplus-list (vector private-good-surpluses intermediate-good-surpluses nature-surpluses labor-surpluses public-good-surpluses pollutant-surpluses)
        supply-list (util/get-supply-list t2)
        demand-list (util/get-demand-list t2)
        new-price-deltas (util/update-price-deltas supply-list demand-list surplus-list) ; TODO : Replace by aggregating above deltas?
        new-percent-surplus (util/update-percent-surplus supply-list demand-list surplus-list)
        threshold-report (util/report-threshold surplus-list supply-list demand-list)
        iteration (inc (:iteration t2))]
    (assoc t2 :private-good-prices private-good-prices
              :private-good-surpluses private-good-surpluses
              :intermediate-good-prices intermediate-good-prices
              :intermediate-good-surpluses intermediate-good-surpluses
              :nature-prices nature-prices
              :nature-surpluses nature-surpluses
              :labor-prices labor-prices
              :labor-surpluses labor-surpluses
              :public-good-prices public-good-prices
              :public-good-surpluses public-good-surpluses
              :pollutant-prices pollutant-prices
              :pollutant-surpluses pollutant-surpluses
              :demand-list demand-list
              :surplus-list surplus-list
              :supply-list supply-list
              :threshold-report threshold-report
              :price-deltas new-price-deltas
              :pdlist new-percent-surplus
              :iteration iteration)))

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
                       "ppex001" ppex001/ccs
                       "ppex002" ppex002/ccs))
               :wcs (util/add-ids
                     (case @experiment
                       "ppex001" ppex001/wcs
                       "ppex002" ppex002/wcs))))))

(defn truncate-number [n]
  (gstring/format "%.3f" n))

(defn divvy-up [seq-to-use]
  (if (empty? seq-to-use)
    seq-to-use
    (->> seq-to-use
         flatten
         (mapv truncate-number)
         (partition-all 10)
         (mapv (partial into [])))))

(defn show-color [threshold-report-excerpt]
  (let [tre (first threshold-report-excerpt)
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
          [:option {:key :ppex002} "ppex002"]
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
    (let [td-cell-style {:border "1px solid #ddd" :text-align "center" :vertical-align "middle" :padding "8px"}] 
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
              [:td {:style td-cell-style} (or (str (mapv truncate-number (get @globals :private-good-prices))) "")]
              [:td {:style td-cell-style} (or (str (mapv truncate-number (get @globals :intermediate-good-prices))) "")]
              [:td {:style td-cell-style} (or (str (mapv truncate-number (get @globals :nature-prices))) "")]
              [:td {:style td-cell-style} (or (str (mapv truncate-number (get @globals :labor-prices))) "")]
              [:td {:style td-cell-style} (or (str (mapv truncate-number (get @globals :public-good-prices))) "")]
              [:td {:style td-cell-style} (or (str (mapv truncate-number (get @globals :pollutant-prices))) "")]
             ]
             ; TODO : restore new deltas?
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Price Deltas"]
              [:td {:style td-cell-style} (str (or (take 1 (divvy-up (get @globals :pdlist))) "[]"))]
              [:td {:style td-cell-style} (str (or (drop 1 (take 2 (divvy-up (get @globals :pdlist)))) "[]"))]
              [:td {:style td-cell-style} (str (or (drop 2 (take 3 (divvy-up (get @globals :pdlist)))) "[]"))]
              [:td {:style td-cell-style} (str (or (drop 3 (take 4 (divvy-up (get @globals :pdlist)))) "[]"))]
              [:td {:style td-cell-style} (str (or (vector (take 1 (drop 40 (get @globals :pdlist)))) "[]"))]
              [:td {:style td-cell-style} (str (or (vector (drop 41 (get @globals :pdlist))) "[]"))]
             ]
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Supply"]
              [:td {:style td-cell-style} (str (or (take 1 (divvy-up (get @globals :supply-list))) "[]"))]
              [:td {:style td-cell-style} (str (or (drop 1 (take 2 (divvy-up (get @globals :supply-list)))) "[]"))]
              [:td {:style td-cell-style} (str (or (drop 2 (take 3 (divvy-up (get @globals :supply-list)))) "[]"))]
              [:td {:style td-cell-style} (str (or (drop 3 (take 4 (divvy-up (get @globals :supply-list)))) "[]"))]
              [:td {:style td-cell-style} (str (or (vector (take 1 (drop 40 (get @globals :supply-list)))) "[]"))]
              [:td {:style td-cell-style} (str (or (vector (drop 41 (get @globals :supply-list))) "[]"))]
             ]
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Demand"]
              [:td {:style td-cell-style} (str (or (take 1 (divvy-up (get @globals :demand-list))) "[]"))]
              [:td {:style td-cell-style} (str (or (drop 1 (take 2 (divvy-up (get @globals :demand-list)))) "[]"))]
              [:td {:style td-cell-style} (str (or (drop 2 (take 3 (divvy-up (get @globals :demand-list)))) "[]"))]
              [:td {:style td-cell-style} (str (or (drop 3 (take 4 (divvy-up (get @globals :demand-list)))) "[]"))]
              [:td {:style td-cell-style} (str (or (vector (take 1 (drop 40 (get @globals :demand-list)))) "[]"))]
              [:td {:style td-cell-style} (str (or (vector (drop 41 (get @globals :demand-list))) "[]"))]
             ]
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Surplus"]
              [:td {:style td-cell-style} (str (or (take 1 (divvy-up (get @globals :surplus-list))) "[]"))]
              [:td {:style td-cell-style} (str (or (drop 1 (take 2 (divvy-up (get @globals :surplus-list)))) "[]"))]
              [:td {:style td-cell-style} (str (or (drop 2 (take 3 (divvy-up (get @globals :surplus-list)))) "[]"))]
              [:td {:style td-cell-style} (str (or (drop 3 (take 4 (divvy-up (get @globals :surplus-list)))) "[]"))]
              [:td {:style td-cell-style} (str (or (vector (take 1 (drop 40 (get @globals :surplus-list)))) "[]"))]
              [:td {:style td-cell-style} (str (or (vector (drop 41 (get @globals :surplus-list))) "[]"))]
             ]
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Percent Surplus / Threshold Met?"]
              [:td {:style (assoc td-cell-style :background (show-color (take 1 (divvy-up (get @globals :threshold-report)))))} (str (or (take 1 (divvy-up (get @globals :threshold-report))) "[]"))]
              [:td {:style (assoc td-cell-style :background (show-color (drop 1 (take 2 (divvy-up (get @globals :threshold-report))))))} (str (or (drop 1 (take 2 (divvy-up (get @globals :threshold-report)))) "[]"))]
              [:td {:style (assoc td-cell-style :background (show-color (drop 2 (take 3 (divvy-up (get @globals :threshold-report))))))} (str (or (drop 2 (take 3 (divvy-up (get @globals :threshold-report)))) "[]"))]
              [:td {:style (assoc td-cell-style :background (show-color (drop 3 (take 4 (divvy-up (get @globals :threshold-report))))))} (str (or (drop 3 (take 4 (divvy-up (get @globals :threshold-report)))) "[]"))]
              [:td {:style (assoc td-cell-style :background (show-color (vector (take 1 (drop 40 (get @globals :threshold-report))))))} (str (or (drop 4 (take 5 (divvy-up (get @globals :threshold-report)))) "[]"))]
              [:td {:style (assoc td-cell-style :background (show-color (vector (drop 41 (get @globals :threshold-report)))))} (str (or (drop 5 (take 6 (divvy-up (get @globals :threshold-report)))) "[]"))]]]]))

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

