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
   [pequod-plus.ppex002 :as ppex002]
   [pequod-plus.ppex003 :as ppex003]))

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
  (let [wcs (mapv (partial proposal (:price-data t)) (:wcs t))
        ccs (mapv (partial consume (t :private-goods) (t :public-good-types) (t :pollutant-types) (count (t :ccs)) (get-in t [:price-data])) (t :ccs))
        price-data (update-surpluses-prices t)
        surplus-data (get-pricing-data price-data :surplus)
        supply-data (get-pricing-data price-data :supply)
        demand-data (get-pricing-data price-data :demand)
        price-deltas (get-pricing-data price-data :price-delta-to-use)
        pd-list (get-pricing-data price-data :pd)
        percent-surplus (update-percent-surplus supply-data demand-data surplus-data)
        threshold-report (report-threshold supply-data demand-data surplus-data)
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
                     (case @experiment
                       "ppex001" ppex001/ccs
                       "ppex002" ppex002/ccs
                       "ppex003" ppex003/ccs))
               :wcs (add-ids
                     (case @experiment
                       "ppex001" ppex001/wcs
                       "ppex002" ppex002/wcs
                       "ppex003" ppex003/wcs))))))

(defn truncate-number [n]
  (gstring/format "%.3f" n))

(defn uncreative-divvy [s]
  (vector (take 10 s)
          (take 10 (drop 10 s))
          (take 10 (drop 20 s))
          (take 10 (drop 30 s))
          (take 1 (drop 40 s))
          (take 1 (drop 41 s))))

(defn divvy-up [seq-to-use]
  (if (empty? seq-to-use)
    seq-to-use
    (->> seq-to-use
         flatten
         (mapv truncate-number)
         uncreative-divvy
         (mapv (partial into [])))))

; handle empty threshold: app breaks if taking nth from an empty sequence
(defn het [s n]
  (if (empty? s)
    s
    (nth s n)))

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
          [:option {:key :ppex002} "ppex002"]
          [:option {:key :ppex003} "ppex003"]
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
          pdlist-to-use (divvy-up (get @globals :pd-data))
          supply-list-to-use (divvy-up (get @globals :supply-data))
          demand-list-to-use (divvy-up (get @globals :demand-list))
          surplus-list-to-use (divvy-up (get @globals :surplus-data))
          threshold-to-use (divvy-up (get @globals :threshold-report))
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
              [:td {:style td-cell-style} (or (str (mapv truncate-number (:private-goods price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv truncate-number (:intermediate-goods price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv truncate-number (:nature price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv truncate-number (:labor price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv truncate-number (:public-goods price-data))) "")]
              [:td {:style td-cell-style} (or (str (mapv truncate-number (:pollutants price-data))) "")]
             ]
             ; TODO : restore new deltas?
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Price Deltas"]
              [:td {:style td-cell-style} (str (or (nth pdlist-to-use 0) "[]"))]
              [:td {:style td-cell-style} (str (or (nth pdlist-to-use 1) "[]"))]
              [:td {:style td-cell-style} (str (or (nth pdlist-to-use 2) "[]"))]
              [:td {:style td-cell-style} (str (or (nth pdlist-to-use 3) "[]"))]
              [:td {:style td-cell-style} (str (or (nth pdlist-to-use 4) "[]"))]
              [:td {:style td-cell-style} (str (or (nth pdlist-to-use 5) "[]"))]
             ]
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Supply"]
              [:td {:style td-cell-style} (str (or (nth supply-list-to-use 0) "[]"))]
              [:td {:style td-cell-style} (str (or (nth supply-list-to-use 1) "[]"))]
              [:td {:style td-cell-style} (str (or (nth supply-list-to-use 2) "[]"))]
              [:td {:style td-cell-style} (str (or (nth supply-list-to-use 3) "[]"))]
              [:td {:style td-cell-style} (str (or (nth supply-list-to-use 4) "[]"))]
              [:td {:style td-cell-style} (str (or (nth supply-list-to-use 5) "[]"))]
             ]
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Demand"]
              [:td {:style td-cell-style} (str (or (nth demand-list-to-use 0) "[]"))]
              [:td {:style td-cell-style} (str (or (nth demand-list-to-use 1) "[]"))]
              [:td {:style td-cell-style} (str (or (nth demand-list-to-use 2) "[]"))]
              [:td {:style td-cell-style} (str (or (nth demand-list-to-use 3) "[]"))]
              [:td {:style td-cell-style} (str (or (nth demand-list-to-use 4) "[]"))]
              [:td {:style td-cell-style} (str (or (nth demand-list-to-use 5) "[]"))]
             ]
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Surplus"]
              [:td {:style td-cell-style} (str (or (nth surplus-list-to-use 0) "[]"))]
              [:td {:style td-cell-style} (str (or (nth surplus-list-to-use 1) "[]"))]
              [:td {:style td-cell-style} (str (or (nth surplus-list-to-use 2) "[]"))]
              [:td {:style td-cell-style} (str (or (nth surplus-list-to-use 3) "[]"))]
              [:td {:style td-cell-style} (str (or (nth surplus-list-to-use 4) "[]"))]
              [:td {:style td-cell-style} (str (or (nth surplus-list-to-use 5) "[]"))]
             ]
             [:tr {:style {:border "1px solid #ddd"}}
              [:td {:style (assoc td-cell-style :font-weight "bold")} "Percent Surplus / Threshold Met?"]
              [:td {:style (assoc td-cell-style :background (show-color (het threshold-to-use 0)))}
                   (str (het threshold-to-use 0))]
              [:td {:style (assoc td-cell-style :background (show-color (het threshold-to-use 1)))}
                   (str (het threshold-to-use 1))]
              [:td {:style (assoc td-cell-style :background (show-color (het threshold-to-use 2)))}
                   (str (het threshold-to-use 2))]
              [:td {:style (assoc td-cell-style :background (show-color (het threshold-to-use 3)))}
                   (str (het threshold-to-use 3))]
              [:td {:style (assoc td-cell-style :background (show-color (het threshold-to-use 4)))}
                   (str (het threshold-to-use 4))]
              [:td {:style (assoc td-cell-style :background (show-color (het threshold-to-use 5)))}
                   (str (het threshold-to-use 5))]]]]))

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

