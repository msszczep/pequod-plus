(ns pequod-plus.core
  (:require
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [reagent.session :as session]
   [reitit.frontend :as reitit]
   [clerk.core :as clerk]
   [accountant.core :as accountant]))

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
         :public-goods              10
         :pollutants                10

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
  (let [t2 (assoc t :ccs (map (partial util/consume (t :private-goods) (t :private-good-prices) (t :public-good-types) (t :public-good-prices) (t :pollutants) (t :pollutant-prices) (count (t :ccs)))
                              (t :ccs))
                    :wcs (map (partial util/proposal (t :private-good-prices) (t :intermediate-good-prices) (t :nature-prices) (t :labor-prices) (t :public-good-prices) (t :pollutant-prices))
                              (t :wcs)))
        {private-good-prices :prices, private-good-surpluses :surpluses, private-good-new-deltas :new-deltas} (util/update-surpluses-prices "private-goods" (t2 :private-goods) (t2 :private-good-prices) (t2 :wcs) (t2 :ccs) (t2 :natural-resources-supply) (t2 :labor-supply) (t2 :pdlist) (last (t2 :private-goods)) (last (t2 :intermediate-inputs)) (t2 :resources) (t2 :labors) (t2 :pollutants))
        {intermediate-good-prices :prices, intermediate-good-surpluses :surpluses, intermediate-good-new-deltas :new-deltas} (util/update-surpluses-prices "intermediate" (t2 :intermediate-inputs) (t2 :intermediate-good-prices) (t2 :wcs) (t2 :ccs) (t2 :natural-resources-supply) (t2 :labor-supply) (t2 :pdlist) (last (t2 :private-goods)) (last (t2 :intermediate-inputs)) (t2 :resources) (t2 :labors) (t2 :pollutants))
        {nature-prices :prices, nature-surpluses :surpluses, nature-new-deltas :new-deltas} (util/update-surpluses-prices "nature" (t2 :nature-types) (t2 :nature-prices) (t2 :wcs) (t2 :ccs) (t2 :natural-resources-supply) (t2 :labor-supply) (t2 :pdlist) (last (t2 :private-goods)) (last (t2 :intermediate-inputs)) (t2 :resources) (t2 :labors) (t2 :pollutants))
        {labor-prices :prices, labor-surpluses :surpluses, labor-new-deltas :new-deltas} (util/update-surpluses-prices "labor" (t2 :labor-types) (t2 :labor-prices) (t2 :wcs) (t2 :ccs) (t2 :natural-resources-supply) (t2 :labor-supply) (t2 :pdlist) (last (t2 :private-goods)) (last (t2 :intermediate-inputs)) (t2 :resources) (t2 :labors) (t2 :pollutants))
        {public-good-prices :prices, public-good-surpluses :surpluses, public-good-new-deltas :new-deltas} (util/update-surpluses-prices "public-goods" (t2 :public-good-types) (t2 :public-good-prices) (t2 :wcs) (t2 :ccs) (t2 :natural-resources-supply) (t2 :labor-supply) (t2 :pdlist) (last (t2 :private-goods)) (last (t2 :intermediate-inputs)) (t2 :resources) (t2 :labors) (t2 :pollutants))
        {pollutant-prices :prices, pollutant-surpluses :surpluses, pollutant-new-deltas :new-deltas} (util/update-surpluses-prices "pollutant" (t2 :public-good-types) (t2 :public-good-prices) (t2 :wcs) (t2 :ccs) (t2 :natural-resources-supply) (t2 :labor-supply) (t2 :pdlist) (last (t2 :private-goods)) (last (t2 :intermediate-inputs)) (t2 :resources) (t2 :labors) (t2 :pollutants))
        surplus-list (vector private-good-surpluses intermediate-good-surpluses nature-surpluses labor-surpluses public-good-surpluses pollutant-surpluses)
        supply-list (util/get-supply-list t2)
        demand-list (util/get-demand-list t2)
        new-price-deltas (util/update-price-deltas supply-list demand-list surplus-list) ; TODO : Replace by aggregating above deltas?
        new-percent-surplus (util/update-percent-surplus supply-list demand-list surplus-list)
        threshold-report (util/report-threshold surplus-list supply-list demand-list)
        iteration (inc (:iteration t2))
        ]
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

(defn truncate-number [n]
  (gstring/format "%.3f" n))

(defn partition-by-ten
  [seq-to-use]
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

;; -------------------------
;; Routes

(def router
  (reitit/router
   [["/" :index]
    ["/items"
     ["" :items]
     ["/:item-id" :item]]
    ["/about" :about]]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

;; -------------------------
;; Page components

(defn home-page []
  (fn []
    [:span.main
     [:h1 "Welcome to pequod-plus"]
     [:ul
      [:li [:a {:href (path-for :items)} "Items of pequod-plus"]]
      [:li [:a {:href "/broken/link"} "Broken link"]]]]))



(defn items-page []
  (fn []
    [:span.main
     [:h1 "The items of pequod-plus"]
     [:ul (map (fn [item-id]
                 [:li {:name (str "item-" item-id) :key (str "item-" item-id)}
                  [:a {:href (path-for :item {:item-id item-id})} "Item: " item-id]])
               (range 1 60))]]))


(defn item-page []
  (fn []
    (let [routing-data (session/get :route)
          item (get-in routing-data [:route-params :item-id])]
      [:span.main
       [:h1 (str "Item " item " of pequod-plus")]
       [:p [:a {:href (path-for :items)} "Back to the list of items"]]])))


(defn about-page []
  (fn [] [:span.main
          [:h1 "About pequod-plus"]]))


;; -------------------------
;; Translate routes -> page components

(defn page-for [route]
  (case route
    :index #'home-page
    :about #'about-page
    :items #'items-page
    :item #'item-page))


;; -------------------------
;; Page mounting component

(defn current-page []
  (fn []
    (let [page (:current-page (session/get :route))]
      [:div
       [:header
        [:p [:a {:href (path-for :index)} "Home"] " | "
         [:a {:href (path-for :about)} "About pequod-plus"]]]
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

