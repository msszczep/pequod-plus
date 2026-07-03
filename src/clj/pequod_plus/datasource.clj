(ns pequod-plus.datasource
    (:require [pequod-plus.populate :as p]
              [next.jdbc :as jdbc]
              [clojure.java.shell :as shell]))

; lein run -m pequod-plus.gen NS-TO-USE
; cc big: 3000 10 1 1
; cc small: 30 10 1 1

; wcs big: 1000 1000 1000
; wcs small: 10 10 10

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
                                    );"]
    (do
      (jdbc/execute! ds [ccs-table])
      (jdbc/execute! ds [private-goods-table])
      (jdbc/execute! ds [public-goods-table])
      (jdbc/execute! ds [pollutant-permissions-table])
      (jdbc/execute! ds [private-good-prices-table])
      (jdbc/execute! ds [public-good-prices-table])
      (jdbc/execute! ds [pollutant-prices-table]))))

(defn create-normalized-wcs-tables [ds]
  (let [wcs-table "CREATE TABLE wcs (
                     id INTEGER PRIMARY KEY,
                     effort REAL,
                     industry INTEGER,
                     product INTEGER,
                     output REAL,
                     effort_elasticity REAL,
                     total_factor_productivity REAL,
                     disutility_of_effort_coefficient INTEGER,
                     disutility_of_effort_exponent REAL
                   );"
        nature-table "CREATE TABLE nature (
                        wc_id INTEGER,
                        nature_id INTEGER,
                        exponent REAL,
                        coefficient INT,
                        augment REAL,
                        PRIMARY KEY (wc_id, nature_id)
                      );"
        labor-table "CREATE TABLE labor (
                       wc_id INTEGER,
                       labor_id INTEGER,
                       exponent REAL,
                       coefficient INT,
                       augment REAL,
                       PRIMARY KEY (wc_id, labor_id)
                     );"
        pollutant-demands-table "CREATE TABLE pollutant_demands (
                                   wc_id INTEGER,
                                   pollutant_id INTEGER,
                                   exponent REAL,
                                   coefficient INT,
                                   augment REAL,
                                   PRIMARY KEY (wc_id, pollutant_id)
                                 );"
        intermediate-inputs-table "CREATE TABLE intermediate_inputs (
                                     wc_id INTEGER,
                                     intermediate_input_id INTEGER,
                                     exponent REAL,
                                     coefficient INT,
                                     augment REAL,
                                     PRIMARY KEY (wc_id, intermediate_input_id)
                                   );"
        nature-prices-table "CREATE TABLE nature_prices (
                               id INTEGER,
                               price REAL,
                               price_delta REAL,
                               price_delta_to_use REAL,
                               pd REAL,
                               supply REAL,
                               demand REAL,
                               surplus REAL
                             );"
        labor-prices-table "CREATE TABLE labor_prices (
                               id INTEGER,
                               price REAL,
                               price_delta REAL,
                               price_delta_to_use REAL,
                               pd REAL,
                               supply REAL,
                               demand REAL,
                               surplus REAL
                            );"
        ii-prices-table "CREATE TABLE intermediate_input_prices (
                           id INTEGER,
                           price REAL,
                           price_delta REAL,
                           price_delta_to_use REAL,
                           pd REAL,
                           supply REAL,
                           demand REAL,
                           surplus REAL
                         );"
       ]
    (do
      (jdbc/execute! ds [wcs-table])
      (jdbc/execute! ds [nature-table])
      (jdbc/execute! ds [labor-table])
      (jdbc/execute! ds [pollutant-demands-table])
      (jdbc/execute! ds [intermediate-inputs-table])
      (jdbc/execute! ds [nature-prices-table])
      (jdbc/execute! ds [labor-prices-table])
      (jdbc/execute! ds [ii-prices-table])
    )))

(defn import-data-into-ccs-tables [db]
  (shell/sh "sqlite3" db
    ".mode csv"
    ".import resources/ccs.csv ccs"
    ".import resources/private_goods.csv private_goods"
    ".import resources/public_goods.csv public_goods"
    ".import resources/pollutant_permissions.csv pollutant_permissions"
    ".import resources/private_good_prices.csv private_good_prices"
    ".import resources/public_good_prices.csv public_good_prices"
    ".import resources/pollutant_prices.csv pollutant_prices"
    "CREATE INDEX idx_private_cc_id ON private_goods(cc_id);"
    "CREATE INDEX idx_private_good_id ON private_goods(good_id);"
    "CREATE INDEX idx_public_cc ON public_goods(cc_id);"
    "CREATE INDEX idx_public_good_id ON public_goods(good_id);"
    "CREATE INDEX idx_pp_cc_id ON pollutant_permissions(cc_id);"
    "CREATE INDEX idx_pp_pollutant_id ON pollutant_permissions(pollutant_id);"
    "CREATE INDEX idx_ccs ON ccs(id);"
    "CREATE INDEX idx_private_good_prices ON private_good_prices(id)"
    "CREATE INDEX idx_public_good_prices ON public_good_prices(id)"
    "CREATE INDEX idx_pollutant_prices ON pollutant_prices(id)"
    ".quit"))

(defn import-data-into-wcs-tables [db]
  (shell/sh "sqlite3" db
    ".mode csv"
    ".import resources/wcs.csv wcs"
    ".import resources/nature.csv nature"
    ".import resources/labor.csv labor"
    ".import resources/pollutant_demands.csv pollutant_demands"
    ".import resources/intermediate_inputs.csv intermediate_inputs"
    ".import resources/nature_prices.csv nature_prices"
    ".import resources/labor_prices.csv labor_prices"
    ".import resources/intermediate_input_prices.csv intermediate_input_prices"
    "CREATE INDEX idx_nature_wc_id ON nature(wc_id);"
    "CREATE INDEX idx_nature_nature_id ON nature(nature_id);"
    "CREATE INDEX idx_labor_wc_id ON labor(wc_id);"
    "CREATE INDEX idx_labor_labor_id ON labor(labor_id);"
    "CREATE INDEX idx_pd_wc_id ON pollutant_demands(wc_id);"
    "CREATE INDEX idx_pd_pollutant_id ON pollutant_demands(pollutant_id);"
    "CREATE INDEX idx_ii_wc_id ON intermediate_inputs(wc_id);"
    "CREATE INDEX idx_ii_ii_id ON intermediate_inputs(intermediate_input_id);"
    "CREATE INDEX idx_wcs ON wcs(id);"
    "CREATE INDEX idx_nature_prices ON nature_prices(id)"
    "CREATE INDEX idx_labor_prices ON labor_prices(id)"
    "CREATE INDEX idx_ii_prices ON intermediate_input_prices(id)"
    ".quit"))

(defn -main []
  (let [db "pequod-csv-test.db"
        ds (jdbc/get-datasource {:dbtype "sqlite" :dbname db})]
    (do
      (p/create-all-ccs-csv-files)
      (p/create-all-wcs-csv-files)
      (create-normalized-ccs-tables ds)
      (create-normalized-wcs-tables ds)
      (import-data-into-ccs-tables db)
      (import-data-into-wcs-tables db))))

; time lein run -m pequod-plus.datasource
