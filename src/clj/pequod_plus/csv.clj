(ns pequod-plus.csv
    (:require [pequod-plus.ccs-csv :as c]
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
                                    );"]
    (do
      (jdbc/execute! ds [ccs-table])
      (jdbc/execute! ds [private-goods-table])
      (jdbc/execute! ds [public-goods-table])
      (jdbc/execute! ds [pollutant-permissions-table]))))

(defn import-data-into-ccs-tables [db]
  (shell/sh "sqlite3" db
    ".mode csv"
    ".import resources/ccs.csv ccs"
    ".import resources/private_goods.csv private_goods"
    ".import resources/public_goods.csv public_goods"
    ".import resources/pollutant_permissions.csv pollutant_permissions"
    "CREATE INDEX idx_private_cc_id ON private_goods(cc_id);"
    "CREATE INDEX idx_private_good_id ON private_goods(good_id);"
    "CREATE INDEX idx_public_cc ON public_goods(cc_id);"
    "CREATE INDEX idx_public_good_id ON public_goods(good_id);"
    "CREATE INDEX idx_pp_cc_id ON pollutant_permissions(cc_id);"
    "CREATE INDEX idx_pp_pollutant_id ON pollutant_permissions(pollutant_id);"
    "CREATE INDEX idx_ccs ON ccs(id);"
    ".quit"))

(defn -main []
  (let [db "pequod-csv-test.db"
        ds (jdbc/get-datasource {:dbtype "sqlite" :dbname db})]
    (do
      (c/create-all-ccs-csv-files)
      (create-normalized-ccs-tables ds)
      (import-data-into-ccs-tables db))))

; time lein run -m pequod-plus.csv
