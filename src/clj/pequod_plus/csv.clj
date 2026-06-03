(ns pequod-plus.csv
    (:require [pequod-plus.ccs-csv :as c]))

; lein run -m pequod-plus.gen NS-TO-USE
; cc big: 3000 10 1 1
; cc small: 30 10 1 1

; wcs big: 1000 1000 1000
; wcs small: 10 10 10


(defn -main []
  (c/create-all-ccs-csv-files))

