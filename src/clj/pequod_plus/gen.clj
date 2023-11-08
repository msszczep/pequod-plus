(ns pequod-plus.gen
    (:require [pequod-plus.ccs :as ccs]
              [pequod-plus.wcs :as wcs]))

; lein run -m pequod-plus.gen NS-TO-USE
; cc big: 3000 10 1 1
; cc small: 30 10 1 1

; wcs big: 1000 1000 1000
; wcs small: 10 10 10


(defn -main [& [ns-to-use]]
  (println (format "(ns pequod-plus.%s)" ns-to-use))
  (println "")
  (println "(def ccs ")
  (clojure.pprint/pprint (ccs/create-ccs-bulk 30 10 1 1))
  (println ")")
  (println "")
  (println "(def wcs ")
  (clojure.pprint/pprint (wcs/create-wcs-bulk 10 10 10))
  (println ")")
)

