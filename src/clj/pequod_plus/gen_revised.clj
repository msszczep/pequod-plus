(ns pequod-plus.gen-revised
    (:require [pequod-plus.ccs-revised :as ccs]
              [pequod-plus.wcs-revised :as wcs]))

; time lein run -m pequod-plus.gen-revised ppex001 > ppex001.cljs
; cc big: 3000 10 1 1
; cc small: 30 10 1 1

; wcs big: 1000 1000 1000
; wcs small: 10 10 10


(defn -main [& [ns-to-use]]
  (println (format "(ns pequod-plus.%s)" ns-to-use))
  (println "")
  (println "(def ccs ")
  (clojure.pprint/pprint (ccs/create-ccs-bulk 30000 100 100 1))
  (println ")")
  (println "")
  (println "(def wcs ")
  (clojure.pprint/pprint (wcs/create-wcs-bulk 10000 10000 10000))
  (println ")")
)

