(ns pequod-plus.gen
    (:require [pequod-plus.ccs :as ccs]
              [pequod-plus.wcs :as wcs]))

; lein run -m pequod-plus.gen NS-TO-USE

(defn -main [& [ns-to-use]]
  (println (format "(ns pequod-plus.%s)" ns-to-use))
  (println "")
  (println "(def ccs ")
  (clojure.pprint/pprint (ccs/create-ccs-bulk 30 10 10 10 10))
  (println ")")
  (println "(def wcs ")
  (clojure.pprint/pprint (wcs/create-wcs-bulk 10 10 10))
  (println ")")
)

