(ns pequod-plus.prod
  (:require [pequod-plus.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
