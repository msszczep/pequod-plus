(ns pequod-plus.doo-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [pequod-plus.core-test]))

(doo-tests 'pequod-plus.core-test)
