(ns rst.assert-macros
  (:require  [clojure.test :as t]
             [rst.test-support :refer [transform-cond-nodes]]))

(defmacro assert-node
  ([expected actual]
   `(assert-node ~expected ~actual nil))
  ([expected actual msg]
   `(let [expected# ~expected
          actual# ~actual
          msg# ~msg
          [cond-expected# cond-actual#] (transform-cond-nodes expected# actual#)]
      (t/is (= cond-expected# cond-actual#) msg#))))
