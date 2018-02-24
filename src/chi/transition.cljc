(ns rst.transition
  (:require [rst.node :as n]))

(defn create []
  (n/create {:type :transition}))
