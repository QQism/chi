(ns rst.preserve
  (:require [rst.node :as n]))

(defn create [text]
  (n/create {:type :preserve :value text}))
