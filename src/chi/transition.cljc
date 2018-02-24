(ns chi.transition
  (:require [chi.node :as n]))

(defn create []
  (n/create {:type :transition}))
