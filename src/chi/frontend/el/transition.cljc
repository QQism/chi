(ns chi.frontend.el.transition
  (:require [chi.frontend.node :as n]))

(defn create []
  (n/create {:type :transition}))
