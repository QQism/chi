(ns chi.frontend.el.preserve
  (:require [chi.frontend.node :as n]))

(defn create [text]
  (n/create {:type :preserve :value text}))
