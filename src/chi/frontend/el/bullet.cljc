(ns chi.frontend.el.bullet
  (:require [chi.frontend.node :as n]))

(defn create-list [style]
  (n/create {:type :bullet-list
             :style style
             :children []}))

(defn create-item [indent]
  (n/create {:type :bullet-item
             :indent indent
             :children []}))
