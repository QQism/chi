(ns rst.paragraph
  (:require [rst.node :as n]
            [rst.text :as text]))

(defn create [text]
  (n/create {:type :paragraph
             :children (text/create-inline-markup text)}))
