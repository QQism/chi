(ns rst.paragraph
  (:require [rst.node :as n]
            [rst.verse :as verse]))

(defn create [text]
  (n/create {:type :paragraph
             :children (verse/create-inline-markup text)}))
