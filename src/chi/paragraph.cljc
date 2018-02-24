(ns chi.paragraph
  (:require [chi.node :as n]
            [chi.verse :as verse]))

(defn create [text]
  (n/create {:type :paragraph
             :children (verse/create-inline-markup text)}))
