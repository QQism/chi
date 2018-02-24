(ns chi.frontend.el.paragraph
  (:require [chi.frontend.node :as n]
            [chi.frontend.el.verse :as verse]))

(defn create [text]
  (n/create {:type :paragraph
             :children (verse/create-inline-markup text)}))
