(ns chi.frontend.el.blockquote
  (:require [chi.frontend.node :as n]
            [chi.frontend.el.verse :as verse]))


(defn create [indent]
  (n/create {:type :blockquote
             :indent indent
             :children []}))
