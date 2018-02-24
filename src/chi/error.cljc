(ns chi.error
  (:require [chi.node :as n]
            [chi.frontend.el.preserve :as preserve]
            [chi.frontend.el.paragraph :as paragraph]))

(def ^:const levels {::debug    0
                     ::info     1
                     ::warning  2
                     ::error    3
                     ::severe   4})

(defn create
  ([description level pos]
   (n/create {:type :error
              :level (level levels)
              :pos (->> pos (map inc) vec)
              :children [(paragraph/create description)]}))
  ([description level pos block-text]
   (n/create {:type :error
              :level (level levels)
              :pos (->> pos (map inc) vec)
              :children [(paragraph/create description)
                         (preserve/create block-text)]})))
