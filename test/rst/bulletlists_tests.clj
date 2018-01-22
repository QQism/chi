(ns rst.bulletlists-tests
  (:require  [midje.sweet :refer :all]
             [rst.core :refer :all]))

(fact "A single line bullet"
      (let [lines ["- Lorem Ipsum is simply dummy text"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 1))})
        (let [list (-> root :children peek)
              item (-> list :children peek)]
          list => (contains {:type :bullet-list
                             :style "-"
                             :children #(-> % count (= 1))})
          item => (contains {:type :bullet-item
                             :children (just
                                        [(contains
                                          {:type :text
                                           :value "Lorem Ipsum is simply dummy text"})])}))))
