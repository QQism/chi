(ns rst.bulletlists-tests
  (:require  [midje.sweet :refer :all]
             [rst.core :refer :all]))

(fact "A single line bullet item"
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
(fact "A multi-line line bullet"
      (let [lines ["- Lorem Ipsum is simply dummy text"
                   ""
                   "  Lorem Ipsum is simply dummy's standard"]
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
                                          {:type :paragraph
                                           :children (just
                                                      [(contains
                                                        {:type :text
                                                         :value "Lorem Ipsum is simply dummy text"})])})
                                         (contains
                                          {:type :paragraph
                                           :children (just
                                                      [(contains
                                                        {:type :text
                                                         :value "Lorem Ipsum is simply dummy's standard"})])})])}))))
