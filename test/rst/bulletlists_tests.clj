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

(fact "A single line bullet item and blockquote"
      (let [lines ["-   Lorem Ipsum is simply dummy text"
                   "  This is a separated blockquote"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 3))})
        (let [[list error blockquote] (-> root :children)
              item (-> list :children peek)]
          list => (contains {:type :bullet-list
                             :style "-"
                             :children #(-> % count (= 1))})
          item => (contains {:type :bullet-item
                             :children (just
                                        [(contains
                                          {:type :text
                                           :value "Lorem Ipsum is simply dummy text"})])})
          error => (contains
                    {:type :error
                     :level 2
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Bullet list ends without a blank line; unexpected unindent."})])})])})
          blockquote => (contains
                         {:type :blockquote
                          :indent 2
                          :children (just
                                     [(contains
                                       {:type :text
                                        :value "This is a separated blockquote"})])}))))
