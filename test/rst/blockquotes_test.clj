(ns rst.blockquotes-test
  (:require  [midje.sweet :refer :all]
             [rst.core :refer :all]))

(fact "Parse a simple blockquotes"
      (let [lines ["Lorem Ipsum is simply dummy text"
                   ""
                   "  Lorem Ipsum is simply dummy"
                   "  's standard dummy text ever"
                   ""
                   "  Lorem Ipsum has been the industry's standard"
                   ""
                   ""]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 2))})
        (let [[paragraph blockquote] (:children root)]
          paragraph => (contains
                        {:type :paragraph
                         :children (just
                                    [(contains
                                      {:type :text
                                       :value "Lorem Ipsum is simply dummy text"})])})
          blockquote => (contains
                         {:type :blockquotes
                          :indent 2
                          :children #(-> % count (= 2))})
          (let [[paragraph-1st paragraph-2nd] (:children blockquote)]
            paragraph-1st => (contains
                              {:type :paragraph
                               :children (just
                                          [(contains
                                            {:type :text
                                             :value "Lorem Ipsum is simply dummy 's standard dummy text ever"})])})
            paragraph-2nd => (contains
                              {:type :paragraph
                               :children (just
                                          [(contains
                                            {:type :text
                                             :value "Lorem Ipsum has been the industry's standard"})])})))))
