(ns rst.blockquotes-test
  (:require  [midje.sweet :refer :all]
             [rst.core :refer :all]))

(fact "A simple blockquotes ends with a blank line"
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



(fact "A simple blockquotes ends without a blank line"
      (let [lines ["  Lorem Ipsum is simply dummy"
                   "  's standard dummy text ever"
                   ""
                   "  Lorem Ipsum has been the industry's standard"
                   "Lorem Ipsum is simply dummy text"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 3))})
        (let [[blockquote error paragraph] (:children root)]
          blockquote => (contains
                         {:type :blockquotes
                          :indent 2
                          :children #(-> % count (= 2))})
          error => (contains
                    {:type :error
                     :level 2
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Block quote ends without a blank line; unexpected unindent."})])})])})
          paragraph => (contains
                        {:type :paragraph
                         :children (just
                                    [(contains
                                      {:type :text
                                       :value "Lorem Ipsum is simply dummy text"})])}))))

(fact "A simple blockquotes ends with a blank line"
      (let [lines ["Lorem Ipsum is"
                   "simply dummy text"
                   "  Lorem Ipsum is simply dummy"
                   "  's standard dummy text ever"
                   ""
                   "  Lorem Ipsum has been the industry's standard"
                   ""
                   ""]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 3))})
        (let [[paragraph error blockquote] (:children root)]
          paragraph => (contains {:type :paragraph
                                  :children (just
                                             [(contains
                                               {:type :text
                                                :value "Lorem Ipsum is simply dummy text"})])})
          error => (contains
                    {:type :error
                     :level 3
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Unexpected indentation."})])})])})
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
                                             :value "Lorem Ipsum has been the industry's standard"})])}))
          )))

(fact "A simple blockquotes ends with no line"
      (let [lines ["  Lorem Ipsum is simply dummy"
                   "  's standard dummy text ever"
                   ""
                   "  Lorem Ipsum has been the industry's standard"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 1))})
        (let [[blockquote] (:children root)]
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
