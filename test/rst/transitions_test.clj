(ns rst.transitions-test
  (:require  [midje.sweet :refer :all]
             [rst.core :refer :all]))

(fact "Line is longer than 4 char => Transition between paragraphs"
      (let [lines ["Lorem Ipsum is simply dummy text of"
                   "the printing and typesetting industry."
                   ""
                   "===="
                   ""
                   "Lorem Ipsum has been the industry's standard..."]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 3))})
        (let [[first-paragraph transition second-paragraph] (:children root)]
          first-paragraph => (contains
                              {:type :paragraph
                               :children (just
                                          [(contains
                                            {:type :text
                                             :value "Lorem Ipsum is simply dummy text of the printing and typesetting industry."})])})
          transition => (contains {:type :transition})
          second-paragraph => (contains
                               {:type :paragraph
                                :children (just
                                           [(contains
                                             {:type :text
                                              :value "Lorem Ipsum has been the industry's standard..."})])}))))

(fact "Line is not longer than 4 char => Not Transition"
      (let [lines ["Lorem Ipsum is simply dummy text of"
                   "the printing and typesetting industry."
                   ""
                   "==="
                   ""
                   "Lorem Ipsum has been the industry's standard..."]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 3))})
        (let [[first-paragraph second-paragraph third-paragraph] (:children root)]
          first-paragraph => (contains
                              {:type :paragraph
                               :children (just
                                          [(contains
                                            {:type :text
                                             :value "Lorem Ipsum is simply dummy text of the printing and typesetting industry."})])})
          second-paragraph => (contains
                              {:type :paragraph
                               :children (just
                                          [(contains
                                            {:type :text
                                             :value "==="})])})
          third-paragraph => (contains
                               {:type :paragraph
                                :children (just
                                           [(contains
                                             {:type :text
                                              :value "Lorem Ipsum has been the industry's standard..."})])}))))

(fact "Document ends with a transition and line is longer than 4 char => Error"
      (let [lines ["Lorem Ipsum is simply dummy text of"
                   "the printing and typesetting industry."
                   ""
                   "===="
                   ""]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 3))})
        (let [[first-paragraph transition error] (:children root)]
          first-paragraph => (contains
                              {:type :paragraph
                               :children (just
                                          [(contains
                                            {:type :text
                                             :value "Lorem Ipsum is simply dummy text of the printing and typesetting industry."})])})
          transition => (contains {:type :transition})
          error => (contains
                    {:type :error
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Document may not end with a transition."})])})])}))))

(fact "Document ends with a transition and line is not longer than 4 char => paragraph"
      (let [lines ["Lorem Ipsum is simply dummy text of"
                   "the printing and typesetting industry."
                   ""
                   "==="
                   ""]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 2))})
        (let [[first-paragraph second-paragraph] (:children root)]
          first-paragraph => (contains
                              {:type :paragraph
                               :children (just
                                          [(contains
                                            {:type :text
                                             :value "Lorem Ipsum is simply dummy text of the printing and typesetting industry."})])})
          second-paragraph => (contains
                               {:type :paragraph
                                :children (just
                                           [(contains
                                             {:type :text
                                              :value "==="})])}))))
