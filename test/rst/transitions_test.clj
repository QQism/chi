(ns rst.transitions-test
  (:require  [midje.sweet :refer :all]
             [rst.core :refer :all]))

(fact "Transitions"
      (let [lines ["Lorem Ipsum is simply dummy text of"
                   "the printing and typesetting industry."
                   "==="
                   "Lorem Ipsum has been the industry's standard dummy text ever since..."]
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
                                              :value "Lorem Ipsum has been the industry's standard dummy text ever since..."})])}))))
