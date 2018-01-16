(ns rst.sections-test
  (:require  [midje.sweet :refer :all]
             [rst.core :refer :all]))

(fact "Single Underline section with single line paragraph"
      (let [lines ["Section Title"
                   "============="
                   "Section paragraph content"]
            root (process-document lines)]
        root  => (contains {:type :root :children #(-> % count (= 1))})
        (let [section (-> root :children first)]
          section => (contains {:type :section
                                :style "underline="
                                :children #(-> % count (= 2))})
          (let [[header paragraph] (:children section)]
            header => (contains
                       {:type :header
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "Section Title"})])})
            paragraph => (contains
                          {:type :paragraph
                           :children (just
                                      [(contains
                                        {:type :text
                                         :value "Section paragraph content"})])})))))

(fact "Two sections with single line paragraphs"
      (let [lines ["First Title"
                   "==========="
                   "First paragraph content"
                   ""
                   "Second Title"
                   "============"
                   "Second paragraph content"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 2))})
        (let [[first-section second-section] (:children root)]
          first-section => (contains {:type :section :style "underline=" :children #(-> % count (= 2))})
          second-section => (contains {:type :section :style "underline=" :children #(-> % count (= 2))})
          (let [[first-header first-paragraph] (-> first-section :children)
                [second-header second-paragraph] (-> second-section :children)]
            first-header => (contains
                             {:type :header
                              :children (just
                                         [(contains
                                           {:type :text
                                            :value "First Title"})])})
            first-paragraph => (contains
                                {:type :paragraph
                                 :children (just
                                            [(contains
                                              {:type :text
                                               :value "First paragraph content"})])})
            second-header => (contains
                              {:type :header
                               :children (just
                                          [(contains
                                            {:type :text
                                             :value "Second Title"})])})
            second-paragraph => (contains
                                 {:type :paragraph
                                  :children (just
                                             [(contains
                                               {:type :text
                                                :value "Second paragraph content"})])})))))


