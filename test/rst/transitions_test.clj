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

(fact "Document may not begin with a transition."
      (let [lines ["===="
                   ""
                   "Lorem Ipsum is simply dummy text of"
                   "the printing and typesetting industry."]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 3))})
        (let [[error transition paragraph] (:children root)]
          error => (contains
                    {:type :error
                     :level 3
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Document or section may not begin with a transition."})])})])})
          transition => (contains {:type :transition})
          paragraph => (contains
                        {:type :paragraph
                         :children (just
                                    [(contains
                                      {:type :text
                                       :value "Lorem Ipsum is simply dummy text of the printing and typesetting industry."})])}))))

(fact "Section may not begin with a transition."
      (let [lines ["Section"
                   "======="
                   ""
                   "===="
                   ""
                   "Lorem Ipsum is simply dummy text of"
                   "the printing and typesetting industry."]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 1))})
        (let [[header error transition paragraph] (-> root :children first :children)]
          header => (contains {:type :header})
          error => (contains
                    {:type :error
                     :level 3
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Document or section may not begin with a transition."})])})])})
          transition => (contains {:type :transition})
          paragraph => (contains
                        {:type :paragraph
                         :children (just
                                    [(contains
                                      {:type :text
                                       :value "Lorem Ipsum is simply dummy text of the printing and typesetting industry."})])}))))

(fact "Adjacent transitions raise the error"
      (let [lines ["Lorem Ipsum is simply dummy text of"
                   ""
                   "======="
                   ""
                   "===="
                   ""
                   "Lorem Ipsum is simply dummy text of"
                   "the printing and typesetting industry."]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 5))})
        (let [[paragraph-1 transition-1 error transition-2 paragraph-2] (-> root :children)]
          paragraph-1 => (contains {:type :paragraph})
          transition-1 => (contains {:type :transition})
          error => (contains
                    {:type :error
                     :level 3
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "At least one body element must separate transitions; adjacent transitions are not allowed."})])})])})
          transition-2 => (contains {:type :transition})
          paragraph-2 => (contains {:type :paragraph}))))
