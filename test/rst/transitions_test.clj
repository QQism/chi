(ns rst.transitions-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [rst.test-support :refer :all]
            [rst.core :refer [process-document]]))


(deftest transition-between-paragraphs
  (testing "Line is longer than 4 chars"
    (let [lines ["Lorem Ipsum is simply dummy text of"
                 "the printing and typesetting industry."
                 ""
                 "===="
                 ""
                 "Lorem Ipsum has been the industry's standard..."]
          root (process-document lines)]
      (assert-node {:type :root :children [:children-count 3 count]} root)

      (let [[paragraph-1 transition paragraph-2] (:children root)]
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value (str "Lorem Ipsum is simply dummy text of "
                                    "the printing and typesetting industry.")}]}
                     paragraph-1)
        (assert-node {:type :transition} transition)
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "Lorem Ipsum has been the industry's standard..."}]}
                     paragraph-2))))

  (testing "Line is not longer than 4 chars"
    (let [lines ["Lorem Ipsum is simply dummy text of"
                 "the printing and typesetting industry."
                 ""
                 "==="
                 ""
                 "Lorem Ipsum has been the industry's standard..."]
          root (process-document lines)]
      (assert-node {:type :root :children [:children-count 3 count]} root)

      (let [[paragraph-1 paragraph-2 paragraph-3] (:children root)]
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value (str "Lorem Ipsum is simply dummy text of "
                                    "the printing and typesetting industry.")}]}
                     paragraph-1)
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "==="}]}
                     paragraph-2)
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "Lorem Ipsum has been the industry's standard..."}]}
                     paragraph-3)))))

(deftest document-ends-with-transition
  (testing "Lines is longer than 4 chars"
    (let [lines ["Lorem Ipsum is simply dummy text of"
                 "the printing and typesetting industry."
                 ""
                 "===="
                 ""]
          root (process-document lines)]
      (assert-node {:type :root :children [:children-count 3 count]} root)

      (let [[paragraph transition error] (:children root)]
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value (str "Lorem Ipsum is simply dummy text of "
                                    "the printing and typesetting industry.")}]}
                     paragraph)
        (assert-node {:type :transition} transition)
        (assert-node {:type :error
                      :level 3
                      :pos [4 1]
                      :children
                      [{:type :paragraph
                        :children [{:type :text
                                    :value "Document may not end with a transition."}]}]}
                     error))))

  (testing "Lines is not longer than 4 chars"
    (let [lines ["Lorem Ipsum is simply dummy text of"
                 "the printing and typesetting industry."
                 ""
                 "==="
                 ""]
          root (process-document lines)]
      (assert-node {:type :root :children [:children-count 2 count]} root)

      (let [[paragraph-1 paragraph-2] (:children root)]
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value (str "Lorem Ipsum is simply dummy text of "
                                    "the printing and typesetting industry.")}]}
                     paragraph-1)
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "==="}]}
                     paragraph-2)))))


(deftest document-begins-with-transition
  (let [lines ["===="
               ""
               "Lorem Ipsum is simply dummy text of"
               "the printing and typesetting industry."]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 3 count]} root)
    (let [[error transition paragraph] (:children root)]
      (assert-node {:type :error
                    :level 3
                    :pos [1 1]
                    :children
                    [{:type :paragraph
                      :children
                      [{:type :text
                        :value (str "Document or section may not "
                                    "begin with a transition.")}]}]}
                   error)
      (assert-node {:type :transition} transition)

      (assert-node {:type :paragraph
                    :children
                    [{:type :text
                      :value (str "Lorem Ipsum is simply dummy text of "
                                  "the printing and typesetting industry.")}]}
                   paragraph))))

(deftest section-begins-with-transition
  (let [lines ["Section"
               "======="
               ""
               "===="
               ""
               "Lorem Ipsum is simply dummy text of"
               "the printing and typesetting industry."]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 1 count]} root)

    (let [[section] (:children root)]
      (assert-node {:type :section
                    :style "underline="
                    :children [:children-count 4 count]} section)

      (let [[header error transition paragraph] (:children section)]
        (assert-node {:type :header} header)
        (assert-node {:type :error
                      :level 3
                      :pos [4 1]
                      :children
                      [{:type :paragraph
                        :children [{:type :text
                                    :value "Document or section may not begin with a transition."}]}]}
                     error)
        (assert-node {:type :transition} transition)
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value (str "Lorem Ipsum is simply dummy text of "
                                    "the printing and typesetting industry.")}]}
                     paragraph)))))

(deftest adjacent-transitions
  (let [lines ["Lorem Ipsum is simply dummy text of"
               ""
               "======="
               ""
               "===="
               ""
               "Lorem Ipsum is simply dummy text of"
               "the printing and typesetting industry."]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 5 count]} root)

    (let [[paragraph-1 transition-1 error transition-2 paragraph-2] (:children root)]
      (assert-node {:type :paragraph} paragraph-1)
      (assert-node {:type :transition} transition-1)
      (assert-node {:type :error
                    :level 3
                    :pos [5 1]
                    :children
                    [{:type :paragraph
                      :children
                      [{:type :text
                        :value (str "At least one body element must separate transitions; "
                                    "adjacent transitions are not allowed.")}]}]}
                   error)
      (assert-node {:type :transition} transition-2)
      (assert-node {:type :paragraph} paragraph-2))))
