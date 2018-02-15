(ns rst.blockquotes-new-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [rst.test-support :refer :all]
            [rst.core :refer [process-document]]))


(deftest single-line-blockquote-ends-with-blank-line
  (let [lines ["Lorem Ipsum is simply dummy text"
               ""
               "  Lorem Ipsum is simply dummy"
               ""]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 2 count]} root)

    (let [[paragraph blockquote] (:children root)]
      (assert-node {:type :paragraph
                    :children
                    [{:type :text
                      :value "Lorem Ipsum is simply dummy text"}]}
                   paragraph)

      (assert-node {:type :blockquote
                    :indent 2
                    :children
                    [{:type :text
                      :value "Lorem Ipsum is simply dummy"}]}
                   blockquote))))

(deftest multi-line-blockquote-ends
  (testing "EOF"
    (let [lines ["Lorem Ipsum is simply dummy text"
                 ""
                 "  Lorem Ipsum is simply dummy"
                 "  's standard dummy text ever"
                 ""
                 "  Lorem Ipsum has been the industry's standard"]
          root (process-document lines)]
      (assert-node {:type :root :children [:children-count 2 count]} root)

      (let [[paragraph blockquote] (:children root)]
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "Lorem Ipsum is simply dummy text"}]}
                     paragraph)
        (assert-node {:type :blockquote
                      :indent 2
                      :children [:children-count 2 count]}
                     blockquote)

        (let [[paragraph-1 paragraph-2] (:children blockquote)]
          (assert-node {:type :paragraph
                        :children
                        [{:type :text
                          :value (str "Lorem Ipsum is simply dummy "
                                      "'s standard dummy text ever")}]}
                       paragraph-1)
          (assert-node {:type :paragraph
                        :children
                        [{:type :text
                          :value "Lorem Ipsum has been the industry's standard"}]}
                       paragraph-2)))))

  (testing "with a blank line then a paragraph"
    (let [lines ["  Lorem Ipsum is simply dummy"
                 "  's standard dummy text ever"
                 ""
                 "  Lorem Ipsum has been the industry's standard"
                 ""
                 "Lorem Ipsum is simply dummy text"]
          root (process-document lines)]
      (assert-node {:type :root :children [:children-count 2 count]} root)

      (let [[blockquote paragraph] (:children root)]
        (assert-node {:type :blockquote
                      :indent 2
                      :children [:children-count 2 count]}
                     blockquote)

        (let [[paragraph-1 paragraph-2] (:children blockquote)]
          (assert-node {:type :paragraph
                        :children
                        [{:type :text
                          :value (str "Lorem Ipsum is simply dummy "
                                      "'s standard dummy text ever")}]}
                       paragraph-1)
          (assert-node {:type :paragraph
                        :children
                        [{:type :text
                          :value "Lorem Ipsum has been the industry's standard"}]}
                       paragraph-2))

        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "Lorem Ipsum is simply dummy text"}]}
                     paragraph))))

  (testing "without a blank line then a paragraph"
    (let [lines ["  Lorem Ipsum is simply dummy"
                 "  's standard dummy text ever"
                 ""
                 "  Lorem Ipsum has been the industry's standard"
                 "Lorem Ipsum is simply dummy text"]
          root (process-document lines)]
      (assert-node {:type :root :children [:children-count 3 count]} root)

      (let [[blockquote error paragraph] (:children root)]
        (assert-node {:type :blockquote
                      :indent 2
                      :children [:children-count 2 count]}
                     blockquote)

        (let [[paragraph-1 paragraph-2] (:children blockquote)]
          (assert-node {:type :paragraph
                        :children
                        [{:type :text
                          :value (str "Lorem Ipsum is simply dummy "
                                      "'s standard dummy text ever")}]}
                       paragraph-1)
          (assert-node {:type :paragraph
                        :children
                        [{:type :text
                          :value "Lorem Ipsum has been the industry's standard"}]}
                       paragraph-2))

        (assert-node {:type :error
                      :level 2
                      :pos [5 1]
                      :children
                      [{:type :paragraph
                        :children
                        [{:type :text
                          :value (str "Block quote ends without a blank line; "
                                      "unexpected unindent.")}]}]}
                     error)
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "Lorem Ipsum is simply dummy text"}]}
                     paragraph)))))

(deftest multi-line-blockquote-starts-without-blank-line
  (let [lines ["Lorem Ipsum is"
               "simply dummy text"
               "  Lorem Ipsum is simply dummy"
               "  's standard dummy text ever"
               ""
               "  Lorem Ipsum has been the industry's standard"]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 3 count]} root)

    (let [[paragraph error blockquote] (:children root)]
      (assert-node {:type :paragraph
                    :children [{:type :text
                                :value "Lorem Ipsum is simply dummy text"}]}
                   paragraph)
      (assert-node {:type :error
                    :level 3
                    :pos [3 1]
                    :children [{:type :paragraph
                                :children [{:type :text
                                            :value "Unexpected indentation."}]}]}
                   error)
      (assert-node {:type :blockquote
                    :indent 2
                    :children [:children-count 2 count]}
                   blockquote)

      (let [[paragraph-1 paragraph-2] (:children blockquote)]
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value (str "Lorem Ipsum is simply dummy "
                                    "'s standard dummy text ever")}]}
                     paragraph-1)
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "Lorem Ipsum has been the industry's standard"}]}
                     paragraph-2)))))

(deftest multi-line-blockquote-with-unexpected-transition
  (let [lines ["  Lorem Ipsum is simply dummy"
               "  's standard dummy text ever"
               ""
               "  ===="]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 1 count]} root)

    (let [[blockquote] (:children root)]
      (assert-node {:type :blockquote
                    :indent 2
                    :children [:children-count 2 count]}
                   blockquote)

      (let [[paragraph error] (:children blockquote)]
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value (str "Lorem Ipsum is simply dummy "
                                    "'s standard dummy text ever")}]}
                     paragraph)
        (assert-node {:type :error
                      :level 4
                      :pos [4 3]
                      :children
                      [{:type :paragraph
                        :children
                        [{:type :text
                          :value "Unexpected section title or transition."}]}
                       {:type :preserve
                        :value "===="}]}
                     error)))))

(deftest blockquote-with-section-line
  (let [lines ["  Lorem Ipsum is simply dummy"
               "  's standard dummy text ever"
               ""
               "  Joe"
               "  ==="]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 1 count]} root)

    (let [[blockquote] (:children root)]
      (assert-node {:type :blockquote
                    :indent 2
                    :children [:children-count 2 count]}
                   blockquote)

      (let [[paragraph error] (:children blockquote)]
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value (str "Lorem Ipsum is simply dummy "
                                    "'s standard dummy text ever")}]}
                     paragraph)

        (assert-node {:type :error
                      :level 4
                      :pos [5 3]
                      :children
                      [{:type :paragraph
                        :children
                        [{:type :text
                          :value "Unexpected section title."}]}
                       {:type :preserve
                        :value "Joe\r\n==="}]}
                     error)))))
