(ns chi.frontend.bulletlists-test
  (:require #?(:cljs [cljs.test    :as t :refer-macros [deftest testing]]
               :clj  [clojure.test :as t :refer        [deftest testing]])
            #?(:cljs [chi.test-support :refer [assert-node]]
               :clj  [chi.assert-macros :refer [assert-node]])
            [chi.core :refer [process-document]]))

#?(:cljs (enable-console-print!))

(deftest single-line-bullet-item
  (let [lines ["- Lorem Ipsum is simply dummy text"]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 1 count]} root)

    (let [[list] (:children root)]
      (assert-node {:type :bullet-list
                    :style "-"
                    :children [:children-count 1 count]}
                   list)

      (let [[item] (:children list)]
        (assert-node {:type :bullet-item
                      :indent 2
                      :children
                      [{:type :text
                        :value "Lorem Ipsum is simply dummy text"}]}
                     item)))))

(deftest multi-line-bullet-item
  (let [lines ["- Lorem Ipsum is simply dummy text"
               ""
               "  Lorem Ipsum is simply dummy's standard"]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 1 count]} root)

    (let [[list] (:children root)]
      (assert-node {:type :bullet-list
                    :style "-"
                    :children [:children-count 1 count]}
                   list)

      (let [[item] (:children list)]
        (assert-node {:type :bullet-item
                      :indent 2
                      :children
                      [{:type :paragraph
                        :children
                        [{:type :text
                          :value "Lorem Ipsum is simply dummy text"}]}
                       {:type :paragraph
                        :children
                        [{:type :text
                          :value "Lorem Ipsum is simply dummy's standard"}]}]}
                     item)))))


(deftest single-line-bullet-list
  (let [lines ["- First item"
               "- Second item"]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 1 count]} root)

    (let [[list] (-> root :children)]
      (assert-node {:type :bullet-list
                    :style "-"
                    :children [:children-count 2 count]}
                   list)

      (let [[item-1 item-2] (:children list)]
        (assert-node {:type :bullet-item
                      :indent 2
                      :children
                      [{:type :text
                        :value "First item"}]}
                     item-1)
        (assert-node {:type :bullet-item
                      :indent 2
                      :children
                      [{:type :text
                        :value "Second item"}]}
                     item-2)))))


(deftest simple-bullet-list-and-paragraph
  (testing "without a blank line in between"
    (let [lines ["- First item"
                 "- Second item"
                 "Lorem Ipsum is simply dummy text"]
          root (process-document lines)]
      (assert-node {:type :root :children [:children-count 3 count]} root)

      (let [[list error paragraph] (:children root)]
        (assert-node {:type :bullet-list
                      :style "-"
                      :children [:children-count 2 count]}
                     list)

        (let [[item-1 item-2] (:children list)]
          (assert-node {:type :bullet-item
                        :children
                        [{:type :text
                          :value "First item"}]}
                       item-1)
          (assert-node {:type :bullet-item
                        :children
                        [{:type :text
                          :value "Second item"}]}
                       item-2))

        (assert-node {:type :error
                      :level 2
                      :pos [3 1]
                      :children
                      [{:type :paragraph
                        :children
                        [{:type :text
                          :value (str "Bullet list ends without a blank line; "
                                      "unexpected unindent.")}]}]}
                     error)
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "Lorem Ipsum is simply dummy text"}]}
                     paragraph))))

  (testing "with a blank line in between"
    (let [lines ["- First item"
                 "- Second item"
                 ""
                 "Lorem Ipsum is simply dummy text"]
          root (process-document lines)]
      (assert-node {:type :root :children [:children-count 2 count]} root)
      (let [[list paragraph] (:children root)]
        (assert-node {:type :bullet-list
                      :style "-"
                      :children [:children-count 2 count]}
                     list)

        (let [[item-1 item-2] (:children list)]
          (assert-node {:type :bullet-item
                        :indent 2
                        :children
                        [{:type :text
                          :value "First item"}]}
                       item-1)
          (assert-node {:type :bullet-item
                        :indent 2
                        :children
                        [{:type :text
                          :value "Second item"}]}
                       item-2))

        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "Lorem Ipsum is simply dummy text"}]}
                     paragraph)))))

(deftest multi-line-bullet-list
  (let [lines ["- First item"
               "- Second item"
               ""
               "  Lorem Ipsum is simply dummy's standard"]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 1 count]} root)

    (let [[list] (:children root)]
      (assert-node {:type :bullet-list
                    :style "-"
                    :children [:children-count 2 count]}
                   list)

      (let [[item-1 item-2] (:children list)]
        (assert-node {:type :bullet-item
                      :indent 2
                      :children
                      [{:type :text
                        :value "First item"}]}
                     item-1)
        (assert-node {:type :bullet-item
                      :indent 2
                      :children
                      [{:type :paragraph
                        :children [{:type :text
                                    :value "Second item"}]}
                       {:type :paragraph
                        :children [{:type :text
                                    :value "Lorem Ipsum is simply dummy's standard"}]}]}
                     item-2)))))

(deftest single-line-bullet-item-and-blockquote-without-blank-line
  (testing "without a blank line in between"
    (let [lines ["-   Lorem Ipsum is simply dummy text"
                 "  This is a separated blockquote"]
          root (process-document lines)]
      (assert-node {:type :root :children [:children-count 3 count]} root)

      (let [[list error blockquote] (:children root)]
        (assert-node {:type :bullet-list
                      :style "-"
                      :children [:children-count 1 count]}
                     list)

        (let [[item] (:children list)]
          (assert-node {:type :bullet-item
                        :indent 4
                        :children
                        [{:type :text
                          :value "Lorem Ipsum is simply dummy text"}]}
                       item))

        (assert-node {:type :error
                      :level 2
                      :pos [2 1]
                      :children
                      [{:type :paragraph
                        :children
                        [{:type :text
                          :value (str "Bullet list ends without a blank line; "
                                      "unexpected unindent.")}]}]}
                     error)
        (assert-node {:type :blockquote
                      :indent 2
                      :children
                      [{:type :text
                        :value "This is a separated blockquote"}]}
                     blockquote))))

  (testing "with a blank line in between"
    (let [lines ["-   Lorem Ipsum is simply dummy text"
                 ""
                 "  This is a separated blockquote"]
          root (process-document lines)]
      (assert-node {:type :root :children [:children-count 2 count]} root)

      (let [[list blockquote] (:children root)
            item (-> list :children peek)]
        (assert-node {:type :bullet-list
                      :style "-"
                      :children [:children-count 1 count]}
                     list)

        (let [[item] (:children list)]
          (assert-node {:type :bullet-item
                        :indent 4
                        :children
                        [{:type :text
                          :value "Lorem Ipsum is simply dummy text"}]}
                       item))

        (assert-node {:type :blockquote
                      :indent 2
                      :children
                      [{:type :text
                        :value "This is a separated blockquote"}]}
                     blockquote)))))

(deftest two-simple-adjacent-bullet-lists
  (testing "without a blank line in between"
    (let [lines ["- First item"
                 "- Second item"
                 "+ Another item"]
          root (process-document lines)]
      (assert-node {:type :root :children [:children-count 3 count]} root)

      (let [[list-1 error list-2] (:children root)]
        (assert-node {:type :bullet-list
                      :style "-"
                      :children [:children-count 2 count]}
                     list-1)
        (assert-node {:type :bullet-list
                      :style "+"
                      :children [:children-count 1 count]}
                     list-2)

        (let [[item-1 item-2] (:children list-1)]
          (assert-node {:type :bullet-item 
                        :indent 2
                        :children
                        [{:type :text
                          :value "First item"}]}
                       item-1)
          (assert-node {:type :bullet-item
                        :indent 2
                        :children
                        [{:type :text
                          :value "Second item"}]}
                       item-2))

        (assert-node {:type :error
                      :level 2
                      :pos [3 1]
                      :children
                      [{:type :paragraph
                        :children
                        [{:type :text
                          :value (str "Bullet list ends without a blank line; "
                                      "unexpected unindent.")}]}]}
                     error)

        (let [[item] (:children list-2)]
          (assert-node {:type :bullet-item
                        :indent 2
                        :children
                        [{:type :text
                          :value "Another item"}]}
                       item)))))
  (testing "with a blank line in between"
    (let [lines ["- First item"
                 "- Second item"
                 ""
                 "+ Another item"]
          root (process-document lines)]
      (assert-node {:type :root :children [:children-count 2 count]} root)

      (let [[list-1 list-2] (:children root)]
        (assert-node {:type :bullet-list
                      :style "-"
                      :children [:children-count 2 count]}
                     list-1)
        (assert-node {:type :bullet-list
                      :style "+"
                      :children [:children-count 1 count]}
                     list-2)

        (let [[item-1 item-2] (:children list-1)]
          (assert-node {:type :bullet-item 
                        :indent 2
                        :children
                        [{:type :text
                          :value "First item"}]}
                       item-1)
          (assert-node {:type :bullet-item
                        :indent 2
                        :children
                        [{:type :text
                          :value "Second item"}]}
                       item-2))

        (let [[item] (:children list-2)]
          (assert-node {:type :bullet-item
                        :indent 2
                        :children
                        [{:type :text
                          :value "Another item"}]}
                       item))))))

(deftest bullet-list-with-nested-section
      (let [lines ["- First item"
                   "- Second item"
                   "  ==========="]
            root (process-document lines)]
        (assert-node {:type :root :children [:children-count 1 count]} root)

        (let [[list] (-> root :children)]
          (assert-node {:type :bullet-list
                        :style "-"
                        :children [:children-count 2 count]}
                       list)

          (let [[item-1 item-2] (:children list)]
            (assert-node {:type :bullet-item
                          :indent 2
                          :children
                          [{:type :text
                            :value "First item"}]}
                         item-1)

            (assert-node {:type :bullet-item
                          :indent 2
                          :children [:children-count 1 count]}
                         item-2)

            (let [[error] (:children item-2)]
              (assert-node {:type :error
                            :level 4
                            :pos [3 3]
                            :children
                            [{:type :paragraph
                              :children
                              [{:type :text
                                :value "Unexpected section title."}]}
                             {:type :preserve
                              :value (str "Second item\r\n"
                                          "===========")}]}
                           error))))))

(deftest nested-bullet-list
  (let [lines ["- - First Item"
               "  - Second Item"
               "- Another Item"]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 1 count]} root)

    (let [[list] (:children root)]
      (assert-node {:type :bullet-list
                    :style "-"
                    :children [:children-count 2 count]}
                   list)

      (let [[item-1 item-2] (:children list)]
        (assert-node {:type :bullet-item
                      :indent 2
                      :children [:children-count 1 count]}
                     item-1)

        (assert-node {:type :bullet-item
                      :indent 2
                      :children
                      [{:type :text
                        :value "Another Item"}]}
                     item-2)

        (let [[sub-list] (:children item-1)]
          (assert-node {:type :bullet-list
                        :style "-"
                        :children [:children-count 2 count]}
                       sub-list)

          (let [[sub-item-1 sub-item-2] (:children sub-list)]
            (assert-node {:type :bullet-item
                          :indent 4
                          :children
                          [{:type :text
                            :value "First Item"}]}
                         sub-item-1)
            (assert-node {:type :bullet-item
                          :indent 4
                          :children
                          [{:type :text
                            :value "Second Item"}]}
                         sub-item-2)))))))
