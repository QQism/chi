(ns chi.frontend.sections-test
  (:require #?(:cljs [cljs.test    :as t :refer-macros [deftest testing]]
               :clj  [clojure.test :as t :refer        [deftest testing]])
            #?(:cljs [chi.test-support :refer [assert-node]]
               :clj  [chi.assert-macros :refer [assert-node]])
            [chi.frontend.parser :refer [lines->ast]]))

#?(:cljs (enable-console-print!))

(deftest single-underline-section
  (testing "with single line paragraph"
    (let [lines ["Section Title"
                 "============="
                 "Section paragraph content"]
          root (lines->ast lines)]
      (assert-node {:type :root :children [:children-count 1 count]} root)

      (let [[section] (:children root)]
        (assert-node {:type :section
                      :style "underline="
                      :level 1
                      :children [:children-count 2 count]}
                     section)

        (let [[header paragraph] (:children section)]
          (assert-node {:type :header
                        :level 1
                        :children [{:type :text
                                    :value "Section Title"}]}
                       header)
          (assert-node {:type :paragraph
                        :children
                        [{:type :text
                          :value "Section paragraph content"}]}
                       paragraph))))))

(deftest single-overline-section
  (testing "with single line paragraph"
    (let [lines ["============="
                 "Section Title"
                 "============="
                 "Section paragraph content"]
          root (lines->ast lines)]
      (assert-node {:type :root :children [:children-count 1 count]} root)

      (let [[section] (:children root)]
        (assert-node {:type :section
                      :style "overline="
                      :level 1
                      :children [:children-count 2 count]}
                     section)

        (let [[header paragraph] (:children section)]
          (assert-node {:type :header
                        :level 1
                        :children [{:type :text
                                    :value "Section Title"}]}
                       header)
          (assert-node {:type :paragraph
                        :children
                        [{:type :text
                          :value "Section paragraph content"}]}
                       paragraph))))))

(deftest two-same-level-sections-on
  (let [lines ["First Title"
               "==========="
               "First paragraph content"
               ""
               "Second Title"
               "============"
               "Second paragraph content"]
        root (lines->ast lines)]
    (assert-node {:type :root :children [:children-count 2 count]} root)

    (let [[section-1 section-2] (:children root)]
      (assert-node {:type :section
                    :style "underline="
                    :level 1
                    :children [:children-count 2 count]}
                   section-1)
      (assert-node {:type :section
                    :style "underline="
                    :level 1
                    :children [:children-count 2 count]}
                   section-2)

      (let [[header-1 paragraph-1] (:children section-1)
            [header-2 paragraph-2] (:children section-2)]
        (assert-node {:type :header
                      :level 1
                      :children
                      [{:type :text
                        :value "First Title"}]}
                     header-1)
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "First paragraph content"}]}
                     paragraph-1)
        (assert-node {:type :header
                      :level 1
                      :children
                      [{:type :text
                        :value "Second Title"}]}
                     header-2)
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "Second paragraph content"}]}
                     paragraph-2)))))

(deftest nested-sections
  (let [lines ["First Title"
               "==========="
               "First paragraph content"
               ""
               "------------"
               "Nested Title"
               "------------"
               "Nested content"
               ""
               "Nested Nested Title"
               "+++++++++++++++++++"
               ""
               "Second Title"
               "============"
               "Second paragraph content"
               ""
               "Another Nested Title"
               "++++++++++++++++++++"
               "Another nested content"]
        root (lines->ast lines)]
    (assert-node {:type :root :children [:children-count 2 count]} root)

    (let [[section-1 section-2] (:children root)]
      (assert-node {:type :section
                    :style "underline="
                    :level 1
                    :children [:children-count 3 count]}
                   section-1)

      (assert-node {:type :section
                    :style "underline="
                    :level 1
                    :children [:children-count 3 count]}
                   section-1)

      (let [[header-1 paragraph-1 nested-section-1] (:children section-1)
            [header-2 paragraph-2 nested-section-2] (:children section-2)]
        (assert-node {:type :header
                      :level 1
                      :children
                      [{:type :text
                        :value "First Title"}]}
                     header-1)
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "First paragraph content"}]}
                     paragraph-1)
        (assert-node {:type :section
                      :style "overline-"
                      :level 2
                      :children [:children-count 3 count]}
                     nested-section-1)
        (assert-node {:type :header
                      :level 1
                      :children
                      [{:type :text
                        :value "Second Title"}]}
                     header-2)
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "Second paragraph content"}]}
                     paragraph-2)
        (assert-node {:type :section
                      :style "underline+"
                      :level 2
                      :children [:children-count 2 count]}
                     nested-section-2)

        (let [[header paragraph nested-section] (:children nested-section-1)]
          (assert-node {:type :header
                        :level 2
                        :children
                        [{:type :text
                          :value "Nested Title"}]}
                       header)
          (assert-node {:type :paragraph
                        :children
                        [{:type :text
                          :value "Nested content"}]}
                       paragraph)
          (assert-node {:type :section
                        :style "underline+"
                        :level 3
                        :children
                        [{:type :header
                          :level 3
                          :children
                          [{:type :text
                            :value "Nested Nested Title"}]}]}
                       nested-section))

        (let [[header paragraph] (:children nested-section-2)]
          (assert-node {:type :header
                        :level 2
                        :children
                        [{:type :text
                          :value "Another Nested Title"}]}
                       header)
          (assert-node {:type :paragraph
                        :children
                        [{:type :text
                          :value "Another nested content"}]}
                       paragraph))))))

(deftest title-underline-shorter-than-title-text
  (testing "with underline longer than 3 chars"
    (let [lines ["Section Title"
                 "===="
                 "Paragraph content"]
          root (lines->ast lines)]
      (assert-node {:type :root :children [:children-count 1 count]} root)

      (let [[section] (:children root)]
        (assert-node {:type :section
                      :style "underline="
                      :children [:children-count 3 count]}
                     section)

        (let [[header error paragraph] (:children section)]
          (assert-node {:type :header
                        :children
                        [{:type :text
                          :value "Section Title"}]}
                       header)
          (assert-node {:type :error
                        :level 2
                        :pos [2 1]
                        :children
                        [{:type :paragraph
                          :children
                          [{:type :text
                            :value "Title underline too short."}]}
                         {:type :preserve
                          :value "Section Title\r\n===="}]}
                       error)
          (assert-node {:type :paragraph
                        :children
                        [{:type :text
                          :value "Paragraph content"}]}
                       paragraph)))))

  (testing "with underline not longer than 3 chars"
    (let [lines ["Section Title"
                 "==="
                 "Paragraph content"]
          root (lines->ast lines)]
      (assert-node {:type :root :children [:children-count 1 count]} root)

      (let [[paragraph] (:children root)]
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "Section Title === Paragraph content"}]}
                     paragraph)))))

(deftest short-title-underline-text-and-title
  (let [lines ["Hea"
               "==="
               "Paragraph content"]
        root (lines->ast lines)]
    (assert-node {:type :root :children [:children-count 1 count]} root)

    (let [[section] (:children root)]
      (assert-node {:type :section
                    :style "underline="
                    :level 1
                    :children [:children-count 2 count]}
                   section)

      (let [[header paragraph] (:children section)]
        (assert-node {:type :header
                      :children
                      [{:type :text
                        :value "Hea"}]}
                     header)
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "Paragraph content"}]}
                     paragraph)))))

(deftest title-overline-shorter-than-title-text
  (testing "with line longer than 3 chars"
    (let [lines ["===="
                 "Section Title"
                 "===="
                 "Paragraph content"]
          root (lines->ast lines)]
      (assert-node {:type :root :children [:children-count 1 count]} root)

      (let [[section] (:children root)]
        (assert-node {:type :section
                      :style "overline="
                      :level 1
                      :children [:children-count 3 count]}
                     section)

        (let [[header error paragraph] (:children section)]
          (assert-node {:type :header
                        :children
                        [{:type :text
                          :value "Section Title"}]}
                       header)
          (assert-node {:type :error
                        :level 2
                        :pos [1 1]
                        :children
                        [{:type :paragraph
                          :children
                          [{:type :text
                            :value "Title overline too short."}]}
                         {:type :preserve
                          :value "====\r\nSection Title\r\n===="}]}
                       error)
          (assert-node {:type :paragraph
                        :children
                        [{:type :text
                          :value "Paragraph content"}]}
                       paragraph)))))

  (testing "with line not longer than 3 chars"
    (let [lines ["==="
                 "Section Title"
                 "==="
                 "Paragraph content"]
          root (lines->ast lines)]
      (assert-node {:type :root :children [:children-count 1 count]} root)

      (let [[paragraph] (:children root)]
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value (str "=== Section Title === "
                                    "Paragraph content")}]}
                     paragraph)))))

(deftest title-underline-is-missing
  (testing "underline longer than 3 chars"
    (let [lines ["===="
                 "Section Title"
                 "Paragraph content"]
          root (lines->ast lines)]
      (assert-node {:type :root :children [:children-count 1 count]} root)

      (let [[error] (:children root)]
        (assert-node {:type :error
                      :level 4
                      :pos [1 1]
                      :children
                      [{:type :paragraph
                        :children
                        [{:type :text
                          :value "Missing matching underline for section title overline."}]}
                       {:type :preserve
                        :value "====\r\nSection Title\r\nParagraph content"}]}
                     error))))

  (testing "underline not longer than 3 chars"
    (let [lines ["==="
                 "Section Title"
                 "Paragraph content"]
          root (lines->ast lines)]
      (assert-node {:type :root :children [:children-count 1 count]} root)

      (let [[paragraph] (:children root)]
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value (str "=== Section Title "
                                    "Paragraph content")}]}
                     paragraph)))))

(deftest title-overline-and-underline-mismatched
  (testing "overline longer than 3 character"
    (let [lines ["===="
                 "Section Title"
                 "=="
                 "Paragraph content"]
          root (lines->ast lines)]
      (assert-node {:type :root :children [:children-count 2 count]} root)

      (let [[error paragraph] (:children root)]
        (assert-node {:type :error
                      :level 4
                      :pos [1 1]
                      :children
                      [{:type :paragraph
                        :children
                        [{:type :text
                          :value "Title overline & underline mismatch."}]}
                       {:type :preserve
                        :value "====\r\nSection Title\r\n=="}]}
                     error)
        (assert-node {:type :paragraph
                      :children [{:type :text
                                  :value "Paragraph content"}]}
                     paragraph))))

  (testing "overline not longer than 3 chars"
    (let [lines ["==="
                 "Section Title"
                 "=="
                 "Paragraph content"]
          root (lines->ast lines)]
      (assert-node {:type :root :children [:children-count 1 count]} root)

      (let [[paragraph] (:children root)]
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "=== Section Title == Paragraph content"}]}
                     paragraph)))))

(deftest title-overline-incomplete
  (testing  "overline longer than 3 chars"
    (let [lines ["===="
                 "Section Title"]
          root (lines->ast lines)]
      (assert-node {:type :root :children [:children-count 1 count]} root)

      (let [[error] (:children root)]
        (assert-node {:type :error
                      :level 4
                      :pos [1 1]
                      :children
                      [{:type :paragraph
                        :children
                        [{:type :text
                          :value "Incomplete section title."}]}
                       {:type :preserve
                        :value "====\r\nSection Title"}]}
                     error))))

  (testing "overline not longer than 3 chars"
    (let [lines ["==="
                 "Section Title"]
          root (lines->ast lines)]
      (assert-node {:type :root :children [:children-count 1 count]} root)

      (let [[paragraph] (:children root)]
        (assert-node {:type :paragraph
                      :children
                      [{:type :text
                        :value "=== Section Title"}]}
                     paragraph)))))
