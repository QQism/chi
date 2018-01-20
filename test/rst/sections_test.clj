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

(fact "Single overline section with single line paragraph"
      (let [lines ["============="
                   "Section Title"
                   "============="
                   "Section paragraph content"]
            root (process-document lines)]
        root  => (contains {:type :root :children #(-> % count (= 1))})
        (let [section (-> root :children first)]
          section => (contains {:type :section
                                :style "overline="
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

(fact "Two sections on the same level with single line paragraphs"
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
          first-section => (contains
                            {:type :section
                             :style "underline="
                             :children #(-> % count (= 2))})
          second-section => (contains
                             {:type :section
                              :style "underline="
                              :children #(-> % count (= 2))})
          (let [[first-header first-paragraph] (:children first-section)
                [second-header second-paragraph] (:children second-section)]
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


(fact "Nested sections with single line paragraphs"
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
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 2))})
        (let [[first-section second-section] (:children root)]
          first-section => (contains
                            {:type :section
                             :style "underline="
                             :children #(-> % count (= 3))})
          second-section => (contains
                             {:type :section
                              :style "underline="
                              :children #(-> % count (= 3))})
          (let [[first-header first-paragraph first-nested-section] (:children first-section)
                [second-header second-paragraph second-nested-section] (:children second-section)]
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
            first-nested-section => (contains
                                     {:type :section
                                      :style "overline-"
                                      :children #(-> % count (= 3))})
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
                                                :value "Second paragraph content"})])})
            second-nested-section => (contains
                                      {:type :section
                                       :style "underline+"
                                       :children #(-> % count (= 2))})
            (let [[header paragraph nested-section] (:children first-nested-section)]
              header => (contains
                         {:type :header
                          :children (just
                                     [(contains
                                       {:type :text
                                        :value "Nested Title"})])})
              paragraph => (contains
                            {:type :paragraph
                             :children (just
                                        [(contains
                                          {:type :text
                                           :value "Nested content"})])})
              nested-section => (contains
                                 {:type :section
                                  :style "underline+"
                                  :children (just
                                             [(contains
                                               {:type :header
                                                :children (just
                                                           [(contains
                                                             {:type :text
                                                              :value "Nested Nested Title"})])})])}))
            (let [[header paragraph] (:children second-nested-section)]
              header => (contains
                         {:type :header
                          :children (just
                                     [(contains
                                       {:type :text
                                        :value "Another Nested Title"})])})
              paragraph => (contains
                            {:type :paragraph
                             :children (just
                                        [(contains
                                          {:type :text
                                           :value "Another nested content"})])}))))))

(fact "Title underline is shorter than title text but longer than 3 characters"
      (let [lines ["Section Title"
                   "===="
                   "Paragraph content"]
            root (process-document lines)]
        root  => (contains {:type :root :children #(-> % count (= 1))})
        (let [section (-> root :children first)]
          section => (contains {:type :section
                                :style "underline="
                                :children #(-> % count (= 3))})
          (let [[header error paragraph] (:children section)]
            header => (contains
                       {:type :header
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "Section Title"})])})
            error => (contains
                      {:type :error
                       :level 2
                       :children (just
                                  [(contains
                                    {:type :paragraph
                                     :children (just
                                                [(contains
                                                  {:type :text
                                                   :value "Title underline too short."})])})
                                   (contains
                                    {:type :preserve
                                     :value "Section Title\r\n===="})])})
            paragraph => (contains
                          {:type :paragraph
                           :children (just
                                      [(contains
                                        {:type :text
                                         :value "Paragraph content"})])})))))

(fact "Title underline is shorter than title text but not longer than 3 characters"
      (let [lines ["Section Title"
                   "==="
                   "Paragraph content"]
            root (process-document lines)]
        root  => (contains {:type :root :children #(-> % count (= 1))})
        (let [paragraph (-> root :children first)]
          paragraph => (contains {:type :paragraph
                                  :children (just
                                             [(contains
                                               {:type :text
                                                :value "Section Title === Paragraph content"})])}))))

(fact "Title underline is not longer than title text but not longer than 3 characters"
      (let [lines ["Hea"
                   "==="
                   "Paragraph content"]
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
                                      :value "Hea"})])})
            paragraph => (contains
                          {:type :paragraph
                           :children (just
                                      [(contains
                                        {:type :text
                                         :value "Paragraph content"})])})))))

(fact "Title overline and overline are shorter than title text but not longer than 3 characters"
      (let [lines ["==="
                   "Section Title"
                   "==="
                   "Paragraph content"]
            root (process-document lines)]
        root  => (contains {:type :root :children #(-> % count (= 1))})
        (let [section (-> root :children first)]
          section => (contains {:type :section
                                :style "overline="
                                :children #(-> % count (= 3))})
          (let [[header error paragraph] (:children section)]
            header => (contains
                       {:type :header
                       :children (just
                                  [(contains
                                    {:type :text
                                     :value "Section Title"})])})
            error => (contains
                      {:type :error
                       :level 2
                       :children (just
                                  [(contains
                                    {:type :paragraph
                                     :children (just
                                                [(contains
                                                  {:type :text
                                                   :value "Title overline too short."})])})
                                   (contains
                                    {:type :preserve
                                     :value "===\r\nSection Title\r\n==="})])})
            paragraph => (contains
                          {:type :paragraph
                           :children (just
                                      [(contains
                                        {:type :text
                                         :value "Paragraph content"})])})))))

(fact "Title overline is missing underline and longer than 3 characters"
      (let [lines ["===="
                   "Section Title"
                   "Paragraph content"]
            root (process-document lines)]
        root  => (contains {:type :root :children #(-> % count (= 1))})
        (let [error (-> root :children first)]
          error => (contains
                    {:type :error
                     :level 4
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Missing matching underline for section title overline."})])})
                                 (contains
                                  {:type :preserve
                                   :value "====\r\nSection Title\r\nParagraph content"})])}))))

(fact "Title overline is missing underline and not longer than 3 characters"
      (let [lines ["==="
                   "Section Title"
                   "Paragraph content"]
            root (process-document lines)]
        root  => (contains {:type :root :children #(-> % count (= 1))})
        (let [paragraph (-> root :children first)]
          paragraph => (contains
                        {:type :paragraph
                         :children (just
                                    [(contains
                                      {:type :text
                                       :value "=== Section Title Paragraph content"})])}))))

(fact "Title overline and underline are mismatched and longer than 3 characters"
      (let [lines ["===="
                   "Section Title"
                   "=="
                   "Paragraph content"]
            root (process-document lines)]
        root  => (contains {:type :root :children #(-> % count (= 2))})
        (let [[error paragraph] (:children root)]
          error => (contains
                    {:type :error
                     :level 4
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Title overline & underline mismatch."})])})
                                 (contains
                                  {:type :preserve
                                   :value "====\r\nSection Title\r\n=="})
                                 ])})
          paragraph => (contains
                        {:type :paragraph
                         :children (just
                                    [(contains
                                      {:type :text
                                       :value "Paragraph content"})])}))))

(fact "Title overline and underline are mismatched and not longer than 3 characters"
      (let [lines ["==="
                   "Section Title"
                   "=="
                   "Paragraph content"]
            root (process-document lines)]
        root  => (contains {:type :root :children #(-> % count (= 1))})
        (let [paragraph (-> root :children first)]
          paragraph => (contains
                        {:type :paragraph
                         :children (just
                                    [(contains
                                      {:type :text
                                       :value "=== Section Title == Paragraph content"})])}))))

(fact "Title overline is incomplete and longer than 3 characters"
      (let [lines ["===="
                   "Section Title"]
            root (process-document lines)]
        root  => (contains {:type :root :children #(-> % count (= 1))})
        (let [error (-> root :children first)]
          error => (contains
                    {:type :error
                     :level 4
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Incomplete section title."})])})
                                 (contains
                                  {:type :preserve
                                   :value "====\r\nSection Title"})])}))))

(fact "Title overline is incomplete and not longer than 3 characters"
      (let [lines ["==="
                   "Section Title"]
            root (process-document lines)]
        root  => (contains {:type :root :children #(-> % count (= 1))})
        (let [paragraph (-> root :children first)]
          paragraph => (contains
                        {:type :paragraph
                         :children (just
                                    [(contains
                                      {:type :text
                                       :value "=== Section Title"})])}))))
