(ns rst.bulletlists-tests
  (:require  [midje.sweet :refer :all]
             [rst.core :refer :all]))

(fact "A single line bullet item"
      (let [lines ["- Lorem Ipsum is simply dummy text"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 1))})
        (let [list (-> root :children peek)
              item (-> list :children peek)]
          list => (contains
                   {:type :bullet-list
                    :style "-"
                    :children #(-> % count (= 1))})
          item => (contains
                   {:type :bullet-item
                    :children (just
                               [(contains
                                 {:type :text
                                  :value "Lorem Ipsum is simply dummy text"})])}))))
(fact "A multi-line line bullet"
      (let [lines ["- Lorem Ipsum is simply dummy text"
                   ""
                   "  Lorem Ipsum is simply dummy's standard"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 1))})
        (let [list (-> root :children peek)
              item (-> list :children peek)]
          list => (contains
                   {:type :bullet-list
                    :style "-"
                    :children #(-> % count (= 1))})
          item => (contains
                   {:type :bullet-item
                    :children (just
                               [(contains
                                 {:type :paragraph
                                  :children (just
                                             [(contains
                                               {:type :text
                                                :value "Lorem Ipsum is simply dummy text"})])})
                                (contains
                                 {:type :paragraph
                                  :children (just
                                             [(contains
                                               {:type :text
                                                :value "Lorem Ipsum is simply dummy's standard"})])})])}))))

(fact "A multi-line line bullet list"
      (let [lines ["- First item"
                   "- Second item"
                   ""
                   "  Lorem Ipsum is simply dummy's standard"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 1))})
        (let [list (-> root :children peek)
              [item-1 item-2] (:children list)]
          list => (contains
                   {:type :bullet-list
                    :style "-"
                    :children #(-> % count (= 2))})
          item-1 => (contains
                   {:type :bullet-item
                    :children (just
                               [(contains
                                 {:type :text
                                  :value "First item"})])})
          item-2 => (contains
                   {:type :bullet-item
                    :children (just
                               [(contains
                                 {:type :paragraph
                                  :children (just
                                             [(contains
                                               {:type :text
                                                :value "Second item"})])})
                                (contains
                                 {:type :paragraph
                                  :children (just
                                             [(contains
                                               {:type :text
                                                :value "Lorem Ipsum is simply dummy's standard"})])})])}))))

(fact "A single line bullet item and blockquote without a blank line between"
      (let [lines ["-   Lorem Ipsum is simply dummy text"
                   "  This is a separated blockquote"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 3))})
        (let [[list error blockquote] (-> root :children)
              item (-> list :children peek)]
          list => (contains
                   {:type :bullet-list
                    :style "-"
                    :children #(-> % count (= 1))})
          item => (contains
                   {:type :bullet-item
                    :children (just
                               [(contains
                                 {:type :text
                                  :value "Lorem Ipsum is simply dummy text"})])})
          error => (contains
                    {:type :error
                     :level 2
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Bullet list ends without a blank line; unexpected unindent."})])})])})
          blockquote => (contains
                         {:type :blockquote
                          :indent 2
                          :children (just
                                     [(contains
                                       {:type :text
                                        :value "This is a separated blockquote"})])}))))


(fact "A single line bullet item and blockquote with a blank line between"
      (let [lines ["-   Lorem Ipsum is simply dummy text"
                   ""
                   "  This is a separated blockquote"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 2))})
        (let [[list blockquote] (-> root :children)
              item (-> list :children peek)]
          list => (contains
                   {:type :bullet-list
                    :style "-"
                    :children #(-> % count (= 1))})
          item => (contains
                   {:type :bullet-item
                    :children (just
                               [(contains
                                 {:type :text
                                  :value "Lorem Ipsum is simply dummy text"})])})
          blockquote => (contains
                         {:type :blockquote
                          :indent 2
                          :children (just
                                     [(contains
                                       {:type :text
                                        :value "This is a separated blockquote"})])}))))

(fact "A simple bullet list"
      (let [lines ["- First item"
                   "- Second item"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 1))})
        (let [[list] (-> root :children)
              [item-1st item-2nd] (-> list :children)]
          list => (contains
                   {:type :bullet-list
                    :style "-"
                    :children #(-> % count (= 2))})
          item-1st => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "First item"})])})
          item-2nd => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "Second item"})])}))))

(fact "A simple bullet list and a paragraph with a blank line between"
      (let [lines ["- First item"
                   "- Second item"
                   ""
                   "Lorem Ipsum is simply dummy text"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 2))})
        (let [[list paragraph] (-> root :children)
              [item-1st item-2nd] (-> list :children)]
          list => (contains
                   {:type :bullet-list
                    :style "-"
                    :children #(-> % count (= 2))})
          item-1st => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "First item"})])})
          item-2nd => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "Second item"})])})
          paragraph => (contains
                        {:type :paragraph
                         :children (just
                                    [(contains
                                      {:type :text
                                       :value "Lorem Ipsum is simply dummy text"})])}))))

(fact "A simple bullet list and a paragraph without a blank line between"
      (let [lines ["- First item"
                   "- Second item"
                   "Lorem Ipsum is simply dummy text"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 3))})
        (let [[list error paragraph] (-> root :children)
              [item-1st item-2nd] (-> list :children)]
          list => (contains
                   {:type :bullet-list
                    :style "-"
                    :children #(-> % count (= 2))})
          item-1st => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "First item"})])})
          item-2nd => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "Second item"})])})
          error => (contains
                    {:type :error
                     :level 2
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Bullet list ends without a blank line; unexpected unindent."})])})])})
          paragraph => (contains
                        {:type :paragraph
                         :children (just
                                    [(contains
                                      {:type :text
                                       :value "Lorem Ipsum is simply dummy text"})])}))))

(fact "Two simple bullet lists without a blank line between"
      (let [lines ["- First item"
                   "- Second item"
                   "+ Another item"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 3))})
        (let [[list-1 error list-2] (-> root :children)
              [item-1st item-2nd] (-> list-1 :children)
              [item-3rd] (-> list-2 :children)]
          list-1 => (contains
                   {:type :bullet-list
                    :style "-"
                    :children #(-> % count (= 2))})
          list-2 => (contains
                     {:type :bullet-list
                      :style "+"
                      :children #(-> % count (= 1))})
          item-1st => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "First item"})])})
          item-2nd => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "Second item"})])})
          error => (contains
                    {:type :error
                     :level 2
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Bullet list ends without a blank line; unexpected unindent."})])})])})
          item-3rd => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "Another item"})])}))))

(fact "Two simple bullet lists with a blank line between"
      (let [lines ["- First item"
                   "- Second item"
                   ""
                   "+ Another item"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 2))})
        (let [[list-1 list-2] (-> root :children)
              [item-1st item-2nd] (-> list-1 :children)
              [item-3rd] (-> list-2 :children)]
          list-1 => (contains
                   {:type :bullet-list
                    :style "-"
                    :children #(-> % count (= 2))})
          list-2 => (contains
                     {:type :bullet-list
                      :style "+"
                      :children #(-> % count (= 1))})
          item-1st => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "First item"})])})
          item-2nd => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "Second item"})])})
          item-3rd => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "Another item"})])}))))

(fact "A simple bullet list and a line without a blank line between"
      (let [lines ["- First item"
                   "- Second item"
                   "===="
                   ""]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 4))})
        (let [[list error-1 transition error-2] (-> root :children)
              [item-1st item-2nd] (-> list :children)]
          list => (contains
                   {:type :bullet-list
                    :style "-"
                    :children #(-> % count (= 2))})
          item-1st => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "First item"})])})
          item-2nd => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "Second item"})])})
          error-1 => (contains
                    {:type :error
                     :level 2
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Bullet list ends without a blank line; unexpected unindent."})])})])})
          transition => (contains {:type :transition})
          error-2 => (contains
                    {:type :error
                     :children (just
                                [(contains
                                  {:type :paragraph
                                   :children (just
                                              [(contains
                                                {:type :text
                                                 :value "Document may not end with a transition."})])})])}))))

(fact "A simple bullet list and a line with a blank line between"
      (let [lines ["- First item"
                   "- Second item"
                   ""
                   "===="
                   ""]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 3))})
        (let [[list transition error] (-> root :children)
              [item-1st item-2nd] (-> list :children)]
          list => (contains
                   {:type :bullet-list
                    :style "-"
                    :children #(-> % count (= 2))})
          item-1st => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "First item"})])})
          item-2nd => (contains
                       {:type :bullet-item
                        :children (just
                                   [(contains
                                     {:type :text
                                      :value "Second item"})])})
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
