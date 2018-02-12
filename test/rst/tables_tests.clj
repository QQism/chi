(ns rst.tables-tests
  (:require [rst.core :refer [process-document]]
            [midje.sweet :refer :all]
            [clojure.string :as string]))

(fact "Simple grid table without a header"
      (let [lines ["+------------+------------+-----------+"
                   "| Cell 1-1   |  Cell 1-2  |  Cell 1-3 |"
                   "+------------+------------+-----------+"
                   "| Cell 2-1   |  Cell 2-2  |  Cell 2-3 |"
                   "+------------+------------+-----------+"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 1))})
        (let [[table] (:children root)
              [table-body] (:children table)]
          table => (contains {:type :table
                              :col-ids (sorted-set 0 13 26)
                              :children #(-> % count (= 1))})
          table-body => (contains {:type :table-body :children #(-> % count (= 2))})

          (let [[row-1 row-2] (:children table-body)]
            row-1 => (contains {:type :row :id 0 :children #(-> % count (= 3))})
            row-2 => (contains {:type :row :id 2 :children #(-> % count (= 3))})

            (let [[cell-1 cell-2 cell-3] (:children row-1)]
              cell-1 => (contains {:type :cell :top 0 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-1"})])})
              cell-2 => (contains {:type :cell :top 0 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-2"})])})
              cell-3 => (contains {:type :cell :top 0 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-3"})])}))
            (let [[cell-4 cell-5 cell-6] (:children row-2)]
              cell-4 => (contains {:type :cell :top 2 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-1"})])})
              cell-5 => (contains {:type :cell :top 2 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-2"})])})
              cell-6 => (contains {:type :cell :top 2 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-3"})])}))))))

(fact "Simple grid table with a header"
      (let [lines ["+------------+------------+-----------+"
                   "| Header 1   |  Header 2  |  Header 3 |"
                   "+============+============+===========+"
                   "| Cell 1-1   |  Cell 1-2  |  Cell 1-3 |"
                   "+------------+------------+-----------+"
                   "| Cell 2-1   |  Cell 2-2  |  Cell 2-3 |"
                   "+------------+------------+-----------+"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 1))})
        (let [[table] (:children root)
              [table-header table-body] (:children table)]
          table => (contains {:type :table
                              :col-ids (sorted-set 0 13 26)
                              :children #(-> % count (= 2))})

          table-header => (contains {:type :table-header :children #(-> % count (= 1))})

          (let [[row-1] (:children table-header)]
            row-1 => (contains {:type :row :id 0 :children #(-> % count (= 3))})
            (let [[header-cell-1 header-cell-2 header-cell-3] (:children row-1)]
              header-cell-1 => (contains {:type :cell :top 0 :left 0 :width 12 :height 1
                                          :children (just [(contains {:type :text :value "Header 1"})])})
              header-cell-2 => (contains {:type :cell :top 0 :left 13 :width 12 :height 1
                                          :children (just [(contains {:type :text :value "Header 2"})])})
              header-cell-3 => (contains {:type :cell :top 0 :left 26 :width 11 :height 1
                                          :children (just [(contains {:type :text :value "Header 3"})])})))

          table-body => (contains {:type :table-body :children #(-> % count (= 2))})

          (let [[row-2 row-3] (:children table-body)]
            row-2 => (contains {:type :row :id 2 :children #(-> % count (= 3))})
            row-3 => (contains {:type :row :id 4 :children #(-> % count (= 3))})

            (let [[cell-1 cell-2 cell-3] (:children row-2)]
              cell-1 => (contains {:type :cell :top 2 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-1"})])})
              cell-2 => (contains {:type :cell :top 2 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-2"})])})
              cell-3 => (contains {:type :cell :top 2 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-3"})])}))
            (let [[cell-4 cell-5 cell-6] (:children row-3)]
              cell-4 => (contains {:type :cell :top 4 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-1"})])})
              cell-5 => (contains {:type :cell :top 4 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-2"})])})
              cell-6 => (contains {:type :cell :top 4 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-3"})])}))))))


(fact "Simple grid table with non valid header"
      (let [lines ["+------------+------------+-----------+"
                   "| Header 1   |  Header 2  |  Header 3 |"
                   "+============+============+===========+"
                   "| Cell 1-1   |  Cell 1-2  |  Cell 1-3 |"
                   "+============+============+===========+"
                   "| Cell 2-1   |  Cell 2-2  |  Cell 2-3 |"
                   "+------------+------------+-----------+"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 1))})

        (let [[error] (:children root)]
          error => (contains {:type :error :level 3
                              :pos [1 1]
                              :children (just
                                         [(contains
                                           {:type :paragraph
                                            :children (just
                                                       [(contains
                                                         {:type :text
                                                          :value "Malformed table. Multiple head/body row separators (table lines 2, 4); only one allowed."})])})
                                          (contains
                                           {:type :preserve
                                            :value (string/join "\r\n" lines)})])}))))


(fact "Malformed table with a non rectangle cell"
      (let [lines ["+------------+------------+-----------+"
                   "|A malformed |                        |"
                   "|            +                        |"
                   "| table      |                        |"
                   "+------------+                        +"
                   "| This is a non rectangle cell        |"
                   "+------------+------------+-----------+"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 1))})

        (let [[error] (:children root)]
          error => (contains {:type :error :level 3
                              :pos [1 1]
                              :children (just
                                         [(contains
                                           {:type :paragraph
                                            :children (just
                                                       [(contains
                                                         {:type :text
                                                          :value "Malformed table. Parse incomplete."})])})
                                          (contains {:type :preserve
                                                     :value (string/join "\r\n" lines)})])}))))

(fact "Simple grid table and a paragraph without a blank line"
      (let [lines ["+------------+------------+-----------+"
                   "| Cell 1-1   |  Cell 1-2  |  Cell 1-3 |"
                   "+------------+------------+-----------+"
                   "| Cell 2-1   |  Cell 2-2  |  Cell 2-3 |"
                   "+------------+------------+-----------+"
                   "Here should be a blank line."]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 3))})
        (let [[table error paragraph] (:children root)
              [table-body] (:children table)]
          table => (contains {:type :table
                              :col-ids (sorted-set 0 13 26)
                              :children #(-> % count (= 1))})
          table-body => (contains {:type :table-body :children #(-> % count (= 2))})

          (let [[row-1 row-2] (:children table-body)]
            row-1 => (contains {:type :row :id 0 :children #(-> % count (= 3))})
            row-2 => (contains {:type :row :id 2 :children #(-> % count (= 3))})

            (let [[cell-1 cell-2 cell-3] (:children row-1)]
              cell-1 => (contains {:type :cell :top 0 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-1"})])})
              cell-2 => (contains {:type :cell :top 0 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-2"})])})
              cell-3 => (contains {:type :cell :top 0 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-3"})])}))
            (let [[cell-4 cell-5 cell-6] (:children row-2)]
              cell-4 => (contains {:type :cell :top 2 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-1"})])})
              cell-5 => (contains {:type :cell :top 2 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-2"})])})
              cell-6 => (contains {:type :cell :top 2 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-3"})])})))

          error => (contains {:type :error :pos [5 1] :level 2
                              :children (just [(contains {:type :paragraph
                                                          :children (just [(contains {:type :text
                                                                                      :value "Blank line required after table."})])})])})
          paragraph => (contains {:type :paragraph
                                  :children (just [(contains {:type :text
                                                              :value "Here should be a blank line."})])}))))


(fact "Simple grid table and a paragraph with a blank line"
      (let [lines ["+------------+------------+-----------+"
                   "| Cell 1-1   |  Cell 1-2  |  Cell 1-3 |"
                   "+------------+------------+-----------+"
                   "| Cell 2-1   |  Cell 2-2  |  Cell 2-3 |"
                   "+------------+------------+-----------+"
                   ""
                   "This is a paragraph."]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 2))})
        (let [[table paragraph] (:children root)
              [table-body] (:children table)]
          table => (contains {:type :table
                              :col-ids (sorted-set 0 13 26)
                              :children #(-> % count (= 1))})
          table-body => (contains {:type :table-body :children #(-> % count (= 2))})

          (let [[row-1 row-2] (:children table-body)]
            row-1 => (contains {:type :row :id 0 :children #(-> % count (= 3))})
            row-2 => (contains {:type :row :id 2 :children #(-> % count (= 3))})

            (let [[cell-1 cell-2 cell-3] (:children row-1)]
              cell-1 => (contains {:type :cell :top 0 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-1"})])})
              cell-2 => (contains {:type :cell :top 0 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-2"})])})
              cell-3 => (contains {:type :cell :top 0 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-3"})])}))
            (let [[cell-4 cell-5 cell-6] (:children row-2)]
              cell-4 => (contains {:type :cell :top 2 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-1"})])})
              cell-5 => (contains {:type :cell :top 2 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-2"})])})
              cell-6 => (contains {:type :cell :top 2 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-3"})])})))

          paragraph => (contains {:type :paragraph
                                  :children (just [(contains {:type :text
                                                              :value "This is a paragraph."})])}))))

(fact "Complex grid table with cells spanning rows and columns without header"
      (let [lines ["+------------+------------+-----------+"
                   "| Cell 1-1   |  Cell 1-2  |  Cell 1-3 |"
                   "|            +------------+-----------+"
                   "| More rows  | Cell 2-2   | Cell 2-3  |"
                   "+------------+------------+-----------+"
                   "| Cell 3-1   | Cells may span columns.|"
                   "+------------+------------+-----------+"
                   "| Cell 4-1   | Cells may  | - Item 1  |"
                   "+------------+ span rows. +-----------+"
                   "| Cell 5-1   |            | - Item 2  |"
                   "+------------+------------+-----------+"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 1))})
        (let [[table] (:children root)
              [table-body] (:children table)]
          table => (contains {:type :table
                              :col-ids (sorted-set 0 13 26)
                              :children #(-> % count (= 1))})
          table-body => (contains {:type :table-body :children #(-> % count (= 5))})
          (let [[row-1 row-2 row-3 row-4 row-5] (:children table-body)]
            row-1 => (contains {:type :row :id 0 :children #(-> % count (= 3))})
            row-2 => (contains {:type :row :id 2 :children #(-> % count (= 2))})
            row-3 => (contains {:type :row :id 4 :children #(-> % count (= 2))})
            row-4 => (contains {:type :row :id 6 :children #(-> % count (= 3))})
            row-5 => (contains {:type :row :id 8 :children #(-> % count (= 2))})

            (let [[cell-1 cell-2 cell-3] (:children row-1)]
              cell-1 => (contains {:type :cell :top 0 :left 0 :width 12 :height 3
                                   :children (just [(contains
                                                     {:type :paragraph :children
                                                      (just [(contains {:type :text :value "Cell 1-1"})])})
                                                    (contains
                                                     {:type :paragraph :children
                                                      (just [(contains {:type :text :value "More rows"})])})])})
              cell-2 => (contains {:type :cell :top 0 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-2"})])})
              cell-3 => (contains {:type :cell :top 0 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-3"})])}))
            (let [[cell-4 cell-5] (:children row-2)]
              cell-4 => (contains {:type :cell :top 2 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-2"})])})
              cell-5 => (contains {:type :cell :top 2 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-3"})])}))
            (let [[cell-6 cell-7] (:children row-3)]
              cell-6 => (contains {:type :cell :top 4 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 3-1"})])})
              cell-7 => (contains {:type :cell :top 4 :left 13 :width 24 :height 1
                                   :children (just [(contains {:type :text :value "Cells may span columns."})])}))
            (let [[cell-8 cell-9 cell-10] (:children row-4)]
              cell-8 => (contains {:type :cell :top 6 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 4-1"})])})
              cell-9 => (contains {:type :cell :top 6 :left 13 :width 12 :height 3
                                   :children (just [(contains {:type :text :value "Cells may span rows."})])})
              cell-10 => (contains {:type :cell :top 6 :left 26 :width 11 :height 1
                                    :children (just [(contains
                                                      {:type :bullet-list :children
                                                       (just [(contains
                                                               {:type :bullet-item :children
                                                                (just [(contains
                                                                        {:type :text :value "Item 1"})])})])})])}))
            (let [[cell-11 cell-12] (:children row-5)]
              cell-11 => (contains {:type :cell :top 8 :left 0 :width 12 :height 1
                                    :children (just [(contains {:type :text :value "Cell 5-1"})])})
              cell-12 => (contains {:type :cell :top 8 :left 26 :width 11 :height 1
                                    :children (just [(contains
                                                      {:type :bullet-list :children
                                                       (just [(contains
                                                               {:type :bullet-item :children
                                                                (just [(contains
                                                                        {:type :text :value "Item 2"})])})])})])}))))))


(fact "Complex grid table with cells spanning rows and columns with header"
      (let [lines ["+------------+------------+-----------+"
                   "| Cell 1-1   |  Cell 1-2  |  Cell 1-3 |"
                   "|            +------------+-----------+"
                   "| More rows  | Cell 2-2   | Cell 2-3  |"
                   "+------------+------------+-----------+"
                   "| Cell 3-1   | Cells may span columns.|"
                   "+============+============+===========+"
                   "| Cell 4-1   | Cells may  | - Item 1  |"
                   "+------------+ span rows. +-----------+"
                   "| Cell 5-1   |            | - Item 2  |"
                   "+------------+------------+-----------+"]
            root (process-document lines)]
        root => (contains {:type :root :children #(-> % count (= 1))})
        (let [[table] (:children root)
              [table-header table-body] (:children table)]
          table => (contains {:type :table
                              :col-ids (sorted-set 0 13 26)
                              :children #(-> % count (= 2))})

          table-header => (contains {:type :table-header :children #(-> % count (= 3))})
          (let [[row-1 row-2 row-3] (:children table-header)]
            row-1 => (contains {:type :row :id 0 :children #(-> % count (= 3))})
            row-2 => (contains {:type :row :id 2 :children #(-> % count (= 2))})
            row-3 => (contains {:type :row :id 4 :children #(-> % count (= 2))})

            (let [[cell-1 cell-2 cell-3] (:children row-1)]
              cell-1 => (contains {:type :cell :top 0 :left 0 :width 12 :height 3
                                   :children (just [(contains
                                                     {:type :paragraph :children
                                                      (just [(contains {:type :text :value "Cell 1-1"})])})
                                                    (contains
                                                     {:type :paragraph :children
                                                      (just [(contains {:type :text :value "More rows"})])})])})
              cell-2 => (contains {:type :cell :top 0 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-2"})])})
              cell-3 => (contains {:type :cell :top 0 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Cell 1-3"})])}))
            (let [[cell-4 cell-5] (:children row-2)]
              cell-4 => (contains {:type :cell :top 2 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-2"})])})
              cell-5 => (contains {:type :cell :top 2 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Cell 2-3"})])}))
            (let [[cell-6 cell-7] (:children row-3)]
              cell-6 => (contains {:type :cell :top 4 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 3-1"})])})
              cell-7 => (contains {:type :cell :top 4 :left 13 :width 24 :height 1
                                   :children (just [(contains {:type :text :value "Cells may span columns."})])})))

          table-body => (contains {:type :table-body :children #(-> % count (= 2))})
          (let [[row-4 row-5] (:children table-body)]
            row-4 => (contains {:type :row :id 6 :children #(-> % count (= 3))})
            row-5 => (contains {:type :row :id 8 :children #(-> % count (= 2))})

            (let [[cell-8 cell-9 cell-10] (:children row-4)]
              cell-8 => (contains {:type :cell :top 6 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Cell 4-1"})])})
              cell-9 => (contains {:type :cell :top 6 :left 13 :width 12 :height 3
                                   :children (just [(contains {:type :text :value "Cells may span rows."})])})
              cell-10 => (contains {:type :cell :top 6 :left 26 :width 11 :height 1
                                    :children (just [(contains
                                                      {:type :bullet-list :children
                                                       (just [(contains
                                                               {:type :bullet-item :children
                                                                (just [(contains
                                                                        {:type :text :value "Item 1"})])})])})])}))
            (let [[cell-11 cell-12] (:children row-5)]
              cell-11 => (contains {:type :cell :top 8 :left 0 :width 12 :height 1
                                    :children (just [(contains {:type :text :value "Cell 5-1"})])})
              cell-12 => (contains {:type :cell :top 8 :left 26 :width 11 :height 1
                                    :children (just [(contains
                                                      {:type :bullet-list :children
                                                       (just [(contains
                                                               {:type :bullet-item :children
                                                                (just [(contains
                                                                        {:type :text :value "Item 2"})])})])})])}))))))
