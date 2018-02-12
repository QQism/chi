(ns rst.tables-tests
  (:require [rst.core :refer [process-document]]
            [midje.sweet :refer :all]))

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

          (let [[header-row-1] (:children table-header)]
            header-row-1 => (contains {:type :row :id 0 :children #(-> % count (= 3))})
            (let [[header-cell-1 header-cell-2 header-cell-3] (:children header-row-1)]
              header-cell-1 => (contains {:type :cell :top 0 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Header 1"})])})
              header-cell-2 => (contains {:type :cell :top 0 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Header 2"})])})
              header-cell-3 => (contains {:type :cell :top 0 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Header 3"})])})
              ))

          table-body => (contains {:type :table-body :children #(-> % count (= 2))})

          (let [[row-1 row-2] (:children table-body)]
            row-1 => (contains {:type :row :id 2 :children #(-> % count (= 3))})
            row-2 => (contains {:type :row :id 4 :children #(-> % count (= 3))})

          (let [[cell-1 cell-2 cell-3] (:children row-1)]
            cell-1 => (contains {:type :cell :top 2 :left 0 :width 12 :height 1
                                 :children (just [(contains {:type :text :value "Cell 1-1"})])})
            cell-2 => (contains {:type :cell :top 2 :left 13 :width 12 :height 1
                                 :children (just [(contains {:type :text :value "Cell 1-2"})])})
            cell-3 => (contains {:type :cell :top 2 :left 26 :width 11 :height 1
                                 :children (just [(contains {:type :text :value "Cell 1-3"})])}))
          (let [[cell-4 cell-5 cell-6] (:children row-2)]
            cell-4 => (contains {:type :cell :top 4 :left 0 :width 12 :height 1
                                 :children (just [(contains {:type :text :value "Cell 2-1"})])})
            cell-5 => (contains {:type :cell :top 4 :left 13 :width 12 :height 1
                                 :children (just [(contains {:type :text :value "Cell 2-2"})])})
            cell-6 => (contains {:type :cell :top 4 :left 26 :width 11 :height 1
                                 :children (just [(contains {:type :text :value "Cell 2-3"})])}))))))

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
      (let [lines [
                   "+------------+------------+-----------+"
                   "| Header 1   |  Header 2  |  Header 3 |"
                   "|            +------------+-----------+"
                   "| body row 1 | Column 2   | Column 3  |"
                   "+------------+------------+-----------+"
                   "| body row 2 | Cells may span columns.|"
                   "+------------+------------+-----------+"
                   "| body row 3 | Cells may  | - Cells   |"
                   "+------------+ span rows. +-----------+"
                   "| body row 4 |            | - blocks. |"
                   "+------------+------------+-----------+"

                   ;;"+------------+------------+-----------+"
                   ;;"| Header 1   |                        |"
                   ;;"|            +                        +"
                   ;;"| body row 1 |                        |"
                   ;;"+------------+                        +"
                   ;;"| body row 2   Cells may span columns.|"
                   ;;"+------------+------------+-----------+"
                   ]
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
                                                      (just [(contains {:type :text :value "Header 1"})])})
                                                    (contains
                                                     {:type :paragraph :children
                                                      (just [(contains {:type :text :value "body row 1"})])})])})
              cell-2 => (contains {:type :cell :top 0 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Header 2"})])})
              cell-3 => (contains {:type :cell :top 0 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Header 3"})])}))
            (let [[cell-4 cell-5] (:children row-2)]
              cell-4 => (contains {:type :cell :top 2 :left 13 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "Column 2"})])})
              cell-5 => (contains {:type :cell :top 2 :left 26 :width 11 :height 1
                                   :children (just [(contains {:type :text :value "Column 3"})])}))
            (let [[cell-6 cell-7] (:children row-3)]
              cell-6 => (contains {:type :cell :top 4 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "body row 2"})])})
              cell-7 => (contains {:type :cell :top 4 :left 13 :width 24 :height 1
                                   :children (just [(contains {:type :text :value "Cells may span columns."})])}))
            (let [[cell-8 cell-9 cell-10] (:children row-4)]
              cell-8 => (contains {:type :cell :top 6 :left 0 :width 12 :height 1
                                   :children (just [(contains {:type :text :value "body row 3"})])})
              cell-9 => (contains {:type :cell :top 6 :left 13 :width 12 :height 3
                                   :children (just [(contains {:type :text :value "Cells may span rows."})])})
              cell-10 => (contains {:type :cell :top 6 :left 26 :width 11 :height 1
                                    :children (just [(contains
                                                      {:type :bullet-list :children
                                                       (just [(contains
                                                               {:type :bullet-item :children
                                                                (just [(contains
                                                                        {:type :text :value "Cells"})])})])})])}))
            (let [[cell-11 cell-12] (:children row-5)]
              cell-11 => (contains {:type :cell :top 8 :left 0 :width 12 :height 1
                                    :children (just [(contains {:type :text :value "body row 4"})])})
              cell-12 => (contains {:type :cell :top 8 :left 26 :width 11 :height 1
                                    :children (just [(contains
                                                      {:type :bullet-list :children
                                                       (just [(contains
                                                               {:type :bullet-item :children
                                                                (just [(contains
                                                                        {:type :text :value "blocks."})])})])})])}))))))


{:type :table :pos [0 0] :style :grid :width 39 :height 11
 :col-ids [0 13 26]
 :children
 [{:type :table-body
   :children
   [{:type :row :id 0 :children
     [{:type :cell :cell-pos [0  0] :size [12 3] :rowspan 2 :colspan 1 :children [{:type :paragraph
                                                                                   :children [{:type :text :value "Header 1"}]}
                                                                                  {:type :paragraph
                                                                                   :children [{:type :text :value "body row 1"}]}]}
      {:type :cell :cell-pos [0 13] :size [12 1] :rowspan 1 :colspan 1 :children [{:type :text :value "Header 2"}]}
      {:type :cell :cell-pos [0 26] :size [11 1] :rowspan 1 :colspan 1 :children [{:type :text :value "Header 3"}]}]}

    {:type :row :id 2 :children
     [{:type :cell :cell-pos [2 13] :size [12 1] :rowspan 1 :colspan 1 :children [{:type :text :value "Column 2"}]}
      {:type :cell :cell-pos [2 26] :size [11 1] :rowspan 1 :colspan 1 :children [{:type :text :value "Column 3"}]}]}

    {:type :row :id 4 :children
     [{:type :cell :cell-pos [4  0] :size [12 1] :rowspan 1 :colspan 1 :children [{:type :text :value "body row 2"}]}
      {:type :cell :cell-pos [4 13] :size [24 1] :rowspan 1 :colspan 2 :children [{:type :text :value "Cells may span columns."}]}]}

    {:type :row :id 6 :children
     [{:type :cell :cell-pos [6  0] :size [12 1] :rowspan 1 :colspan 1 :children [{:type :text :value "body row 3"}]}
      {:type :cell :cell-pos [6 13] :size [12 3] :rowspan 2 :colspan 1 :children [{:type :paragraph
                                                                                   :children [{:type :text
                                                                                               :value "Cells may span rows."}]}]}
      {:type :cell :cell-pos [6 26] :size [11 1] :rowspan 1 :colspan 1 :children [{:type :bullet-list
                                                                                   :children [{:type :bullet-item
                                                                                               :style "-"
                                                                                               :children [{:type :text
                                                                                                           :value "Cells"}]}]}]}]}

    {:type :row :id 8 :children
     [{:type :cell :cell-pos [8  0] :size [12 1] :rowspan 1 :colspan 1 :children [{:type :text :value "body row 4"}]}
      {:type :cell :cell-pos [8 26] :size [11 1] :rowspan 1 :colspan 1 :children [{:type :bullet-list
                                                        :children [{:type :bullet-item
                                                                    :style "-"
                                                                    :children [{:type :text
                                                                                :value "blocks."}]}]}]}]}]}]}
