(ns rst.tables-new-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [rst.test-support :refer :all]
            [rst.core :refer [process-document]]))

(deftest simple-table-without-header
  (let [lines ["+------------+------------+-----------+"
               "| Cell 1-1   |  Cell 1-2  |  Cell 1-3 |"
               "+------------+------------+-----------+"
               "| Cell 2-1   |  Cell 2-2  |  Cell 2-3 |"
               "+------------+------------+-----------+"]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 1 count]} root)

    (let [[table] (:children root)]
      (assert-node {:type :table
                    :width 39
                    :height 5
                    :col-ids (sorted-set 0 13 26)
                    :children [:children-count 1 count]}
                   table)

      (let [[body] (:children table)]
        (assert-node {:type :table-body :children [:children-count 2 count]} body)

        (let [[row-1 row-2] (:children body)]
          (assert-node {:type :row :id 0 :children [:children-count 3 count]} row-1)
          (assert-node {:type :row :id 2 :children [:children-count 3 count]} row-2)

          (let [[cell-1-1 cell-1-2 cell-1-3] (:children row-1)]
            (assert-node {:type :cell :top 0 :left 0
                          :width 12 :height 1 :children [{:type :text :value "Cell 1-1"}]}
                         cell-1-1)
            (assert-node {:type :cell :top 0 :left 13
                          :width 12 :height 1 :children [{:type :text :value "Cell 1-2"}]}
                         cell-1-2)
            (assert-node {:type :cell :top 0 :left 26
                          :width 11 :height 1 :children [{:type :text :value "Cell 1-3"}]}
                         cell-1-3))

          (let [[cell-2-1 cell-2-2 cell-2-3] (:children row-2)]
            (assert-node {:type :cell :top 2 :left 0
                          :width 12 :height 1 :children [{:type :text :value "Cell 2-1"}]}
                         cell-2-1)
            (assert-node {:type :cell :top 2 :left 13
                          :width 12 :height 1 :children [{:type :text :value "Cell 2-2"}]}
                         cell-2-2)
            (assert-node {:type :cell :top 2 :left 26
                          :width 11 :height 1 :children [{:type :text :value "Cell 2-3"}]}
                         cell-2-3)))))))

(deftest empty-grid-table
  (let [lines ["+------------+------------+-----------+"
               "+------------+------------+-----------+"]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 1 count]} root)


    (let [[table] (:children root)]
      (assert-node {:type :table
                    :width 39
                    :height 2
                    :col-ids (sorted-set 0 13 26)
                    :children [:children-count 1 count]}
                   table)


      (let [[body] (:children table)]
        (assert-node {:type :table-body :children [:children-count 1 count]} body)

        (let [[row-1] (:children body)]
          (assert-node {:type :row :id 0 :children
                        [{:type :cell :top 0 :left 0  :width 12 :height 0 :children []}
                         {:type :cell :top 0 :left 13 :width 12 :height 0 :children []}
                         {:type :cell :top 0 :left 26 :width 11 :height 0 :children []}]}
                       row-1))))))

(deftest simple-grid-table-with-header
  (let [lines ["+------------+------------+-----------+"
               "| Header 1   |  Header 2  |  Header 3 |"
               "+============+============+===========+"
               "| Cell 1-1   |  Cell 1-2  |  Cell 1-3 |"
               "+------------+------------+-----------+"
               "| Cell 2-1   |  Cell 2-2  |  Cell 2-3 |"
               "+------------+------------+-----------+"]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 1 count]} root)

    (let [[table] (:children root)]
      (assert-node {:type :table
                    :width 39
                    :height 7
                    :col-ids (sorted-set 0 13 26)
                    :children [:children-count 2 count]}
                   table)
      (let [[header body] (:children table)]
        (assert-node {:type :table-header :children [:children-count 1 count]} header)

        (let [[row-1] (:children header)]
          (assert-node {:type :row :id 0 :children [:children-count 3 count]} row-1)

          (let [[header-cell-1 header-cell-2 header-cell-3] (:children row-1)]

            (assert-node {:type :cell :top 0 :left 0
                          :width 12 :height 1 :children [{:type :text :value "Header 1"}]}
                         header-cell-1)

            (assert-node {:type :cell :top 0 :left 13
                          :width 12 :height 1 :children [{:type :text :value "Header 2"}]}
                         header-cell-2)

            (assert-node {:type :cell :top 0 :left 26
                          :width 11 :height 1 :children [{:type :text :value "Header 3"}]}
                         header-cell-3)))

        (assert-node {:type :table-body :children [:children-count 2 count]} body)

        (let [[row-2 row-3] (:children body)]
          (assert-node {:type :row :id 2 :children [:children-count 3 count]} row-2)
          (assert-node {:type :row :id 4 :children [:children-count 3 count]} row-3)

          (let [[cell-1 cell-2 cell-3] (:children row-2)]
            (assert-node {:type :cell :top 2 :left 0 :width 12 :height 1
                          :children [{:type :text :value "Cell 1-1"}]}
                         cell-1)
            (assert-node {:type :cell :top 2 :left 13 :width 12 :height 1
                          :children [{:type :text :value "Cell 1-2"}]}
                         cell-2)
            (assert-node {:type :cell :top 2 :left 26 :width 11 :height 1
                          :children [{:type :text :value "Cell 1-3"}]}
                         cell-3))

          (let [[cell-4 cell-5 cell-6] (:children row-3)]
            (assert-node {:type :cell :top 4 :left 0 :width 12 :height 1
                          :children [{:type :text :value "Cell 2-1"}]}
                         cell-4)
            (assert-node {:type :cell :top 4 :left 13 :width 12 :height 1
                          :children [{:type :text :value "Cell 2-2"}]}
                         cell-5)
            (assert-node {:type :cell :top 4 :left 26 :width 11 :height 1
                          :children [{:type :text :value "Cell 2-3"}]}
                         cell-6)))))))

(deftest simple-grid-table-with-non-valid-header
  (let [lines ["+------------+------------+-----------+"
               "| Header 1   |  Header 2  |  Header 3 |"
               "+============+============+===========+"
               "| Cell 1-1   |  Cell 1-2  |  Cell 1-3 |"
               "+============+============+===========+"
               "| Cell 2-1   |  Cell 2-2  |  Cell 2-3 |"
               "+------------+------------+-----------+"]
        root (process-document lines)]
    (assert-node {:type :root :children [:children-count 1 count]} root)

    (let [[error] (:children root)]
      (assert-node {:type :error
                    :level 3
                    :pos [1 1]
                    :children [{:type :paragraph
                                :children
                                [{:type :text
                                  :value (str "Malformed table. "
                                              "Multiple head/body row separators "
                                              "(table lines 2, 4); only one allowed.")}]}
                               {:type :preserve
                                :value (string/join "\r\n" lines)}]}
                   error))))

(deftest complex-grid-table-with-cells-spanning-rows-and-columns-without-header
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
    (assert-node {:type :root :children [:children-count 1 count]} root)

    (let [[table] (:children root)
          [body] (:children table)]
      (assert-node {:type :table
                    :col-ids (sorted-set 0 13 26)
                    :children [:children-count 1 count]} table)
      (assert-node {:type :table-body :children [:children-count 5 count]} body)

      (let [[row-1 row-2 row-3 row-4 row-5] (:children body)]
        (assert-node {:type :row :id 0 :children [:children-count 3 count]} row-1)
        (assert-node {:type :row :id 2 :children [:children-count 2 count]} row-2)
        (assert-node {:type :row :id 4 :children [:children-count 2 count]} row-3)
        (assert-node {:type :row :id 6 :children [:children-count 3 count]} row-4)
        (assert-node {:type :row :id 8 :children [:children-count 2 count]} row-5)

        (let [[cell-1 cell-2 cell-3] (:children row-1)]
          (assert-node {:type :cell :top 0 :left 0 :width 12 :height 3
                        :children [{:type :paragraph :children
                                    [{:type :text :value "Cell 1-1"}]}
                                   {:type :paragraph :children
                                    [{:type :text :value "More rows"}]}]}
                       cell-1)
          (assert-node {:type :cell :top 0 :left 13 :width 12 :height 1
                        :children [{:type :text :value "Cell 1-2"}]}
                       cell-2)
          (assert-node {:type :cell :top 0 :left 26 :width 11 :height 1
                        :children [{:type :text :value "Cell 1-3"}]}
                       cell-3))

        (let [[cell-4 cell-5] (:children row-2)]
          (assert-node {:type :cell :top 2 :left 13 :width 12 :height 1
                        :children [{:type :text :value "Cell 2-2"}]}
                       cell-4)
          (assert-node {:type :cell :top 2 :left 26 :width 11 :height 1
                        :children [{:type :text :value "Cell 2-3"}]}
                       cell-5))

        (let [[cell-6 cell-7] (:children row-3)]
          (assert-node {:type :cell :top 4 :left 0 :width 12 :height 1
                        :children [{:type :text :value "Cell 3-1"}]}
                       cell-6)
          (assert-node {:type :cell :top 4 :left 13 :width 24 :height 1
                        :children [{:type :text :value "Cells may span columns."}]}
                       cell-7))

        (let [[cell-8 cell-9 cell-10] (:children row-4)]
          (assert-node {:type :cell :top 6 :left 0 :width 12 :height 1
                        :children [{:type :text :value "Cell 4-1"}]}
                       cell-8)
          (assert-node {:type :cell :top 6 :left 13 :width 12 :height 3
                        :children [{:type :text :value "Cells may span rows."}]}
                       cell-9)
          (assert-node {:type :cell :top 6 :left 26 :width 11 :height 1
                        :children [{:type :bullet-list :style "-" :children
                                    [{:type :bullet-item :indent 29 :children
                                      [{:type :text :value "Item 1"}]}]}]}
                       cell-10))

        (let [[cell-11 cell-12] (:children row-5)]
          (assert-node {:type :cell :top 8 :left 0 :width 12 :height 1
                        :children [{:type :text :value "Cell 5-1"}]}
                       cell-11)
          (assert-node {:type :cell :top 8 :left 26 :width 11 :height 1
                        :children [{:type :bullet-list :style "-" :children
                                    [{:type :bullet-item :indent 29 :children
                                      [{:type :text :value "Item 2"}]}]}]}
                       cell-12))))))

(deftest complex-grid-table-with-cells-spanning-rows-and-columns-with-header
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
        (assert-node {:type :root :children [:children-count 1 count]} root)

        (let [[table] (:children root)
              [header body] (:children table)]

          (assert-node {:type :table
                        :col-ids (sorted-set 0 13 26)
                        :children [:children-count 2 count]} table)
          (assert-node {:type :table-header :children [:children-count 3 count]} header)
          (assert-node {:type :table-body   :children [:children-count 2 count]} body)

          (let [[row-1 row-2 row-3] (:children header)]
            (assert-node {:type :row :id 0 :children [:children-count 3 count]} row-1)
            (assert-node {:type :row :id 2 :children [:children-count 2 count]} row-2)
            (assert-node {:type :row :id 4 :children [:children-count 2 count]} row-3)

            (let [[cell-1 cell-2 cell-3] (:children row-1)]
              (assert-node {:type :cell :top 0 :left 0 :width 12 :height 3
                            :children [{:type :paragraph :children
                                        [{:type :text :value "Cell 1-1"}]}
                                       {:type :paragraph :children
                                        [{:type :text :value "More rows"}]}]}
                           cell-1)
              (assert-node {:type :cell :top 0 :left 13 :width 12 :height 1
                            :children [{:type :text :value "Cell 1-2"}]}
                           cell-2)
              (assert-node {:type :cell :top 0 :left 26 :width 11 :height 1
                            :children [{:type :text :value "Cell 1-3"}]}
                           cell-3))

            (let [[cell-4 cell-5] (:children row-2)]
              (assert-node {:type :cell :top 2 :left 13 :width 12 :height 1
                            :children [{:type :text :value "Cell 2-2"}]}
                           cell-4)
              (assert-node {:type :cell :top 2 :left 26 :width 11 :height 1
                            :children [{:type :text :value "Cell 2-3"}]}
                           cell-5))

            (let [[cell-6 cell-7] (:children row-3)]
              (assert-node {:type :cell :top 4 :left 0 :width 12 :height 1
                            :children [{:type :text :value "Cell 3-1"}]}
                           cell-6)
              (assert-node {:type :cell :top 4 :left 13 :width 24 :height 1
                            :children [{:type :text :value "Cells may span columns."}]}
                           cell-7)))

          (let [[row-4 row-5] (:children body)]
            (assert-node {:type :row :id 6 :children [:children-count 3 count]} row-4)
            (assert-node {:type :row :id 8 :children [:children-count 2 count]} row-5)

            (let [[cell-8 cell-9 cell-10] (:children row-4)]
              (assert-node {:type :cell :top 6 :left 0 :width 12 :height 1
                            :children [{:type :text :value "Cell 4-1"}]}
                           cell-8)
              (assert-node {:type :cell :top 6 :left 13 :width 12 :height 3
                            :children [{:type :text :value "Cells may span rows."}]}
                           cell-9)
              (assert-node {:type :cell :top 6 :left 26 :width 11 :height 1
                            :children [{:type :bullet-list :style "-" :children
                                        [{:type :bullet-item :indent 29 :children
                                          [{:type :text :value "Item 1"}]}]}]}
                           cell-10))

            (let [[cell-11 cell-12] (:children row-5)]
              (assert-node {:type :cell :top 8 :left 0 :width 12 :height 1
                            :children [{:type :text :value "Cell 5-1"}]}
                           cell-11)
              (assert-node {:type :cell :top 8 :left 26 :width 11 :height 1
                            :children [{:type :bullet-list :style "-" :children
                                        [{:type :bullet-item :indent 29 :children
                                          [{:type :text :value "Item 2"}]}]}]}
                           cell-12))))))

(deftest malformed-table-with-non-rectangle-cell
      (let [lines ["+------------+------------+-----------+"
                   "|A malformed |                        |"
                   "|            +                        |"
                   "| table      |                        |"
                   "+------------+                        +"
                   "| This is a non rectangle cell        |"
                   "+------------+------------+-----------+"]
            root (process-document lines)]
        (assert-node {:type :root :children [:children-count 1 count]} root)

        (let [[error] (:children root)]
          (assert-node {:type :error
                        :level 3
                        :pos [1 1]
                        :children [{:type :paragraph
                                    :children
                                    [{:type :text
                                      :value (str "Malformed table. "
                                                  "Parse incomplete.")}]}
                                   {:type :preserve
                                    :value (string/join "\r\n" lines)}]}
                       error))))

(deftest simple-grid-table-and-paragraph-without-blank-line
      (let [lines ["+------------+------------+-----------+"
                   "| Cell 1-1   |  Cell 1-2  |  Cell 1-3 |"
                   "+------------+------------+-----------+"
                   "| Cell 2-1   |  Cell 2-2  |  Cell 2-3 |"
                   "+------------+------------+-----------+"
                   "Here should be a blank line."]
            root (process-document lines)]
        (assert-node {:type :root :children [:children-count 3 count]} root)

        (let [[table error paragraph] (:children root)
              [body] (:children table)]
          (assert-node {:type :table
                        :width 39
                        :height 5
                        :col-ids (sorted-set 0 13 26)
                        :children [:children-count 1 count]}
                       table)

          (assert-node {:type :table-body :children [:children-count 2 count]} body)

          (let [[body] (:children table)]
            (assert-node {:type :table-body :children [:children-count 2 count]} body)

            (let [[row-1 row-2] (:children body)]
              (assert-node {:type :row :id 0 :children [:children-count 3 count]} row-1)
              (assert-node {:type :row :id 2 :children [:children-count 3 count]} row-2)

              (let [[cell-1-1 cell-1-2 cell-1-3] (:children row-1)]
                (assert-node {:type :cell :top 0 :left 0
                              :width 12 :height 1 :children [{:type :text :value "Cell 1-1"}]}
                             cell-1-1)
                (assert-node {:type :cell :top 0 :left 13
                              :width 12 :height 1 :children [{:type :text :value "Cell 1-2"}]}
                             cell-1-2)
                (assert-node {:type :cell :top 0 :left 26
                              :width 11 :height 1 :children [{:type :text :value "Cell 1-3"}]}
                             cell-1-3))

              (let [[cell-2-1 cell-2-2 cell-2-3] (:children row-2)]
                (assert-node {:type :cell :top 2 :left 0
                              :width 12 :height 1 :children [{:type :text :value "Cell 2-1"}]}
                             cell-2-1)
                (assert-node {:type :cell :top 2 :left 13
                              :width 12 :height 1 :children [{:type :text :value "Cell 2-2"}]}
                             cell-2-2)
                (assert-node {:type :cell :top 2 :left 26
                              :width 11 :height 1 :children [{:type :text :value "Cell 2-3"}]}
                             cell-2-3))))

          (assert-node {:type :error
                        :level 2
                        :pos [5 1]
                        :children [{:type :paragraph
                                    :children
                                    [{:type :text
                                      :value "Blank line required after table."}]}]}
                       error)

          (assert-node {:type :paragraph
                        :children [{:type :text
                                    :value "Here should be a blank line."}]}
                       paragraph))))

(deftest simple-grid-table-and-paragraph-with-blank-line
      (let [lines ["+------------+------------+-----------+"
                   "| Cell 1-1   |  Cell 1-2  |  Cell 1-3 |"
                   "+------------+------------+-----------+"
                   "| Cell 2-1   |  Cell 2-2  |  Cell 2-3 |"
                   "+------------+------------+-----------+"
                   ""
                   "This is a paragraph."]
            root (process-document lines)]

        (assert-node {:type :root :children [:children-count 2 count]} root)

        (let [[table paragraph] (:children root)
              [body] (:children table)]
          (assert-node {:type :table
                        :width 39
                        :height 5
                        :col-ids (sorted-set 0 13 26)
                        :children [:children-count 1 count]}
                       table)

          (assert-node {:type :table-body :children [:children-count 2 count]} body)

          (let [[body] (:children table)]
            (assert-node {:type :table-body :children [:children-count 2 count]} body)

            (let [[row-1 row-2] (:children body)]
              (assert-node {:type :row :id 0 :children [:children-count 3 count]} row-1)
              (assert-node {:type :row :id 2 :children [:children-count 3 count]} row-2)

              (let [[cell-1-1 cell-1-2 cell-1-3] (:children row-1)]
                (assert-node {:type :cell :top 0 :left 0
                              :width 12 :height 1 :children [{:type :text :value "Cell 1-1"}]}
                             cell-1-1)
                (assert-node {:type :cell :top 0 :left 13
                              :width 12 :height 1 :children [{:type :text :value "Cell 1-2"}]}
                             cell-1-2)
                (assert-node {:type :cell :top 0 :left 26
                              :width 11 :height 1 :children [{:type :text :value "Cell 1-3"}]}
                             cell-1-3))

              (let [[cell-2-1 cell-2-2 cell-2-3] (:children row-2)]
                (assert-node {:type :cell :top 2 :left 0
                              :width 12 :height 1 :children [{:type :text :value "Cell 2-1"}]}
                             cell-2-1)
                (assert-node {:type :cell :top 2 :left 13
                              :width 12 :height 1 :children [{:type :text :value "Cell 2-2"}]}
                             cell-2-2)
                (assert-node {:type :cell :top 2 :left 26
                              :width 11 :height 1 :children [{:type :text :value "Cell 2-3"}]}
                             cell-2-3))))

          (assert-node {:type :paragraph
                        :children [{:type :text
                                    :value "This is a paragraph."}]}
                       paragraph))))
