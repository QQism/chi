(ns rst.tables-tests
  (:require [rst.core :refer [process-document]]
            [midje.sweet :refer :all]))

(fact "Simple grid table without header"
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
              cell-1 => (contains {:type :cell :cell-pos [0 0] :size [12 3]
                                   :children (just [(contains
                                                     {:type :paragraph :children
                                                      (just [(contains {:type :text :value "Header 1"})])})
                                                    (contains
                                                     {:type :paragraph :children
                                                      (just [(contains {:type :text :value "body row 1"})])})])})
              cell-2 => (contains {:type :cell :cell-pos [0 13] :size [12 1]
                                   :children (just [(contains {:type :text :value "Header 2"})])})
              cell-3 => (contains {:type :cell :cell-pos [0 26] :size [11 1]
                                   :children (just [(contains {:type :text :value "Header 3"})])}))
            (let [[cell-4 cell-5] (:children row-2)]
              cell-4 => (contains {:type :cell :cell-pos [2 13] :size [12 1]
                                   :children (just [(contains {:type :text :value "Column 2"})])})
              cell-5 => (contains {:type :cell :cell-pos [2 26] :size [11 1]
                                   :children (just [(contains {:type :text :value "Column 3"})])}))
            (let [[cell-6 cell-7] (:children row-3)]
              cell-6 => (contains {:type :cell :cell-pos [4 0] :size [12 1]
                                   :children (just [(contains {:type :text :value "body row 2"})])})
              cell-7 => (contains {:type :cell :cell-pos [4 13] :size [24 1]
                                   :children (just [(contains {:type :text :value "Cells may span columns."})])}))
            (let [[cell-8 cell-9 cell-10] (:children row-4)]
              cell-8 => (contains {:type :cell :cell-pos [6 0] :size [12 1]
                                   :children (just [(contains {:type :text :value "body row 3"})])})
              cell-9 => (contains {:type :cell :cell-pos [6 13] :size [12 3]
                                   :children (just [(contains {:type :text :value "Cells may span rows."})])})
              cell-10 => (contains {:type :cell :cell-pos [6 26] :size [11 1]
                                    :children (just [(contains
                                                      {:type :bullet-list :children
                                                       (just [(contains
                                                               {:type :bullet-item :children
                                                                (just [(contains
                                                                        {:type :text :value "Cells"})])})])})])}))
            (let [[cell-11 cell-12] (:children row-5)]
              cell-11 => (contains {:type :cell :cell-pos [8 0] :size [12 1]
                                    :children (just [(contains {:type :text :value "body row 4"})])})
              cell-12 => (contains {:type :cell :cell-pos [8 26] :size [11 1]
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
     [{:type :cell :cell-pos [0  0] :size [12 3] :children [{:type :paragraph
                                                        :children [{:type :text :value "Header 1"}]}
                                                       {:type :paragraph
                                                        :children [{:type :text :value "body row 1"}]}]}
      {:type :cell :cell-pos [0 13] :size [12 1] :children [{:type :text :value "Header 2"}]}
      {:type :cell :cell-pos [0 26] :size [11 1] :children [{:type :text :value "Header 3"}]}]}

    {:type :row :id 2 :children
     [{:type :cell :cell-pos [2 13] :size [12 1]  :children [{:type :text :value "Column 2"}]}
      {:type :cell :cell-pos [2 26] :size [11 1]  :children [{:type :text :value "Column 3"}]}]}

    {:type :row :id 4 :children
     [{:type :cell :cell-pos [4  0] :size [12 1] :children [{:type :text :value "body row 2"}]}
      {:type :cell :cell-pos [4 13] :size [24 1] :children [{:type :text :value "Cells may span columns."}]}]}

    {:type :row :id 6 :children
     [{:type :cell :cell-pos [6  0] :size [12 1] :children [{:type :text :value "body row 3"}]}
      {:type :cell :cell-pos [6 13] :size [12 3] :children [{:type :paragraph
                                                        :children [{:type :text
                                                                    :value "Cells may span rows."}]}]}
      {:type :cell :cell-pos [6 26] :size [11 1] :children [{:type :bullet-list
                                                        :children [{:type :bullet-item
                                                                    :style "-"
                                                                    :children [{:type :text
                                                                                :value "Cells"}]}]}]}]}

    {:type :row :id 8 :children
     [{:type :cell :cell-pos [8  0] :size [12 1] :children [{:type :text :value "body row 4"}]}
      {:type :cell :cell-pos [8 26] :size [11 1] :children [{:type :bullet-list
                                                        :children [{:type :bullet-item
                                                                    :style "-"
                                                                    :children [{:type :text
                                                                                :value "blocks."}]}]}]}]}]}]}
