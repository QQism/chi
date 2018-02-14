(ns rst.tables-new-test
  (:require [clojure.test :refer :all]
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

      (let [[table-body] (:children table)]
        (assert-node {:type :table-body :children [:children-count 2 count]} table-body)

        (let [[row-1 row-2] (:children table-body)]
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
