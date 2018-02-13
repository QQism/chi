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
    (is (assert-node root {:type :root :children [:children-count 1 count]}))

    (let [[table] (:children root)]
      (is (assert-node table {:type :table
                              :width 39
                              :height 5
                              :col-ids (sorted-set 0 13 26)
                              :children [:children-count 1 count]}))

      (let [[table-body] (:children table)]
        (is (assert-node table-body {:type :table-body :children [:children-count 2 count]}))

        (let [[row-1 row-2] (:children table-body)]
          (is (assert-node row-1 {:type :row :id 0 :children [:children-count 3 count]}))
          (is (assert-node row-2 {:type :row :id 2 :children [:children-count 3 count]}))

          (let [[cell-1-1 cell-1-2 cell-1-3] (:children row-1)]
            (is (assert-node cell-1-1 {:type :cell :top 0 :left 0
                                       :width 12 :height 1 :children [{:type :text :value "Cell 1-1"}]}))
            (is (assert-node cell-1-2 {:type :cell :top 0 :left 13
                                       :width 12 :height 1 :children [{:type :text :value "Cell 1-2"}]}))
            (is (assert-node cell-1-3 {:type :cell :top 0 :left 26
                                       :width 11 :height 1 :children [{:type :text :value "Cell 1-3"}]})))

          (let [[cell-2-1 cell-2-2 cell-2-3] (:children row-2)]
            (is (assert-node cell-2-1 {:type :cell :top 2 :left 0
                                       :width 12 :height 1 :children [{:type :text :value "Cell 2-1"}]}))
            (is (assert-node cell-2-2 {:type :cell :top 2 :left 13
                                       :width 12 :height 1 :children [{:type :text :value "Cell 2-2"}]}))
            (is (assert-node cell-2-3 {:type :cell :top 2 :left 26
                                       :width 11 :height 1 :children [{:type :text :value "Cell 2-3"}]}))))))))
