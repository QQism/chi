(ns rst.paragraph-new-test
  (:require [clojure.test :refer :all]
            [rst.core :refer :all]))

(deftest single-line-paragraph
  (let [lines ["Lorem Ipsum is simply dummy text"]
        root (process-document lines)]
    (are [fs r] (= ((apply comp fs) root) r)
      [count :children] 1
      [#(select-keys % [:type]) first :children] {:type :paragraph}
      [count :children first :children] 1
      [#(select-keys % [:type :value]) first :children first :children] {:type :text
                                                                         :value "Lorem Ipsum is simply dummy text"}
      )))
