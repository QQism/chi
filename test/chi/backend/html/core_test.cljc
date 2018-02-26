(ns chi.backend.html.core-test
  (:require #?(:cljs [cljs.test    :as t :refer-macros [deftest testing is]]
               :clj  [clojure.test :as t :refer        [deftest testing is]])
            #?(:cljs [chi.test-support :refer [assert-node]]
               :clj  [chi.assert-macros :refer [assert-node]])
            [clojure.string :as string]
            [chi.backend.html.core :refer [ast->html]]))

#?(:cljs (enable-console-print!))

(deftest pretty-format
  (let [ast {:type :root
             :children [{:type :paragraph
                         :children [{:type :text
                                     :value "Lorem Ipsum"}]}
                        {:type :bullet-list
                         :style "-"
                         :children [{:type :bullet-item
                                     :indent 2
                                     :children [{:type :text :value "First Item"}]}
                                    {:type :bullet-item
                                     :indent 2
                                     :children [{:type :text :value "Second Item"}]}
                                    ]}
                        ]}]
    (testing "with pretty format"
      (is (= ["<p>"
              "  Lorem Ipsum"
              "</p>"
              "<ul>"
              "  <li>"
              "    First Item"
              "  </li>"
              "  <li>"
              "    Second Item"
              "  </li>"
              "</ul>"]
             (string/split (ast->html ast {:pretty true}) #"\r\n")
             )))
    (testing "without pretty format"
      (is (= "<p>Lorem Ipsum</p><ul><li>First Item</li><li>Second Item</li></ul>"
             (ast->html ast nil))))))
