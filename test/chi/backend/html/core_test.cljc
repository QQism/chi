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
             (string/split (ast->html ast {:pretty true}) #"\r\n"))))
    (testing "without pretty format"
      (is (= "<p>Lorem Ipsum</p><ul><li>First Item</li><li>Second Item</li></ul>"
             (ast->html ast nil))))))

(deftest nested-sections
  (let [ast {:type :root,
             :children
             [{:type :section,:style "underline=",:level 1,:name "first-title",
               :children
               [{:type :header,:children [{:type :text, :value "First Title"}],:level 1}
                {:type :paragraph,:children [{:type :text,:value "First paragraph content"}]}
                {:type :section,:style "overline-",:level 2,:name "nested-title",
                 :children
                 [{:type :header,:children [{:type :text, :value "Nested Title"}],:level 2}
                  {:type :paragraph,:children [{:type :text, :value "Nested content"}]}
                  {:type :section,:style "underline+",:level 3,:name "nested-nested-title",
                   :children
                   [{:type :header,:children [{:type :text,:value "Nested Nested Title"}],:level 3}]}]}]}
              {:type :section,:style "underline=",:level 1,:name "second-title",
               :children
               [{:type :header,:children [{:type :text, :value "Second Title"}],:level 1}
                {:type :paragraph,:children [{:type :text,:value "Second paragraph content"}]}
                {:type :section,:style "underline+",:level 2,:name "another-nested-title",:children
                 [{:type :header,
                   :children
                   [{:type :text,:value "Another Nested Title"}],:level 2}
                  {:type :paragraph,:children [{:type :text,:value "Another nested content"}]}]}]}]}]
    (is (= ["<section>"
            "  <h1>"
            "    First Title"
            "  </h1>"
            "  <p>"
            "    First paragraph content"
            "  </p>"
            "  <section>"
            "    <h2>"
            "      Nested Title"
            "    </h2>"
            "    <p>"
            "      Nested content"
            "    </p>"
            "    <section>"
            "      <h3>"
            "        Nested Nested Title"
            "      </h3>"
            "    </section>"
            "  </section>"
            "</section>"
            "<section>"
            "  <h1>"
            "    Second Title"
            "  </h1>"
            "  <p>"
            "    Second paragraph content"
            "  </p>"
            "  <section>"
            "    <h2>"
            "      Another Nested Title"
            "    </h2>"
            "    <p>"
            "      Another nested content"
            "    </p>"
            "  </section>"
            "</section>"]
           (string/split (ast->html ast {:pretty true}) #"\r\n")))))
