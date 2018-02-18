(ns rst.render.html.dom)

(def nodes-tags {:root "body"
                 :paragraph "p"
                 :table "table"
                 :table-body "tbody"
                 :table-header "thead"
                 :row "tr"
                 :cell "td"
                 :bullet-list "ul"
                 :bullet-item "li"
                 :blockquote "blockquote"
                 :section "section"
                 :text nil
                 :strong-emphasis "string"
                 :emphasis "em"})

(defn ^:private html-attrs [opts]
  (reduce-kv (fn [s k v] (str s " " (name k) "=\"" v "\"") ) "" opts))

(defn open-tag
  ([tag opts]
   (str "<" tag (html-attrs opts) ">"))
  ([tag]
   (open-tag tag nil)))

(defn close-tag [tag]
  (str "</" tag ">"))

(defprotocol IHtmlRender
  (node->html [_]))

(defn ast->html [ast]
  ;;TODO recur render html node  recursively
  ;; Test with deeeep tree
  (let [tag (-> ast :type nodes-tags)
        children (:children ast)]
    (if tag (str (open-tag tag)
                 (node->html children)
                 (close-tag tag))
        (:value ast)
        )))

(extend-protocol IHtmlRender
  #?(:clj clojure.lang.PersistentArrayMap
     :cljs cljs.core.PersistentArrayMap)
  (node->html [node] (ast->html node))
  #?(:clj clojure.lang.PersistentVector
     :cljs cljs.core.PersistentVector)
  (node->html [nodes]
    (reduce (fn [s n] (str s (node->html n))) "" nodes))
  nil
  (node->html [_] ""))

(def test-ast {:type :root
               :children[{:type :paragraph :children [{:type :text :value "Hello"}]}
                         {:type :paragraph :children [{:type :text :value "World"}]}]})
(ast->html test-ast)
