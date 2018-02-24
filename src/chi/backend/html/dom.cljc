(ns rst.backend.html.dom
  (:require [clojure.zip :as z]))

(def nodes-tags {:root nil
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
   (if tag (str "<" tag (html-attrs opts) ">") ""))
  ([tag]
   (open-tag tag nil)))

(defn close-tag [tag]
  (if tag (str "</" tag ">") ""))

(defn zip-node [node]
  (z/zipper map?
            #(-> % :children seq)
            (fn [n children]
              (assoc n :children (vec children)))
            node))

(def test-ast {:type :root
               :children[{:type :paragraph :children [{:type :text :value "Hello"}]}
                         {:type :paragraph :children [{:type :text :value "World"}]}
                         {:type :bullet-list
                          :children [{:type :bullet-item
                                      :children [{:type :text :value "First Item"}]}
                                     {:type :bullet-item
                                      :children [{:type :paragraph :children [{:type :text :value "Second Item Para"}]}
                                                 {:type :paragraph :children [{:type :text :value "Another Para"}]}]}
                                     {:type :bullet-item
                                      :children [{:type :text :value "Third Item"}]}]}
                         {:type :paragraph :children []}
                         ]})

(defn create-deep-ast [ast]
  (loop [t (zip-node ast)
         c 0]
    (if (< c 100);;(< c 648)
      (let []
        (recur (-> t (z/append-child ast) z/down z/right) (inc c)))
      (z/root t))))

;; To avoid stack over flow if the ast is nested too deeply,
;; This implementation uses stacks of nodes and doms to store data while calling `recur`
(defn nodes->html
  ""
  [nodes doms]
  (let [ast (peek nodes)]
    (if-not (nil? ast)
      (if (:children ast)
        (let [new-nodes (apply conj nodes nil (:children ast))]
          (recur new-nodes doms))
        (let [new-nodes (pop nodes)
              dom (:value ast)
              new-doms (apply vector dom doms)]
          (recur new-nodes new-doms)))
      (if (not-empty nodes)
        (let [next-node (-> nodes pop peek)
              new-nodes (-> nodes pop pop)
              tag (-> next-node :type nodes-tags)
              c (-> next-node :children count)
              inner-doms (subvec doms 0 c)
              new-doms (subvec doms c)
              html (str (open-tag tag)
                        (apply str inner-doms)
                        (close-tag tag))]
          (recur new-nodes (apply vector html new-doms)))
        (peek doms)))))

(defn ast->html [ast opts]
  (nodes->html [ast] []))

(defn ast->html-1 [ast]
  (let [tag (-> ast :type nodes-tags)
        children (:children ast)]
    (if children (str (open-tag tag)
                 (reduce #(str %1 (ast->html-1 %2)) "" children)
                 (close-tag tag))
        (:value ast)
        )))

(time (doall
       (let [node (create-deep-ast test-ast)
             ;;test-ast
             ]
         (ast->html node nil))
       ))

(time (doall
       (let [node;; (create-deep-ast test-ast)
             test-ast
             ]
         (ast->html-1 node))
       ))

(let [node ;;(create-deep-ast test-ast)
      test-ast
      ]
  (ast->html node nil))
