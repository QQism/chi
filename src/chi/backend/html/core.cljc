(ns chi.backend.html.core)

(def ^:private nodes-tags {:root nil
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

(defn ^:private open-tag
  ([tag opts]
   (if tag (str "<" tag (html-attrs opts) ">") ""))
  ([tag]
   (open-tag tag nil)))

(defn ^:private close-tag [tag]
  (if tag (str "</" tag ">") ""))

;; To avoid stack over flow if the ast is nested too deeply,
;; This implementation uses stacks of nodes and doms to store data while calling `recur`
(defn ^:private nodes->html
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
