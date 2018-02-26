(ns chi.backend.html.core
  (:require [clojure.string :as string]))

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
                           :header "h"
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

(defn ^:private pretty-html [otag ctag doms]
  (if (not-empty otag)
    (let[indented-doms (map #(string/replace % "\r\n" "\r\n  ") doms)]
      (str otag
           (reduce #(str %1 "\r\n  " %2) "" indented-doms)
           "\r\n"
           ctag))
    (reduce #(str %1 %2 "\r\n") "" doms)))

(defn ^:private raw-html [otag ctag doms]
  (str otag (apply str doms) ctag))

(defn build-nested-html [tag inner-doms opts]
  (let [otag (open-tag tag)
        ctag (close-tag tag)]
    (if (:pretty opts)
      (pretty-html otag ctag inner-doms)
      (raw-html otag ctag inner-doms))))

;; To avoid stack over flow if the ast is nested too deeply,
;; instead of using the trivial recursion,
;; this implementation uses stacks of nodes and doms to store data while calling `recur`
;;
;; Given that tree n nodes with n= r+l,
;; r is the number of root nodes, l is the number of leaf nodes
;; Comlexity:
;;   - Time: 0(2r+l)
;;   - Space:
;;     + nodes stack: 0(logN)
;;     + doms stack: 0(logN)
;;
;; This implementation may subject to change later
(defn ^:private nodes->html
  [nodes doms opts]
  (let [ast (peek nodes)]
    (if-not (nil? ast)
      (if (:children ast)
        (let [new-nodes (apply conj nodes nil (:children ast))]
          (recur new-nodes doms opts))
        (let [new-nodes (pop nodes)
              dom (:value ast)
              new-doms (apply vector dom doms)]
          (recur new-nodes new-doms opts)))
      (if (not-empty nodes)
        (let [next-node (-> nodes pop peek)
              new-nodes (-> nodes pop pop)
              tag (-> next-node :type nodes-tags)
              c (-> next-node :children count)
              inner-doms (subvec doms 0 c)
              new-doms (subvec doms c)
              html (build-nested-html tag inner-doms opts)]
          (recur new-nodes (apply vector html new-doms) opts))
        (peek doms)))))

(defn ast->html [ast opts]
  (nodes->html [ast] [] opts))
