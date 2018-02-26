(ns chi.backend.html.core
  (:require [clojure.string :as string]))

(defrecord Tag [name attrs])

(defmulti create-tag
  (fn [node opts]
    (:type node)))

(defmethod create-tag :root [node opts]
  (let [{template :template} opts]
    (if template
      (map->Tag {:name "body"})
      (map->Tag {:name nil}))))

(defmethod create-tag :paragraph [node opts]
  (map->Tag {:name "p"}))

(defmethod create-tag :table [node opts]
  (map->Tag {:name "table"}))

(defmethod create-tag :table-body [node opts]
  (map->Tag {:name "tbody"}))

(defmethod create-tag :table-header [node opts]
  (map->Tag {:name "thead"}))

(defmethod create-tag :row [node opts]
  (map->Tag {:name "tr"}))

(defmethod create-tag :cell [node opts]
  (map->Tag {:name "td"}))

(defmethod create-tag :bullet-list [node opts]
  (map->Tag {:name "ul"}))

(defmethod create-tag :bullet-item [node opts]
  (map->Tag {:name "li"}))

(defmethod create-tag :blockquote [node opts]
  (map->Tag {:name "blockquote"}))

(defmethod create-tag :section [node opts]
  (let [name (:name node)]
    (map->Tag {:name "div"
               :attrs {:class "section" :id name}})))

(defmethod create-tag :header [node opts]
  (let [level (:level node)]
    (map->Tag {:name (str "h" level)})))

(defmethod create-tag :text [node opts]
  (map->Tag {:name nil}))

(defmethod create-tag :strong-emphasis [node opts]
  (map->Tag {:name "strong"}))

(defmethod create-tag :emphasis [node opts]
  (map->Tag {:name "em"}))

(defn ^:private html-attrs [opts]
  (reduce-kv (fn [s k v] (str s " " (name k) "=\"" v "\"") ) "" opts))

(defn ^:private open-tag
  ([tag]
   (let [{name :name attrs :attrs} tag]
     (if name (str "<" name (html-attrs attrs) ">") ""))))

(defn ^:private close-tag [tag]
  (let [{name :name} tag]
    (if name (str "</" name ">") "")))

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
              tag (create-tag next-node opts)
              c (-> next-node :children count)
              inner-doms (subvec doms 0 c)
              new-doms (subvec doms c)
              html (build-nested-html tag inner-doms opts)]
          (recur new-nodes (apply vector html new-doms) opts))
        (peek doms)))))

(defn ast->html [ast opts]
  (nodes->html [ast] [] opts))
