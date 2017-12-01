(ns rst.parser
  (:require [hiccup.core :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.zip :as zip]
            [clojure.walk :as walk]
            [clojure.core.match :refer [match]]))

(def title-data (slurp (io/resource "demo.rst")))

(defprotocol TreeNode
  (branch? [node] "Is it possible for node to have children?")
  (node-children [node] "Return children of this node.")
  (make-node [node children] "Makes new node from existing node and new children."))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(keep-indexed #(if (odd? %1) %2) [:a :b :c :d :e])

(defrecord RootNode [name children])
(defrecord TitleNode [content pos children])
(defrecord DocNode [content pos children])
(defrecord SpaceNode [pos])
(defrecord NewlineNode [pos])
(defrecord EofNode [pos])

(extend-protocol TreeNode
  DocNode
  (branch? [node] true)
  (node-children [node]
    (seq (:children node)))
  (make-node [node children]
    (DocNode. (:content node) [0 0] children))
  SpaceNode
  (branch? [_] false)
  (node-children [_] nil)
  (make-node [node children] nil)
  NewlineNode
  (branch? [_] false)
  (node-children [_] nil)
  (make-node [node children] nil)
  EofNode
  (branch? [_] false)
  (node-children [_] nil)
  (make-node [node children] nil)
  RootNode
  (branch? [node] true)
  (node-children [node]
    (seq (:children node)))
  (make-node [node children]
    (DocNode. (:content node) [0 0] children)))

(def parent
  (DocNode.
   "Hello" [0 0]
   [(DocNode. "Child-1" [0 0] '())
    (DocNode. "Child-2" [0 0] [(DocNode. "Child-2-1" [0 0] '())
                               (DocNode. "Child-2-2" [0 0] '())
                               (DocNode. "Child-2-3" [0 0] '())])
    (DocNode. "Child-3" [0 0] [(DocNode. "Child-3-1" [0 0] '())
                               (DocNode. "Child-3-2" [0 0] '())])]))

(defn tree-zip
  "Make a zipper out of a tree."
  [root]
  (zip/zipper branch? node-children make-node root))

(def tree-z (tree-zip parent))

(-> tree-z
    zip/down ;; Child 1
    zip/right ;; Child 2
    zip/down ;; Child 2-1
    zip/right ;; Child 2-2
    zip/right ;; Child 2-3
    zip/remove ;; Remove Child 2-3, back at Child 2-2
    zip/up ;; Child 2
    (zip/replace (DocNode. "Child 2-x" [0 0] '()))
    (zip/insert-left (DocNode. "Child 1.5" [0 0] '()))
    zip/root)

;; (-> tree-z
;;     zip/append-child (DocNode. "Child 4" [0 0] []))


(zip/append-child tree-z (DocNode. "Child 4" [0 0] []))

(def root-doc-node
  (RootNode. "Root Node" '()))

(def tree-doc (tree-zip root-doc-node))

(def eof -1)
(def new-line 10)
(def space 32)

(defn create-node [c pos children]
  (case c
    -1 (EofNode. pos)
    10 (NewlineNode. pos)
    32 (SpaceNode. pos)
    (DocNode. (char c) pos children)))

(defn next-pos-tree [tree]
  (-> tree
      zip/children
      zip/rightmost
      first
      ((fn [node]
         (if (nil? node)
           [0 0]
           (let [[line col] (:pos node)]
             (if (instance? NewlineNode node)
               [(inc line) 0]
               [line (inc col)])))))))

(defn new-block? [node]
  (and (instance? NewlineNode node) (instance? NewlineNode (zip/prev node))))

(defn enter-child? [node]
  (or (instance? RootNode node) (instance? TitleNode node)))

(defn add-node-to-tree [loc next-node]
  (let [last-node (zip/node loc)]
    (if (enter-child? last-node)
      (zip/insert-child loc next-node)
      (zip/insert-right loc next-node))))

(enter-child? (zip/root tree-doc))
(add-node-to-tree tree-doc (DocNode. "fds" [0 0] []))

(with-open [r (clojure.java.io/input-stream "./resources/simple.rst")]
  (loop [c    (.read r)
         line 0
         col  0
         tree tree-doc]
    (if (not= c eof)
      (do
        (let [newline? (= c 10)
              next-line (if newline? (inc line) line)
              next-col (if newline? 0 (inc col))
                                        ; space? (= c SPACE)
                                        ; char-val (if space? "space" (char c))
              ]
          (recur (.read r)
                 next-line
                 next-col
                 (zip/append-child tree
                                   (create-node c [next-line next-col] '())))))
      tree)))

(with-open [r (clojure.java.io/input-stream "./resources/demo.rst")]
  (loop [c (.read r)
         line 0
         col 0]
    (if (not= c eof)
      (println (char c)))))
