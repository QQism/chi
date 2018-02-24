(ns chi.frontend.tree
  (:refer-clojure :exclude [replace remove])
  (:require [clojure.zip :as z]))

(def ^:const up z/up)
(def ^:const down z/down)
(def ^:const left z/left)
(def ^:const right z/right)
(def ^:const root z/root)
(def ^:const node z/node)
(def ^:const children z/children)
(def ^:const path z/path)
(def ^:const edit z/edit)
(def ^:const replace z/replace)
(def ^:const remove z/remove)
(def ^:const append-child z/append-child)
(def ^:const rightmost z/rightmost)
(def ^:const zipper z/zipper)

(defn move-to-latest-child [zt]
  (-> zt down rightmost))

(defn uid-path [zt]
  (-> zt
      path
      (->> (map :uid))
      (conj (:uid (node zt)))
      reverse))

(defn up-to-root [zt]
  (if-let [parent (up zt)]
    (recur parent)
    zt))

(defn children-loc [zt]
  (let [children-count (-> zt children count)
        first-child (down zt)]
    (take children-count (iterate right first-child))))

(defn find-loc-with-uid [zt child-uid]
  (if (-> zt node :uid (= child-uid))
    zt
    (some #(if (-> % node :uid (= child-uid)) %)
          (children-loc zt))))

(defn node->zipper-tree [node]
  (zipper map?
          #(-> % :children seq)
          (fn [n children]
            (assoc n :children (vec children)))
          node))
