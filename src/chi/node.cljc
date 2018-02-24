(ns rst.node)

(def ^{:private true :const true} uid-prefix "AST_NODE_")

(defn ^:private get-uid []
  (gensym uid-prefix))

(defn create [n]
  (merge {:uid (get-uid)} n))
