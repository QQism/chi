(ns rst.test-support
  (:require [clojure.test :refer :all]
            [clojure.walk :refer [postwalk walk]]))

(defn remove-uid [node]
  (postwalk (fn [n]
              (if (and (map? n) (:uid n))
                (dissoc n :uid)
                n)) node))

(defn assert-node
  ([node expected remove-uid?]
   (let [ks (keys expected)
         keys-node (cond-> node
                     (not-empty ks) (select-keys ks)
                     remove-uid? remove-uid)
         nodes (reduce-kv (fn [[n ex] k v]
                            (if (and (vector? v)
                                     (= (count v) 3)
                                     (keyword? (first v))
                                     (function? (last v)))
                              (let [[new-k r f] v
                                    actual (f (k n))]
                                [(-> n
                                     (assoc new-k actual)
                                     (dissoc k))
                                 (-> ex
                                     (assoc new-k r)
                                     (dissoc k))])
                              [n ex]))
                          [keys-node expected]
                          expected)]
     (println (str "Nodes:" nodes))
     (apply = nodes)))
  ([node expected]
   (assert-node node expected true)))

(let [m {:uid 659,:type :root,
         :children [{:uid 662,:type :table,:style :grid,
                     :pos [0 0],:width 39,:height 5,
                     :col-ids #{0 13 26},:header-idx nil,
                     :children
                     [{:uid 661,:type :table-body,
                       :children
                       [{:uid 660,:type :row,:id 0,
                         :children
                         [{:uid 664,:type :cell,:top 0,:left 0,:width 12,:height 1,
                           :children
                           [{:uid 665, :type :text, :value "Cell 1-1"}]}
                          {:uid 668,:type :cell,:top 0,:left 13,:width 12,:height 1,
                           :children [{:uid 669, :type :text, :value "Cell 1-2"}]}
                          {:uid 672,:type :cell,:top 0,:left 26,:width 11,:height 1,
                           :children [{:uid 673, :type :text, :value "Cell 1-3"}]}]}
                        {:uid 663,:type :row,:id 2,
                         :children
                         [{:uid 675,:type :cell,:top 2,:left 0,:width 12,:height 1,
                           :children
                           [{:uid 676, :type :text, :value "Cell 2-1"}]}
                          {:uid 678,:type :cell,:top 2,:left 13,:width 12,:height 1,
                           :children
                           [{:uid 679, :type :text, :value "Cell 2-2"}]}
                          {:uid 681,:type :cell,:top 2,:left 26,:width 11,:height 1,
                           :children
                           [{:uid 682, :type :text, :value "Cell 2-3"}]}]}]
                       }]}]}]
  (remove-uid m))

walk

(defn print-node
  ([node]
   (clojure.pprint/write node :stream nil))
  ([node ks]
   (print-node (select-keys node ks))))
