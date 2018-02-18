(ns rst.test-support
  (:require #?(:cljs [cljs.test    :as t]
               :clj  [clojure.test :as t])
            [clojure.pprint]
            [clojure.walk :refer [postwalk]]))

#?(:cljs (enable-console-print!))

(defn remove-uid [node]
  (postwalk (fn [n]
              (if (and (map? n) (:uid n))
                (dissoc n :uid)
                n)) node))

(def function? #?(:cljs fn?
                  :clj t/function?))

(defn transform-cond-nodes
  ([expected node remove-uid?]
   (let [ks (keys expected) keys-node (cond-> node
                     (not-empty ks) (select-keys ks)
                     remove-uid? remove-uid)]
     (reduce-kv (fn [[ex n] k v]
                  (if (and (vector? v)
                           (= (count v) 3)
                           (keyword? (first v))
                           (function? (last v)))
                    (let [[new-k r f] v
                          actual (f (k n))]
                      [(-> ex
                           (assoc new-k r)
                           (dissoc k))
                       (-> n
                           (assoc new-k actual)
                           (dissoc k))])
                    [ex n]))
                [expected keys-node]
                expected)))
  ([expected node]
   (transform-cond-nodes expected node true)))

(defn assert-node
  ([expected actual]
   (assert-node expected actual nil))
  ([expected actual msg]
   (let [[cond-expected cond-actual] (transform-cond-nodes expected actual)]
     (t/is (= cond-expected cond-actual) msg))))

(defn print-node
  ([node]
   (clojure.pprint/write node :stream nil))
  ([node ks]
   (print-node (select-keys node ks))))
