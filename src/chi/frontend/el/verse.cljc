(ns chi.frontend.el.verse
  (:require [clojure.string :as string]
            [chi.frontend.node :as n]
            [chi.frontend.patterns :as patt]))

(defn create-inline-markup [text]
  ;; TODO: parse the text into an vector of text
  ;; if there are inline-markups, parse them accordingly
  [(n/create {:type :text
              :value (string/trimr text)})])

(defn line-indent [line]
  (if-let [match (patt/match :indent line)]
    (let [[_ spaces _] match]
      (count spaces))
    0))

(defn trivial-block-indents [lines]
  (or (reduce (fn [m line]
                (if-not (empty? line)
                  (let [indent (line-indent line)]
                    (if m (min m indent) indent))
                  m))
              nil lines)
      0))

(defn strip-indents [lines indent]
  (reduce (fn [xs line]
            (if-not (empty? line)
              (conj xs (subs line indent))
              (conj xs line)))
          [] lines))

(defn text-block-size
  "Get the size of the square block of text"
  [lines]
  [(-> lines (nth 0) count) (count lines)])

(defn nth-2d [lines row col]
  (-> lines (nth row) (nth col)))
