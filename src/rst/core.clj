(ns rst.core
  (:require [hiccup.core :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.core.match :refer [match]]))


;; Records

(defrecord DocumentLine [index content])
(defrecord Title [content from to])
(defrecord Paragraph [content from to])
(defrecord EmphasisInlineMarkup [content from to])
(defrecord PlainText [content from to])

;; Traverse document

(def title-data (slurp (io/resource "demo.rst")))

(defn body-to-lines [content]
  (string/split content #"\r|\n"))

(def raw-document
  (into {} (map-indexed #(-> [%1 %2]) (body-to-lines title-data))))

(defn line-content-at [idx col]
  (if (= 0 col)
    (get raw-document idx nil)
    (string/join (last (split-at col (get raw-document idx nil))))))

(defn line-content-at-2 [idx col]
  (if-let [line-content (get raw-document idx nil)]
    (if-let [content (last (split-at col line-content))]
      (string/join content)
      nil) nil))

(line-content-at-2 8 4)

(defn prev-line [idx col]
  (line-content-at (dec idx) col))

(defn curr-line [idx col]
  (line-content-at idx col))

(defn next-line [idx col]
  (line-content-at (inc idx) col))

(prev-line 10000 10000)


(string/join (last (split-at 4 (get raw-document 1000 nil))))

(get raw-document 8 nil)

(into {} (map-indexed #(-> [%1 %2]) (body-to-lines title-data)))

(-> raw-document)

;;
;; Title matcher
;;

(def adornment-chars "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \\ ] ^ _ ` { | } ~")

(def adornments (as-> adornment-chars xs
                  (string/split  xs #"\s")
                  (map #(str "\\" % "+") xs)
                  (string/join "|" xs)
                  (str  "^(" xs ")$")
                  (re-pattern xs)))

(str adornments)

(defn title-section-start? [line]
  (match [line]
         [nil] false
         :else (some? (re-find adornments line))))

(defn title-match [idx]
  (let [prev (prev-line idx 0)
        curr (curr-line idx 0)
        next (next-line idx 0)]
    (cond (and (not-empty curr) (title-section-start? next))
          (let [next-idx         (inc idx)
                prev-idx         (dec idx)
                next-last-column (dec (count next))]
            (cond (empty? prev)
                  (map->Title {:content curr :from [idx, 0] :to [next-idx next-last-column]})
                  (and (title-section-start? prev) (empty? (prev-line prev-idx 0)))
                  (map->Title {:content curr :from [prev-idx 0] :to [next-idx next-last-column]})
                  :else :nil))
          :else nil)))

(title-match 4)

;;
;; Inline markup matcher
;;

(def inline-emphasis-start #"(^|\s)\*\S.*")

;;
;; https://stackoverflow.com/questions/3262195/compact-clojure-code-for-regular-expression-matches-and-their-position-in-string?rq=1a
;;
(defn re-pos [re s]
  (loop [m (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

(re-pos inline-emphasis-start "*There This is *this is\nokok* I know that this is a part of **a sto\nry**, right?")

(defn re-find-indexed [re s]
  (let [m (re-matcher re s)]
    (if (.find m) (hash-map (.start m) (.group m)) nil)))

(re-find-indexed inline-emphasis-start "There This is *this is\nokok* I know that this is a part of **a sto\nry**, right?")

(def inline-strong-emphasis-char "**")

(re-seq inline-emphasis-start "*There This is *this is\nokok* I know that this is a part of **a sto\nry**, right?")

(defn inline-markup-matcher [content]
  (if-let [m (re-find-indexed inline-emphasis-start content)]
    (let [content (first (vals m))
          index   (first (keys m))]
      [(map->PlainText {:content ""}) (map->EmphasisInlineMarkup {:content content :from index})]
      )
    (map->PlainText {:content content :from 0})))

(inline-markup-matcher "There This is *this is\nokok* I know that this is a part of **a sto\nry**, right?")
(inline-markup-matcher "There This is ")

;; Order
;;


;;
;; Paragraph matcher
;;

(defn next-paragraph-match [paragraph]
  (let [[idx _] (:to paragraph)
        next    (next-line idx 0)]
    (if (not-empty next)
      (recur (-> paragraph
                 (update :content #(str % " " next))
                 (update :to #(let [[idx _] %]
                                [(inc idx) (dec (count next))]))))
      paragraph)))

(-> (map->Paragraph {:content "test" :from [4 0] :to [4 10]})
    (update :content #(str % "Hi"))
    (update :to (fn [_] [1 1])))

(defn paragraph-match [idx]
  (let [prev (prev-line idx 0)
        curr (curr-line idx 0)
        next (next-line idx 0)]
    (if (empty? prev)
      (next-paragraph-match (map->Paragraph {:content curr :from [idx 0] :to [idx (dec (count curr))]}))
      nil)))

(paragraph-match 7)

(def document
  {:lines []
   :tree  {:type    :document
           :content []}})

;;
;; Document
;;

(defn process-line [acc line]
  ;;(update acc :lines conj line)
  (let [[index content] line]
    (update-in acc [:tree :content]
               conj (map->Title {:content   content
                                 :line-from index}))))

(reduce process-line document raw-document)

(def sample-syntax-tree
  "
  0  ***********
  1  Hello World
  2  ***********
  3  *I'm* John*
  4  ===========
  "
  {:type    :document
   :content [{:type      :title
              :mark      "*"
              :level 0
              :line-from 0
              :line-to   2
              :content   [{:type      :string
                           :line-from 1
                           :line-to   1
                           :content   "Hello World"}]}
             {:type      :title
              :mark      "="
              :line-from 3
              :line-to   4
              :content   [{:type    :italics
                           :content {:type    :text
                                     :line-from 3
                                     :line-to 3
                                     :content "I'm"}},
                          {:type :text :content "John*"}]}]})
