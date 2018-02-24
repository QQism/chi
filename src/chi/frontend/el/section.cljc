(ns chi.frontend.el.section
  (:require [clojure.string :as string]
            [chi.frontend.node :as n]
            [chi.frontend.tree :as t]
            [chi.frontend.el.verse :as verse]
            [chi.frontend.error :as err]))

(def non-alphanum-7-bit #"([\!-\/\:-\@\[-\`\{-\~])")

(defn ^:private normalize-section-name [name]
  (-> name
      (string/replace non-alphanum-7-bit "")
      string/trim
      (string/replace #"\s{1,}" "-")
      string/lower-case))

(defn ^:private create-header [txt line]
  (n/create {:type :header
             :children (verse/create-inline-markup txt)}))

(defn create [txt line style]
  (let [adornment (first line)]
    (n/create {:type :section
               :style (str style adornment)
               :name (normalize-section-name txt)
               :children [(create-header txt line)]})))

(defn error-section-title-too-short [pos style text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        msg (str "Title " style " too short.")]
    (err/create msg ::err/warning pos block-text)))

(defn error-section-mismatching-underline [pos text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        msg "Missing matching underline for section title overline."]
    (err/create msg ::err/severe pos block-text)))

(defn error-section-mismatching-overline-underline [pos text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        msg "Title overline & underline mismatch."]
    (err/create msg ::err/severe pos block-text)))

(defn error-incomplete-section-title [pos text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        msg "Incomplete section title."]
    (err/create msg ::err/severe pos block-text)))

(defn error-unexpected-section-title [pos text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        msg "Unexpected section title."]
    (err/create msg ::err/severe pos block-text)))

(defn error-unexpected-section-title-or-transition [pos text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        msg "Unexpected section title or transition."]
    (err/create msg ::err/severe pos block-text)))

(defn is-title-short? [style text-lines]
  (case style
    "underline"
    (let [[text underline] text-lines]
      (< (count underline) (count text)))
    "overline"
    (let [[overline text _] text-lines]
      (< (count overline) (count text)))
    false))
