(ns rst.verse
  (:require [clojure.string :as string]
            [rst.node :as n]))

(defn create-inline-markup [text]
  ;; TODO: parse the text into an vector of text
  ;; if there are inline-markups, parse them accordingly
  [(n/create {:type :text
              :value (string/trimr text)})])
