(ns chi.core
  (:refer-clojure :exclude [compile])
  (:require [chi.frontend.parser :refer [lines->ast]]
            [chi.backend.html.core :refer [ast->html]]))

(defn ^:export compile [lines opts]
  (let []
    (-> lines lines->ast (ast->html opts))))
