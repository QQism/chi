(ns chi.core
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :refer [split-lines]]
            [chi.frontend.parser :refer [lines->ast]]
            [chi.backend.html.core :refer [ast->html]]
            #?(:cljs [chi.debug :refer-macros [log]])))

(defn ^:private keywordize-keys [opts]
  #?(:cljs (js->clj opts :keywordize-keys true)
     :clj opts))

(defn ^:export compile
  ([content opts]
   (let [lines (split-lines content)
         keywordized-opts (keywordize-keys opts)]
     #?(:cljs (log opts))
     (-> lines lines->ast (ast->html keywordized-opts))))
  ([content]
   (compile content nil)))
