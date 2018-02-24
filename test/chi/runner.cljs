(ns chi.runner
  (:require [cljs.test :as t :include-macros true]
            [doo.runner :refer-macros [doo-tests]]
            [chi.blockquotes-test]
            [chi.bulletlists-test]
            [chi.paragraphs-test]
            [chi.sections-test]
            [chi.tables-test]
            [chi.transitions-test]))

(doo-tests 'chi.blockquotes-test
           'chi.bulletlists-test
           'chi.paragraphs-test
           'chi.sections-test
           'chi.tables-test
           'chi.transitions-test)
