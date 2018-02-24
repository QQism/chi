(ns chi.runner
  (:require [cljs.test :as t :include-macros true]
            [doo.runner :refer-macros [doo-tests]]
            [chi.frontend.blockquotes-test]
            [chi.frontend.bulletlists-test]
            [chi.frontend.paragraphs-test]
            [chi.frontend.sections-test]
            [chi.frontend.tables-test]
            [chi.frontend.transitions-test]))

(doo-tests 'chi.frontend.blockquotes-test
           'chi.frontend.bulletlists-test
           'chi.frontend.paragraphs-test
           'chi.frontend.sections-test
           'chi.frontend.tables-test
           'chi.frontend.transitions-test)
