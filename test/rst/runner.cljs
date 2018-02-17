(ns rst.runner
  (:require [cljs.test :as t :include-macros true]
            [doo.runner :refer-macros [doo-tests]]
            [rst.blockquotes-test]
            [rst.bulletlists-test]
            [rst.paragraphs-test]
            [rst.sections-test]
            [rst.tables-test]
            [rst.transitions-test]))

(doo-tests 'rst.blockquotes-test
           'rst.bulletlists-test
           'rst.paragraphs-test
           'rst.sections-test
           'rst.tables-test
           'rst.transitions-test)
