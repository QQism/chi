(ns rst.debug)

(defmacro log
  "Print with console.log, if it exists."
  [& forms]
  `(when (exists? js/console)
     (.log js/console ~@forms)))
