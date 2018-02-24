(ns chi.frontend.patterns)

(def ^{:private true :const true} patterns
  {:blank   #" *$"
   :indent  #"(\s+)(.+)"
   :grid-table-top #"\+-[-\+]+-\+ *$"
   :grid-table-head-sep #"\+=[=+]+=\+ *$"
   :grid-table-left-side #"^(\+|\|).*"
   :grid-table-right-side #".*(\+|\|)$"
   :bullet  #"([-+*\u2022\u2023\u2043])(\s+)(.*|$)"
   :line    #"([\!-\/\:-\@\[-\`\{-\~])\1* *$"
   :text    #".+"})

(defn match [transition-name line]
  (-> patterns transition-name (re-matches line)))

(defn match? [transition-name line]
  (-> (match transition-name line)
      nil? not))
