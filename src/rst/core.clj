(ns rst.core
  (:require  [clojure.java.io :as io]
             [clojure.string :as string]
             [clojure.pprint :refer [pprint]]
             [clojure.zip :as z]))

;; https://dev.clojure.org/jira/browse/CLJS-1871
(defn ^:declared process-lines [lines node init-state indent])

(def error-levels {:debug    0
                   :info     1
                   :warning  2
                   :error    3
                   :severe   4})

(defprotocol IReadingLines
  (current-line [_])
  (next-line [_])
  (forward [_])
  (eof? [_])
  (not-eof? [_])
  (eof-on-next? [_])
  (not-eof-on-next? [_])
  (indented? [_]))

(defprotocol IStateManagement
  (push-state [_ state])
  (pop-state [_])
  (current-state [_])
  (add-line-to-remains [_ line])
  (update-remains [_ lines])
  (clear-remains [_])
  (remains? [_]))

(defrecord DocumentContext
    [ast lines current-idx states indent remains reported-level]
  IReadingLines
  (current-line [_] (nth lines current-idx))
  (next-line [_] (nth lines (inc current-idx)))
  (forward [_] (update _ :current-idx inc))
  (eof? [_] (>= current-idx (count lines)))
  (not-eof? [_] (not (eof? _)))
  (eof-on-next? [_] (>= (inc current-idx) (count lines)))
  (not-eof-on-next? [_] (not (eof-on-next? _)))
  (indented? [_] (> indent 0))
  IStateManagement
  (push-state [_ state] (update _ :states conj state))
  (pop-state [_] (update _ :states pop))
  (current-state [_] (-> _ :states peek))
  (add-line-to-remains [_ line] (update _ :remains conj line))
  (update-remains [_ lines] (assoc _ :remains lines))
  (clear-remains [_] (update-remains _ []))
  (remains? [_] (-> _ :remains empty? not)))

(defn make-context [lines ast init-state indent]
  (map->DocumentContext {:ast ast
                         :lines lines
                         :indent indent
                         :current-idx 0
                         :states [init-state]
                         :remains []
                         :reported-level (:info error-levels)}))

(defn update-ast [context f & args]
  (update context :ast #(apply f % args)))

(def iid-counter (atom 0))

(defn get-iid []
  (swap! iid-counter inc))

(defn get-iid-path [ast]
  (-> ast
      z/path
      (->> (map :iid))
      (conj (:iid (z/node ast)))
      reverse))

(defn up-to-root [ast]
  (let [depth (count (z/path ast))]
    (reduce (fn [d _] (z/up d)) ast (range depth))))

(defn children-loc [ast]
  (let [children-count (-> ast z/children count)
        first-child (z/down ast)]
    (take children-count (iterate z/right first-child))))

(def non-alphanum-7-bit #"([\!-\/\:-\@\[-\`\{-\~])")

(defn normalize-section-name [name]
  (-> name
      (string/replace non-alphanum-7-bit "")
      string/trim
      (string/replace #"\s{1,}" "-")
      string/lower-case))

(def patterns
  {:blank   #" *$"
   :line    #"([\!-\/\:-\@\[-\`\{-\~])\1* *$"
   :indent  #"(\s+)(.+)"
   :bullet  #"([-+*\u2022\u2023\u2043])(\s+)(.*|$)"
   :text    #".+"})

(re-matches (:indent patterns) " This Hello")
(re-matches (:line patterns) "***")
(re-matches (:bullet patterns) "* Item")

(let [[_ style spacing text] (re-matches (:bullet patterns) "*  hello - this")
      indent (inc (count spacing))]
  (do
    (println (str "Style: " style))
    (println (str "Indent: " indent))
    (println (str "Text: " text))))

(def transition-patterns
  {:blank     {:pattern (:blank patterns)}
   :indent    {:pattern (:indent patterns)}
   ;;:bullet    {:pattern (:bullet patterns)}
   :line      {:pattern (:line patterns)}
   :text      {:pattern (:text patterns)}})

(defn parse [context transition match]
  (-> transition :parse (apply [context match])))

(defn match-transition [transition-name line]
  (-> (transition-name transition-patterns)
      :pattern
      (re-matches line)))

(defn match-transition? [transition-name line]
  (-> (match-transition transition-name line)
      nil? not))

(defn create-preserve [text]
  {:type :preserve
   :iid (get-iid)
   :value text})

(defn create-inline-markup-text [text]
  ;; TODO: parse the text into an vector of text
  ;; if there are inline-markups, parse them accordingly
  [{:type :text
    :iid (get-iid)
    :value (string/trimr text)}])

(defn create-header [text line]
  {:type :header,
   :iid (get-iid)
   :children (create-inline-markup-text text)})

(defn create-section [text line style]
  (let [adornment (first line)]
    {:type :section,
     :iid (get-iid)
     :style (str style adornment)
     :name (normalize-section-name text)
     :children [(create-header text line)]}))

(defn create-paragraph [text]
  {:type :paragraph
   :iid (get-iid)
   :children (create-inline-markup-text text)})

(defn create-transition []
  {:type :transition
   :iid (get-iid)})

(defn create-error
  ([description level block-text]
   {:type :error
    :level (level error-levels)
    :iid (get-iid)
    :children [(create-paragraph description)
               (create-preserve block-text)]})
  ([description level]
   {:type :error
    :level (level error-levels)
    :iid (get-iid)
    :children [(create-paragraph description)]}))

(defn create-blockquote [indent]
  {:type :blockquotes
   :indent indent
   :iid (get-iid)
   :children []})

(defn append-node [ast node]
  (-> ast (z/append-child node)))

(defn append-error [ast error]
  (append-node ast error))

(defn move-to-latest-child [ast]
  (-> ast z/down z/rightmost))

(defn append-section [ast section]
  ;; Append the section as the new child of the current node location
  ;; Move the location to the position of the new child
  (-> ast
      (append-node section)
      move-to-latest-child))

(defn append-transition [ast transition]
  (append-node ast transition))

(defn find-loc-with-iid [ast child-iid]
  (if (-> ast z/node :iid (= child-iid))
    ast
    (some #(if (-> % z/node :iid (= child-iid)) %)
          (children-loc ast))))

;; TODO: add the current loc id to path
(defn find-matching-section-loc [ast style]
  ;; Travel in the whole doc from top to the current path
  ;; - if there is a section having the same style as the new section
  ;;   | return the location
  ;; - else
  ;;   | return nil
  (let [current-iid-path (get-iid-path ast)]
    (loop [current-loc (up-to-root ast)
           depth 0]
      (if (< depth (count current-iid-path))
        (let [current-iid (nth current-iid-path depth)]
          (if-let [section-loc (find-loc-with-iid current-loc current-iid)]
            (let [section (z/node section-loc)]
              (if (= (:style section) style)
                current-loc
                (recur section-loc (inc depth))))))))))

(defn append-section-in-matched-location [ast section]
  ;; Find the matching parent section location
  ;; Append the new section as a child of the current path
  (let [style (:style section)]
    (if-let [matched-parent-section (find-matching-section-loc ast style)]
      (append-section matched-parent-section section)
      (append-section ast section))))

(defn append-error-section-title-too-short [ast style text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        error (create-error (str "Title " style " too short.")
                            :warning
                            block-text)]
    (append-error ast error)))

(defn append-error-section-mismatching-underline [ast text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        error (create-error "Missing matching underline for section title overline."
                            :severe block-text)]
    (append-error ast error)))

(defn append-error-section-mismatching-overline-underline [ast text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        error (create-error "Title overline & underline mismatch."
                            :severe block-text)]
    (append-error ast error)))

(defn append-error-incomplete-section-title [ast text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        error (create-error "Incomplete section title."
                            :severe block-text)]
    (append-error ast error)))

(defn append-error-unexpected-section-title [ast text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        error (create-error "Unexpected section title."
                            :severe block-text)]
    (append-error ast error)))

(defn append-error-unexpected-section-title-or-transition [ast text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        error (create-error "Unexpected section title or transition."
                            :severe block-text)]
    (append-error ast error)))

(defn is-section-title-short? [style text-lines]
  (case style
    "underline"
    (let [[text underline] text-lines]
      (< (count underline) (count text)))
    "overline"
    (let [[overline text _] text-lines]
      (< (count overline) (count text)))
    false))

(defn append-applicable-error-section-title-too-short [ast style text-lines]
  (if (is-section-title-short? style text-lines)
    (append-error-section-title-too-short ast style text-lines)
    ast))

(defn append-section-line->text-line [ast text-lines]
  (let [[overline text _] text-lines
        section-style "overline"
        new-section (create-section text overline section-style)]
    (-> ast (append-section-in-matched-location new-section)
        (append-applicable-error-section-title-too-short section-style
                                                         text-lines))))

(defn append-section-text->line [ast text-lines]
  (let [[text underline] text-lines
        section-style "underline"
        new-section (create-section text underline section-style)]
    (-> ast
        (append-section-in-matched-location new-section)
        (append-applicable-error-section-title-too-short section-style
                                                         text-lines))))


(defn document-begin? [ast]
  (let [siblings (z/children ast)
        siblings-count (count siblings)]
    (or (= siblings-count 0)
        (and (= siblings-count 1)
             (-> siblings first :type (= :header))))))

(defn last-sibling-transition? [ast]
  (-> ast z/down z/rightmost z/node :type (= :transition)))

(defn append-applicable-error-doc-start-with-transition [ast]
  (if (document-begin? ast)
    (let [error (create-error "Document or section may not begin with a transition."
                              :error)]
      (append-error ast error))
    ast))

(defn append-applicable-error-adjacent-transitions [ast]
  (if (last-sibling-transition? ast)
    (let [error (create-error "At least one body element must separate transitions; adjacent transitions are not allowed."
                              :error)]
      (append-error ast error))
    ast))

(defn append-error-doc-end-with-transition [ast]
  (let [error (create-error "Document may not end with a transition."
                            :error)]
    (append-error ast error)))

(defn append-transition-line->blank [ast]
  (let [transition (create-transition)]
    (-> ast
        append-applicable-error-doc-start-with-transition
        append-applicable-error-adjacent-transitions
        (append-transition transition))))

(defn append-applicable-error-blockquote-no-end-blank-line [ast lines eof?]
  (if (not (or (match-transition? :blank (peek lines)) eof?))
    (let [error (create-error "Block quote ends without a blank line; unexpected unindent." :warning)]
      (append-error ast error))
    ast))

(defn append-error-unexpected-indentation [ast]
  (let [error (create-error "Unexpected indentation." :error)]
    (append-error ast error)))

(defn append-blockquote-body->indent [ast lines lines-indent current-indent]
  (let [raw-indent (+ lines-indent current-indent)
        blockquote (create-blockquote raw-indent)
        processed-blockquote (process-lines lines blockquote :body lines-indent)]
    (-> ast
        (append-node processed-blockquote))))

(def body->blank {:name :blank,
                  :state :body,
                  :parse (fn [context _]
                           (-> context forward))})

(def body->line {:name :line,
                 :state :body,
                 :parse (fn [context _]
                          (let [line (current-line context)
                                short-line? (< (count line) 4)]
                            (if (indented? context)
                              (if short-line?
                                (-> context
                                    forward
                                    (add-line-to-remains line)
                                    (push-state :text))
                                (-> context
                                    (update-ast append-error-unexpected-section-title-or-transition [line])
                                    forward))
                              (-> context
                                  forward
                                  (add-line-to-remains line)
                                  (push-state :line)))))})

(defn append-paragraph [ast block-text]
  (let [paragraph (create-paragraph block-text)]
    (append-node ast paragraph)))

(defn append-text [ast text]
  (let [text (create-inline-markup-text text)]
    (append-node ast text)))

(def body->text
  {:name :text,
   :state :body,
   :parse (fn [context _]
            (if (not-eof? context)
              (let [line (current-line context)]
                (if (not-eof-on-next? context)
                  (-> context
                      forward
                      (add-line-to-remains line)
                      (push-state :text))
                  ;; next line if EOF
                  (let [text-lines (-> context :remains (conj line))]
                    (-> context
                        (update-ast append-paragraph (string/join " " text-lines))
                        forward
                        clear-remains))))))})

(def line->blank {:name :blank
                  :state :line
                  :parse (fn [context _]
                           ;; Check if it is the last line of the document
                           ;; - Y: Raise error
                           ;; - N: Create a transition
                           (let [prev-text-line (-> context :remains peek)
                                 prev-short-line? (< (count prev-text-line) 4)]
                             (if prev-short-line?
                               (-> context
                                   (update-ast append-paragraph prev-text-line)
                                   forward
                                   clear-remains
                                   pop-state)
                               (loop [current-context context]
                                 (if (not-eof? current-context)
                                   (let [line (current-line current-context)]
                                     (if (match-transition? :blank line)
                                       (recur (-> current-context forward))
                                       (-> current-context
                                           (update-ast append-transition-line->blank)
                                           clear-remains
                                           pop-state)))
                                   (-> current-context
                                       (update-ast append-transition-line->blank)
                                       (update-ast append-error-doc-end-with-transition)
                                       clear-remains
                                       pop-state))))))})

(def line->text {:name :text,
                 :state :line
                 :parse (fn [context _]
                          ;; Read the next line
                          ;; If it is a lines
                          ;; - Y: Check if overline and underline are matched
                          ;;   - Y: Create a section
                          ;;   - N: - If it is a short line (lesser than 4 char)
                          ;;     - Y: switch to text state
                          ;;     - N: Create an error
                          ;; - N: - If it is a short line
                          ;;   - Y: Switch to the text state
                          ;;   - N: Create an error
                          (let [{text-lines :remains} context
                                current-text-line (current-line context)
                                current-text-lines (conj text-lines current-text-line)
                                prev-text-line (peek text-lines)
                                prev-short-line? (< (count prev-text-line) 4)]
                            (if (not-eof-on-next? context)
                              (let [next-text-line (next-line context)
                                    next-text-lines (conj current-text-lines next-text-line)]
                                (if (match-transition? :line next-text-line)
                                  (if (= prev-text-line next-text-line)
                                    (-> context
                                        (update-ast append-section-line->text-line next-text-lines)
                                        forward
                                        forward
                                        clear-remains
                                        pop-state)
                                    (if prev-short-line?
                                      (-> context
                                          forward
                                          forward
                                          (update-remains next-text-lines)
                                          pop-state
                                          (push-state :text))
                                      (-> context
                                          (update-ast append-error-section-mismatching-overline-underline
                                                      next-text-lines)
                                          forward
                                          forward
                                          clear-remains
                                          pop-state)))
                                  (if prev-short-line?
                                    (-> context
                                        forward
                                        forward
                                        (update-remains next-text-lines)
                                        pop-state
                                        (push-state :text))
                                    (-> context
                                        (update-ast append-error-section-mismatching-underline
                                                    next-text-lines)
                                        forward
                                        forward
                                        clear-remains
                                        pop-state))))
                              (if prev-short-line?
                                (-> context
                                    forward
                                    (add-line-to-remains current-text-line)
                                    pop-state
                                    (push-state :text))
                                (-> context
                                    (update-ast append-error-incomplete-section-title current-text-lines)
                                    forward
                                    clear-remains
                                    pop-state)))))})

(def line->line {:name :line,
                 :state :line
                 :parse (fn [context _]
                          ;; Append the error message
                          )})

(def text->blank {:name :blank,
                  :state :text,
                  :parse (fn [context _]
                           (let [text-lines (:remains context)
                                 block-text (string/join " " text-lines)]
                             (-> context
                                 (update-ast append-paragraph block-text)
                                 forward
                                 clear-remains
                                 pop-state)))})

(def text->text {:name :text,
                 :state :text,
                 :parse (fn [context _]
                          ;; Read the whole block of text until the blank line
                          ;; keep track the index
                          (loop [current-context context]
                            (if (not-eof? current-context)
                              (let [line (current-line current-context)]
                                (cond (match-transition? :blank line)
                                      ;; blank line, create paragraph & return
                                      (let [block-text (string/join " " (:remains current-context))]
                                        (-> current-context
                                            (update-ast append-paragraph block-text)
                                            clear-remains
                                            pop-state))
                                      (match-transition? :indent line)
                                      (let [block-text (string/join " " (:remains current-context))]
                                        (-> current-context 
                                            (update-ast append-paragraph block-text)
                                            (update-ast append-error-unexpected-indentation)
                                            clear-remains
                                            pop-state))
                                      :else
                                      (recur (-> current-context
                                                 forward
                                                 (add-line-to-remains line)))))
                              ;; EOF, create paragraph & return
                              (let [block-text (string/join " " (:remains current-context))]
                                (-> current-context
                                    (update-ast append-paragraph block-text)
                                    forward
                                    clear-remains
                                    pop-state)))))})

(def text->line {:name :line,
                 :state :text,
                 :parse (fn [context match]
                          (let [prev-text-line (-> context :remains peek)
                                line (current-line context)
                                line-count (count line)
                                short-line? (< line-count 4)
                                shorter-than-prev? (< line-count (count prev-text-line))
                                text-lines [prev-text-line line]]
                            ;; There are two possible cases:
                            ;; - line is shorter than the text (in remains)
                            ;;     AND line is shorter than 4 characters
                            ;;     | switch to transition text->text
                            ;;   - else
                            ;;     | create the section
                            (if (and short-line? shorter-than-prev?)
                              (parse context text->text match)
                              (if (indented? context)
                                (-> context
                                    (update-ast append-error-unexpected-section-title text-lines)
                                    forward
                                    clear-remains
                                    pop-state)
                                (-> context
                                    (update-ast append-section-text->line text-lines)
                                    forward
                                    clear-remains
                                    pop-state)))))})

(defn read-indented-lines [context indent]
  (let [{text-lines :remains} context
        indented-pattern (re-pattern (str " {" indent "}(.*)"))]
    (loop [indented-lines text-lines
           current-context context]
      (if (not-eof? current-context)
        (let [line (current-line current-context)]
          (if-let [match (re-matches indented-pattern line)]
            (recur (conj indented-lines (nth match 1)) (forward current-context))
            (if (match-transition? :blank line)
              (recur (conj indented-lines "") (forward current-context))
              [indented-lines
               (-> current-context
                   clear-remains)])))
        [indented-lines
         (-> current-context
             clear-remains)]))))

(def bullet->blank {:name :blank
                    :state :bullet
                    :parse (fn [context _]
                             (forward context))})

(def body->indent {:name :indent,
                   :state :body,
                   :parse (fn [context [line indent-str text]]
                            (let [indent (count indent-str)
                                  current-ident (:indent context)
                                  [lines next-context] (read-indented-lines context indent)
                                  eof? (eof? next-context)]
                              (-> next-context
                                  (update-ast append-blockquote-body->indent
                                              lines
                                              indent
                                              current-ident)
                                  (update-ast append-applicable-error-blockquote-no-end-blank-line
                                              lines
                                              eof?))))})

;;TODO: need to figure out the way to handle bullet-list state and indent
(def body->bullet {:name :bullet,
                   :state :body,
                   :parse (fn [context match]
                            (-> context
                                (push-state :bulletlist))
                            )})

(def body->enum {:name :enum,
                 :state :body,
                 :parse (fn [context match])})

(def body->field-marker {:name :enum,
                         :state :body,
                         :parse (fn [context match])})

(def body->option-marker {:name :enum,
                          :state :body,
                          :parse (fn [context match])})

(def states
  {:body           {:transitions [body->blank
                                  body->indent
                                  ;;body->bullet
                                  ;;body->enum
                                  ;;body->field-marker
                                  ;;body->option-marker
                                  ;;body->doctest
                                  ;;body->line-block
                                  ;;body->grid-table-top
                                  ;;body->simple-table-top
                                  ;;body->explicit-markup
                                  ;;body->anonymous
                                  body->line
                                  body->text]}
   :bulletlist    {:transitions [bullet->blank
                                  ;;bullet->indent
                                  ;;bullet->bullet
                                  ;;bullet->enum
                                  ;;bullet->field-marker
                                  ;;bullet->option-marker
                                  ;;bullet->doctest
                                  ;;bullet->line-block
                                  ;;bullet->grid-table-top
                                  ;;bullet->simple-table-top
                                  ;;bullet->explicit-markup
                                  ;;bullet->anonymous
                                  body->line
                                  body->text]}
   :line           {:transitions [line->blank line->text line->line]}
   :text           {:transitions [text->blank
                                  ;;text->indent
                                  text->line
                                  text->text]}})

(defn next-transition [state line]
  (some #(if-let [match (match-transition (:name %) line)]
           [% match])
        (-> states state :transitions)))

;;(defn find-matched-transition [transitions name]
;;  (some (fn [transition]
;;          (if (-> transition :name (= name)) transition))
;;        transitions))
;;
;;(find-matched-transition (:transitions (:text states)) :text)
;;
;;(-> states
;;    :text
;;    :transitions (find-matched-transition :text))

(def content-lines (-> (io/resource "blockquotes.rst")
                       slurp
                       (string/split #"\r|\n")
                       ;; Remove right whitespaces
                       (->> (map string/trimr))
                       ;; Become a vector, giving the random access with 0(1) complexity
                       vec))

(re-matches #" {1}(.*)" " - Hello")

(defn zip-node [node]
  (z/zipper map? ;;#(not= (:type %) :text)
            #(-> % :children seq)
            (fn [n children]
              (assoc n :children (vec children)))
            node))

(defn clean-up-remains [context]
  (if (remains? context)
    (let [current-state (current-state context)
          [transition match] (next-transition current-state "")]
      (parse context transition match))
    context))

(defn process-lines [lines node init-state indent]
  (let [init-ast (zip-node node)
        init-context (make-context lines init-ast init-state indent)]
    (loop [context init-context]
      (if (not-eof? context)
        (let [line (current-line context)
              current-state (current-state context)
              [transition match] (next-transition current-state line)
              new-context (parse context transition match)]
          (recur new-context))
        (-> (clean-up-remains context)
            :ast
            z/root)))))

(defn process-document [document-lines]
  (let [root-node {:type :root :iid (get-iid) :children []}
        no-indent 0
        init-state :body]
    (process-lines document-lines root-node init-state no-indent)))

;;(process-document content-lines)
