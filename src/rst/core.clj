(ns rst.core
  (:require  [clojure.java.io :as io]
             [clojure.string :as string]
             [clojure.pprint :refer [pprint]]
             [clojure.zip :as z]))

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
  (not-eof-on-next? [_]))

(defprotocol IStateManagement
  (push-state [_ state])
  (pop-state [_])
  (current-state [_])
  (add-line-to-remains [_ line])
  (update-remains [_ lines])
  (clear-remains [_])
  (remains? [_]))

(defrecord DocumentContext
    [doc lines current-idx states remains reported-level]
  IReadingLines
  (current-line [_] (nth lines current-idx))
  (next-line [_] (nth lines (inc current-idx)))
  (forward [_] (update _ :current-idx inc))
  (eof? [_] (>= current-idx (count lines)))
  (not-eof? [_] (not (eof? _)))
  (eof-on-next? [_] (>= (inc current-idx) (count lines)))
  (not-eof-on-next? [_] (not (eof-on-next? _)))
  IStateManagement
  (push-state [_ state] (update _ :states conj state))
  (pop-state [_] (update _ :states pop))
  (current-state [_] (-> _ :states peek))
  (add-line-to-remains [_ line] (update _ :remains conj line))
  (update-remains [_ lines] (assoc _ :remains lines))
  (clear-remains [_] (update-remains _ []))
  (remains? [_] (-> _ :remains empty? not)))

(defn make-context [lines doc init-state indent]
  (map->DocumentContext {:doc doc
                         :lines lines
                         :indent indent
                         :current-idx 0
                         :states [init-state]
                         :remains []
                         :reported-level (:info error-levels)}))

(defn update-doc [context f & args]
  (update context :doc #(apply f % args)))

(def iid-counter (atom 0))

(defn get-iid []
  (swap! iid-counter inc))

(defn get-iid-path [doc]
  (-> doc
      z/path
      (->> (map :iid))
      (conj (:iid (z/node doc)))
      reverse))

(defn up-to-root [doc]
  (let [depth (count (z/path doc))]
    (reduce (fn [d _] (z/up d)) doc (range depth))))

(defn children-loc [doc]
  (let [children-count (-> doc z/children count)
        first-child (z/down doc)]
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

(defn create-blockquote []
  {:type :blockquotes
   :iid (get-iid)
   :children []})

(defn append-node [doc node]
  (-> doc (z/append-child node)))

(defn append-error [doc error]
  (append-node doc error))

(defn move-to-latest-child [doc]
  (-> doc z/down z/rightmost))

(defn append-section [doc section]
  ;; Append the section as the new child of the current node location
  ;; Move the location to the position of the new child
  (-> doc
      (append-node section)
      move-to-latest-child))

(defn append-transition [doc transition]
  (append-node doc transition))

(defn find-loc-with-iid [doc-loc child-iid]
  (if (-> doc-loc z/node :iid (= child-iid))
    doc-loc
    (some #(if (-> % z/node :iid (= child-iid)) %)
          (children-loc doc-loc))))

;; TODO: add the current loc id to path
(defn find-matching-section-loc [doc-loc style]
  ;; Travel in the whole doc from top to the current path
  ;; - if there is a section having the same style as the new section
  ;;   | return the location
  ;; - else
  ;;   | return nil
  (let [current-iid-path (get-iid-path doc-loc)]
    (loop [current-loc (up-to-root doc-loc)
           depth 0]
      (if (< depth (count current-iid-path))
        (let [current-iid (nth current-iid-path depth)]
          (if-let [section-loc (find-loc-with-iid current-loc current-iid)]
            (let [section (z/node section-loc)]
              (if (= (:style section) style)
                current-loc
                (recur section-loc (inc depth))))))))))

(defn append-section-in-matched-location [doc section]
  ;; Find the matching parent section location
  ;; Append the new section as a child of the current path
  (let [style (:style section)]
    (if-let [matched-parent-section (find-matching-section-loc doc style)]
      (append-section matched-parent-section section)
      (append-section doc section))))

(defn append-error-section-title-too-short [doc style text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        error (create-error (str "Title " style " too short.")
                            :warning
                            block-text)]
    (append-error doc error)))

(defn append-error-section-mismatching-underline [doc text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        error (create-error "Missing matching underline for section title overline."
                            :severe
                            block-text)]
    (append-error doc error)))

(defn append-error-section-mismatching-overline-underline [doc text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        error (create-error "Title overline & underline mismatch."
                            :severe
                            block-text)]
    (append-error doc error)))

(defn append-error-incomplete-section-title [doc text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        error (create-error "Incomplete section title."
                            :severe
                            block-text)]
    (append-error doc error)))

(defn is-section-title-short? [style text-lines]
  (case style
    "underline"
    (let [[text underline] text-lines]
      (< (count underline) (count text)))
    "overline"
    (let [[overline text _] text-lines]
      (< (count overline) (count text)))
    false))

(defn append-applicable-error-section-title-too-short [doc style text-lines]
  (if (is-section-title-short? style text-lines)
    (append-error-section-title-too-short doc style text-lines)
    doc))

(defn append-section-line->text-line [doc text-lines]
  (let [[overline text _] text-lines
        section-style "overline"
        new-section (create-section text overline section-style)]
    (-> doc (append-section-in-matched-location new-section)
        (append-applicable-error-section-title-too-short section-style
                                                         text-lines))))

(defn append-section-text->line [doc text-lines]
  (let [[text underline] text-lines
        section-style "underline"
        new-section (create-section text underline section-style)]
    (-> doc
        (append-section-in-matched-location new-section)
        (append-applicable-error-section-title-too-short section-style
                                                         text-lines))))

(defn append-error-doc-end-with-transition [doc]
  (let [error (create-error "Document may not end with a transition." :error)]
    (append-error doc error)))

(defn append-transition-line->blank [doc]
  (let [transition (create-transition)]
    (append-transition doc transition)))

(defn append-blockquote-body->indent [doc indent]
  (let [blockquote-node (create-blockquote)
        ]
    (append-node doc blockquote-node)))

(def body->blank {:name :blank,
                  :state :body,
                  :parse (fn [context _]
                           (-> context forward))})

(def body->line {:name :line,
                 :state :body,
                 :parse (fn [context _]
                          (let [line (current-line context)]
                            (-> context
                                forward
                                (add-line-to-remains line)
                                (push-state :line))))})

(defn append-text [doc block-text]
  (let [paragraph (create-paragraph block-text)]
    (append-node doc paragraph)))

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
                        (update-doc append-text (string/join " " text-lines))
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
                                   (update-doc append-text prev-text-line)
                                   forward
                                   clear-remains
                                   pop-state)
                               (loop [current-context context]
                                 (if (not-eof? current-context)
                                   (let [line (current-line current-context)]
                                     (if (match-transition? :blank line)
                                       (recur (-> current-context forward))
                                       (-> current-context
                                           (update-doc append-transition-line->blank)
                                           clear-remains
                                           pop-state)))
                                   (-> current-context
                                       (update-doc append-transition-line->blank)
                                       (update-doc append-error-doc-end-with-transition)
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
                                        (update-doc append-section-line->text-line next-text-lines)
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
                                          (update-doc append-error-section-mismatching-overline-underline
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
                                        (update-doc append-error-section-mismatching-underline
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
                                    (update-doc append-error-incomplete-section-title current-text-lines)
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
                                 (update-doc append-text block-text)
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
                                (if (not (match-transition? :blank line))
                                  (recur (-> current-context
                                             forward
                                             (add-line-to-remains line)))
                                  ;; blank line, create paragraph & return
                                  (let [block-text (string/join " " (:remains current-context))]
                                    (-> current-context
                                        (update-doc append-text block-text)
                                        clear-remains
                                        pop-state))))
                              ;; EOF, create paragraph & return
                              (let [block-text (string/join " " (:remains current-context))]
                                (-> current-context
                                    (update-doc append-text block-text)
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
                                shorter-than-prev? (< line-count (count prev-text-line))]
                            ;; There are two possible cases:
                            ;; - line is shorter than the text (in remains)
                            ;;     AND line is shorter than 4 characters
                            ;;     | switch to transition text->text
                            ;;   - else
                            ;;     | create the section
                            (if (and short-line? shorter-than-prev?)
                              (parse context text->text match)
                              (-> context
                                  (update-doc append-section-text->line [prev-text-line line])
                                  forward
                                  clear-remains
                                  pop-state))))})

(def body->indent {:name :indent,
                   :state :body,
                   :parse (fn [context [line indent-str text]]
                            (let [indent (count indent-str)]
                              (do
                               (println (str "Line: " line))
                               (println (str "Line: " indent))
                               (println (str "Line: " text))
                               (forward context)
                               )))})

;;TODO: need to figure out the way to handle bullet-list state and indent
(def body->bullet {:name :bullet,
                   :state :body,
                   :parse (fn [context match]
                            (-> context
                                (push-state :bullet-list))
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
   :line           {:transitions [line->blank line->text line->line]}
   :text           {:transitions [text->blank
                                  ;;text->indent
                                  text->line
                                  text->text]}
   :bullet-list    {:transitions [body->blank
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
                                  body->text]}})

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

(def content-lines (-> (io/resource "list.rst")
               slurp
               (string/split #"\r|\n")
               ;; Remove right whitespaces
               (->> (map string/trimr))
               ;; Become a vector, giving the random access with 0(1) complexity
               vec))

(re-matches #" {1}(.*)" " - Hello")

(defn read-indented-lines [context indent]
  (let [{current-idx :current-idx remains :remains} context
        indented-pattern (re-pattern (str " {" indent "}(.*)"))]
    (loop [indented-lines remains
           current-context context]
      (if (not-eof? current-context)
        (let [line (current-line current-context)]
          (if-let [match (re-matches indented-pattern line)]
            (recur (conj indented-lines (nth match 1)) (forward current-context))
            (if (match-transition :blank line)
              (recur indented-lines (forward current-context))
              [indented-lines
               (-> current-context
                   forward
                   clear-remains)])))
        [indented-lines
         (-> current-context
             forward
             clear-remains)]))))

;;(read-indented-lines
;; (-> content-lines
;;     make-context
;;     forward forward
;;     (add-line-to-remains "First Item")) 2)

(defn zip-doc [doc]
  (z/zipper map? ;;#(not= (:type %) :text)
            #(-> % :children seq)
            (fn [node children]
              (assoc node :children (vec children)))
            doc))

(defn clean-up-remains [context]
  (if (remains? context)
    (let [current-state (current-state context)
          [transition match] (next-transition current-state "")]
      (parse context transition match))
    context))

(defn process-lines [lines doc-node init-state indent]
  (let [init-doc (zip-doc doc-node)
        init-context (make-context lines init-doc init-state indent)]
    (loop [context init-context]
      (if (not-eof? context)
        (let [line (current-line context)
              current-state (current-state context)
              [transition match] (next-transition current-state line)
              new-context (parse context transition match)]
          (recur new-context))
        (-> (clean-up-remains context)
            :doc
            z/root)))))

(defn process-document [document-lines]
  (let [root-node {:type :root :iid (get-iid) :children []}
        no-indent 0
        init-state :body]
    (process-lines document-lines root-node init-state no-indent)))

(process-document content-lines)
