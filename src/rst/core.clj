(ns rst.core
  (:require  [clojure.java.io :as io]
             [clojure.string :as string]
             [clojure.pprint :refer [pprint]]
             [clojure.zip :as z]))

;; https://dev.clojure.org/jira/browse/CLJS-1871
(defn ^:declared process-lines [lines node pos])

(def error-levels {:debug    0
                   :info     1
                   :warning  2
                   :error    3
                   :severe   4})

(defrecord StateTransition [state name parse])

(defprotocol IReadingLines
  (current-line [_])
  (next-line [_])
  (forward [_])
  (backward [_ n])
  (eof? [_])
  (eof-on-next? [_])
  (indented? [_]))

(defprotocol IStateManagement
  (push-state [_ state])
  (pop-state [_])
  (current-state [_])
  (add-to-buffers [_ line])
  (update-buffers [_ lines])
  (clear-buffers [_])
  (buffers? [_]))

(defrecord DocumentContext
    [ast lines current-idx states transition pos buffers reported-level]
  IReadingLines
  (current-line [_] (nth lines current-idx))
  (next-line [_] (nth lines (inc current-idx)))
  (forward [context] (-> context
                         (update :current-idx inc)
                         (update :pos (fn [[row col]] [(inc row) col]))))
  (backward
    [context n]
    (-> context
        (update :current-idx #(- % n))
        (update :pos (fn [[row col]] [(- row n) col]))))
  (eof? [_] (>= current-idx (count lines)))
  (eof-on-next? [_] (>= (inc current-idx) (count lines)))
  (indented? [_] (> (peek pos) 0))
  IStateManagement
  (push-state [_ state] (update _ :states conj state))
  (pop-state [_] (update _ :states pop))
  (current-state [_] (-> _ :states peek))
  (add-to-buffers [_ line] (update _ :buffers conj line))
  (update-buffers [_ lines] (assoc _ :buffers lines))
  (clear-buffers [_] (update-buffers _ []))
  (buffers? [_] (-> _ :buffers empty? not)))

(defn make-context
  [lines ast init-state init-pos]
  (map->DocumentContext {:ast ast
                         :lines lines
                         :pos init-pos
                         :current-idx 0
                         :states [init-state]
                         :buffers []
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
  (loop [node ast]
    (if-let [parent (z/up node)]
      (recur parent)
      node)))

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
   :indent  #"(\s+)(.+)"
   :grid-table-top #"\+-[-\+]+-\+ *$"
   :grid-table-head-sep #"\+=[=+]+=\+ *$"
   :grid-table-left-side #"^(\+|\|).*"
   :grid-table-right-side #".*(\+|\|)$"
   :bullet  #"([-+*\u2022\u2023\u2043])(\s+)(.*|$)"
   :line    #"([\!-\/\:-\@\[-\`\{-\~])\1* *$"
   :text    #".+"})

(re-matches (:indent patterns) " This Hello")
(re-matches (:line patterns) "***")
(re-matches (:bullet patterns) "* Item")
(re-matches (:grid-table-top patterns) "+--+------+")
(re-matches (:grid-table-head-sep patterns) "+===+")
(re-matches (:grid-table-left-side patterns) "+ jj")
(re-matches (:grid-table-right-side patterns) "+ jj   +  ")

;;(let [[_ style spacing text] (re-matches (:bullet patterns) "*   Hello")
;;      indent (inc (count spacing))]
;;  (do
;;    (println (str "Style: " style))
;;    (println (str "Indent: " indent))
;;    (println (str "Text: " text))))

(defn parse [context transition match]
  (-> transition :parse (apply [context match])))

(defn match-transition [transition-name line]
  (-> patterns transition-name (re-matches line)))

(defn match-transition? [transition-name line]
  (-> (match-transition transition-name line)
      nil? not))

(defn create-node [n]
  (merge {:iid (get-iid)} n))

(defn create-preserve [text]
  (create-node {:type :preserve :value text}))

(defn create-inline-markup-text [text]
  ;; TODO: parse the text into an vector of text
  ;; if there are inline-markups, parse them accordingly
  [(create-node {:type :text
                 :value (string/trimr text)})])

(defn create-header [text line]
  (create-node {:type :header
                :children (create-inline-markup-text text)}))

(defn create-section [text line style]
  (let [adornment (first line)]
    (create-node {:type :section
                  :style (str style adornment)
                  :name (normalize-section-name text)
                  :children [(create-header text line)]})))

(defn create-paragraph [text]
  (create-node {:type :paragraph
               :children (create-inline-markup-text text)}))

(defn create-transition []
  (create-node {:type :transition}))

(defn create-error
  ([description level pos]
   (create-node {:type :error
                 :level (level error-levels)
                 :pos (->> pos (map inc) vec)
                 :children [(create-paragraph description)]}))
  ([description level pos block-text]
   (create-node {:type :error
                 :level (level error-levels)
                 :pos (->> pos (map inc) vec)
                 :children [(create-paragraph description)
                            (create-preserve block-text)]})))

(defn create-blockquote [indent]
  (create-node {:type :blockquote
                :indent indent
                :children []}))

(defn create-bullet-list [style]
  (create-node {:type :bullet-list
                :style style
                :children []}))

(defn create-bullet-item [indent]
  (create-node {:type :bullet-item
                :indent indent
                :children []}))

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

(defn append-error-section-title-too-short [ast pos style text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        msg (str "Title " style " too short.")
        error (create-error msg :warning pos block-text)]
    (append-error ast error)))

(defn append-error-section-mismatching-underline [ast pos text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        msg "Missing matching underline for section title overline."
        error (create-error msg :severe pos block-text)]
    (append-error ast error)))

(defn append-error-section-mismatching-overline-underline [ast pos text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        msg "Title overline & underline mismatch."
        error (create-error msg :severe pos block-text)]
    (append-error ast error)))

(defn append-error-incomplete-section-title [ast pos text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        msg "Incomplete section title."
        error (create-error msg :severe pos block-text)]
    (append-error ast error)))

(defn append-error-unexpected-section-title [ast pos text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        msg "Unexpected section title."
        error (create-error msg :severe pos block-text)]
    (append-error ast error)))

(defn append-error-unexpected-section-title-or-transition [ast pos text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        msg "Unexpected section title or transition."
        error (create-error msg :severe pos block-text)]
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

(defn append-applicable-error-section-title-too-short [ast pos style text-lines]
  (if (is-section-title-short? style text-lines)
    (append-error-section-title-too-short ast pos style text-lines)
    ast))

(defn append-section-line->text-line [ast pos text-lines]
  (let [[overline text _] text-lines
        section-style "overline"
        new-section (create-section text overline section-style)]
    (-> ast (append-section-in-matched-location new-section)
        (append-applicable-error-section-title-too-short pos
                                                         section-style
                                                         text-lines))))

(defn append-section-text->line [ast pos text-lines]
  (let [[text underline] text-lines
        section-style "underline"
        new-section (create-section text underline section-style)]
    (-> ast
        (append-section-in-matched-location new-section)
        (append-applicable-error-section-title-too-short pos
                                                         section-style
                                                         text-lines))))

(defn document-begin? [ast]
  (let [siblings (z/children ast)
        siblings-count (count siblings)]
    (or (= siblings-count 0)
        (and (= siblings-count 1)
             (-> siblings first :type (= :header))))))

(defn last-sibling-transition? [ast]
  (-> ast z/down z/rightmost z/node :type (= :transition)))

(defn append-applicable-error-doc-start-with-transition [ast pos]
  (if (document-begin? ast)
    (let [msg "Document or section may not begin with a transition."
          error (create-error msg :error pos)]
      (append-error ast error))
    ast))

(defn append-applicable-error-adjacent-transitions [ast pos]
  (if (last-sibling-transition? ast)
    (let [msg "At least one body element must separate transitions; adjacent transitions are not allowed."
          error (create-error msg :error pos)]
      (append-error ast error))
    ast))

(defn append-error-doc-end-with-transition [ast pos]
  (let [msg "Document may not end with a transition."
        error (create-error msg :error pos)]
    (append-error ast error)))

(defn append-transition-line->blank [ast pos]
  (let [transition (create-transition)]
    (-> ast
        (append-applicable-error-doc-start-with-transition pos)
        (append-applicable-error-adjacent-transitions pos)
        (append-transition transition))))

(defn append-applicable-error-blockquote-no-end-blank-line [ast pos lines eof?]
  (if-not (or (match-transition? :blank (peek lines)) eof?)
    (let [msg "Block quote ends without a blank line; unexpected unindent."
          error (create-error msg :warning pos)]
      (append-error ast error))
    ast))

(defn append-applicable-error-bullet-no-end-blank-line [ast pos idx lines]
  (if (> idx 0)
    (let [prev-line (nth lines (dec idx))]
      (if (match-transition? :blank prev-line)
        ast
        (let [msg "Bullet list ends without a blank line; unexpected unindent."
              error (create-error msg :warning pos)]
          (append-error ast error))))
    ast))

(defn append-applicable-error-table-no-end-blank-line [ast pos idx lines]
  (let [current-line (nth lines idx :eof)]
    (if-not (or (= current-line :eof) (match-transition? :blank current-line))
      (let [msg "Blank line required after table."
            [row col] pos
            prev-pos [(dec row) col]
            error (create-error msg :warning prev-pos)]
        (append-error ast error))
      ast)))

(defn append-error-unexpected-indentation [ast pos]
  (let [msg "Unexpected indentation."
        error (create-error msg :error pos)]
    (append-error ast error)))

(defn append-blockquote-body->indent [ast lines indent pos]
  (let [[row col] pos
        raw-indent (+ indent col)
        blockquote (create-blockquote raw-indent)
        new-pos [row raw-indent]
        processed-blockquote (process-lines lines blockquote new-pos)]
    (-> ast
        (append-node processed-blockquote))))

(defn append-bullet-list [ast style]
  (let [bullet-list (create-bullet-list style)]
    (-> ast
        (append-node bullet-list)
        move-to-latest-child)))

(defn append-bullet-item [ast lines indent pos]
  (let [[row col] pos
        raw-indent (+ col indent)
        bullet-item (create-bullet-item raw-indent)
        new-pos [row raw-indent]
        processed-bullet-item (process-lines lines bullet-item new-pos)]
    (-> ast (append-node processed-bullet-item))))

(defn append-preserve-text [ast lines]
  (let [block-text (string/join "\r\n" lines)
        preserve-text (create-preserve block-text)]
    (-> ast (append-node preserve-text))))

(defn append-error-malformed-table [ast pos lines]
  (let [block-text (string/join "\r\n" lines)
        msg "Malformed table."
        error (create-error msg :error pos block-text)]
    (append-error ast error)))

(defn body->blank [context _]
  (-> context forward))

(defn body->line [context _]
  (let [line (current-line context)
        pos (:pos context)
        short-line? (< (count line) 4)]
    (if (indented? context)
      (if short-line?
        (-> context
            forward
            (add-to-buffers line)
            (push-state :text))
        (-> context
            (update-ast append-error-unexpected-section-title-or-transition
                        pos
                        [line])
            forward))
      (-> context
          forward
          (add-to-buffers line)
          (push-state :line)))))

(defn append-paragraph [ast block-text]
  (let [paragraph (create-paragraph block-text)]
    (append-node ast paragraph)))

(defn anppend-text [ast text]
  (let [text (create-inline-markup-text text)]
    (append-node ast text)))

(defn body->text
  [context _]
  (if-not (eof? context)
    (let [line (current-line context)]
      (if-not (eof-on-next? context)
        (-> context
            forward
            (add-to-buffers line)
            (push-state :text))
        ;; next line if EOF
        (let [text-lines (-> context :buffers (conj line))]
          (-> context
              (update-ast append-paragraph (string/join " " text-lines))
              forward
              clear-buffers))))))

(defn line->blank [context _]
  ;; Check if it is the last line of the document
  ;; - Y: Raise error
  ;; - N: Create a transition
  (let [{[row col] :pos buffers :buffers} context
        prev-text-line (peek buffers)
        prev-short-line? (< (count prev-text-line) 4)
        prev-pos [(dec row) col]]
    (if prev-short-line?
      (-> context
          (update-ast append-paragraph prev-text-line)
          forward
          clear-buffers
          pop-state)
      (loop [current-context context]
        (if-not (eof? current-context)
          (let [line (current-line current-context)]
            (if (match-transition? :blank line)
              (recur (-> current-context forward))
              (-> current-context
                  (update-ast append-transition-line->blank prev-pos)
                  clear-buffers
                  pop-state)))
          (-> current-context
              (update-ast append-transition-line->blank prev-pos)
              (update-ast append-error-doc-end-with-transition prev-pos)
              clear-buffers
              pop-state))))))

(defn line->text [context _]
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
  (let [{text-lines :buffers [row col] :pos} context
        prev-pos [(dec row) col]
        current-text-line (current-line context)
        current-text-lines (conj text-lines current-text-line)
        prev-text-line (peek text-lines)
        prev-short-line? (< (count prev-text-line) 4)]
    (if-not (eof-on-next? context)
      (let [next-text-line (next-line context)
            next-text-lines (conj current-text-lines next-text-line)]
        (if (match-transition? :line next-text-line)
          (if (= prev-text-line next-text-line)
            (-> context
                (update-ast append-section-line->text-line prev-pos next-text-lines)
                forward
                forward
                clear-buffers
                pop-state)
            (if prev-short-line?
              (-> context
                  forward
                  forward
                  (update-buffers next-text-lines)
                  pop-state
                  (push-state :text))
              (-> context
                  (update-ast append-error-section-mismatching-overline-underline
                              prev-pos
                              next-text-lines)
                  forward
                  forward
                  clear-buffers
                  pop-state)))
          (if prev-short-line?
            (-> context
                forward
                forward
                (update-buffers next-text-lines)
                pop-state
                (push-state :text))
            (-> context
                (update-ast append-error-section-mismatching-underline
                            prev-pos
                            next-text-lines)
                forward
                forward
                clear-buffers
                pop-state))))
      (if prev-short-line?
        (-> context
            forward
            (add-to-buffers current-text-line)
            pop-state
            (push-state :text))
        (-> context
            (update-ast append-error-incomplete-section-title
                        prev-pos
                        current-text-lines)
            forward
            clear-buffers
            pop-state)))))

(defn line->line [context _]
  ;; Append the error message
  )

(defn text->blank [context _]
  (let [text-lines (:buffers context)
        block-text (string/join " " text-lines)]
    (-> context
        (update-ast append-paragraph block-text)
        forward
        clear-buffers
        pop-state)))

(defn text->text [context _]
  ;; Read the whole block of text until the blank line
  ;; keep track the index
  (loop [current-context context]
    (if-not (eof? current-context)
      (let [line (current-line current-context)]
        (cond (match-transition? :blank line)
              ;; blank line, create paragraph & return
              (let [block-text (string/join " " (:buffers current-context))]
                (-> current-context
                    (update-ast append-paragraph block-text)
                    clear-buffers
                    pop-state))
              (match-transition? :indent line)
              (let [block-text (string/join " " (:buffers current-context))
                    pos (:pos current-context)]
                (-> current-context
                    (update-ast append-paragraph block-text)
                    (update-ast append-error-unexpected-indentation pos)
                    clear-buffers
                    pop-state))
              :else
              (recur (-> current-context
                         forward
                         (add-to-buffers line)))))
      ;; EOF, create paragraph & return
      (let [block-text (string/join " " (:buffers current-context))]
        (-> current-context
            (update-ast append-paragraph block-text)
            forward
            clear-buffers
            pop-state)))))

(defn text->line [context match]
  (let [{pos :pos buffers :buffers} context
        prev-text-line (peek buffers)
        line (current-line context)
        line-count (count line)
        short-line? (< line-count 4)
        shorter-than-prev? (< line-count (count prev-text-line))
        text-lines [prev-text-line line]]
    ;; There are two possible cases:
    ;; - line is shorter than the text (in buffers)
    ;;     AND line is shorter than 4 characters
    ;;     | switch to transition text->text
    ;;   - else
    ;;     | create the section
    (if (and short-line? shorter-than-prev?)
      (text->text context match)
      (if (indented? context)
        (-> context
            (update-ast append-error-unexpected-section-title pos text-lines)
            forward
            clear-buffers
            pop-state)
        (-> context
            (update-ast append-section-text->line pos text-lines)
            forward
            clear-buffers
            pop-state)))))

(defn read-indented-lines [context indent]
  (let [{text-lines :buffers} context
        indented-pattern (re-pattern (str " {" indent "}(.*)"))]
    (loop [indented-lines text-lines
           current-context context]
      (if-not (eof? current-context)
        (let [line (current-line current-context)]
          (if-let [match (re-matches indented-pattern line)]
            (recur (conj indented-lines (nth match 1)) (forward current-context))
            (if (match-transition? :blank line)
              (recur (conj indented-lines "") (forward current-context))
              (-> current-context
                  (update-buffers indented-lines)))))
        (-> current-context
            (update-buffers indented-lines))))))

(defn body->indent
  [context [line indent-str text]]
  (let [current-indent (count indent-str)
        pos (:pos context)
        next-context (read-indented-lines context
                                          current-indent)
        eof? (eof? next-context)
        {next-pos :pos lines :buffers} next-context]
    (-> next-context
        (update-ast append-blockquote-body->indent
                    lines
                    current-indent
                    pos)
        (update-ast append-applicable-error-blockquote-no-end-blank-line
                    next-pos
                    lines
                    eof?)
        clear-buffers)))

(defn body->bullet
  [context [_ style spacing text]]
  (let [indent (inc (count spacing))
        pos (:pos context)
        next-context (read-indented-lines
                      (-> context
                          (add-to-buffers text)
                          forward)
                      indent)
        lines (:buffers next-context)]
    (-> next-context
        (update-ast append-bullet-list style)
        (update-ast append-bullet-item lines indent pos)
        clear-buffers
        (push-state :bullet))))

(defn body->enum [context match])

(defn ^:private extract-text-block
  "Extract the text block from the current line
   to the nearest blank line"
  [context]
  (loop [current-context context]
    (if-not (eof? current-context)
      (let [line (current-line current-context)]
        (if-not (match-transition? :blank line)
          (recur (-> current-context
                     (add-to-buffers line)
                     forward))
          current-context))
      current-context)))

(defn ^:private backward-buffers [context idx]
  (let [lines (:buffers context)
        end-idx (-> lines count dec)
        offset-idx (- end-idx idx)
        sub-lines (-> lines (subvec 0 (inc idx)))]
    (-> context
        (backward offset-idx)
        (update-buffers sub-lines))))

(defn ^:private mark-table-bottom
  "Find the bottom of the table,
   move the context to after that line"
  [context]
  (let [lines (:buffers context)
        c (count lines)
        end-idx (dec c)]
    (if (> c 0)
      (loop [idx end-idx]
        (if (> idx 2)
          (let [line (nth lines idx)]
            (if (match-transition? :grid-table-top line)
              (backward-buffers context idx)
              (recur (dec idx))))
          (backward-buffers context idx)))
      context)))

(defn ^:private mark-table-left-edge
  "Check every line of buffers if it belongs to the table
   Stop when it does not and move the context to that line"
  [context]
  (let [lines (:buffers context)
        c (count lines)]
    (if (> c 0)
      (loop [idx 0]
        (if (< idx c)
          (let [line (nth lines idx)]
            (if (match-transition? :grid-table-left-side line)
              (recur (inc idx))
              (backward-buffers context (dec idx))))
          context))
      context)))

(defn ^:private mark-table-right-edge [context]
  (let [lines (:buffers context)
        width (-> lines first count)
        c (count lines)]
    (if (> c 0)
      (loop [idx 0]
        (if (< idx c)
          (let [line (nth lines idx)]
            (if (and (= width (count line))
                     (match-transition? :grid-table-right-side line))
              (recur (inc idx))
              (let [[row col] (:pos context)
                    last-pos [(- row c )col]]
                (-> context
                    (update-ast append-error-malformed-table last-pos lines)
                    clear-buffers))))
          context))
      context)))

(defn ^:private find-grid-table-headers [lines]
  (reduce-kv (fn [heads idx line]
               (if (match-transition? :grid-table-head-sep line)
                 (conj heads idx)
                 heads))
             [] lines))

(defn ^:private extract-table-block [context]
  (-> context
      extract-text-block
      mark-table-left-edge
      mark-table-bottom
      mark-table-right-edge))

;;(let [context (make-context content-lines
;;                            (zip-node {:type :root :iid (get-iid) :children []})
;;                            :body
;;                            [0 0])
;;      next-context (extract-text-block context)
;;      lines (:buffers next-context)
;;      end-idx (-> lines count dec)]
;;  (loop [idx end-idx]
;;    (if (> idx 2)
;;      (let [line (nth lines idx)]
;;        (if (match-transition? :grid-table-top line)
;;          (backward-buffers next-context idx)
;;          (recur (dec idx))))
;;      (backward-buffers next-context idx))))

(defn create-grid-table [ast width ]
  (create-node {:type :table
                }))

(defn append-grid-table [ast pos lines]
  (if-not (empty? lines)
    ast
    ast))


(defn isolate-grid-table [context]
  (let [next-context (extract-table-block context)]
    next-context))

(defn body->grid-table-top
  [context match]
  (let [next-context (isolate-grid-table context)
        {current-idx :current-idx pos :pos buffers :buffers lines :lines} next-context
        [row col] pos
        table-pos [(- row (count buffers)) col]]
    (-> next-context
        (update-ast append-grid-table table-pos buffers)
        (update-ast append-applicable-error-table-no-end-blank-line
                    pos
                    current-idx
                    lines)
        clear-buffers)))

(def body->field-marker {:name :enum,
                         :state :body,
                         :parse (fn [context match])})

(def body->option-marker {:name :enum,
                          :state :body,
                          :parse (fn [context match])})

(defn bullet->blank [context _]
  (forward context))

(defn ^:private bullet->others [context match transition]
  (let [lines (:lines context)
        {current-idx :current-idx pos :pos} context]
    (-> context
        pop-state
        (update-ast z/up)
        (update-ast append-applicable-error-bullet-no-end-blank-line
                    pos
                    current-idx
                    lines)
        (transition match))))

(defn bullet->indent [context match]
  (bullet->others context match body->indent))

(defn bullet->text [context match]
  (bullet->others context match body->text))

(defn bullet->line [context match]
  (bullet->others context match body->line))

(defn bullet->bullet
  [context [_ style spacing text]]
  (let [bullet-list (-> context :ast z/node)
        indent (inc (count spacing))
        next-context (read-indented-lines
                      (-> context
                          (add-to-buffers text)
                          forward)
                      indent)
        indented-lines (:buffers next-context)
        {lines :lines idx :current-idx pos :pos} context]
    (if (and (= (:type bullet-list) :bullet-list)
             (= (:style bullet-list) style))
      (-> next-context
          (update-ast append-bullet-item indented-lines indent pos)
          clear-buffers)
      (-> next-context
          (update-ast z/up)
          (update-ast append-applicable-error-bullet-no-end-blank-line
                      pos
                      idx
                      lines)
          (update-ast append-bullet-list style)
          (update-ast append-bullet-item indented-lines indent pos)
          clear-buffers))))

(def states
  {:body           {:transitions [(StateTransition. :body :blank body->blank)
                                  (StateTransition. :body :indent body->indent)
                                  (StateTransition. :body :bullet body->bullet)
                                  ;;body->enum
                                  ;;body->field-marker
                                  ;;body->option-marker
                                  ;;body->doctest
                                  ;;body->line-block
                                  (StateTransition. :body :grid-table-top body->grid-table-top)
                                  ;;body->simple-table-top
                                  ;;body->explicit-markup
                                  ;;body->anonymous
                                  (StateTransition. :body :line body->line)
                                  (StateTransition. :body :text body->text)]}
   :bullet         {:transitions [(StateTransition. :bullet :blank bullet->blank)
                                  (StateTransition. :bullet :indent bullet->indent)
                                  (StateTransition. :bullet :bullet bullet->bullet)
                                  ;;bullet->enum
                                  ;;bullet->field-marker
                                  ;;bullet->option-marker
                                  ;;bullet->doctest
                                  ;;bullet->line-block
                                  ;;bullet->grid-table-top
                                  ;;bullet->simple-table-top
                                  ;;bullet->explicit-markup
                                  ;;bullet->anonymous
                                  (StateTransition. :bullet :line bullet->line)
                                  (StateTransition. :bullet :text bullet->text)]}
   :line           {:transitions [(StateTransition. :line :blank line->blank)
                                  (StateTransition. :line :text line->text)
                                  (StateTransition. :line :line line->line)]}
   :text           {:transitions [(StateTransition. :text :blank text->blank)
                                  ;;text->indent
                                  (StateTransition. :text :line text->line)
                                  (StateTransition. :text :text text->text)]}})

(defn next-transition [state line]
  (some #(if-let [match (match-transition (:name %) line)]
           [% match])
        (-> states state :transitions)))

(def content-lines (-> (io/resource "table.rst")
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

(defn single-paragraph-document? [ast]
  (let [children (-> ast up-to-root z/children)]
    (and (= (count children) 1)
         (-> children first :type (= :paragraph)))))


(defn unwrap-root-paragraph [ast]
  (let [paragraph (-> ast up-to-root z/down)
        children (z/children paragraph)]
    (reduce (fn [t child]
              (z/append-child t child))
            (-> paragraph z/remove) children)))

(defn unwrap-single-indented-document
  "If the indented document contains a single paragraph
  unwrap the paragraph content"
  [context]
  (if (and (indented? context) (-> context :ast single-paragraph-document?))
    (update-ast context unwrap-root-paragraph)
    context))

(defn clean-up-buffers [context]
  (if (buffers? context)
    (let [current-state (current-state context)
          [transition match] (next-transition current-state "")]
      (parse context transition match))
    context))

(defn process-lines [lines node pos]
  (let [init-ast (zip-node node)
        init-context (make-context lines init-ast :body pos)]
    (loop [context init-context]
      (if-not (eof? context)
        (let [line (current-line context)
              current-state (current-state context)
              [transition match] (next-transition current-state line)
              new-context (parse context transition match)]
          (recur new-context))
        (-> context
            clean-up-buffers
            unwrap-single-indented-document
            :ast
            z/root)))))

(defn process-document [document-lines]
  (let [root-node (create-node {:type :root :children []})
        pos [0 0]]
    (process-lines document-lines root-node pos)))

;;(process-document content-lines)

