(ns rst.core
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.zip :as z]
            [clojure.set :as set]))

;; https://dev.clojure.org/jira/browse/CLJS-1871
(defn ^:declared process-lines [lines node pos])
(defn ^:declared next-transition [state line])
(defn ^:declared parse [ctx transition match])

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
  (transit [_] [_ line])
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
  (forward [this] (-> this
                      (update :current-idx inc)
                      (update :pos (fn [[row col]] [(inc row) col]))))
  (backward [this n] (-> this
                         (update :current-idx #(- % n))
                         (update :pos (fn [[row col]] [(- row n) col]))))
  (eof? [_] (>= current-idx (count lines)))
  (eof-on-next? [_] (>= (inc current-idx) (count lines)))
  (indented? [_] (> (peek pos) 0))
  IStateManagement
  (transit [this] (transit this (current-line this)))
  (transit [this line]
    ;; TODO get the current state, get the correct transition and match according to the line
    (let [state (current-state this)
          [transition match] (next-transition state line)]
      (parse this transition match)))
  (push-state [this state] (update this :states conj state))
  (pop-state [this] (update this :states pop))
  (current-state [this] (-> this :states peek))
  (add-to-buffers [this line] (update this :buffers conj line))
  (update-buffers [this lines] (assoc this :buffers lines))
  (clear-buffers [this] (update-buffers this []))
  (buffers? [this] (-> this :buffers empty? not)))

(defn make-context
  [lines ast init-state init-pos]
  (map->DocumentContext {:ast ast
                         :lines lines
                         :pos init-pos
                         :current-idx 0
                         :states [init-state]
                         :buffers []
                         :reported-level (:info error-levels)}))

(defn update-ast [ctx f & args]
  (update ctx :ast #(apply f % args)))

(def ^{:private true :const true} uid-prefix "AST_NODE_")

(defn get-uid []
  (gensym uid-prefix))

(defn get-uid-path [ast]
  (-> ast
      z/path
      (->> (map :uid))
      (conj (:uid (z/node ast)))
      reverse))

(defn up-to-root [ast]
  (if-let [parent (z/up ast)]
    (recur parent)
    ast))

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

(defn parse [ctx transition match]
  (-> transition :parse (apply [ctx match])))

(defn match-transition [transition-name line]
  (-> patterns transition-name (re-matches line)))

(defn match-transition? [transition-name line]
  (-> (match-transition transition-name line)
      nil? not))

(defn create-node [n]
  (merge {:uid (get-uid)} n))

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

(defn find-loc-with-uid [ast child-uid]
  (if (-> ast z/node :uid (= child-uid))
    ast
    (some #(if (-> % z/node :uid (= child-uid)) %)
          (children-loc ast))))

;; TODO: add the current loc id to path
(defn find-matching-section-loc [ast style]
  ;; Travel in the whole doc from top to the current path
  ;; - if there is a section having the same style as the new section
  ;;   | return the location
  ;; - else
  ;;   | return nil
  (let [current-uid-path (get-uid-path ast)]
    (loop [current-loc (up-to-root ast)
           depth 0]
      (if (< depth (count current-uid-path))
        (let [current-uid (nth current-uid-path depth)]
          (if-let [section-loc (find-loc-with-uid current-loc current-uid)]
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

(defn has-no-end-blank-line? [lines idx]
  (let [current-line (nth lines idx :eof)]
    (not (or (= current-line :eof) (match-transition? :blank current-line)))))

(defn append-error-table-no-end-blank-line [ast pos]
  (let [msg "Blank line required after table."
        [row col] pos
        prev-pos [(dec row) col]
        error (create-error msg :warning prev-pos)]
    (append-error ast error)))

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

(defn create-error-malformed-table
  ([lines pos description]
   (let [block-text (string/join "\r\n" lines)
         msg (string/join " " ["Malformed table." description])]
     (create-error (string/trimr msg) :error pos block-text)))
  ([lines pos]
   (create-error-malformed-table lines pos "")))

(defn append-error-malformed-table [ast pos lines]
  (let [error (create-error-malformed-table lines pos)]
    (append-error ast (error))))

(defn body->blank [ctx _]
  (-> ctx forward))

(defn body->line [ctx _]
  (let [line (current-line ctx)
        pos (:pos ctx)
        short-line? (< (count line) 4)]
    (if (indented? ctx)
      (if short-line?
        (-> ctx
            forward
            (add-to-buffers line)
            (push-state :text))
        (-> ctx
            (update-ast append-error-unexpected-section-title-or-transition
                        pos
                        [line])
            forward))
      (-> ctx
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
  [ctx _]
  (if-not (eof? ctx)
    (let [line (current-line ctx)]
      (if-not (eof-on-next? ctx)
        (-> ctx
            forward
            (add-to-buffers line)
            (push-state :text))
        ;; next line if EOF
        (let [text-lines (-> ctx :buffers (conj line))]
          (-> ctx
              (update-ast append-paragraph (string/join " " text-lines))
              forward
              clear-buffers))))))

(defn line->blank [ctx _]
  ;; Check if it is the last line of the document
  ;; - Y: Raise error
  ;; - N: Create a transition
  (let [{[row col] :pos buffers :buffers} ctx
        prev-text-line (peek buffers)
        prev-short-line? (< (count prev-text-line) 4)
        prev-pos [(dec row) col]]
    (if prev-short-line?
      (-> ctx
          (update-ast append-paragraph prev-text-line)
          forward
          clear-buffers
          pop-state)
      (loop [current-ctx ctx]
        (if-not (eof? current-ctx)
          (let [line (current-line current-ctx)]
            (if (match-transition? :blank line)
              (recur (-> current-ctx forward))
              (-> current-ctx
                  (update-ast append-transition-line->blank prev-pos)
                  clear-buffers
                  pop-state)))
          (-> current-ctx
              (update-ast append-transition-line->blank prev-pos)
              (update-ast append-error-doc-end-with-transition prev-pos)
              clear-buffers
              pop-state))))))

(defn line->text [ctx _]
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
  (let [{text-lines :buffers [row col] :pos} ctx
        prev-pos [(dec row) col]
        current-text-line (current-line ctx)
        current-text-lines (conj text-lines current-text-line)
        prev-text-line (peek text-lines)
        prev-short-line? (< (count prev-text-line) 4)]
    (if-not (eof-on-next? ctx)
      (let [next-text-line (next-line ctx)
            next-text-lines (conj current-text-lines next-text-line)]
        (if (match-transition? :line next-text-line)
          (if (and (= prev-text-line next-text-line)
                   (or (not prev-short-line?)
                       (<= (count current-text-line) (count prev-text-line))))
            (-> ctx
                (update-ast append-section-line->text-line prev-pos next-text-lines)
                forward
                forward
                clear-buffers
                pop-state)
            (if prev-short-line?
              (-> ctx
                  forward
                  forward
                  (update-buffers next-text-lines)
                  pop-state
                  (push-state :text))
              (-> ctx
                  (update-ast append-error-section-mismatching-overline-underline
                              prev-pos
                              next-text-lines)
                  forward
                  forward
                  clear-buffers
                  pop-state)))
          (if prev-short-line?
            (-> ctx
                forward
                forward
                (update-buffers next-text-lines)
                pop-state
                (push-state :text))
            (-> ctx
                (update-ast append-error-section-mismatching-underline
                            prev-pos
                            next-text-lines)
                forward
                forward
                clear-buffers
                pop-state))))
      (if prev-short-line?
        (-> ctx
            forward
            (add-to-buffers current-text-line)
            pop-state
            (push-state :text))
        (-> ctx
            (update-ast append-error-incomplete-section-title
                        prev-pos
                        current-text-lines)
            forward
            clear-buffers
            pop-state)))))

(defn line->line [ctx _]
  ;; Append the error message
  )

(defn text->blank [ctx _]
  (let [text-lines (:buffers ctx)
        block-text (string/join " " text-lines)]
    (-> ctx
        (update-ast append-paragraph block-text)
        forward
        clear-buffers
        pop-state)))

(defn text->text [ctx _]
  ;; Read the whole block of text until the blank line
  ;; keep track the index
  (loop [current-ctx ctx]
    (if-not (eof? current-ctx)
      (let [line (current-line current-ctx)]
        (cond (match-transition? :blank line)
              ;; blank line, create paragraph & return
              (let [block-text (string/join " " (:buffers current-ctx))]
                (-> current-ctx
                    (update-ast append-paragraph block-text)
                    clear-buffers
                    pop-state))
              (match-transition? :indent line)
              (let [block-text (string/join " " (:buffers current-ctx))
                    pos (:pos current-ctx)]
                (-> current-ctx
                    (update-ast append-paragraph block-text)
                    (update-ast append-error-unexpected-indentation pos)
                    clear-buffers
                    pop-state))
              :else
              (recur (-> current-ctx
                         forward
                         (add-to-buffers line)))))
      ;; EOF, create paragraph & return
      (let [block-text (string/join " " (:buffers current-ctx))]
        (-> current-ctx
            (update-ast append-paragraph block-text)
            forward
            clear-buffers
            pop-state)))))

(defn text->line [ctx match]
  (let [{pos :pos buffers :buffers} ctx
        prev-text-line (peek buffers)
        line (current-line ctx)
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
      (text->text ctx match)
      (if (indented? ctx)
        (-> ctx
            (update-ast append-error-unexpected-section-title pos text-lines)
            forward
            clear-buffers
            pop-state)
        (-> ctx
            (update-ast append-section-text->line pos text-lines)
            forward
            clear-buffers
            pop-state)))))

(defn read-indented-lines [ctx indent]
  (let [{text-lines :buffers} ctx
        indented-pattern (re-pattern (str " {" indent "}(.*)"))]
    (loop [indented-lines text-lines
           current-ctx ctx]
      (if-not (eof? current-ctx)
        (let [line (current-line current-ctx)]
          (if-let [match (re-matches indented-pattern line)]
            (recur (conj indented-lines (nth match 1)) (forward current-ctx))
            (if (match-transition? :blank line)
              (recur (conj indented-lines "") (forward current-ctx))
              (-> current-ctx
                  (update-buffers indented-lines)))))
        (-> current-ctx
            (update-buffers indented-lines))))))

(defn body->indent [ctx [line indent-str text]]
  (let [current-indent (count indent-str)
        pos (:pos ctx)
        next-ctx (read-indented-lines ctx current-indent)
        eof? (eof? next-ctx)
        {next-pos :pos lines :buffers} next-ctx]
    (-> next-ctx
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
  [ctx [_ style spacing text]]
  (let [indent (inc (count spacing))
        pos (:pos ctx)
        next-ctx (read-indented-lines
                  (-> ctx
                      (add-to-buffers text)
                      forward)
                  indent)
        lines (:buffers next-ctx)]
    (-> next-ctx
        (update-ast append-bullet-list style)
        (update-ast append-bullet-item lines indent pos)
        clear-buffers
        (push-state :bullet))))

(defn body->enum [ctx match])

(defn ^:private extract-text-block
  "Extract the text block from the current line
   to the nearest blank line"
  [ctx]
  (if-not (eof? ctx)
    (let [line (current-line ctx)]
      (if-not (match-transition? :blank line)
        (recur (-> ctx
                   (add-to-buffers line)
                   forward))
        ctx))
    ctx))

(defn ^:private backward-buffers [ctx idx]
  (let [lines (:buffers ctx)
        end-idx (-> lines count dec)
        offset-idx (- end-idx idx)
        sub-lines (->> lines (split-at (inc idx)) first vec)]
    (-> ctx
        (backward offset-idx)
        (update-buffers sub-lines))))

(defn ^:private mark-table-bottom
  "Find the bottom of the table,
   move the context to after that line"
  [ctx]
  (let [lines (:buffers ctx)
        c (count lines)
        end-idx (dec c)]
    (if (> c 0)
      (loop [idx end-idx]
        (if (> idx 2)
          (let [line (nth lines idx)]
            (if (match-transition? :grid-table-top line)
              (backward-buffers ctx idx)
              (recur (dec idx))))
          (backward-buffers ctx idx)))
      ctx)))

(defn ^:private mark-table-left-edge
  "Check every line of buffers if it belongs to the table
   Stop when it does not and move the context to that line"
  [ctx]
  (let [lines (:buffers ctx)
        c (count lines)]
    (if (> c 0)
      (loop [idx 0]
        (if (< idx c)
          (let [line (nth lines idx)]
            (if (match-transition? :grid-table-left-side line)
              (recur (inc idx))
              (backward-buffers ctx (dec idx))))
          ctx))
      ctx)))

(defn ^:private validate-table-right-edge [ctx]
  (let [lines (:buffers ctx)
        width (-> lines first count)
        c (count lines)]
    (if (> c 0)
      (loop [idx 0]
        (if (< idx c)
          (let [line (nth lines idx)]
            (if (and (= width (count line))
                     (match-transition? :grid-table-right-side line))
              (recur (inc idx))
              (let [[row col] (:pos ctx)
                    last-pos [(- row c )col]]
                (-> ctx
                    (update-ast append-error-malformed-table last-pos lines)
                    clear-buffers))))
          ctx))
      ctx)))

(defn ^:private find-grid-table-header-ids [lines]
  (reduce-kv (fn [heads idx line]
               (if (match-transition? :grid-table-head-sep line)
                 (conj heads idx)
                 heads))
             [] lines))

(defn ^:private extract-table-block [ctx]
  (-> ctx
      extract-text-block
      mark-table-left-edge
      mark-table-bottom
      validate-table-right-edge))

(defn ^:private text-block-size
  "Get the size of the square block of text"
  [lines]
  [(-> lines (nth 0) count) (count lines)])

(defn nth-2d [lines row col]
  (-> lines (nth row) (nth col)))

(defn line-indent [line]
  (if-let [match (match-transition :indent line)]
    (let [[_ spaces _] match]
      (count spaces))
    0))

(defn ^:private trivial-block-indents [lines]
  (or (reduce (fn [m line]
                (if-not (empty? line)
                  (let [indent (line-indent line)]
                    (if m (min m indent) indent))
                  m))
              nil lines)
      0))

(defn strip-indents [lines indent]
  (reduce (fn [xs line]
            (if-not (empty? line)
              (conj xs (subs line indent))
              (conj xs line)))
          [] lines))

(defn ^:private get-cell-content
  [block [top left bottom right]]
  (let [t (+ top 1)
        l (+ left 1)
        width (- right left)
        height (- bottom t)
        rows (subvec block t bottom)]
    (reduce (fn [lines line]
              (let [cell-line (-> line (subs l right) string/trimr)]
                (conj lines cell-line)))
            [] rows)))

(defn create-table-cell [[top left bottom right] content [file-row file-col]]
  (let [cell (create-node {:type :cell
                           :top top
                           :left left
                           :width (- right left 1)
                           :height (- bottom top 1)
                           :children []})
        trivial-indents (trivial-block-indents content)
        stripped-content (strip-indents content trivial-indents)
        pos [(+ file-row top 1) (+ left file-col 1)]]
    (process-lines stripped-content cell pos)))

(defn create-table-row [id]
  (create-node {:type :row :id id :children []}))

(defn create-table-header []
  (create-node {:type :table-header :children []}))

(defn create-table-body []
  (create-node {:type :table-body :children [(create-table-row 0)]}))

(defn create-grid-table [width height pos]
  (create-node {:type :table
                :style :grid
                :pos pos
                :width width
                :height height
                :col-ids (sorted-set 0)
                :header-idx nil
                :children [(create-table-body)]}))

(defn move-to-table-body [ast]
  (if (-> ast z/node :type (= :table))
    (loop [table-child (-> ast z/down)]
      (cond (-> table-child z/node :type (= :table-body)) table-child
            (nil? table-child) nil
            :else (recur (z/right table-child))))
    nil))

(defn move-to-table-header [ast]
  (if (-> ast z/node :type (= :table))
    (loop [table-child (-> ast z/down)]
      (cond (-> table-child z/node :type (= :table-header)) table-child
            (nil? table-child) nil
            :else (recur (z/right table-child))))
    nil))

(defn move-to-body-row [ast id]
  (let [table-body-ast (move-to-table-body ast)]
    (loop [row-ast (z/down table-body-ast)]
      (if-not (nil? row-ast)
        (if (-> row-ast z/node :id (= id))
          row-ast
          (recur (z/right row-ast)))
        nil))))

(defn append-table-body-row [ast row-id]
  (let [table (z/node ast)
        height (:height table)]
    (if (< row-id (dec height))
      (let [row (create-table-row row-id)
            existing-row (move-to-body-row ast row-id)]
        (if (nil? existing-row)
          (let [table-body (move-to-table-body ast)]
            (-> table-body (append-node row)
                z/up))
          ast))
      ast)))

(defn append-table-col-id [ast col-id]
  (z/edit ast
          (fn [table]
            (let [col-ids (:col-ids table)
                  width (:width table)]
              (if-not (or (contains? col-ids col-id) (>= col-id (dec width)))
                (update table :col-ids #(conj %1 col-id))
                table)))))

(defn append-table-body-cell [ast cell]
  (let [row-id (:top cell)
        row-ast (move-to-body-row ast row-id)]
    (-> row-ast (append-node cell)
        z/up z/up)))

(defn ^:private scan-up [ast lines top left bottom right]
  (loop [table-ast ast
         row-id (dec bottom)]
    (if-not (< row-id top)
      (let [c (nth-2d lines row-id left)]
        (cond (> row-id top) (case c
                               \+ (recur (append-table-body-row ast row-id) (dec row-id))
                               \| (recur table-ast (dec row-id))
                               [table-ast nil])
              (= row-id top) (if (= c \+)
                               [table-ast [top left bottom right]]
                               [table-ast nil])
              :else [table-ast nil]))
      [table-ast nil])))

(defn ^:private scan-left [ast lines top left bottom right]
  (loop [table-ast ast
         col-id (dec right)]
    (if-not (< col-id left)
      (let [c (nth-2d lines bottom col-id)]
        (cond (> col-id left) (case c
                                \+ (recur (append-table-col-id ast col-id) (dec col-id))
                                \- (recur table-ast (dec col-id))
                                [table-ast nil])
              (= col-id left) (if (= c \+)
                                (scan-up table-ast lines top left bottom right)
                                [table-ast nil])
              :else [table-ast nil]))
      [table-ast nil])))

(defn ^:private scan-down [ast lines top left right]
  (let [table (z/node ast)
        height (:height table)]
    (loop [table-ast ast
           row-id (inc top)]
      (if (< row-id height)
        (let [c (nth-2d lines row-id right)]
          (case c
            \+ (let [[next-table-ast cell-pos] (-> ast (append-table-body-row row-id)
                                                   (scan-left lines top left row-id right))]
                 (if-not cell-pos
                   (recur next-table-ast (inc row-id))
                   [next-table-ast cell-pos]))
            \| (recur table-ast (inc row-id))
            [ast nil]))
        [ast nil]))))

(defn ^:private scan-right [ast lines top left]
  (let [table (z/node ast)
        width (:width table)]
    (loop [table-ast ast
           col-id (inc left)]
      (if (< col-id width)
        (let [c (nth-2d lines top col-id)]
          (case c
            \+ (let [[next-table-ast cell-pos] (-> ast (append-table-col-id col-id)
                                                   (scan-down lines top left col-id))]
                 (if-not cell-pos
                   (recur next-table-ast (inc col-id))
                   [next-table-ast cell-pos]))
            \- (recur table-ast (inc col-id))
            [ast nil]))
        [ast nil]))))

(defn ^:private valid-top-left-cell-corner? [[top left] [width height]]
  (and (not= top (dec height)) (not= left (dec width))))

(defn ^:private sort-table-body-rows [ast]
  (-> ast
      move-to-table-body
      (z/edit
       (fn [table-body]
         (update table-body :children #(->> % (sort-by :id) vec))))
      z/up))

(defn ^:private sort-table-header-rows [ast]
  (if (-> ast z/node :header-idx)
    (-> ast
        move-to-table-header
        (z/edit
         (fn [table-body]
           (update table-body :children #(->> % (sort-by :id) vec))))
        z/up)
    ast))

(defn ^:private table-body-cells [table]
  (let [table-body (some #(if (= (:type %) :table-body) %) (:children table))
        rows (:children table-body)]
    (reduce (fn [cells row]
              (apply conj cells (:children row))) [] rows)))

(defn ^:private table-cells-area [cells]
  (let [cell-sizes (map #(identity [(:width %) (:height %)]) cells)]
    (reduce (fn [area [width height]]
              (+ area (* (inc width) (inc height)))) 0 cell-sizes)))

(defn ^:private badform-table? [table]
  (let [cells (table-body-cells table)
        cells-area (table-cells-area cells)
        table-width (:width table)
        table-height (:height table)
        table-area (* table-width table-height)]
    (not= table-area
          (+ cells-area table-height table-width -1))))

(defn ^:private split-header-and-body-rows [ast]
  (let [table (z/node ast)]
    (if-let [header-id (:header-idx table)]
      (let [table-header (some #(if (= (:type %) :table-header) %) (:children table))
            table-body (some #(if (= (:type %) :table-body) %) (:children table))
            body-rows (:children table-body)
            header-rows (filter #(< (:id %) header-id) body-rows)
            updated-table-header (assoc table-header :children header-rows)
            row-comparator #(< (:id %1) (:id %2))
            updated-table-body (update table-body :children
                                       #(vec (set/difference (apply sorted-set-by row-comparator %)
                                                             (apply sorted-set-by row-comparator header-rows))))]
        (z/edit ast #(assoc % :children [updated-table-header updated-table-body])))
      ast)))

(defn scan-table-cells [ast lines]
  (let [table (z/node ast)
        {width :width height :height pos :pos} table
        init-table-ast ast]
    (loop [table-ast init-table-ast
           corners [[0 0]]]
      (if-not (empty? corners)
        (let [[top left] (peek corners)
              next-corners (pop corners)]
          (let [[next-table-ast cell-pos] (scan-right table-ast lines top left)]
            (if (and (vector? cell-pos) (= (count cell-pos) 4))
              (let [[_ _ bottom right] cell-pos
                    cell-content (get-cell-content lines cell-pos)
                    cell-node (create-table-cell cell-pos cell-content pos)]
                (recur (-> next-table-ast (append-table-body-cell cell-node))
                       (-> next-corners (cond->
                                            (valid-top-left-cell-corner? [top right] [width height])
                                          (conj [top right])
                                          (valid-top-left-cell-corner? [bottom left] [width height])
                                          (conj [bottom left]))
                           distinct sort reverse vec)))
              (recur table-ast (-> corners sort reverse vec pop)))))
        (if (-> table-ast z/node badform-table?)
          (z/replace table-ast (create-error-malformed-table lines pos "Parse incomplete."))
          (-> table-ast
              split-header-and-body-rows
              sort-table-header-rows
              sort-table-body-rows
              z/up))))))

(defn ^:private remove-table-header-line [lines header-idx]
  (if header-idx
    (assoc lines header-idx (-> lines
                                (nth header-idx)
                                (string/replace "=" "-")))
    lines))

(defn scan-table-block [ast lines pos]
  (let [[width height] (text-block-size lines)
        init-table (create-grid-table width height pos)
        init-table-ast (-> ast (append-node init-table) move-to-latest-child)
        header-ids (find-grid-table-header-ids lines)]
    (case (count header-ids)
      0 (scan-table-cells init-table-ast lines)
      1 (let [header-idx (first header-ids)
              non-header-lines (remove-table-header-line lines header-idx)]
          (-> init-table-ast
              (z/edit #(assoc % :header-idx header-idx))
              (append-node (create-table-header))
              ;;(append-node (create-table-body))
              (scan-table-cells non-header-lines)))
      (let [msg (str "Multiple head/body row separators (table lines "
                     (string/join ", " header-ids)
                     "); only one allowed.")]
        (z/replace init-table-ast (create-error-malformed-table lines pos msg))))))

(defn isolate-grid-table [ctx]
  (let [next-ctx (extract-table-block ctx)]
    next-ctx))

(defn body->grid-table-top
  [ctx match]
  (let [next-ctx (isolate-grid-table ctx)
        {current-idx :current-idx pos :pos buffers :buffers lines :lines} next-ctx
        [row col] pos
        table-pos [(- row (count buffers)) col]]
    (cond-> next-ctx
      (not-empty buffers) (update-ast scan-table-block buffers table-pos)
      (has-no-end-blank-line? lines current-idx) (update-ast append-error-table-no-end-blank-line pos)
      :true clear-buffers)))

(def body->field-marker {:name :enum,
                         :state :body,
                         :parse (fn [ctx match])})

(def body->option-marker {:name :enum,
                          :state :body,
                          :parse (fn [ctx match])})

(defn bullet->blank [ctx _]
  (forward ctx))

(defn ^:private bullet->others [ctx match transition]
  (let [lines (:lines ctx)
        {current-idx :current-idx pos :pos} ctx]
    (-> ctx
        pop-state
        (update-ast z/up)
        (update-ast append-applicable-error-bullet-no-end-blank-line
                    pos
                    current-idx
                    lines)
        (transition match))))

(defn bullet->indent [ctx match]
  (bullet->others ctx match body->indent))

(defn bullet->text [ctx match]
  (bullet->others ctx match body->text))

(defn bullet->line [ctx match]
  (bullet->others ctx match body->line))

(defn bullet->bullet
  [ctx [_ style spacing text]]
  (let [bullet-list (-> ctx :ast z/node)
        indent (inc (count spacing))
        next-ctx (read-indented-lines
                  (-> ctx
                      (add-to-buffers text)
                      forward)
                  indent)
        indented-lines (:buffers next-ctx)
        {lines :lines idx :current-idx pos :pos} ctx]
    (if (and (= (:type bullet-list) :bullet-list)
             (= (:style bullet-list) style))
      (-> next-ctx
          (update-ast append-bullet-item indented-lines indent pos)
          clear-buffers)
      (-> next-ctx
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
  [ctx]
  (if (and (indented? ctx) (-> ctx :ast single-paragraph-document?))
    (update-ast ctx unwrap-root-paragraph)
    ctx))

(defn clean-up-buffers [ctx]
  (if (buffers? ctx)
    (transit ctx "")
    ctx))

;; Input: ctx string -> new-ctx
;; Input: ctx transition match (derived from string) -> new-ctx
;; Context Function

(defn process-lines [lines node pos]
  (let [init-ast (zip-node node)
        init-ctx (make-context lines init-ast :body pos)]
    (loop [ctx init-ctx]
      (if-not (eof? ctx)
        (recur (transit ctx))
        (-> ctx
            clean-up-buffers
            unwrap-single-indented-document
            :ast
            z/root)))))

(defn process-document [document-lines]
  (let [root-node (create-node {:type :root :children []})
        pos [0 0]]
    (process-lines document-lines root-node pos)))
