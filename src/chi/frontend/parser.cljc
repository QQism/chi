(ns chi.frontend.parser
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [chi.frontend.context :as c]
            [chi.frontend.node :as n]
            [chi.frontend.tree :as t]
            [chi.frontend.error :as err]
            [chi.frontend.patterns :as patt]
            [chi.frontend.el.preserve :as preserve]
            [chi.frontend.el.paragraph :as paragraph]
            [chi.frontend.el.section :as section]
            [chi.frontend.el.transition :as transition]
            [chi.frontend.el.table :as table]
            [chi.frontend.el.verse :as verse]
            [chi.frontend.el.blockquote :as blockquote])
  #?(:clj (:import [chi.frontend.context DocumentContext])))

;; https://dev.clojure.org/jira/browse/CLJS-1871
(defn ^:declared parse [lines node pos])
(defn ^:declared next-transition [state line])

(defprotocol IParserTransit
  (transit [_] [_ line])
  (clean-up-buffers [_]))

#?(:cljs (def DocumentContext c/DocumentContext))

(extend-type DocumentContext
  IParserTransit
  (transit
    ([this] (transit this (c/current-line this)))
    ([this line]
     (let [state (c/current-state this)
           [transition match] (next-transition state line)]
       (transition this match))))
  (clean-up-buffers [this]
    (if (c/buffers? this)
      (transit this "")
      this)))

(defn update-zt [ctx f & args]
  (update ctx :zt #(apply f % args)))

(defn create-blockquote [indent]
  (n/create {:type :blockquote
             :indent indent
             :children []}))

(defn create-bullet-list [style]
  (n/create {:type :bullet-list
             :style style
             :children []}))

(defn create-bullet-item [indent]
  (n/create {:type :bullet-item
             :indent indent
             :children []}))

(defn append-error [zt error]
  (t/append-child zt error))

(defn append-section [zt section]
  ;; Append the section as the new child of the current node location
  ;; Move the location to the position of the new child
  (-> zt
      (t/append-child section)
      t/move-to-latest-child))

(defn append-transition [zt transition]
  (t/append-child zt transition))

(defn find-matching-section-loc [zt style]
  ;; Travel in the whole doc from top to the current path
  ;; - if there is a section having the same style as the new section
  ;;   | return the location
  ;; - else
  ;;   | return nil
  (let [current-uid-path (t/uid-path zt)]
    (loop [current-loc (t/up-to-root zt)
           depth 0]
      (if (< depth (count current-uid-path))
        (let [current-uid (nth current-uid-path depth)]
          (if-let [section-loc (t/find-loc-with-uid current-loc current-uid)]
            (let [section (t/node section-loc)]
              (if (= (:style section) style)
                current-loc
                (recur section-loc (inc depth))))))))))

(defn append-section-in-matched-location [zt section]
  ;; Find the matching parent section location
  ;; Append the new section as a child of the current path
  (let [style (:style section)]
    (if-let [matched-parent-section (find-matching-section-loc zt style)]
      (append-section matched-parent-section section)
      (append-section zt section))))

(defn append-applicable-error-section-title-too-short [zt pos style text-lines]
  (if (section/is-title-short? style text-lines)
    (append-error zt (section/error-section-title-too-short pos style text-lines))
    zt))

(defn append-section-line->text-line [zt pos text-lines]
  (let [[overline text _] text-lines
        section-style "overline"
        new-section (section/create text overline section-style)]
    (-> zt (append-section-in-matched-location new-section)
        (append-applicable-error-section-title-too-short pos
                                                         section-style
                                                         text-lines))))

(defn append-section-text->line [zt pos text-lines]
  (let [[text underline] text-lines
        section-style "underline"
        new-section (section/create text underline section-style)]
    (-> zt
        (append-section-in-matched-location new-section)
        (append-applicable-error-section-title-too-short pos
                                                         section-style
                                                         text-lines))))

(defn document-begin? [zt]
  (let [siblings (t/children zt)
        siblings-count (count siblings)]
    (or (= siblings-count 0)
        (and (= siblings-count 1)
             (-> siblings first :type (= :header))))))

(defn last-node-transition? [zt]
  (-> zt t/move-to-latest-child t/node :type (= :transition)))

(defn append-applicable-error-doc-start-with-transition [zt pos]
  (if (document-begin? zt)
    (let [msg "Document or section may not begin with a transition."
          error (err/create msg ::err/error pos)]
      (append-error zt error))
    zt))

(defn append-applicable-error-adjacent-transitions [zt pos]
  (if (last-node-transition? zt)
    (let [msg "At least one body element must separate transitions; adjacent transitions are not allowed."
          error (err/create msg ::err/error pos)]
      (append-error zt error))
    zt))

(defn append-error-doc-end-with-transition [zt pos]
  (let [msg "Document may not end with a transition."
        error (err/create msg ::err/error pos)]
    (append-error zt error)))

(defn append-transition-line->blank [zt pos]
  (let [transition (transition/create)]
    (-> zt
        (append-applicable-error-doc-start-with-transition pos)
        (append-applicable-error-adjacent-transitions pos)
        (append-transition transition))))

(defn append-applicable-error-blockquote-no-end-blank-line [zt pos lines eof?]
  (if-not (or (patt/match? :blank (peek lines)) eof?)
    (let [msg "Block quote ends without a blank line; unexpected unindent."
          error (err/create msg ::err/warning pos)]
      (append-error zt error))
    zt))

(defn append-applicable-error-bullet-no-end-blank-line [zt pos idx lines]
  (if (> idx 0)
    (let [prev-line (nth lines (dec idx))]
      (if (patt/match? :blank prev-line)
        zt
        (let [msg "Bullet list ends without a blank line; unexpected unindent."
              error (err/create msg ::err/warning pos)]
          (append-error zt error))))
    zt))

(defn has-no-end-blank-line? [lines idx]
  (let [current-line (nth lines idx :eof)]
    (not (or (= current-line :eof) (patt/match? :blank current-line)))))

(defn append-error-table-no-end-blank-line [zt pos]
  (let [msg "Blank line required after table."
        [row col] pos
        prev-pos [(dec row) col]
        error (err/create msg ::err/warning prev-pos)]
    (append-error zt error)))

(defn append-error-unexpected-indentation [zt pos]
  (let [msg "Unexpected indentation."
        error (err/create msg ::err/error pos)]
    (append-error zt error)))

(defn append-blockquote-body->indent [zt lines indent pos]
  (let [[row col] pos
        raw-indent (+ indent col)
        blockquote (create-blockquote raw-indent)
        new-pos [row raw-indent]
        processed-blockquote (parse lines blockquote new-pos)]
    (-> zt
        (t/append-child processed-blockquote))))

(defn append-bullet-list [zt style]
  (let [bullet-list (create-bullet-list style)]
    (-> zt
        (t/append-child bullet-list)
        t/move-to-latest-child)))

(defn append-bullet-item [zt lines indent pos]
  (let [[row col] pos
        raw-indent (+ col indent)
        bullet-item (create-bullet-item raw-indent)
        new-pos [row raw-indent]
        processed-bullet-item (parse lines bullet-item new-pos)]
    (-> zt (t/append-child processed-bullet-item))))

(defn append-preserve-text [zt lines]
  (let [block-text (string/join "\r\n" lines)
        preserve-text (preserve/create block-text)]
    (-> zt (t/append-child preserve-text))))

(defn append-error-malformed-table [zt pos lines]
  (let [error (table/error-malformed lines pos)]
    (append-error zt (error))))

(defn body->blank [ctx _]
  (-> ctx c/forward))

(defn body->line [ctx _]
  (let [line (c/current-line ctx)
        pos (:pos ctx)
        short-line? (< (count line) 4)]
    (if (c/indented? ctx)
      (if short-line?
        (-> ctx
            c/forward
            (c/add-to-buffers line)
            (c/push-state :text))
        (-> ctx
            (update-zt append-error
                       (section/error-unexpected-section-title-or-transition pos
                                                                             [line]))
            c/forward))
      (-> ctx
          c/forward
          (c/add-to-buffers line)
          (c/push-state :line)))))

(defn append-paragraph [zt block-text]
  (let [paragraph (paragraph/create block-text)]
    (t/append-child zt paragraph)))

(defn anppend-text [zt text]
  (let [text (verse/create-inline-markup text)]
    (t/append-child zt text)))

(defn body->text
  [ctx _]
  (if-not (c/eof? ctx)
    (let [line (c/current-line ctx)]
      (if-not (c/eof-on-next? ctx)
        (-> ctx
            c/forward
            (c/add-to-buffers line)
            (c/push-state :text))
        ;; next line if EOF
        (let [text-lines (-> ctx :buffers (conj line))]
          (-> ctx
              (update-zt append-paragraph (string/join " " text-lines))
              c/forward
              c/clear-buffers))))))

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
          (update-zt append-paragraph prev-text-line)
          c/forward
          c/clear-buffers
          c/pop-state)
      (loop [current-ctx ctx]
        (if-not (c/eof? current-ctx)
          (let [line (c/current-line current-ctx)]
            (if (patt/match? :blank line)
              (recur (-> current-ctx c/forward))
              (-> current-ctx
                  (update-zt append-transition-line->blank prev-pos)
                  c/clear-buffers
                  c/pop-state)))
          (-> current-ctx
              (update-zt append-transition-line->blank prev-pos)
              (update-zt append-error-doc-end-with-transition prev-pos)
              c/clear-buffers
              c/pop-state))))))

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
        current-text-line (c/current-line ctx)
        current-text-lines (conj text-lines current-text-line)
        prev-text-line (peek text-lines)
        prev-short-line? (< (count prev-text-line) 4)]
    (if-not (c/eof-on-next? ctx)
      (let [next-text-line (c/next-line ctx)
            next-text-lines (conj current-text-lines next-text-line)]
        (if (patt/match? :line next-text-line)
          (if (and (= prev-text-line next-text-line)
                   (or (not prev-short-line?)
                       (<= (count current-text-line) (count prev-text-line))))
            (-> ctx
                (update-zt append-section-line->text-line prev-pos next-text-lines)
                c/forward
                c/forward
                c/clear-buffers
                c/pop-state)
            (if prev-short-line?
              (-> ctx
                  c/forward
                  c/forward
                  (c/update-buffers next-text-lines)
                  c/pop-state
                  (c/push-state :text))
              (-> ctx
                  (update-zt append-error
                             (section/error-section-mismatching-overline-underline prev-pos
                                                                                   next-text-lines))
                  c/forward
                  c/forward
                  c/clear-buffers
                  c/pop-state)))
          (if prev-short-line?
            (-> ctx
                c/forward
                c/forward
                (c/update-buffers next-text-lines)
                c/pop-state
                (c/push-state :text))
            (-> ctx
                (update-zt append-error
                           (section/error-section-mismatching-underline prev-pos
                                                                        next-text-lines))
                c/forward
                c/forward
                c/clear-buffers
                c/pop-state))))
      (if prev-short-line?
        (-> ctx
            c/forward
            (c/add-to-buffers current-text-line)
            c/pop-state
            (c/push-state :text))
        (-> ctx
            (update-zt append-error
                       (section/error-incomplete-section-title prev-pos
                                                               current-text-lines))
            c/forward
            c/clear-buffers
            c/pop-state)))))

(defn line->line [ctx _]
  ;; Append the error message
  )

(defn text->blank [ctx _]
  (let [text-lines (:buffers ctx)
        block-text (string/join " " text-lines)]
    (-> ctx
        (update-zt append-paragraph block-text)
        c/forward
        c/clear-buffers
        c/pop-state)))

(defn text->text [ctx _]
  ;; Read the whole block of text until the blank line
  ;; keep track the index
  (loop [current-ctx ctx]
    (if-not (c/eof? current-ctx)
      (let [line (c/current-line current-ctx)]
        (cond (patt/match? :blank line)
              ;; blank line, create paragraph & return
              (let [block-text (string/join " " (:buffers current-ctx))]
                (-> current-ctx
                    (update-zt append-paragraph block-text)
                    c/clear-buffers
                    c/pop-state))
              (patt/match? :indent line)
              (let [block-text (string/join " " (:buffers current-ctx))
                    pos (:pos current-ctx)]
                (-> current-ctx
                    (update-zt append-paragraph block-text)
                    (update-zt append-error-unexpected-indentation pos)
                    c/clear-buffers
                    c/pop-state))
              :else
              (recur (-> current-ctx
                         c/forward
                         (c/add-to-buffers line)))))
      ;; EOF, create paragraph & return
      (let [block-text (string/join " " (:buffers current-ctx))]
        (-> current-ctx
            (update-zt append-paragraph block-text)
            c/forward
            c/clear-buffers
            c/pop-state)))))

(defn text->line [ctx match]
  (let [{pos :pos buffers :buffers} ctx
        prev-text-line (peek buffers)
        line (c/current-line ctx)
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
      (if (c/indented? ctx)
        (-> ctx
            (update-zt append-error
                       (section/error-unexpected-section-title pos
                                                               text-lines))
            c/forward
            c/clear-buffers
            c/pop-state)
        (-> ctx
            (update-zt append-section-text->line pos text-lines)
            c/forward
            c/clear-buffers
            c/pop-state)))))

(defn read-indented-lines [ctx indent]
  (let [{text-lines :buffers} ctx
        indented-pattern (re-pattern (str " {" indent "}(.*)"))]
    (loop [indented-lines text-lines
           current-ctx ctx]
      (if-not (c/eof? current-ctx)
        (let [line (c/current-line current-ctx)]
          (if-let [match (re-matches indented-pattern line)]
            (recur (conj indented-lines (nth match 1)) (c/forward current-ctx))
            (if (patt/match? :blank line)
              (recur (conj indented-lines "") (c/forward current-ctx))
              (-> current-ctx
                  (c/update-buffers indented-lines)))))
        (-> current-ctx
            (c/update-buffers indented-lines))))))

(defn body->indent [ctx [line indent-str text]]
  (let [current-indent (count indent-str)
        pos (:pos ctx)
        next-ctx (read-indented-lines ctx current-indent)
        eof? (c/eof? next-ctx)
        {next-pos :pos lines :buffers} next-ctx]
    (-> next-ctx
        (update-zt append-blockquote-body->indent
                   lines
                   current-indent
                   pos)
        (update-zt append-applicable-error-blockquote-no-end-blank-line
                   next-pos
                   lines
                   eof?)
        c/clear-buffers)))

(defn body->bullet
  [ctx [_ style spacing text]]
  (let [indent (inc (count spacing))
        pos (:pos ctx)
        next-ctx (read-indented-lines
                  (-> ctx
                      (c/add-to-buffers text)
                      c/forward)
                  indent)
        lines (:buffers next-ctx)]
    (-> next-ctx
        (update-zt append-bullet-list style)
        (update-zt append-bullet-item lines indent pos)
        c/clear-buffers
        (c/push-state :bullet))))

(defn body->enum [ctx match])

(defn ^:private extract-text-block
  "Extract the text block from the current line
   to the nearest blank line"
  [ctx]
  (if-not (c/eof? ctx)
    (let [line (c/current-line ctx)]
      (if-not (patt/match? :blank line)
        (recur (-> ctx
                   (c/add-to-buffers line)
                   c/forward))
        ctx))
    ctx))

(defn ^:private backward-buffers [ctx idx]
  (let [lines (:buffers ctx)
        end-idx (-> lines count dec)
        offset-idx (- end-idx idx)
        sub-lines (->> lines (split-at (inc idx)) first vec)]
    (-> ctx
        (c/backward offset-idx)
        (c/update-buffers sub-lines))))

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
            (if (patt/match? :grid-table-top line)
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
            (if (patt/match? :grid-table-left-side line)
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
                     (patt/match? :grid-table-right-side line))
              (recur (inc idx))
              (let [[row col] (:pos ctx)
                    last-pos [(- row c )col]]
                (-> ctx
                    (update-zt append-error-malformed-table last-pos lines)
                    c/clear-buffers))))
          ctx))
      ctx)))

(defn ^:private extract-table-block [ctx]
  (-> ctx
      extract-text-block
      mark-table-left-edge
      mark-table-bottom
      validate-table-right-edge))

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
      (not-empty buffers) (update-zt table/scan-block buffers table-pos parse)
      (has-no-end-blank-line? lines current-idx) (update-zt append-error-table-no-end-blank-line pos)
      :true c/clear-buffers)))

(defn body->field-marker [ctx match])

(defn body->option-marker [ctx match])

(defn bullet->blank [ctx _]
  (c/forward ctx))

(defn ^:private bullet->others [ctx match transition]
  (let [lines (:lines ctx)
        {current-idx :current-idx pos :pos} ctx]
    (-> ctx
        c/pop-state
        (update-zt t/up)
        (update-zt append-applicable-error-bullet-no-end-blank-line
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
  (let [bullet-list (-> ctx :zt t/node)
        indent (inc (count spacing))
        next-ctx (read-indented-lines
                  (-> ctx
                      (c/add-to-buffers text)
                      c/forward)
                  indent)
        indented-lines (:buffers next-ctx)
        {lines :lines idx :current-idx pos :pos} ctx]
    (if (and (= (:type bullet-list) :bullet-list)
             (= (:style bullet-list) style))
      (-> next-ctx
          (update-zt append-bullet-item indented-lines indent pos)
          c/clear-buffers)
      (-> next-ctx
          (update-zt t/up)
          (update-zt append-applicable-error-bullet-no-end-blank-line
                     pos
                     idx
                     lines)
          (update-zt append-bullet-list style)
          (update-zt append-bullet-item indented-lines indent pos)
          c/clear-buffers))))

(def states
  {:body           {:transitions [[:blank body->blank]
                                  [:indent body->indent]
                                  [:bullet body->bullet]
                                  ;;body->enum
                                  ;;body->field-marker
                                  ;;body->option-marker
                                  ;;body->doctest
                                  ;;body->line-block
                                  [:grid-table-top body->grid-table-top]
                                  ;;body->simple-table-top
                                  ;;body->explicit-markup
                                  ;;body->anonymous
                                  [:line body->line]
                                  [:text body->text]]}
   :bullet         {:transitions [[:blank bullet->blank]
                                  [:indent bullet->indent]
                                  [:bullet bullet->bullet]
                                  ;;bullet->enum
                                  ;;bullet->field-marker
                                  ;;bullet->option-marker
                                  ;;bullet->doctest
                                  ;;bullet->line-block
                                  ;;bullet->grid-table-top
                                  ;;bullet->simple-table-top
                                  ;;bullet->explicit-markup
                                  ;;bullet->anonymous
                                  [:line bullet->line]
                                  [:text bullet->text]]}
   :line           {:transitions [[:blank line->blank]
                                  [:text line->text]
                                  [:line line->line]]}
   :text           {:transitions [[:blank text->blank]
                                  ;;text->indent
                                  [:line text->line]
                                  [:text text->text]]}})

;; Transition function: patt/match

(defn next-transition [state line]
  (some #(if-let [match (patt/match (first %) line)]
           [(peek %) match])
        (-> states state :transitions)))

(defn single-paragraph-document? [zt]
  (let [children (-> zt t/up-to-root t/children)]
    (and (= (count children) 1)
         (-> children first :type (= :paragraph)))))

(defn unwrap-root-paragraph [zt]
  (let [paragraph (-> zt t/up-to-root t/down)
        children (t/children paragraph)]
    (reduce (fn [t child]
              (t/append-child t child))
            (-> paragraph t/remove) children)))

(defn unwrap-single-indented-document
  "If the indented document contains a single paragraph
  unwrap the paragraph content"
  [ctx]
  (if (and (c/indented? ctx) (-> ctx :zt single-paragraph-document?))
    (update-zt ctx unwrap-root-paragraph)
    ctx))

;; Input: ctx string -> new-ctx
;; Input: ctx transition match (derived from string) -> new-ctx
;; Context Function

(defn parse [lines node pos]
  (let [init-zt (t/node->zipper-tree node)
        init-ctx (c/create lines init-zt :body pos)]
    (loop [ctx init-ctx]
      (if-not (c/eof? ctx)
        (recur (transit ctx))
        (-> ctx
            clean-up-buffers
            unwrap-single-indented-document
            :zt
            t/root)))))

(defn lines->ast [document-lines]
  (let [root-node (n/create {:type :root :children []})
        pos [0 0]]
    (parse document-lines root-node pos)))
