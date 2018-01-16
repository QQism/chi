(ns rst.core
  (:require  [clojure.java.io :as io]
             [clojure.string :as string]
             [clojure.pprint :refer [pprint]]
             [clojure.zip :as z]))

(defprotocol ReadingLines
  (current-line [_])
  (next-line [_])
  (forward [_])
  (eof? [_])
  (eof-on-next? [_])
  (add-state [_ state])
  (pop-state [_])
  (current-state [_])
  (clear-remains [_]))

(defrecord DocumentContext [lines current-idx states remains warning-level]
  ReadingLines
  (current-line [_] (nth lines current-idx))
  (next-line [_] (nth lines (inc current-idx)))
  (forward [_] (update _ :current-idx inc))
  (eof? [_] (>= current-idx (count lines)))
  (eof-on-next? [_] (>= (inc current-idx) (count lines)))
  (add-state [_ state] (update _ :states conj state))
  (pop-state [_] (update _ :states pop))
  (current-state [_] (-> _ :states peek))
  (clear-remains [_] (assoc _ :remains [])))

(defn make-context [lines]
  (DocumentContext. lines 0 [:body] [] 1))

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
   :indent  #" +"
   :bullet  #"([-+*\u2022\u2023\u2043])(\s+)(.*|$)"
   :text    #".+"})

(re-matches (:bullet patterns) "- this")

(def transition-patterns
  {:blank     {:pattern (:blank patterns)}
   ;;:indent    {:pattern (:indent patterns)}
   ;;:bullet    {:pattern (:bullet patterns)}
   :line      {:pattern (:line patterns)}
   :text      {:pattern (:text patterns)}})

(defn match-transition [transition-name line]
  (-> (transition-name transition-patterns)
      :pattern
      (re-matches line)
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
    :value text}])

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
  ([description block-text]
   {:type :error
    :level 1
    :iid (get-iid)
    :children [(create-paragraph description)
               (create-preserve block-text)]})
  ([description]
   {:type :error
    :level 1
    :iid (get-iid)
    :children [(create-paragraph description)]}))

(defn append-error [node-loc error]
  (-> node-loc (z/append-child error)))

(defn append-section [node-loc section]
  ;; Append the section as the new child of the current node location
  ;; Move the location to the position of the new child
  (-> node-loc
      (z/append-child section)
      z/down
      z/rightmost))

(defn append-transition [node-loc transition]
  (-> node-loc (z/append-child transition)))

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
        error (create-error (str "Title " style " too short." )
                            block-text)]
    (append-error doc error)))

(defn append-error-section-mismatching-underline [doc text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        error (create-error "Missing matching underline for section title overline."
                            block-text)]
    (append-error doc error)))

(defn append-error-section-mismatching-overline-underline [doc text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        error (create-error "Title overline & underline mismatch."
                            block-text)]
    (append-error doc error)))

(defn append-error-incomplete-section-title [doc text-lines]
  (let [block-text (string/join "\r\n" text-lines)
        error (create-error "Incomplete section title."
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

(defn append-section-line->text-line [doc context lines]
  (let [{idx :current-idx text-lines :remains} context
        [overline text] text-lines
        underline-line (nth lines idx)
        new-text-lines (conj text-lines text)
        section-style "overline"
        new-section (create-section text overline section-style)]
    (-> doc (append-section-in-matched-location new-section)
        (append-applicable-error-section-title-too-short section-style
                                                         new-text-lines))))

(defn append-section-text->line [doc context lines]
  (let [{idx :current-idx text-lines :remains} context
        text (peek text-lines)
        underline (nth lines idx)
        new-text-lines (conj text-lines underline)
        section-style "underline"
        new-section (create-section text underline section-style)]
    (-> doc
        (append-section-in-matched-location new-section)
        (append-applicable-error-section-title-too-short section-style
                                                         new-text-lines))))

(defn append-error-doc-end-with-transition [doc]
  (let [error (create-error "Document may not end with a transition.")]
    (append-error doc error)))

(defn append-transition-line->blank [doc]
  (let [transition (create-transition)]
    (append-transition doc transition)))

(def body->blank {:name :blank,
                  :state :body,
                  :parse (fn [doc context lines]
                           [doc (update context :current-idx inc)])})

(def body->line {:name :line,
                 :state :body,
                 :parse (fn [doc context lines]
                          (let [next-state :line
                                idx (:current-idx context)
                                line (nth lines idx)]
                            [doc (-> context
                                     (update :current-idx inc)
                                     (update :remains conj line)
                                     (update :states conj next-state))]))})

(defn append-text [doc block-text]
  (let [paragraph (create-paragraph block-text)]
    (do
      ;;(println "[Current Loc]")
      ;;(pprint doc)
      ;;(println (str "[Path IID] "(get-iid-path doc)))
      ;;(println "\n")
      ;;(println (str "[Paragraph] " block-text))
      ;;(pprint paragraph)
      (-> doc
          ;;z/up
          (z/append-child paragraph)))))

(def body->text
  {:name :text,
   :state :body,
   :parse (fn [doc context lines]
            (let [{current-idx :current-idx remains :remains} context
                  lines-count (count lines)]
              (if (< current-idx lines-count)
                (let [line (nth lines current-idx)
                      text-lines (conj remains line)]
                  (if (< (inc current-idx) lines-count)
                    [doc
                     (-> context
                         (update :remains conj line)
                         (update :current-idx inc)
                         (update :states conj :text))]
                    ;; next line if EOF
                    [(append-text doc (string/join " " text-lines))
                     (-> context
                         (assoc :remains [])
                         (update :current-idx inc))])))))})

(def line->blank {:name :blank
                  :state :line
                  :parse (fn [doc context lines]
                           ;; Check if it is the last line of the document
                           ;; - Y: Raise error
                           ;; - N: Create a transition
                           (loop [current-context context]
                             (let [{idx :current-idx} current-context]
                               (if (< idx (count lines))
                                 (let [line (nth lines idx)]
                                   (if (match-transition :blank line)
                                     (recur (-> current-context
                                                (update :current-idx inc)))
                                     [(append-transition-line->blank doc)
                                      (-> current-context
                                          (assoc :remains [])
                                          (update :states pop))]))
                                 [(-> doc
                                      (append-transition-line->blank)
                                      (append-error-doc-end-with-transition))
                                  (-> current-context
                                      (assoc :remains [])
                                      (update :states pop))]))))})

(def line->text {:name :text,
                 :state :line
                 :parse (fn [doc context lines]
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
                          (let [{idx :current-idx, text-lines :remains} context
                                current-text-line (nth lines idx)
                                current-text-lines (conj text-lines current-text-line)
                                next-idx (inc idx)
                                prev-text-line (peek text-lines)
                                prev-short-line? (< (count prev-text-line) 4)]
                            (if (< next-idx (count lines))
                              (let [next-text-line (nth lines next-idx)
                                    next-text-lines (conj current-text-lines next-text-line)
                                    next-context (-> context
                                                     (update :current-idx inc)
                                                     (assoc :remains current-text-lines))]
                                (if (match-transition :line next-text-line)
                                  (if (= prev-text-line next-text-line)
                                    [(append-section-line->text-line doc next-context lines)
                                     (-> next-context
                                         (assoc :remains [])
                                         (update :current-idx inc)
                                         (update :states pop))]
                                    (if prev-short-line?
                                      [doc
                                       (-> next-context
                                           (assoc :remains next-text-lines)
                                           (update :current-idx inc)
                                           (update :states pop)
                                           (update :states conj :text))]
                                      [(append-error-section-mismatching-overline-underline doc next-text-lines)
                                       (-> next-context
                                           (assoc :remains [])
                                           (update :current-idx inc)
                                           (update :states pop))]
                                      ))
                                  (if prev-short-line?
                                    [doc
                                     (-> next-context
                                         (assoc :remains next-text-lines)
                                         (update :current-idx inc)
                                         (update :states pop)
                                         (update :states conj :text))]
                                    [(append-error-section-mismatching-underline doc next-text-lines)
                                     (-> next-context
                                         (assoc :remains [])
                                         (update :current-idx inc)
                                         (update :states pop))])))
                              (if prev-short-line?
                                [doc
                                 (-> context
                                     (update :remains conj current-text-line)
                                     (assoc :current-idx inc)
                                     (update :states pop)
                                     (update :states conj :text))]
                                [(append-error-incomplete-section-title doc current-text-lines)
                                 (-> context
                                     (update :remains [])
                                     (update :current-idx inc)
                                     (update :status pop))]))))})

(def line->line {:name :line,
                 :state :line
                 :parse (fn [doc context lines]
                          ;; Append the error message
                          )})

(def text->blank {:name :blank,
                  :state :text,
                  :parse (fn [doc context lines]
                           (let [text-lines (:remains context)
                                 block-text (string/join " " text-lines)]
                             [(append-text doc block-text)
                              (-> context
                                  (assoc :remains [])
                                  (update :current-idx inc)
                                  (update :states pop))]))})

(def text->text {:name :text,
                 :state :text,
                 :parse (fn [doc context lines]
                          ;; Read the whole block of text until the blank line
                          ;; keep track the index
                          (loop [current-context context]
                            (let [{idx :current-idx text-lines :remains} current-context
                                  block-text (string/join " " text-lines)]
                              (if (< idx (count lines))
                                (let [line (nth lines idx)]
                                  (if (not (match-transition :blank line))
                                    (recur (-> current-context
                                               (update :current-idx inc)
                                               (update :remains conj line)))
                                    ;; blank line, create paragraph & return
                                    [(append-text doc block-text)
                                     (-> current-context
                                         (assoc :remains [])
                                         (update :states pop))]))
                                ;; EOF, create paragraph & return
                                [(append-text doc block-text)
                                 (-> current-context
                                     (assoc :remains [])
                                     (update :current-idx inc)
                                     (update :states pop))]))))})

(def text->line {:name :line,
                 :state :text,
                 :parse (fn [doc context lines]
                          (let [{:keys [remains, current-idx]} context
                                text (first remains)
                                line (nth lines current-idx)
                                line-count (count line)
                                ]
                            ;; There are two possible cases:
                            ;; - line is shorter than the text (in remains)
                            ;;   - if line is shorter than 4 characters
                            ;;     | switch to transition text->text
                            ;;   - else
                            ;;     | create the section + title + warning
                            ;; - line is equal or longer than the text (in remains)
                            ;;   | create section + title
                            (if (< line-count (count text))
                              (if (< line-count 4)
                                (-> text->text :parse (apply [doc context lines]))
                                [(append-section-text->line doc context lines)
                                 (-> context
                                     (assoc :remains [])
                                     (update :current-idx inc)
                                     (update :states pop))])
                              [(append-section-text->line doc context lines)
                               (-> context
                                   (assoc :remains [])
                                   (update :current-idx inc)
                                   (update :states pop))])))})


(def doc-sample
  {:type :root
   :iid 0
   :children [{:type :section
               :iid 1
               :style "underline="
               :children [{:type :header
                           :iid 2
                           :children [{:type :text
                                       :iid 3
                                       :value "The 1st Header"}]}
                          {:type :section
                           :iid 4
                           :style "underline+"
                           :children [{:type :header
                                       :iid 5
                                       :children [{:type :text
                                                   :iid 6
                                                   :value "Sub 1st Header"}]}]}
                          {:type :section
                           :iid 7
                           :style "underline+"
                           :children [{:type :header
                                       :iid 8
                                       :children [{:type :text
                                                   :iid 9
                                                   :value "Sub 2nd Header"}]}
                                      ;;{:type :section
                                      ;; :iid 10
                                      ;; :style "underline/"
                                      ;; :children [{:type :header
                                      ;;             :iid 11
                                      ;;             :children [{:type :text
                                      ;;                         :iid 12
                                      ;;                         :value "Sub Sub Header"}]}]}
                                      ]}]}]})



;; Each state has a list of possible transitions
;; depend on what type of transition on each state, the parser may decide to do the following tasks
;; - Parse the line
;; - Decide the next state
;; - Update the document tree
;;
;;
;;
;;

(def body->indent {:name :indent,
                   :state :body,
                   :parse (fn [doc context lines])})

(def body->bullet {:name :bullet,
                   :state :body,
                   :parse (fn [doc context lines]
                            )})

(def body->enum {:name :enum,
                 :state :body,
                 :parse (fn [doc context lines])})

(def body->field-marker {:name :enum,
                         :state :body,
                         :parse (fn [doc context lines])})

(def body->option-marker {:name :enum,
                         :state :body,
                         :parse (fn [doc context lines])})

(def states
  {:body           {:transitions [body->blank
                                  ;;body->indent
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
   :text           {:transitions [text->blank text->line text->text]}
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
  (some #(and (match-transition (:name %) line) %)
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

(defn parse [doc context transition]
  (-> transition :parse (apply [doc context (:lines context)])))

(def content-lines (-> (io/resource "simple-sections.rst")
               slurp
               (string/split #"\r|\n")
               ;; Remove right whitespaces
               (->> (map string/trimr))
               ;; Become a vector, giving the random access with 0(1) complexity
               vec))

(re-matches #" {1}(.*)" " - Hello")

(defn read-indented-lines [lines context indent]
  (let [{current-idx :current-idx remains :remains} context
        indented-pattern (re-pattern (str " {" indent "}(.*)"))]
    (loop [indented-lines remains
           idx current-idx]
      (if (< idx (count lines))
        (let [line (nth lines idx)]
          (if-let [match (re-matches indented-pattern line)]
            (recur (conj indented-lines (nth match 1)) (inc idx))
            (if (match-transition :blank line)
              (recur indented-lines (inc idx))
              [indented-lines
               (-> context
                   (assoc :remains [])
                   (assoc :current-idx idx))])))
        [indented-lines
         (-> context
             (assoc :remains [])
             (assoc :current-idx idx))]))))

(read-indented-lines content-lines {:current-idx 2 :remains ["First Item"]} 2)

(defn zip-doc [doc]
  (z/zipper ;;#(not= (:type %) :text)
            map?
            #(-> % :children seq)
            (fn [node children]
              (assoc node :children (vec children)))
            doc))

(defn process-document [document-lines]
  (let [lines (conj document-lines "") ;; The last blank line, avoid the parser being stuck in the middle of a specific state without exiting
        init-context (make-context lines)
        init-doc (zip-doc {:type :root, :iid 1, :children []})]
   (loop [doc init-doc
          context init-context]
     (if (not (eof? context))
       (let [line (current-line context)
             current-state (current-state context)
             transition (next-transition current-state line)
             [new-doc new-context] (parse doc context transition)]
         (do
           ;;(println new-doc)
           ;;(println "PATH")
           ;;(println line)
           ;;(println (get-iid-path new-doc))
           ;;(println new-context)
           ;;(println transition)
           (recur new-doc new-context)))
       (z/root doc)))))

(process-document content-lines)
