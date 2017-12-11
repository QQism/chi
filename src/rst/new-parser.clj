(ns rst.new-parser
  (:require  [clojure.java.io :as io]
             [clojure.string :as string]
             [clojure.pprint :refer [pprint]]
             [clojure.zip :as z]))

(defn debug-node [node]
  (let [{t :type id :iid s :style c :children} node]
    {:t t :id id :s s :c c}))

(def iid-counter (atom 0))

(defn get-iid []
  (swap! iid-counter inc))

(defn get-iid-path [doc]
  (-> doc
      z/path
      (->> (map :iid))
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
  {:blank #" *$"
   :line #"([\!-\/\:-\@\[-\`\{-\~])\1* *$"
   :indent #" +"
   :text #".+"})

(def transition-patterns
  {:line      {:pattern (:line patterns)}
   :underline {:pattern (:line patterns)}
   :text      {:pattern (:text patterns)}
   :blank     {:pattern (:blank patterns)}})

(defn match-transition [transition-name line]
  (-> (transition-name transition-patterns)
      :pattern
      (re-matches line)
      nil? not))

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

(defn append-section [node-loc section]
  ;; Append the section as the new child of the current node location
  ;; Move the location to the position of the new child
  (-> node-loc
      (z/append-child section)
      z/down
      z/rightmost))

(defn find-child-loc [doc-loc child-iid]
  (some #(if (= (:iid (z/node %)) child-iid) %)
        (children-loc doc-loc)))

(defn find-matching-section-loc [doc-loc style]
  ;; Travel in the whole doc from top to the current path
  ;; - if there is a section having the same style as the new section
  ;;   | return the location
  ;; - else
  ;;   | return nil
  (let [current-iid-path (get-iid-path doc-loc)]
    (loop [current-loc (up-to-root doc-loc)
           depth 1]
      (if (< depth (count current-iid-path))
        (let [current-iid (nth current-iid-path depth)]
          (if-let [section-loc (find-child-loc current-loc current-iid)]
            (let [section (z/node section-loc)]
              (if (= (:style section) style)
                current-loc
                (recur section-loc (inc depth))))))))))

(defn create-section-text->line [doc context lines]
  ;; create the new section
  ;; Find the matching parent section location
  ;; Append the new section as a child of the current path
  (let [{idx :current-idx remains :remains} context
        text (peek remains)
        line (nth lines idx)
        new-section (create-section text line "underline")
        style (:style new-section)
        current-iid-path (get-iid-path doc)]
    (if-let [matched-parent-section (find-matching-section-loc doc style)]
      (append-section matched-parent-section new-section)
      (append-section doc new-section))))

(def body->blank {:name :blank,
                  :state :body,
                  :parse (fn [doc context lines]
                           [doc (update context :current-idx inc)])})

(def body->line {:name :line,
                 :state :body,
                 :parse (fn [doc context line]
                          (let [next-state :line]
                            [doc context next-state]))})

(defn append-text [doc block-text]
  (let [paragraph (create-paragraph block-text)]
    (do
      (println "[Current Loc]")
      (pprint doc)
      (println (str "[Path IID] "(get-iid-path doc)))
      (println "\n")
      (println (str "[Paragraph] " block-text))
      (pprint paragraph)
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

(def line->blank {:name :blank,
                  :state :line})

(def line->text {:name :text,
                 :state :line})

(def text->blank {:name :blank,
                  :state :text,
                  :parse (fn [doc context lines]
                           (let [text-lines (:remains context)
                                 block-text (string/join " " text-lines)]
                             [(append-text doc block-text)
                              (-> context
                                  (assoc :remains [])
                                  (update :current-idx inc)
                                  (update :states pop))]
                             ))})

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
                                [(create-section-text->line doc context lines)
                                 (-> context
                                     (assoc :remains [])
                                     (update :current-idx inc)
                                     (update :states pop))])
                              [(create-section-text->line doc context lines)
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

(def states
  {:body      {:transitions [body->blank body->line body->text]}
   :line      {:transitions [line->blank line->text]}
   :text      {:transitions [text->blank text->line text->text]}})

(defn next-transition [state line]
  (some #(and (match-transition (:name %) line) %)
        (-> states state :transitions)))

(defn parse [doc context transition lines]
  (-> transition :parse (apply [doc context lines])))

(def lines (-> (io/resource "demo.rst")
               slurp
               (string/split #"\r|\n")
               (->> (map string/trim))))

(defn zip-doc [doc]
  (z/zipper ;;#(not= (:type %) :text)
            map?
            #(do
               ;;(println "\n\nParent Node")
               ;;(pprint %)
               ;;(println "\nListing children")
               ;;(pprint (:children %))
               (-> % :children seq))
            (fn [node children]
              ;;(assoc node :children (vec children))
              ;;(println "\n\nAppending a new child")
              ;;(println "Node")
              ;;(pprint node)
              ;;(println "\nNew Children")
              ;;(pprint children)
              (assoc node :children (vec children))
              ;;children
              )
            doc))

(let [lines lines
      lines-count (count lines)
      init-context {:states [:body], :current-idx 0, :remains []}
      init-doc (zip-doc {:type :root, :iid 1, :children []})]
  (loop [doc init-doc
         context init-context]
    (let [{:keys [states, current-idx]} context
          current-state (peek states)]
      (if (< current-idx lines-count)
        (let [line (nth lines current-idx)
              transition (next-transition current-state line)
              [new-doc new-context] (parse doc context transition lines)]
          (do
            ;;(println new-doc)
            ;;(println "PATH")
            (println line)
            ;;(println (get-iid-path new-doc))
            ;;(println new-context)
            ;;(println transition)
            (recur new-doc new-context)))
        (z/root doc)))))

;; (def test-section-1 (create-section "Test 1" "======" "underline"))
;; (def test-section-2 (create-section "Test 2" "======" "underline"))
;; (def test-section-3 (create-section "Test 3" "======" "underline"))
;; 
;; 
;; (-> (zip-doc {:type :root, :iid 1, :children []})
;;     (append-section test-section-1)
;;     (append-section test-section-2)
;;     (append-section test-section-3)
;;     ;;z/root
;;     (get-iid-path)
;;     )
