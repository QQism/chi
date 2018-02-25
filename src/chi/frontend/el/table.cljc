(ns chi.frontend.el.table
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [chi.frontend.node :as n]
            [chi.frontend.tree :as t]
            [chi.frontend.el.verse :as verse]
            [chi.frontend.patterns :as patt]
            [chi.frontend.error :as err]))

(defn error-malformed
  ([lines pos description]
   (let [block-text (string/join "\r\n" lines)
         msg (string/join " " ["Malformed table." description])]
     (err/create (string/trimr msg) ::err/error pos block-text)))
  ([lines pos]
   (error-malformed lines pos "")))

(defn ^:private create-cell [[top left bottom right] content [source-row source-col] f]
  (let [cell (n/create {:type :cell
                        :top top
                        :left left
                        :width (- right left 1)
                        :height (- bottom top 1)
                        :children []})
        trivial-indents (verse/trivial-block-indents content)
        stripped-content (verse/strip-indents content trivial-indents)
        pos [(+ top source-row 1) (+ left source-col 1)]]
    (f stripped-content cell pos)))

(defn ^:private cell-content
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

(defn ^:private create-row [id]
  (n/create {:type :row :id id :children []}))

(defn ^:private create-header []
  (n/create {:type :table-header :children []}))

(defn ^:private create-body []
  (n/create {:type :table-body :children [(create-row 0)]}))

(defn ^:private create-grid-table [width height pos]
  (n/create {:type :table
             :style :grid
             :pos pos
             :width width
             :height height
             :col-ids (sorted-set 0)
             :header-idx nil
             :children [(create-body)]}))

(defn ^:private move-to-body [zt]
  (if (-> zt t/node :type (= :table))
    (loop [table-child (-> zt t/down)]
      (cond (-> table-child t/node :type (= :table-body)) table-child
            (nil? table-child) nil
            :else (recur (t/right table-child))))
    nil))

(defn ^:private move-to-header [zt]
  (if (-> zt t/node :type (= :table))
    (loop [table-child (-> zt t/down)]
      (cond (-> table-child t/node :type (= :table-header)) table-child
            (nil? table-child) nil
            :else (recur (t/right table-child))))
    nil))

(defn ^:private move-to-body-row [zt id]
  (let [table-body-zt (move-to-body zt)]
    (loop [row-zt (t/down table-body-zt)]
      (if-not (nil? row-zt)
        (if (-> row-zt t/node :id (= id))
          row-zt
          (recur (t/right row-zt)))
        nil))))

(defn ^:private append-body-row [zt row-id]
  (let [table (t/node zt)
        height (:height table)]
    (if (< row-id (dec height))
      (let [row (create-row row-id)
            existing-row (move-to-body-row zt row-id)]
        (if (nil? existing-row)
          (let [table-body (move-to-body zt)]
            (-> table-body (t/append-child row)
                t/up))
          zt))
      zt)))

(defn ^:private append-col-id [zt col-id]
  (t/edit zt
          (fn [table]
            (let [col-ids (:col-ids table)
                  width (:width table)]
              (if-not (or (contains? col-ids col-id) (>= col-id (dec width)))
                (update table :col-ids #(conj %1 col-id))
                table)))))

(defn ^:private append-body-cell [zt cell]
  (let [row-id (:top cell)
        row-zt (move-to-body-row zt row-id)]
    (-> row-zt (t/append-child cell)
        t/up t/up)))

(defn ^:private scan-up [zt lines top left bottom right]
  (loop [table-zt zt
         row-id (dec bottom)]
    (if-not (< row-id top)
      (let [c (verse/nth-2d lines row-id left)]
        (cond (> row-id top) (case c
                               \+ (recur (append-body-row zt row-id) (dec row-id))
                               \| (recur table-zt (dec row-id))
                               [table-zt nil])
              (= row-id top) (if (= c \+)
                               [table-zt [top left bottom right]]
                               [table-zt nil])
              :else [table-zt nil]))
      [table-zt nil])))

(defn ^:private scan-left [zt lines top left bottom right]
  (loop [table-zt zt
         col-id (dec right)]
    (if-not (< col-id left)
      (let [c (verse/nth-2d lines bottom col-id)]
        (cond (> col-id left) (case c
                                \+ (recur (append-col-id zt col-id) (dec col-id))
                                \- (recur table-zt (dec col-id))
                                [table-zt nil])
              (= col-id left) (if (= c \+)
                                (scan-up table-zt lines top left bottom right)
                                [table-zt nil])
              :else [table-zt nil]))
      [table-zt nil])))

(defn ^:private scan-down [zt lines top left right]
  (let [table (t/node zt)
        height (:height table)]
    (loop [table-zt zt
           row-id (inc top)]
      (if (< row-id height)
        (let [c (verse/nth-2d lines row-id right)]
          (case c
            \+ (let [[next-table-zt cell-pos] (-> zt (append-body-row row-id)
                                                  (scan-left lines top left row-id right))]
                 (if-not cell-pos
                   (recur next-table-zt (inc row-id))
                   [next-table-zt cell-pos]))
            \| (recur table-zt (inc row-id))
            [zt nil]))
        [zt nil]))))

(defn ^:private scan-right [zt lines top left]
  (let [table (t/node zt)
        width (:width table)]
    (loop [table-zt zt
           col-id (inc left)]
      (if (< col-id width)
        (let [c (verse/nth-2d lines top col-id)]
          (case c
            \+ (let [[next-table-zt cell-pos] (-> zt (append-col-id col-id)
                                                  (scan-down lines top left col-id))]
                 (if-not cell-pos
                   (recur next-table-zt (inc col-id))
                   [next-table-zt cell-pos]))
            \- (recur table-zt (inc col-id))
            [zt nil]))
        [zt nil]))))

(defn ^:private valid-top-left-cell-corner? [[top left] [width height]]
  (and (not= top (dec height)) (not= left (dec width))))

(defn ^:private sort-body-rows [zt]
  (-> zt
      move-to-body
      (t/edit
       (fn [table-body]
         (update table-body :children #(->> % (sort-by :id) vec))))
      t/up))

(defn ^:private sort-header-rows [zt]
  (if (-> zt t/node :header-idx)
    (-> zt
        move-to-header
        (t/edit
         (fn [table-body]
           (update table-body :children #(->> % (sort-by :id) vec))))
        t/up)
    zt))

(defn ^:private body-cells [table]
  (let [table-body (some #(if (= (:type %) :table-body) %) (:children table))
        rows (:children table-body)]
    (reduce (fn [cells row]
              (apply conj cells (:children row))) [] rows)))

(defn ^:private cells-area [cells]
  (let [cell-sizes (map #(identity [(:width %) (:height %)]) cells)]
    (reduce (fn [area [width height]]
              (+ area (* (inc width) (inc height)))) 0 cell-sizes)))

(defn ^:private badform? [table]
  (let [cells (body-cells table)
        cells-area (cells-area cells)
        table-width (:width table)
        table-height (:height table)
        table-area (* table-width table-height)]
    (not= table-area
          (+ cells-area table-height table-width -1))))

(defn ^:private split-header-and-body-rows [zt]
  (let [table (t/node zt)]
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
        (t/edit zt #(assoc % :children [updated-table-header updated-table-body])))
      zt)))

(defn ^:private scan-cells [zt lines f]
  (let [table (t/node zt)
        {width :width height :height pos :pos} table
        init-table-zt zt]
    (loop [table-zt init-table-zt
           corners [[0 0]]]
      (if-not (empty? corners)
        (let [[top left] (peek corners)
              next-corners (pop corners)]
          (let [[next-table-zt cell-pos] (scan-right table-zt lines top left)]
            (if (and (vector? cell-pos) (= (count cell-pos) 4))
              (let [[_ _ bottom right] cell-pos
                    cell-content (cell-content lines cell-pos)
                    cell-node (create-cell cell-pos cell-content pos f)]
                (recur (-> next-table-zt (append-body-cell cell-node))
                       (-> next-corners (cond->
                                            (valid-top-left-cell-corner? [top right] [width height])
                                          (conj [top right])
                                          (valid-top-left-cell-corner? [bottom left] [width height])
                                          (conj [bottom left]))
                           distinct sort reverse vec)))
              (recur table-zt (-> corners sort reverse vec pop)))))
        (if (-> table-zt t/node badform?)
          (t/replace table-zt (error-malformed lines pos "Parse incomplete."))
          (-> table-zt
              split-header-and-body-rows
              sort-header-rows
              sort-body-rows
              t/up))))))

(defn ^:private remove-header-line [lines header-idx]
  (if header-idx
    (assoc lines header-idx (-> lines
                                (nth header-idx)
                                (string/replace "=" "-")))
    lines))

(defn ^:private find-grid-table-header-ids [lines]
  (reduce-kv (fn [heads idx line]
               (if (patt/match? :grid-table-head-sep line)
                 (conj heads idx)
                 heads))
             [] lines))

(defn scan-block [zt lines pos f]
  (let [[width height] (verse/text-block-size lines)
        init-table (create-grid-table width height pos)
        init-table-zt (-> zt (t/append-child init-table) t/move-to-latest-child)
        header-ids (find-grid-table-header-ids lines)]
    (case (count header-ids)
      0 (scan-cells init-table-zt lines f)
      1 (let [header-idx (first header-ids)
              non-header-lines (remove-header-line lines header-idx)]
          (-> init-table-zt
              (t/edit #(assoc % :header-idx header-idx))
              (t/append-child (create-header))
              (scan-cells non-header-lines f)))
      (let [msg (str "Multiple head/body row separators (table lines "
                     (string/join ", " header-ids)
                     "); only one allowed.")]
        (t/replace init-table-zt (error-malformed lines pos msg))))))
