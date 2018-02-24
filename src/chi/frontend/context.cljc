(ns chi.frontend.context
  (:require [chi.frontend.error :as err]))

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
    [zt lines current-idx states transition pos buffers reported-level]
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
  (push-state [this state] (update this :states conj state))
  (pop-state [this] (update this :states pop))
  (current-state [this] (-> this :states peek))
  (add-to-buffers [this line] (update this :buffers conj line))
  (update-buffers [this lines] (assoc this :buffers lines))
  (clear-buffers [this] (update-buffers this []))
  (buffers? [this] (-> this :buffers empty? not)))

(defn create
  [lines zt init-state init-pos]
  (map->DocumentContext {:zt zt
                         :lines lines
                         :pos init-pos
                         :current-idx 0
                         :states [init-state]
                         :buffers []
                         :reported-level (:info err/levels)}))
