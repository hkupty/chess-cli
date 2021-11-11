(ns hkupty.chess.engine
  (:require [clojure.string :as str]
            [hkupty.chess
             [rules :as rules :refer [files ranks]]
             [transition :refer [safe->]]])
  (:import (clojure.lang MapEntry)))

(defn tap [x]
  (println x)
  x)

(defn label [& args]
  (println args)
  (last args))

(defn find-piece-that-can-move [-board -move]
  ;; TODO use hints if more than 1 piece can move
  (let [-piece (:piece -move)
        possible-movements-fn (rules/movement-for-pieces -piece)
        pieces-that-can-move (into []
                                   (comp
                                     (filter (comp (partial = -piece) :piece val))
                                     (filter (comp (partial = (:turn -board)) :player val))
                                     (filter (fn [[k v]]
                                               (contains? (possible-movements-fn k v)
                                                          (:coord -move)))))
                                   (:pieces -board))]
    (first (cond->> pieces-that-can-move
      (pos? (dec (count pieces-that-can-move)))
      (filter (comp (partial = (:hint -move)) first key))))))

(defn draw-board [-board]
  (println "\033[H\033[2J")
  (println (str (:failure -board)))
  (loop [[the-rank & rest-rank] (reverse ranks)]
    (print "" (name the-rank) "")
    (run! print
          (interpose " "
                     (map
                       (fn [file]
                         (rules/piece (get-in -board [:pieces [file the-rank]])))
                       files)))
    (println)
    (when (seq rest-rank)
      (recur rest-rank)))
  (println "  " (str/join " " (map name files))))

(defn next-turn [player]
  (if (= :whites player)
    :blacks
    :whites))


(defn get-piece-and-coord [-board coord]
  (let [base (rules/adjust-for-player coord (:turn -board))]
    (MapEntry/create base (get-in -board [:pieces base]))))

(defn get-piece-coords-by-name-and-player[-board piece player]
  (transduce
      (comp
        (filter (comp (partial = player) :player val))
        (filter (comp (partial = piece) :piece val))
        (map key))
    conj
    #{}
    (:pieces -board)))

(defn has-piece-in-the-way? [-board -move -piece]
  (if (= :Knight (:piece (val -piece)))
    false
    (reduce (fn [_ coord]
              (when (some? (label ::has-piece coord (get-in -board [:pieces coord])))
                (reduced true)))
            false (tap (rules/points-between-coords (key -piece) -move)))))

(defn move-piece [-board movement -piece]
  (-> -board
      (update :pieces dissoc (key -piece))
      (assoc-in [:pieces movement] (val -piece))))

(defn adjusted-move-piece [-board coord -piece]
  (move-piece -board (rules/adjust-for-player coord (:turn -board)) -piece))

(defn castles [-board {:keys [tactic] :as -move}]
  (cond
    (and
      (= :castle-kingside tactic)
      (= :King (:piece (val (get-piece-and-coord -board [:e :1]))))
      (= :Rook (:piece (val (get-piece-and-coord -board [:h :1]))))
      (not (has-piece-in-the-way? -board
                                  (rules/adjust-for-player [:g :1] (:turn -board))
                                  (get-piece-and-coord -board [:e :1]))))
    (-> -board
        (adjusted-move-piece [:g :1] (get-piece-and-coord -board [:e :1]))
        (adjusted-move-piece [:f :1] (get-piece-and-coord -board [:h :1])))

    (and
      (= :castle-queenside (tap tactic))
      (= :King (tap (:piece (val (get-piece-and-coord -board [:e :1])))))
      (= :Rook (tap (:piece (val (get-piece-and-coord -board [:a :1])))))
      (not (tap (has-piece-in-the-way? -board
                                  (tap (rules/adjust-for-player [:c :1] (:turn -board)))
                                  (tap (get-piece-and-coord -board [:e :1]))))))
    (-> -board
        (adjusted-move-piece [:c :1] (get-piece-and-coord -board [:e :1]))
        (adjusted-move-piece [:d :1] (get-piece-and-coord -board [:a :1])))
    :else (assoc -board :failure "Can't perform castling")))

(defn normal-move [-board -move]
  (let [-piece (find-piece-that-can-move -board -move)
        in-place (get-in -board [:pieces (:coord -move)])]
    (cond

      ;; Should not capture
      (and (some? in-place)
           ((complement (:flags -move)) :expected-capture))
      (assoc -board :failure "Unexpected capture")

      ;; Captures
      (and (some? in-place)
           ((:flags -move) :expected-capture))
      (-> -board
          (update-in [:captures (:player in-place)] (fnil conj []) in-place)
          (move-piece (:coord -move) -piece))

      ;; Normal movement, blocked by piece in front
      (has-piece-in-the-way? -board (:coord -move) -piece)
      (assoc -board :failure "Movement blocked by piece")

      ;; Movement is legal
      :else (move-piece -board (:coord -move) -piece)))
  )



(defn move [-board -move]
  (-> (if (:tactic -move)
        (castles -board -move)
        (normal-move -board -move))
      (update :turn next-turn)))

(defn game [-board]
  (draw-board -board)
  (let [expr (read-line)
        debug? (= ":debug" expr)
        parsed (when-not debug? (rules/parse-expression expr))
        new-board (try
                    (if debug?
                      (tap -board)
                      (-> -board
                          (dissoc :failure)
                          (move
                            parsed)
                          (update :movements conj expr)))
                    (catch Exception e
                      (println parsed e expr)
                      -board))
        new-board (if-let [fail (:failure new-board)]
                    (assoc -board :failure fail)
                    new-board)]

    (if-not (:mate? new-board)
      (recur new-board))))
