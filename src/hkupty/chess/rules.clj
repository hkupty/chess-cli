(ns hkupty.chess.rules
  (:require [clojure.string :as str]))

(def files [:a :b :c :d :e :f :g :h])
(def ranks [:1 :2 :3 :4 :5 :6 :7 :8])
(def pieces {"N" {:piece :Knight}
             "K" {:piece :King}
             "Q" {:piece :Queen}
             "B" {:piece :Bishop}
             "R" {:piece :Rook}
             "" {:piece :Pawn}})

(def flags
  {"x" :expected-capture
   "+" :expected-check
   "#" :expected-mate})

(def special-movements
  {"O-O" {:tactic :castle-kingside :piece :King}
   "O-O-O" {:tactic :castle-queenside :piece :King}})

(def piece-symbols
  {:Pawn \♙
   :Knight \♘
   :Bishop \♗
   :Rook \♖
   :Queen \♕
   :King \♔})

(defn find-piece [expr]
  (let [[_ p hint] (re-find #"^([NKQBR]|)([a-h])?" expr)]
    (assoc (get pieces p) :hint (keyword hint))))

(defn find-flags [expr]
  {:flags (into #{}
                (comp
                  (filter (comp (partial str/includes? expr) key))
                  (map val))
                flags)})

(defn find-promotion [expr]
  (when-let [promoted (last (re-find #"=([NKQBR])" expr))]
    {:promoted (get pieces promoted)}))

(defn find-movement [expr]
  {:coord (into []
                   (map (comp keyword str))
                   (re-find #"[a-h][1-8]" expr))})

(defn parse-expression [expr]
  (if
    (some (partial = expr) (keys special-movements)) (get special-movements expr)
    (merge
      (find-movement expr)
      (find-piece expr)
      (find-flags expr)
      (find-promotion expr))))

(defn piece [p]
  (cond-> (get piece-symbols (:piece p) " ")
      (= :whites (:player p)) (-> int
                                  ;; Unicode +6 changes piece color
                                  (+ 6)
                                  char)))

(defn- valid-point [n]
  (>= 7 n 0))

(defn fixed-movement [axis from distance]
  (into #{}
        (comp
          (filter valid-point)
          (map #(get axis %)))
    ((juxt (partial + distance) #(- % distance))
   (.indexOf axis from))))

(defn points-to-coord [-f -r]
  (when (and (valid-point -f)
             (valid-point -r))
    [(get files -f)
     (get ranks -r)]))

(defn range-movement [[file rank]]
  (into #{}
        (concat
          (map (fn [-rank] [file -rank]) ranks)
          (map (fn [-file] [-file rank]) files))))

(defn diagonal-movement [[file rank]]
  (let [file-ix (.indexOf files file)
        rank-ix (.indexOf ranks rank)]
    (into #{}
        (comp
          (mapcat
          (fn [ix]
            (let [offset (Math/abs (- ix file-ix))]
              [(points-to-coord ix (+ rank-ix offset))
               (points-to-coord ix (- rank-ix offset))])))
              (filter some?))
        (range 0 8))))

(defn combine [coll1 coll2]
  (reduce (fn [acc i]
            (reduce (fn [acc- j] (conj acc- [i j]))
                    acc coll2))
          [] coll1))

(defn bishop-movement [-position _] (diagonal-movement -position))
(defn rook-movement [-position _] (range-movement -position))

(defn knight-movement [[file rank] _]
  (let [f1 (fixed-movement files file 1)
        f2 (fixed-movement files file 2)
        r1 (fixed-movement ranks rank 1)
        r2 (fixed-movement ranks rank 2)]
    (into #{}
          (concat
            (combine f1 r2)
            (combine f2 r1)))))

(defn queen-movement [[file rank] _]
  (into #{}
        (concat
          (range-movement [file rank])
          (diagonal-movement [file rank]))))

(defn king-movement [[file rank] _]
  (let [f1 (fixed-movement files file 1)
        r1 (fixed-movement ranks rank 1)]
    (into #{}
          (combine f1 r1))))

(defn adjust-point [-value axis delta]
  (->> -value
       (.indexOf axis)
       (+ delta)
       (Math/abs)
       (get axis)))

(defn adjust-coord [coord axis-name delta]
  (cond
    (= :file axis-name) (update coord 0 adjust-point files delta)
    (= :rank axis-name) (update coord 1 adjust-point ranks delta)))

(defn adjust-for-blacks [coord]
  (adjust-coord coord :rank -7))

(defn adjust-for-player [coord player]
  (cond-> coord
    (= :blacks player) (adjust-for-blacks)))

(defn pawn-movement [coord {:keys [player]}]
  (let [base (adjust-for-player coord player)
        base-rank (second base)
        front (adjust-coord base :rank +1)]

    (into #{}
          (comp
            (map #(adjust-for-player % player))
            #_(filter (partial every? some?)))
          (cond-> [front
     (adjust-coord front :file +1)
     (adjust-coord front :file -1)]
      (= :2 base-rank) (conj (adjust-coord front :rank +1))))))

(adjust-for-blacks [:d :7])
(adjust-coord [:d :7] :ranks -5)

(def board
  {:turn :whites
   :movements []
   :taken []
   :pieces
   {
    [:a :7] {:player :blacks :piece :Pawn}
    [:b :7] {:player :blacks :piece :Pawn}
    [:c :7] {:player :blacks :piece :Pawn}
    [:d :7] {:player :blacks :piece :Pawn}
    [:e :7] {:player :blacks :piece :Pawn}
    [:f :7] {:player :blacks :piece :Pawn}
    [:g :7] {:player :blacks :piece :Pawn}
    [:h :7] {:player :blacks :piece :Pawn}
    [:a :8] {:player :blacks :piece :Rook}
    [:b :8] {:player :blacks :piece :Knight}
    [:c :8] {:player :blacks :piece :Bishop}
    [:d :8] {:player :blacks :piece :Queen}
    [:e :8] {:player :blacks :piece :King}
    [:f :8] {:player :blacks :piece :Bishop}
    [:g :8] {:player :blacks :piece :Knight}
    [:h :8] {:player :blacks :piece :Rook}
    [:a :2] {:player :whites :piece :Pawn}
    [:b :2] {:player :whites :piece :Pawn}
    [:c :2] {:player :whites :piece :Pawn}
    [:d :2] {:player :whites :piece :Pawn}
    [:e :2] {:player :whites :piece :Pawn}
    [:f :2] {:player :whites :piece :Pawn}
    [:g :2] {:player :whites :piece :Pawn}
    [:h :2] {:player :whites :piece :Pawn}
    [:a :1] {:player :whites :piece :Rook}
    [:b :1] {:player :whites :piece :Knight}
    [:c :1] {:player :whites :piece :Bishop}
    [:d :1] {:player :whites :piece :Queen}
    [:e :1] {:player :whites :piece :King}
    [:f :1] {:player :whites :piece :Bishop}
    [:g :1] {:player :whites :piece :Knight}
    [:h :1] {:player :whites :piece :Rook}
    }
   })

(def movement-for-pieces
  {:Knight knight-movement
   :Bishop bishop-movement
   :Rook king-movement
   :Queen queen-movement
   :King king-movement
   :Pawn pawn-movement})

(defn points-between-coords [[-file -rank] to]
  (let [from-file (.indexOf files -file)
        from-rank (.indexOf ranks -rank)

        distance-file (- from-file (.indexOf files (first to)))
        distance-rank (- from-rank (.indexOf ranks (second to)))

        offset-file (compare 0 distance-file)
        offset-rank (compare 0 distance-rank)]

    (transduce
      (map (fn [ix] [(adjust-point -file files (* ix offset-file))
                     (adjust-point -rank ranks (* ix offset-rank))]))
      conj
      []
      (range 1 (max (Math/abs distance-file) (Math/abs distance-rank))))))


(comment
  (points-between-coords (adjust-for-blacks [:e :1]) (adjust-for-blacks [:a :1]))
[[:d :8] [:c :8] [:b :8]]
  
  )
