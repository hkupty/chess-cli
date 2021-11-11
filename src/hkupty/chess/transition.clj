(ns hkupty.chess.transition)

(defmacro safe-> [initial-value-or-expr & forms]
  (let [->symbol (fn [ix] (symbol (str "v" ix "#")))
        last-sym (->symbol (dec (count forms)))]
    `(let ~(reduce (fn [bindings [ix new-expr]]
                     (let [prev-symbol (->symbol ix)
                           this-symbol (->symbol (inc ix))]
                       (conj bindings
                             this-symbol
                             `(if-let [failure# (:failure ~prev-symbol)]
                                (assoc ~(->symbol 0) :failure failure#)
                                (-> ~prev-symbol ~new-expr)))))
                   ['v0# (eval initial-value-or-expr)]
                   (map-indexed vector (butlast forms)))
       (if-let [failure# (:failure ~last-sym)]
         (assoc ~(->symbol 0) :failure failure#)
         (-> ~last-sym ~(last forms))))))

