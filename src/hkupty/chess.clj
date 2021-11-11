(ns hkupty.chess
  (:require
    [hkupty.chess.rules :as rules]
    [hkupty.chess.engine :as engine])
  (:gen-class))


(defn -main [& args]
  (engine/game rules/board))
