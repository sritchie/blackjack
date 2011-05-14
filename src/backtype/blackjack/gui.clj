(ns backtype.blackjack.gui
  (:import [javax.swing JFrame]))

(defn frame
  []
  (doto (JFrame. "Hello Frame")
    (.setSize 200 200)
    (.setVisible true)))
