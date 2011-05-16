(ns backtype.blackjack.core-test
  (:refer-clojure :exclude [shuffle])
  (:use backtype.blackjack.core)
  (:use clojure.test))

(def test-player
  [{:suit :spades, :rank :two, :showing? true}
   {:suit :spades, :rank :ten, :showing? true}
   {:suit :hearts, :rank :ten, :showing? true}])

(def test-dealer
  [{:suit :spades, :rank :two, :showing? true}
   {:suit :spades, :rank :five, :showing? true}
   {:suit :hearts, :rank :ten, :showing? true}])
