(ns backtype.blackjack.core-test
  (:use [blackjack.core] :reload))

(def test-hand
  [{:suit :clubs, :rank :ace, :showing? false}
   {:suit :spades, :rank :three, :showing? false}])
