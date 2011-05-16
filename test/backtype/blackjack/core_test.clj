(ns backtype.blackjack.core-test
  (:refer-clojure :exclude [shuffle])
  (:use backtype.blackjack.core)
  (:use clojure.test))

(def test-player
  (ref [{:suit :spades, :rank :two, :showing? true}
        {:suit :spades, :rank :ten, :showing? true}
        {:suit :hearts, :rank :ten, :showing? true}]))

(def test-dealer
  (ref [{:suit :spades, :rank :two, :showing? true}
        {:suit :spades, :rank :five, :showing? true}
        {:suit :hearts, :rank :ten, :showing? true}]))


;; NOT NEEDED FOR MY NEW WORLD
(defn mk-showing
  [bool hand]
  (map #(assoc % :showing? (boolean bool)) hand))

(defn set-showing
  [bool hand]
  (ref-set hand (mk-showing bool @hand)))

(defn add-cards
  "Adds the given item to a hand collection. Must be wrapped in a
  dosync transaction."
  [hand card-seq]
  (alter hand into card-seq))

(defn deal-cards
  "Deals a card from the supplied deck into the supplied hand. If the
  deck is empty, the deck will be refreshed before dealing a card out
  to the players."
  [deck hand n & {:keys [show?]}]
  {:pre [(>= n (count @deck))]}
  (let [[cards new-deck] (split-at n @deck)]
    (dosync
     (ref-set deck new-deck)
     (add-cards hand (mk-showing show? cards)))))

