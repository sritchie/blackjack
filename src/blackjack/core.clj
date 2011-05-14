(ns blackjack.core
  (:refer-clojure :exclude [shuffle]))

;; ## Blackjack
;;
;; First, we're going to need primitives to represent a deck. We'll
;; use a set for suits, and, for now, a map for ranks, so we can
;; represent values as well.

;; Not sure if this is the best way to proceed, but we'll use a global
;; variable here to represent the total number of decks used in the
;; game. This can be rebound, if necessary to the game.

(def *total-decks* 6)

(def suits
  #{:hearts :spades :clubs :diamonds})

(def ranks {:ace 1
            :two 2
            :three 3
            :four 4
            :five 5
            :six 6
            :seven 7
            :eight 8
            :nine 9
            :ten 10 :jack 10 :queen 10 :king 10})

;; Deck generates every card in the deck, representing a card as a map
;; with `:suit`, `:rank` and `:showing?`, to let us know if it's face
;; up. This is going to be helpful in calculating the percent chance
;; of winning a specific hand.
;;
;; Interesting to note that we can't actually use a set to represent a
;; hand, if we're going to have multiple decks.

(def deck-gen
  (for [suit suits
        rank (keys ranks)]
    {:suit suit :rank rank :showing? false}))

(defn shuffle
  [& decks]
  (clojure.core/shuffle (reduce into decks)))

(defn new-deck
  "Returns a new, shuffled deck. If n is supplied, returns `n` decks
shuffle together."
  ([] (shuffle deck-gen))
  ([n]
     {:pre [(pos? n)]}
     (nth (iterate (partial into (new-deck))
                   (new-deck))
          (dec n))))

(defn new-hand
  "Generates a new, empty hand."
  [] [])

;; ## Game Play Mechanics

(defn add-card
  "Adds the given item to a hand collection. Must be wrapped in a
  dosync transaction."
  [hand card]
  (alter hand conj card))

;; TODO: deal with the fact that the deck might become empty. We can't
;; just add a new deck! But once the six decks are exhausted, we're
;; going to want to replace the deck. Do we want to do that in this
;; function? Not really sure about that.
;;
;; TODO: Perhaps we should check the count of the decks -- if we play
;; a hand that gets down below three decks, or maybe below one deck,
;; after the turn we shuffle the discards back into the deck. This has
;; to reset our card counting, of course.

(defn deal-card
  "Deals a card from the supplied deck into the supplied hand. If the
  deck is empty, the deck will be refreshed before dealing a card out
  to the players."
  [deck hand]
  (dosync
   (if-let [f (first @deck)]
     (do (ref-set deck (vec (rest @deck)))
         (add-card hand f))
     (do (ref-set deck (new-deck *total-decks*))
         (add-card hand (first @deck))))))

(defn dump-hand
  "Dumps the contents of the hand into the given discard pile, and
  sets the hand back to its fresh, empty state."
  [discard hand]
  (dosync (alter discard into @hand)
          (ref-set hand (new-hand))))

;; Based on this, the way a game would play would be... initialize the
;; decks and the hands, and then on each turn, based on the player
;; input or the game logic, decide whether to hit or stay. If we hit,
;; we need to `deal-card` into the supplied hand, the calculate scores. 

;; ### Hand Ranking

(defn rank-hand
  "Returns the two possible ranks of any given deal. "
  [card-seq]
  (reduce (fn [[r1 r2] card]
            (if (= card :ace)
              [(inc r1) (+ r1 11)]
              [(+ r1 (card ranks)) (+ r2 (card ranks))]))
          [0 0]
          (for [card card-seq]
            (:rank card))))

(defn rank-initial-deal
  "Shows the rank of an initial deal."
  [deck]
  (rank-hand (map #(assoc % :showing? true)
                  (take 2 deck))))

(defn percent-chance
  "Show the percent chance of getting the given score for a "
  [score n]
  (let [hand-seq (filter #(= score %)
                         (flatten
                          (for [_ (range n)]
                            (->> (new-deck)
                                 (partition 2)
                                 (map (comp second rank-hand))))))
        cts (count hand-seq)]
    (/ cts (* n 26.))))

;; ## Game Loop Functions.

(defn valid? [input]
  (when (= input "face")
    "Excellent, paduan."))

(defn prompt [message]
  (println message)
  (read-line))

(defn get-username []
  (prompt "What is your username?"))

(defn get-move []
  (prompt "What is your move? Your choices are hit, stay, or exit."))

(defn run []
  (let [user-name (get-username)]
    (loop []
      (let [move (get-move)]
        (if (= move "exit")
          (println "Goodbye :(")
          (do (println (case move
                             "exit" 
                             "hit"  "impressive."
                             "stay" "Nice move!"
                             "Sorry, didn't understand that one."))
              (recur)))))))
