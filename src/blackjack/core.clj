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

;; `deck-gen` generates every card in the deck, representing a card as
;; a map with `:suit`, `:rank` and `:showing?`, to let us know if it's
;; face up. This is going to be helpful in calculating the percent
;; chance of winning a specific hand.
;;
;; Interesting to note that we can't actually use a set to represent a
;; hand, if we're going to have multiple decks.

(def deck-gen
  (for [suit suits
        rank (keys ranks)]
    {:suit suit :rank rank :showing? false}))

(defn shuffle
  "Shuffled the supplied (dereffed) decks together."
  [& decks]
  (clojure.core/shuffle (reduce into decks)))

(defn new-deck
  "Returns a new, shuffled deck. If n is supplied, returns `n` decks
   shuffled together."
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

(defn add-cards
  "Adds the given item to a hand collection. Must be wrapped in a
  dosync transaction."
  [hand card-seq]
  (alter hand into card-seq))

;; TODO: deal with the fact that the deck might become empty. We can't
;; just add a new deck! But once the six decks are exhausted, we're
;; going to want to replace the deck. Do we want to do that in this
;; function? Not really sure about that.
;;
;; TODO: Perhaps we should check the count of the decks -- if we play
;; a hand that gets down below three decks, or maybe below one deck,
;; after the turn we shuffle the discards back into the deck. This has
;; to reset our card counting, of course.

(defn deal-cards
  "Deals a card from the supplied deck into the supplied hand. If the
  deck is empty, the deck will be refreshed before dealing a card out
  to the players."
  [deck hand count & {:keys [show?]}]
  (let [set-show (partial map #(assoc % :showing? (boolean show?)))]
    (dosync
     (if-let [f (take count @deck)]
       (do (ref-set deck (vec (drop count @deck)))
           (add-cards hand (set-show f)))
       (do (ref-set deck (new-deck *total-decks*))
           (add-cards hand (set-show (take count @deck))))))))

;; TODO: This needs some work, as a hit isn't this simple, of course.

(defn play-hit
  [deck dealer-hand player-hand]
  (deal-cards deck player-hand 1)
  (deal-cards deck dealer-hand 1))

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
  "Returns the two possible ranks of any given deal. An ace can never
  count as 11 more than once, as this would cause an instant bust --
  we accept an optional argument that returns the highest possible
  value of rank, based on the presence of an ace."
  [card-seq & {:keys [highest?]}]
  (let [rank (reduce (fn [acc card]
                       (+ acc (card ranks)))
                     0
                     (for [card card-seq]
                       (:rank card)))]
    (if (and highest? (some #{:ace} (map :rank card-seq)))
      (+ 10 rank)
      rank)))

(defn rank-initial-deal
  "Shows the rank of an initial deal."
  [deck]
  (rank-hand (map #(assoc % :showing? true)
                  (take 2 deck))))


;; ### Meaningless Testing

(defn percent-chance
  "Show the percent chance of getting the given score over `n` number
  of trials."
  [score n]
  (let [hand-seq (filter #(= score %)
                         (flatten
                          (for [_ (range n)]
                            (->> (new-deck)
                                 (partition 2)
                                 (map (comp second rank-hand))))))
        cts (count hand-seq)]
    (/ cts (* n 26.))))


;; ## Text Representations
;;
;; We need a way to show what a hand looks like!

(defn print-hand
  "Prints out a text representation of the supplied hand."
  [hand show-hidden?]
  (doseq [card hand :let [{:keys [suit rank showing?]} card]]
    (println (if (and (not showing?) (not show-hidden?))
               "Face down card!"
               (format "%s of %s (face %s)"
                      (name rank)
                      (name suit)
                      (if showing? "up" "down"))))))

(defn print-hands
  [dealer-hand player-hand]
  (doseq [[hand title show?] [[dealer-hand "the dealer's" false]
                              [player-hand "your" true]]]
    (println "\n" (format "Here's %s hand (worth %d points):"
                          title (rank-hand hand)))
    (print-hand hand show?))
  (println))

;; ## Game Loop Functions.

(defn prompt [message]
  (println message)
  (read-line))

(defn get-username []
  (prompt "What is your username?"))

(defn get-move []
  (prompt "What is your move? Your choices are hit, stay, or exit."))

(defn failure-test
  "Takes in any number of hands, and decides if one of these hands has
  caused a bust."
  [& hands]
  (if (->> hands
           (map (comp #(rank-hand % :highest? true)
                      deref))
           (some #(>= % 21)))
    (println "You've busted!!")
    (println "still playing :)")))


;; TODO: Check the atom thing, for the chips.

(defn start []
  (let [player-name (get-username)
        deck (ref (new-deck *total-decks*))
        discard (ref [])
        chips (atom 0)
        player-hand (ref (new-hand))
        dealer-hand (ref (new-hand))]
    (doseq [hand [player-hand dealer-hand]]
      (deal-cards deck hand 2 :show? true))
    (loop []
      (print-hands @dealer-hand @player-hand)
      (let [move (get-move)]
        (if (= move "exit")
          (println "Goodbye :(")
          (do (case move
                    "hit"  (play-hit deck dealer-hand player-hand)
                    "stay" (println "Nice move!")
                    (println "Sorry, didn't understand that one."))
              (failure-test dealer-hand player-hand)
              (recur)))))))
