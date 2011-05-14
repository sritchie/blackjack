(ns backtype.blackjack.core
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

(def ranks
  (merge (zipmap [:ace :two :three :four :five
                  :six :seven :eight :nine :ten]
                 (map inc (range)))
         {:jack 10 :queen 10 :king 10}))

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
  [] (ref []))

(defn new-discard
  "Generates a new, empty hand."
  [] (ref []))

(defn new-game
  "Initializes a new game of Blackjack."
  [chips decks]
  {:deck (ref (new-deck decks))
   :discard (ref [])
   :chips (atom chips)
   :player-hand (new-hand)
   :dealer-hand (new-hand)})

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
  [deck hand]
  (deal-cards deck hand 1))

(defn dump-hands
  "Dumps the contents of the hand into the given discard pile, and
  sets the hand back to its fresh, empty state."
  [discard & hands]
  (dosync 
   (doseq [hand hands]
     (alter discard into @hand)
     (ref-set hand @(new-hand)))))

;; Based on this, the way a game would play would be... initialize the
;; decks and the hands, and then on each turn, based on the player
;; input or the game logic, decide whether to hit or stay. If we hit,
;; we need to `deal-card` into the supplied hand, the calculate scores. 

;; ### Hand Scoring

(defn score-hand
  "Returns the two possible scores of any given deal. An ace can never
  count as 11 more than once, as this would cause an instant bust --
  we accept an optional argument that returns the highest possible
  value of rank, based on the presence of an ace."
  [hand]
  (let [rank-seq (map :rank @hand)
        score (reduce (fn [acc card]
                        (+ acc (card ranks)))
                      0
                      rank-seq)]
    (if (some #{:ace} rank-seq)
      [score (+ 10 score)]
      [score])))

(defn push? []
  "COMING")

(defn beats?
  "TODO: Think about PUSH!!! We want the highest hands that aren't
  busted to equal each other."
  [hand1 hand2]
  (map (comp (partial filter #(<= % 21))
             score-hand)
       [hand1 hand2]))

(defn over-16?
  [hand]
  (every? #(>= % 17) (score-hand hand)))

(defn blackjack?
  "Determines whether or not the given hand is a blackjack."
  [hand]
  (and (= 2 (count @hand))
       (some #{21} (score-hand hand))))

(defn busted?
  [hand]
  (not (some #(<= % 21) (score-hand hand))))

(defn check-winners
  [dealer-hand player-hand]
  (cond (push? dealer-hand player-hand) "Push!"
        (beats? player-hand dealer-hand) "Player wins."
        :else "Dealer wins."))


;; ## Text Representations

(defn print-hand
  "Prints out a text representation of the supplied hand."
  [hand show-hidden?]
  (doseq [card @hand :let [{:keys [suit rank showing?]} card]]
    (println (if (and (not showing?) (not show-hidden?))
               "Face down card!"
               (format "%s of %s (face %s)"
                      (name rank)
                      (name suit)
                      (if showing? "up" "down"))))))

(defn score-str
  "Returns a string representation of the score of the game."
  [hand]
  (->> (score-hand hand)
       (filter #(<= % 21))
       (interpose "/" )
       (apply str)))

(defn print-hands
  [dealer-hand player-hand]
  (doseq [[hand title show?] [[dealer-hand "the dealer's" false]
                              [player-hand "your" true]]]
    (println "\n" (format "Here's %s hand (worth %s points):"
                          title (score-str hand)))
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

;; TODO: Check the atom thing, for the chips.
;;
;; Interesting -- so, if the player, hits, you recur and do all of
;; this over again. If the user stays, then the computer goes into
;; playing mode and does its thing.

(defn initial-deal
  [game]
  (let [{:keys [deck dealer-hand player-hand]} game]
    (do (deal-cards deck player-hand 2 :show? true)
        (deal-cards deck dealer-hand 1 :show? true)
        (deal-cards deck dealer-hand 1 :show? false))))

(defn resolve-turn
  "Takes in any number of hands, and decides if one of these hands has
  caused a bust."
  [game]
  (let [{:keys [deck discard dealer-hand player-hand]} game]
    (print-hands dealer-hand player-hand)
    (if (or (blackjack? player-hand) (busted? player-hand))
      (do (prompt (if (busted? player-hand)
                    "Sorry, you busted. Please hit enter to proceed."
                    "Blackjack!!! Please hit enter to proceed."))
          (dump-hands discard dealer-hand player-hand)
          (initial-deal game))
      (prompt "Nice job! Hit enter to continue."))))


(defn start []
  (let [player-name (get-username)
        game (new-game 500 *total-decks*)
        {:keys [deck dealer-hand player-hand]} game]
    (initial-deal game)
    (loop []
      (print-hands dealer-hand player-hand)
      (let [move (get-move)]
        (if (= move "exit")
          (println "Goodbye :(")
          (do (case move
                    "hit"  (do (play-hit deck player-hand)
                               (resolve-turn game))
                    "stay" (println "Dealer does his thing!")
                    (println "Sorry, didn't understand that one."))
              (recur)))))))


;; WISHFUL THINKING

;; (blackjack? player-hand) "Blackjack!"

;; TODO: Clean up.
(defn enact-dealer
  [deck dealer-hand player-hand]
  (loop []
    (print-hands dealer-hand player-hand)
    (if (over-16? dealer-hand)
      (check-winners dealer-hand player-hand)
      (do (play-hit deck dealer-hand)
          (recur)))))
