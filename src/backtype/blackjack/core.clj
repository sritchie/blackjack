(ns backtype.blackjack.core
  (:refer-clojure :exclude [shuffle])
  (:use [clojure.java.shell :only (sh)]))

;; ## Blackjack Data Structures
;;
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
  [n]
  {:pre [(pos? n)]}
  (->> deck-gen
       (repeat n)
       (apply shuffle)
       ref))

(defn new-hand
  "Generates a new, empty hand."
  [] (ref []))

(defn new-discard
  "Generates a new, empty hand."
  [] (ref []))

(defn new-game
  "Initializes a new game of Blackjack."
  [chips decks]
  {:deck (new-deck decks)
   :discard (ref [])
   :chips (atom chips)
   :current-bet 0
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

(defn set-showing
  [bool hand]
  (dosync
   (alter hand
          (partial map #(assoc % :showing? (boolean bool))))))

(defn deal-cards
  "Deals a card from the supplied deck into the supplied hand. If the
  deck is empty, the deck will be refreshed before dealing a card out
  to the players."
  [deck hand count & {:keys [show?]}]
  (let [set-show (partial set-showing show?)]
    (dosync
     (if-let [f (take count @deck)]
       (do (ref-set deck (drop count @deck))
           (add-cards hand (set-show (ref f))))
       (do (ref-set deck @(new-deck *total-decks*))
           (add-cards hand (set-show (ref (take count @deck)))))))))

;; TODO: This needs some work, as a hit isn't this simple, of course.

(defn play-hit
  [deck hand]
  (deal-cards deck hand 1 :show? true))

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

(defn highest-scores
  [& hands]
  (map (comp last
             (partial filter #(<= % 21))
             score-hand)
       hands))

(defn busted?
  [hand]
  (not (some #(<= % 21) (score-hand hand))))

(defn push?
  [hand1 hand2]
  (let [[s1 s2] (highest-scores hand1 hand2)]
    (= s1 s2)))

(defn beats?
  [hand1 hand2]
  (let [[s1 s2] (highest-scores hand1 hand2)]
    (and (not (busted? hand1))
         (> s1 s2))))

(defn over-16?
  [hand]
  (every? #(>= % 17) (score-hand hand)))

(defn twenty-one?
  "Determines whether or not the given hand is a blackjack."
  [hand]
  (some #{21} (score-hand hand)))

(defn report-outcome
  [game reason]
  (let [{:keys [dealer-hand player-hand]} game]
    (cond (busted? dealer-hand) (println "Player wins.")
          (push? dealer-hand player-hand) (println "Push!")
          (beats? player-hand dealer-hand) (if (= :blackjack reason) 
                                             (println "Blackjack!")
                                             (println "Player wins."))
          :else (println "Dealer wins."))))

;; ## Text Representations

(defn score-str
  "Returns a string representation of the score of the game."
  [hand]
  (let [[score-a score-b] (filter #(<= % 21)
                                  (score-hand hand))]
    (when score-a
      (apply str score-a (when score-b ["/" score-b])))))

(defn print-hand
  "Prints out a text representation of the supplied hand."
  [hand]
  (doseq [card @hand :let [{:keys [suit rank showing?]} card]]
    (println (if-not showing?
               "Hidden card."
               (format "%s of %s"
                       (name rank)
                       (name suit)))))
  (println))

(defn print-interface
  "TODO: Clean up the internal ref business."
  [game]
  (-> "clear" sh :out println)
  (let [{:keys [chips dealer-hand player-hand]} game
        ds (score-str (ref (filter :showing? @dealer-hand)))
        ps (score-str player-hand)]
    (println (str "Dealer's hand" (if ds
                                    (format ", showing %s points:" ds)
                                    " (a bust!)")))
    (print-hand dealer-hand)
    (println (str "Your hand" (if ps
                                (format ", showing %s points:" ps)
                                " (a bust!)")))
    (print-hand player-hand)
    (println "You have" @(:chips game) "total chips.\n")))

;; ## Game Loop Functions.

(defn prompt [message]
  (println message)
  (read-line))

(defn get-bet
  []
  (prompt "How many chips would you like to bet?"))

(defn get-move
  "TODO: Filter the <= bullshit out into a function that takes a
  binary predicate and the second argument to that predicate."
  []
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

(defn restart-hand
  "Dumps the hands into the discard, and gets everything going again."
  [game]
  (let [{:keys [discard dealer-hand player-hand]} game]
    (dump-hands discard dealer-hand player-hand)
    (initial-deal game)))

(defn remove-chips [pool amount]
  (dosync (swap! pool - amount)))

(defn settle-chips [game reason]
  (dosync
   (if (= reason :blackjack)
     ())))

(defn end-turn
  ([game & [reason]]
     (let [{:keys [dealer-hand player-hand]} game]
       (set-showing true dealer-hand)
       (settle-chips game reason)
       (print-interface game)
       (report-outcome game reason)
       (restart-hand game)
       (prompt "Please hit enter to play again."))))

(defn enact-dealer
  [game]
  (let [{:keys [deck dealer-hand player-hand]} game]
    (set-showing true dealer-hand)
    (loop []
      (print-interface game)
      (Thread/sleep 600)
      (if (over-16? dealer-hand)
        (end-turn game)
        (do (play-hit deck dealer-hand)
            (recur))))))

(defn enact-player
  [game]
  (let [{:keys [deck dealer-hand player-hand]} game]
    (if (twenty-one? player-hand)
      (end-turn game :blackjack)
      (loop []
        (print-interface game)
        (make-bet game)
        (let [move (get-move)]
          (case move
                "exit" :quit
                "stay" (enact-dealer game)
                "hit" (do (play-hit deck player-hand)
                          (cond (busted? player-hand) (end-turn game)
                                (twenty-one? player-hand) (enact-dealer game)
                                :else (recur)))
                (prompt "Hmm, sorry, I didn't get that. Hit enter to continue.")))))))

(defn start []
  (let [game (new-game 500 *total-decks*)]
    (initial-deal game)
    (loop []
      (if (= :quit (enact-player game))
        "Goodbye!"
        (recur)))))
