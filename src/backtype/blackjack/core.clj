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
       (apply shuffle)))

(def empty-hand [])
(def new-discard [])

(defn new-game
  "Initializes a new game of Blackjack."
  [chips decks]
  {:deck (new-deck decks)
   :discard []
   :chips chips
   :current-bet 0
   :player empty-hand
   :dealer empty-hand})

;; ## Game Play Mechanics

(defn set-card-showing
  [bool card]
  {:pre [(#{true false} bool)]}
  (assoc card :showing? bool))

(defn set-hand-showing
  [bool game hand-kwd]
  (assoc game
    hand-kwd (map (partial set-card-showing bool)
                  (hand-kwd game))))

(def show-hand (partial set-hand-showing true))
(def hide-hand (partial set-hand-showing false))

(defn deal-cards
  "Deals a card from the supplied deck into the supplied hand. If the
  deck is empty, the deck will be refreshed before dealing a card out
  to the players."
  [game n hand-kwd & {:keys [show?]}]
  {:pre [(-> game :deck count (>= n)), (hand-kwd game)]}
  (let [[deck hand] (map game [:deck hand-kwd])
        [cards new-deck] (split-at n deck)
        cards (map #(assoc % :showing? (boolean show?))
                   cards)]
    (assoc game
      hand-kwd (into hand cards)
      :deck new-deck)))

(defn play-hit
  [game hand-kwd]
  (deal-cards game 1 hand-kwd :show? true))

(defn dump-hands
  "Dumps the contents of the hand into the given discard pile, and
  sets the hand back to its fresh, empty state."
  [game]
  (assoc game
    :discard (reduce into (map game [:discard :dealer :player]))
    :dealer empty-hand
    :player empty-hand))

;; ### Hand Scoring

(defn score-hand
  "Returns the two possible scores of any given deal. An ace can never
  count as 11 more than once, as this would cause an instant bust --
  we accept an optional argument that returns the highest possible
  value of rank, based on the presence of an ace."
  [hand]
  (let [rank-seq (map :rank hand)
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
  (let [{:keys [dealer player]} game]
    (cond (busted? dealer) (println "Player wins.")
          (push? dealer player) (println "Push!")
          (beats? player dealer) (if (= :blackjack reason) 
                                   (println "Blackjack!")
                                   (println "Player wins."))
          :else (println "Dealer wins."))
    game))

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
  (doseq [card hand :let [{:keys [suit rank showing?]} card]]
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
  (let [{:keys [chips dealer player]} game
        ds (score-str (filter :showing? dealer))
        ps (score-str player)]
    (println (str "Dealer's hand" (if ds
                                    (format ", showing %s points:" ds)
                                    " (a bust!)")))
    (print-hand dealer)
    (println (str "Your hand" (if ps
                                (format ", showing %s points:" ps)
                                " (a bust!)")))
    (print-hand player)
    (println "You have" (:chips game) "total chips.\n")
    game))

;; ## Game Loop Functions.

(defn prompt [message]
  (println message)
  (read-line))

(defn get-bet []
  (prompt "How many chips would you like to bet?"))

(defn get-move
  "TODO: Filter the <= bullshit out into a function that takes a
  binary predicate and the second argument to that predicate."
  []
  (prompt "What is your move? Your choices are hit, stay, or exit."))

(defn initial-deal
  [game]
  (-> game
      (deal-cards 2 :player :show? true)
      (deal-cards 1 :dealer :show? true)
      (deal-cards 1 :dealer :show? false)))

(defn restart-game
  "Dumps the hands into the discard, and gets everything going again."
  [game]
  (-> game dump-hands initial-deal))

(defn end-turn
  ([game & [reason]]
     (let [ret-game (-> game
                        (show-hand :dealer)
                        print-interface
                        (report-outcome reason)
                        restart-game)]
       (prompt "Please hit enter to play again.")
       ret-game)))

(defn enact-dealer
  [game]
  (loop [game (show-hand game :dealer)]
    (let [dealer (:dealer game)]
      (print-interface game)
      (Thread/sleep 600)
      (if (over-16? dealer)
        (end-turn game)
        (recur (play-hit game :dealer))))))

(defn enact-player
  [game]
  (let [{:keys [deck dealer player]} game]
    (if (twenty-one? (:player game))
      (end-turn game :blackjack)
      (loop [game game]
        (print-interface game)
        (case (get-move)
              "exit" :quit
              "stay" (enact-dealer game)
              "hit" (let [game (play-hit game :player)
                          {:keys [dealer player]} game]
                      (cond (busted? player) (end-turn game)
                            (twenty-one? player) (enact-dealer game)
                            :else (recur game)))
              (do (prompt "Hmm, sorry, I didn't get that. Hit enter to continue.")
                  (recur game)))))))

(defn start []
  (let [game (new-game 500 *total-decks*)]
    (loop [game (initial-deal game)]
      (if (= :quit game)
        "Goodbye!"
        (recur (enact-player game))))))
