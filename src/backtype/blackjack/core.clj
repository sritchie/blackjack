(ns backtype.blackjack.core
  (:refer-clojure :exclude [shuffle])
  (:use [clojure.java.shell :only (sh)])
  (:gen-class))

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
  [chips decks soft]
  {:deck (new-deck decks)
   :discard []
   :player empty-hand
   :dealer empty-hand
   :chips chips
   :current-bet 0
   :turns 0
   :soft-17? (boolean soft)})

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
  (-> game
      (deal-cards 1 hand-kwd :show? true)
      (assoc :turns (-> game :turns inc))))

(defn dump-hands
  "Dumps the contents of the hand into the given discard pile, and
  sets the hand back to its fresh, empty state."
  [game]
  (assoc game
    :discard (reduce into (map game [:discard :dealer :player]))
    :dealer empty-hand
    :player empty-hand
    :turns 0))

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

(defn some-hand [hand pred]
  (some pred (score-hand hand)))

(defn every-hand [hand pred]
  (every? pred (score-hand hand)))

(defn twenty-one? [hand] (some-hand hand #{21}))
(defn busted? [hand] (every-hand hand #(> % 21)))

(defn push?
  [hand1 hand2]
  (apply = (highest-scores hand1 hand2)))

(defn beats?
  [hand1 hand2]
  (let [[s1 s2] (highest-scores hand1 hand2)]
    (and (not (busted? hand1))
         (> s1 s2))))

(defn outcome
  [game]
  (let [{:keys [dealer player turns]} game]
    (cond (busted? dealer) :win
          (push? dealer player) :push
          (beats? player dealer) (if (and (zero? turns)
                                          (twenty-one? player))
                                   :blackjack
                                   :win)
          :else :lose)))

(defn report-outcome
  [game outcome]
  (case outcome
        :blackjack (println "Blackjack!")
        :push (println "Push!")
        :win (println "Player wins.")
        :lose (println "Dealer wins."))
  game)

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
  (let [{:keys [chips current-bet dealer player]} game
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
    (println (format "You have %d chips left. Your current bet is %d.\n"
                     chips current-bet))
    game))

;; ## Game Loop Functions.

(defn prompt [message]
  (println message)
  (read-line))

(defn make-bet
  [game]
  (let [chips (:chips game)
        bet (try (Integer. (prompt "How many chips (up to 100) would you like to bet?"))
                 (catch Exception e :invalid))]
    (if-let [statement (cond
                        (= :invalid bet) "Sorry, that input seems to be invalid."
                        (neg? bet) "Only positive bets, please."
                        (> bet 100) "Under 100, please."
                        (neg? (- chips bet)) "Not enough funding for that!")]
      (do (println statement) (recur game))
      (assoc game
        :chips (- chips bet)
        :current-bet bet))))

(defn resolve-bet
  [game result double?]
  (let [{:keys [chips current-bet]} game
        pay (-> (case result
                      :blackjack (/ 5 2)
                      :win (if double? 3 2)
                      (:lose :push) 0)
                (* current-bet) Math/floor int)]
    (assoc game
      :chips (+ chips pay)
      :current-bet 0)))

(defn initial-deal
  [game]
  (-> game
      (deal-cards 2 :player :show? true)
      (deal-cards 1 :dealer :show? true)
      (deal-cards 1 :dealer :show? false)))

(defn start-turn
  [game]
  (-> game print-interface make-bet initial-deal))

(defn end-turn
  [game & {:keys [double?]}]
  (let [result (outcome game)
        ret-game (-> game
                     (show-hand :dealer)
                     (resolve-bet result double?) ;; FIX!!!
                     print-interface
                     (report-outcome result)
                     dump-hands)]
    (prompt "Please hit enter to play again.")
    ret-game))

(defn dealer-turn
  "Dealer takes his turn, following the rules of soft 17."
  [game & {:keys [double?]}]
  (loop [game (show-hand game :dealer)]
    (let [{:keys [dealer player soft-17?]} game
          hand-test (if soft-17? every-hand some-hand)]
      (print-interface game)
      (Thread/sleep 600)
      (if (or (hand-test dealer #(>= % 17))
              (some-hand player #(>= % 21)))
        (end-turn game :double? double?)
        (recur (play-hit game :dealer))))))

(defn double-down
  [game]
  (-> game
      (play-hit :player)
      (dealer-turn :double? true)))

(defn try-again []
  (println "Hmm, sorry, I didn't get that. Let's try again.")
  (Thread/sleep 1000))

(defn get-move
  [allow-double?]
  (prompt (format "What is your move? Your choices are hit, stay, %sand exit."
                  (if allow-double? "double down, " ""))))

(defn player-turn
  [game]
  (let [game (start-turn game)]
    (if (-> game :player twenty-one?)
      (end-turn game)
      (loop [game game]
        (let [allow-double? (zero? (:turns game))]
          (print-interface game)
          (case (get-move allow-double?)
                "exit" :quit
                "stay" (dealer-turn game)
                "hit" (let [game (play-hit game :player)
                            player (:player game)]
                        (if (or (busted? player)
                                (twenty-one? player))
                          (dealer-turn game)
                          (recur game)))
                "double down" (if allow-double?
                                (double-down game)
                                (do (try-again) (recur game)))
                (do (try-again) (recur game))))))))

(defn -main [& {:keys [soft-17?]}]
  (loop [game (new-game 500 *total-decks* soft-17?)]
    (if (= :quit game)
      "Goodbye!"
      (recur (player-turn game)))))
