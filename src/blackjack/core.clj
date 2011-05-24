;;; ## Functional Blackjack
;;;
;;; This namespace contains all code necessary to run a functional,
;;; text-based game of blackjack. I've implemented support for each of
;;; the basic rules, plus support for surrendering and doubling
;;; down. I decided to try and limit state as much as possible --
;;; blackjack's pure functions take in a game, and return a new game
;;; updated to reflect the new state. The game loop is realized by
;;; recursing a game through the player and dealer turn loops until
;;; the player either quits or busts.
;;;
;;; ### Assumptions and Notes
;;;
;;; I've forced the dealer to hit below 17 (soft or not.) I've also
;;; offered the player the option to surrender early; on surrender,
;;; the player receives half of the initial bet, whether or not the
;;; dealer had a blackjack.
;;;
;;; My doubling down implementation allows for doubled bets on the
;;; first turn, provided that the player can afford the doubled
;;; bet. If not, he's not provided with the option. I do allow doubled
;;; bets that bring the total bet over the limit of 100, as is
;;; standard, I believe.
;;;
;;; If the total number of chips drops below the upper limit, I adjust
;;; the upper limit down, so no bet is ever allowed to exceed the
;;; total number of chips left.
;;;
;;; Rather than pull from an infinite, random sequence of cards, I
;;; chose to use a finite number of decks (limited to between 4 and 8)
;;; which reshuffle themselves after some lower limit is reached at
;;; the end of a turn-- 52 cards, by default. I did this with an eye
;;; toward incorporating a card counting trainer, down the road.

(ns blackjack.core
  (:refer-clojure :exclude [shuffle])
  (:use [clojure.java.shell :only (sh)])
  (:require [clojure.string :as s])
  (:gen-class))

;; ### Data Structures

(def suits #{:hearts :spades :clubs :diamonds})

(def ranks
  (merge (zipmap [:ace :two :three :four :five
                  :six :seven :eight :nine :ten]
                 (iterate inc 1))
         {:jack 10 :queen 10 :king 10}))

(def players #{:dealer :player})
(def player? (comp boolean players))

(defn shuffle
  "Returns a vector containing each of the supplied decks, shuffled
  into some random order."
  [& decks]
  (clojure.core/shuffle (reduce into decks)))

(defn new-deck
  "Returns one deck containing `n` 52-card decks, shuffled into random
  order."
  [n]
  {:pre [(pos? n)]}
  (->> (for [suit suits, rank (keys ranks)]
         {:suit suit :rank rank :showing? false})
       (repeat n)
       (apply shuffle)))

(def empty-hand [])
(def empty-discard [])

(defn new-game
  "Returns a data representation of a new game of blackjack. The
  player will have access to the supplied number of chips, and will be
  able to place bets up to the supplied `bet-limit`. The supplied
  number of decks will be used. (Note that `new-game` between 4 and 8
  decks.)"
  [chips bet-limit decks]
  {:pre [(>= decks 4), (<= decks 8)
         (pos? chips), (pos? bet-limit)]}
  {:deck (new-deck decks)
   :discard empty-discard
   :player empty-hand
   :dealer empty-hand
   :chips chips
   :card-limit 52
   :bet-limit bet-limit
   :current-bet 0
   :turns 0})

(def outcome-map
  {:surrender {:message "You surrendered."
               :payout (/ 1 2)}
   :blackjack {:message "Blackjack!"
               :payout (/ 5 2)}
   :push      {:message "Push!"
               :payout 1}
   :win       {:message "You win."
               :payout 2}
   :lose      {:message "Dealer wins."
               :payout 0}})

;; ### Game Modifiers
;;
;; The functions in this section serve to perform some action on the
;; supplied game, returning a new game containing all changes. To
;; perform multiple actions, we can thread a game through a number of
;; forms, creating the illusion of state.

(defn set-showing
  "For each card in `cards`, sets the values of `:showing?` to the
   truthy value of `bool`."
  [cards bool]
  (map #(assoc % :showing? (boolean bool))
       cards))

(defn show-hand
  "Returns a new game with all cards for the referenced player flipped
  face up. (Note that `player` must be either `:player` or
  `:dealer`)."
  [game player]
  {:pre [(player? player)]}
  (assoc game
    player (set-showing (player game)
                        true)))

(defn deal-cards
  "Returns a new game generated by dealing `n` cards to the player
   referenced by `player`"
  [game n player & {:keys [show?]}]
  {:pre [(player? player)
         (-> game :deck count (>= n))]}
  (let [[cards new-deck] (split-at n (:deck game))
        cards (set-showing cards show?)]
    (assoc game
      player (into (player game) cards)
      :deck new-deck)))

(defn dump-hands
  "Returns a new game generated by dumping the player's and dealer's
  hands into the discard pile. If the number of cards in the deck has
  fallen below the value of `:card-limit` for the supplied game, the
  discard pile will be shuffled back into the deck."
  [{:keys [deck discard dealer player card-limit] :as game}]
  (let [discard (reduce into discard [dealer player])
        [deck discard] (if (< (count deck) card-limit)
                         [(shuffle deck discard) empty-discard]
                         [deck discard])]
    (assoc game
      :deck deck
      :discard discard
      :dealer empty-hand
      :player empty-hand
      :turns 0)))

(defn make-bet
  "Returns a new game generated by placing a bet of the supplied
  amount for player."
  [{:keys [chips bet-limit] :as game} bet]
  {:post [(>= (:chips %) 0)
          (= bet (:current-bet %))
          (<= bet (:bet-limit %))]}
  (assoc game
    :chips (- chips bet)
    :current-bet bet))

(defn resolve-bet
  "Returns a new game generated by calculating the payoff for the
  supplied result based on the current bet, and adding that payoff
  back into the pool. If the game was a loss, all chips disappear, of
  course."
  [{:keys [chips current-bet] :as game} result]
  {:pre [(contains? outcome-map result)]}
  (let [pay (->> (get-in outcome-map [result :payout])
                 (* current-bet))]
    (assoc game
      :chips (+ chips (int pay))
      :current-bet 0)))

(defn scale-bet
  "Returns a new game generated by scaling the current bet by the
   supplied scale factor. Bets can't be scaled by a factor that would
   cause the chips balance to drop below zero."
  [{:keys [current-bet chips] :as game} scale]
  {:pre [(>= (+ chips current-bet)
             (int (* scale current-bet)))]}
  (let [new-bet (int (* scale current-bet))]
    (assoc game
      :current-bet new-bet
      :chips (+ chips current-bet (- new-bet)))))

(defn play-hit
  "Returns a new game generated by dealing a single, face-up card to
  the supplied player."
  [game player]
  {:pre [(player? player)]}
  (-> game
      (deal-cards 1 player :show? true)
      (assoc :turns (-> game :turns inc))))

;; ### Hand Scoring and Predicates

(defn score-hand
  "Returns a vector of up to two possible scores for the supplied
  hand; if an ace exists in the hand and a value of 11 wouldn't cause
  a bust, two scores are returned. (An ace can never count as 11 more
  than once, as this would cause an instant bust)."
  [hand]
  (let [rank-seq (map :rank hand)
        score (reduce + (map ranks rank-seq))]
    (into [score]
          (when (some #{:ace} rank-seq)
            [(+ 10 score)]))))

(defn score-str
  "Returns a string representation of every possible score of the
  supplied hand."
  [hand]
  (->> hand
       (score-hand)
       (filter #(<= % 21))
       (s/join "/")))

(defn top-score
  "Returns the greatest legal score (21 or under) possible with the
  supplied hand."
  [hand]
  (last (filter #(<= % 21)
                (score-hand hand))))

(defn busted? [hand]
  (every? #(> % 21) (score-hand hand)))

(defn over-16? [hand]
  (every? #(>= % 17) (score-hand hand)))

(defn twenty-one? [hand]
  (boolean (some #{21} (score-hand hand))))

(defn push?
  "Returns true of the top scores of the two supplied hands match,
  false otherwise."
  [hand1 hand2]
  (= (top-score hand1)
     (top-score hand2)))

(defn beats?
  "Returns true if the `hand1` achieve a higher score than `hand2`
  without busting, false otherwise."
  [hand1 hand2]
  (and (not (busted? hand1))
       (> (top-score hand1)
          (top-score hand2))))

(defn broke?
  "Returns true of the supplied game contains no chips, between the
  pool and the current bet, and false otherwise."
  [{:keys [chips current-bet]}]
  (zero? (+ chips current-bet)))

;; ### Game State Information

(defn game-outcome
  "Returns a keyword representation of the outcome of the supplied
  game. The boolean `surrender?` indicates whether or not the supplied
  game was surrendered by the user."
  [{:keys [dealer player turns]} surrender?]
  (cond surrender? :surrender
        (busted? dealer) :win
        (push? dealer player) :push
        (beats? player dealer) (if (and (zero? turns)
                                        (twenty-one? player))
                                 :blackjack
                                 :win)
        :else :lose))

(defn special-options
  "Returns a sequence of non-standard blackjack options, based on the
  state of the supplied game.

  Different variations on the game allow different moves, such as
  doubling down, only on the first turn. All such special cases are
  covered within."
  [{:keys [turns chips current-bet]}]
  (when (zero? turns)
    (into [:surrender]
          (when (>= chips current-bet)
            [:double-down]))))

(defn move-choices
  "Returns a sequence of all possible moves, given the current state
  of the supplied game."
  [game]
  (into [:hit :stay] (special-options game)))

;; ### Interface Representations

(defn print-outcome
  "Writes the appropriate outcome message for the supplied key
  `outcome` the to the output stream."
  [game outcome]
  {:pre [(contains? outcome-map outcome)]}
  (println (get-in outcome-map [outcome :message]) "\n")
  game)

(defn print-hand
  "Writes a text representation of the supplied hand to the output
  stream. The value of cards with `:showing?` set to false will not be
  revealed."
  [hand]
  (doseq [card hand :let [{:keys [suit rank showing?]} card]]
    (println (if-not showing?
               "Hidden card."
               (format "%s of %s"
                       (name rank)
                       (name suit))))))

(defn print-hands
  "For the supplied game, prints a text representation of the player's
  and dealer's hands to the output stream, along with a header line
  and a report of the current showing points. Other than output side
  effects, acts as identity and returns game."
  [{:keys [dealer player] :as game}]
  (when-not (empty? dealer)
    (doseq [[holder title] [[dealer "Dealer's"]
                            [player "Your"]]
            :let [points (score-str (filter :showing? holder))]]
      (println (str title " hand"
                    (if (= points "")
                      " (a bust!)"
                      (format ", showing %s points:" points))))
      (print-hand holder)
      (newline)))
  game)

(defn print-betline
  "Prints a text representation of the current number of remaining
  chips and the current bet to the output stream. Other than output
  side effects, acts as identity and returns game."
  [{:keys [chips current-bet] :as game}]
  (print (format "You have %d chips left. " chips))
  (when (pos? current-bet)
    (print (format "Your current bet is %d." current-bet)))
  (print "\n\n")
  game)

(defn print-interface
  "Clears the terminal screen, and prints a text representation of the
  entire game interface to the output stream. Note that this function
  will only succeed if run from a unix terminal capable of accepting
  the `clear` command."
  [game]
  (-> "clear" sh :out println)
  (-> game print-hands print-betline))

;; ### Decision Inputs

(defn prompt [message]
  (println message)
  (read-line))

(defn get-move
  "Obtains a choice from among the supplied `choices` sequence by
  querying the current value of *in*."
  [choices]
  (let [prompt-str (format "What is your move? Your choices are %s, and %s."
                           (->> (butlast choices)
                                (map name)
                                (s/join ", "))
                           (name (last choices)))
        opt (keyword (prompt prompt-str))]
    (or (some #{opt} choices)
        (do (println "Hmm, sorry, that's invalid. Let's try again.")
            (recur choices)))))

(defn get-bet
  "Obtains a valid bet (based on the value of `chips` and the supplied
  bet limit) by querying the current value of *in*."
  [chips bet-limit]
  (let [limit (min chips bet-limit)
        in-str "How many chips (from 1 to %d) would you like to bet? (Or, type exit to quit.)"
        input (or (prompt (format in-str limit))
                  "exit") ; user typed EOF
        bet (try (Integer. input)
                 (catch Exception e input))
        statement (cond (string? bet) "Sorry, that input seems to be invalid."
                        (<= bet 0) "Only positive bets, please."
                        (> bet limit) (str limit " or fewer, please."))]
    (cond (= bet "exit") :exit
          statement (do (println statement)
                        (recur chips bet-limit))
          :else bet)))

;; ### Gameplay

(defn initial-deal
  "Returns a new game generated by dealing 2 cards to the dealer and
  the player. As is standard, the dealer gets to keep one card face
  down."
  [game]
  (-> game
      (deal-cards 2 :player :show? true)
      (deal-cards 1 :dealer :show? true)
      (deal-cards 1 :dealer :show? false)))

(defn start-turn
  "Returns a new game generated by accepting a bet from the user, and
  setting up the initial state of the game."
  [{:keys [chips bet-limit] :as game}]
  (let [bet (get-bet chips bet-limit)]
    (or (#{:exit} bet)
        (-> game
            (make-bet bet)
            dump-hands
            initial-deal))))

(defn end-turn
  "Returns a new game generated by resolving all bets, adjusting the
  chips pool accordingly, printing the outcome, and getting the game
  ready for the next go-around."
  [game & {:keys [surrender?]}]
  (let [result (game-outcome game surrender?)]
    (-> game
        (show-hand :dealer)
        (resolve-bet result)
        print-interface
        (print-outcome result)
        start-turn)))

(defn dealer-turn
  "Game loop of the dealer. Plays out a dealer's hand, following
  blackjack's rules for a dealer forced to hit on a soft 17, and
  returns the resulting game."
  [game]
  (loop [{:keys [dealer player] :as game} (show-hand game :dealer)]
    (print-interface game)
    (Thread/sleep 600)
    (if (or (over-16? dealer)
            (busted? player))
      (end-turn game)
      (recur (play-hit game :dealer)))))

(defn player-turn
  "The player's game loop. With appropriate input from the user, plays
  out a hand until the user busts, reaches 21, or calls stay. After
  this, the game will be resolved by allowing the daeler to play out a
  hand or jumping directly to bet resolution, as appropriate for the
  turn in question. Returns the state of the game reached by playing
  out the current turn."
  [game]
  (loop [game game]
    (print-interface game)
    (if (twenty-one? (:player game))
      (end-turn game)
      (case (-> game move-choices get-move)
            :stay (dealer-turn game)
            :surrender (end-turn game :surrender? true)
            :double-down (-> game
                             (scale-bet 2)
                             (play-hit :player)
                             dealer-turn)
            :hit (let [game (play-hit game :player)
                       player (:player game)]
                   (if (or (busted? player)
                           (twenty-one? player))
                     (dealer-turn game)
                     (recur game)))
            (recur game)))))

(defn game-loop
  "Full game loop, housing the internal player and dealer loops and
  allowing each to simulate state by continually recursing through
  each with a game of blackjack. Exits when the user quits or runs out
  of money."
  [& {:keys [chips bet-limit decks]
      :or {chips 500, bet-limit 100, decks 6}}]
  (loop [game (-> (new-game chips bet-limit decks)
                  print-interface
                  start-turn)]
    (cond (= game :exit) (println "Goodbye!")
          (broke? game)  (println "Sorry, you're out of chips!")
          :else          (recur (player-turn game)))))

(defn -main
  [& args]
  (apply game-loop (map read-string args)))
