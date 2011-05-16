(ns backtype.blackjack.core
  (:refer-clojure :exclude [shuffle])
  (:use [clojure.java.shell :only (sh)])
  (:gen-class))

;; ## Blackjack Data Structures

(def suits #{:hearts :spades :clubs :diamonds})

(def ranks
  (merge (zipmap [:ace :two :three :four :five
                  :six :seven :eight :nine :ten]
                 (map inc (range)))
         {:jack 10 :queen 10 :king 10}))

(def deck-seq
  (for [suit suits
        rank (keys ranks)]
    {:suit suit :rank rank :showing? false}))

(defn shuffle
  "Shuffles together any number of supplied decks."
  [& decks]
  (clojure.core/shuffle (reduce into decks)))

(defn new-deck
  "Returns `n` decks shuffled together."
  [n]
  {:pre [(pos? n)]}
  (->> deck-seq
       (repeat n)
       (apply shuffle)))

(def empty-hand [])
(def new-discard [])

(defn new-game
  "Initializes a new game of Blackjack."
  [chips decks]
  {:deck (new-deck decks)
   :discard new-discard
   :player empty-hand
   :dealer empty-hand
   :chips chips
   :current-bet 0
   :turns 0})

;; ## Game Play Mechanics

(defn set-cards-showing
  [cards bool]
  {:pre [(contains? #{true false} bool)]}
  (map #(assoc % :showing? bool)
       cards))

(defn set-hand-showing
  [bool game hand-kwd]
  (assoc game
    hand-kwd (set-cards-showing (hand-kwd game)
                                bool)))

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
        cards (set-cards-showing cards (boolean show?))]
    (assoc game
      hand-kwd (into hand cards)
      :deck new-deck)))

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

(defn get-outcome
  [game surrender?]
  (let [{:keys [dealer player turns]} game]
    (cond surrender? :surrender
          (busted? dealer) :win
          (push? dealer player) :push
          (beats? player dealer) (if (and (zero? turns)
                                          (twenty-one? player))
                                   :blackjack
                                   :win)
          :else :lose)))

;; ## Text Representations

(defn print-outcome
  [game outcome]
  (case outcome
        :surrender (println "Player surrendered.")
        :blackjack (println "Blackjack!")
        :push (println "Push!")
        :win (println "Player wins.")
        :lose (println "Dealer wins."))
  game)

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

(defn score-str
  "Returns a string representation of the score of the game."
  [hand]
  (let [[score-a score-b] (filter #(<= % 21)
                                  (score-hand hand))]
    (when score-a
      (apply str score-a (when score-b ["/" score-b])))))

(defn print-hands
  [game]
  (let [{:keys [dealer player]} game]
    (when-not (empty? dealer)
      (doseq [[holder title] [[dealer "Dealer's"]
                              [player "Your"]]
              :let [points (->> holder
                                (filter :showing?)
                                score-str)]]
        (println (str title " hand"
                      (if points
                        (format ", showing %s points:" points)
                        " (a bust!)")))
        (print-hand holder)))
    game))

(defn print-betline
  [game]
  (let [{:keys [chips current-bet]} game]
    (println (format "You have %d chips left. Your current bet is %d.\n"
                     chips current-bet))
    game))

(defn print-interface
  [game]
  (-> "clear" sh :out println)
  (-> game print-hands print-betline))

;; ## Game Loop Functions.

(defn prompt [message]
  (println message)
  (read-line))

(defn try-again []
  (println "Hmm, sorry, I didn't get that. Let's try again.")
  (Thread/sleep 1000))

(defn get-move
  [first-turn?]
  (prompt (format "What is your move? Your choices are hit, stay, %sand exit."
                  (if first-turn? "double down, surrender, " ""))))

;; ### Betting

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
                      :surrender (/ 1 2)
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
  [game & {:keys [surrender? double?]}]
  (let [result (get-outcome game surrender?)
        ret-game (-> game
                     (show-hand :dealer)
                     (resolve-bet result double?)
                     print-interface
                     (print-outcome result)
                     dump-hands)]
    (prompt "Please hit enter to play again.")
    ret-game))

(defn play-hit
  [game hand-kwd]
  (-> game
      (deal-cards 1 hand-kwd :show? true)
      (assoc :turns (-> game :turns inc))))

(declare dealer-turn)
(defn double-down
  [game]
  (-> game
      (play-hit :player)
      (dealer-turn :double? true)))

(defn surrender
  [game]
  (end-turn game :surrender? true))

(defn dealer-turn
  "Dealer takes his turn, following the rules of soft 17."
  [game & {:keys [double?]}]
  (loop [game (show-hand game :dealer)]
    (let [{:keys [dealer player]} game]
      (print-interface game)
      (Thread/sleep 600)
      (if (or (every-hand dealer #(>= % 17))
              (some-hand player #(>= % 21)))
        (end-turn game :double? double?)
        (recur (play-hit game :dealer))))))

(defn player-turn
  [game]
  (if (-> game :player twenty-one?)
    (end-turn game)
    (loop [game game]
      (print-interface game)
      (let [first-turn? (zero? (:turns game))
            move (get-move first-turn?)]
        (case move
              "exit" :quit
              "stay" (dealer-turn game)
              "hit" (let [game (play-hit game :player)
                          player (:player game)]
                      (if (or (busted? player)
                              (twenty-one? player))
                        (dealer-turn game)
                        (recur game)))
              (if first-turn?
                (case move
                      "double down" (double-down game)
                      "surrender" (surrender game))
                (do (try-again) (recur game))))))))

(defn -main
  ([] (-main 6))
  ([decks]
     (loop [game (new-game 500 decks)]
       (if (= :quit game)
         "Goodbye!"
         (recur (player-turn (start-turn game)))))))
