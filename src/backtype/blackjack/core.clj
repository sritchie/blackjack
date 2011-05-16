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
(def empty-discard [])

(defn new-game
  "Initializes a new game of Blackjack."
  [chips bet-limit decks]
  {:pre [(>= decks 4) (<= decks 8)]}
  {:deck (new-deck decks)
   :discard empty-discard
   :player empty-hand
   :dealer empty-hand
   :chips chips
   :bet-limit bet-limit
   :current-bet 0
   :turns 0})

;; ## Game Play Mechanics

(defn set-cards-showing
  [cards bool]
  (map #(assoc % :showing? (boolean bool))
       cards))

(defn set-hand-showing
  [bool game hand-kwd]
  (assoc game
    hand-kwd (set-cards-showing (hand-kwd game)
                                bool)))

(def show-hand (partial set-hand-showing true))
(def hide-hand (partial set-hand-showing false))

(defn deal-cards
  "Deals a card from the supplied deck to the player referenced by
  `hand-kwd`. If the deck is empty, the deck will be refreshed before
  dealing a card out to the players."
  [game n hand-kwd & {:keys [show?]}]
  {:pre [(-> game :deck count (>= n)), (hand-kwd game)]}
  (let [[deck hand] (map game [:deck hand-kwd])
        [cards new-deck] (split-at n deck)
        cards (set-cards-showing cards show?)]
    (assoc game
      hand-kwd (into hand cards)
      :deck new-deck)))

(defn dump-hands
  "Dumps hands into discard, and sets the hands back to their fresh,
  empty state."  [game]
  (let [deck (:deck game)
        discard (reduce into (map game [:discard :dealer :player]))
        [deck discard] (if (< (count deck) 52)
                         [(shuffle deck discard) empty-discard]
                         [deck discard])]
    (assoc game
      :deck deck
      :discard discard
      :dealer empty-hand
      :player empty-hand
      :turns 0)))

;; ### Hand Scoring

(defn score-hand
  "Returns up to two possible scores for the supplied deal. An ace can
  never count as 11 more than once, as this would cause an instant
  bust -- we accept an optional argument that returns the highest
  possible value of rank, based on the presence of an ace."
  [hand]
  (let [rank-seq (map :rank hand)
        score (reduce (fn [acc card]
                        (+ acc (card ranks)))
                      0
                      rank-seq)]
    (if (some #{:ace} rank-seq)
      [score (+ 10 score)]
      [score])))

(defn top-score
  [hand]
  (last (filter #(<= % 21)
                (score-hand hand))))

(defn busted? [hand]
  (every? #(> % 21) (score-hand hand)))

(defn over-16? [hand]
  (every? #(>= % 17) (score-hand hand)))

(defn twenty-one? [hand]
  (some #{21} (score-hand hand)))

(defn push? [hand1 hand2]
  (= (top-score hand1)
     (top-score hand2)))

(defn beats? [hand1 hand2]
  (and (not (busted? hand1))
       (> (top-score hand1)
          (top-score hand2))))

(defn broke? [{:keys [chips current-bet]}]
  (zero? (+ chips current-bet)))

(defn get-outcome
  [{:keys [dealer player turns]} surrender?]
  (cond surrender? :surrender
        (busted? dealer) :win
        (push? dealer player) :push
        (beats? player dealer) (if (and (zero? turns)
                                        (twenty-one? player))
                                 :blackjack
                                 :win)
        :else :lose))

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

(defn get-move
  [choices]
  (let [opt (keyword
             (prompt (format "What is your move? Your choices are %s, and %s."
                             (->> (butlast choices)
                                  (map name)
                                  (interpose ", ")
                                  (apply str))
                             (name (last choices)))))]
    (if-let [choice (some #{opt} choices)]
      choice
      (do (println "Hmm, sorry, I didn't get that. Let's try again.")
          (recur choices)))))

(defn get-bet
  [chips bet-limit]
  (let [limit (if (< chips bet-limit) chips bet-limit)
        bet (try (Integer.
                  (prompt (format "How many chips (up to %d) would you like to bet?"
                                  limit)))
                 (catch Exception e :invalid))]
    (if-let [statement (cond
                        (= :invalid bet) "Sorry, that input seems to be invalid."
                        (<= bet 0) "Only positive bets, please."
                        (> bet bet-limit) (str bet-limit " or fewer, please.")
                        (neg? (- chips bet)) "Not enough funding for that!")]
      (do (println statement)
          (recur chips bet-limit))
      bet)))

(defn special-options
  [{:keys [turns chips current-bet bet-limit player]}]
  (let [first-turn? (zero? turns)
        funding? (and (>= chips current-bet)
                      (<= (* 2 current-bet) bet-limit))]
    (when first-turn?
      (if funding?
        [:surrender :double-down]
        [:surender]))))

(defn move-choices
  "These are all possible moves -- special moves "
  [game]
  (reduce into [:hit :stay]
          [(special-options game)
           [:exit]]))

;; ### Betting

(defn make-bet
  [{:keys [chips bet-limit] :as game} bet]
  {:post [(>= (:chips %) 0)
          (= bet (:current-bet %))
          (<= bet (:bet-limit %))]}
  (assoc game
    :chips (- chips bet)
    :current-bet bet))

(defn resolve-bet
  [{:keys [chips current-bet] :as game} result]
  (let [pay (-> (case result
                      :surrender (/ 1 2)
                      :blackjack (/ 5 2)
                      :win 2
                      :push 1
                      :lose 0)
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
  (let [{:keys [chips bet-limit]} game]
    (-> game
        print-interface
        (make-bet (get-bet chips bet-limit))
        initial-deal)))

(defn end-turn
  [game & {:keys [surrender?]}]
  (let [result (get-outcome game surrender?)
        ret-game (-> game
                     (show-hand :dealer)
                     (resolve-bet result)
                     print-interface
                     (print-outcome result)
                     dump-hands)]
    (prompt "Please hit enter to proceed.")
    ret-game))

(defn play-hit
  [game hand-kwd]
  (-> game
      (deal-cards 1 hand-kwd :show? true)
      (assoc :turns (-> game :turns inc))))

(declare dealer-turn)
(defn double-down
  [{:keys [current-bet chips] :as game}]
  (-> game
      (assoc :current-bet (* 2 current-bet))
      (assoc :chips (- chips current-bet))
      (play-hit :player)
      dealer-turn))

(defn dealer-turn
  [game]
  (loop [{:keys [dealer player] :as game} (show-hand game :dealer)]
    (print-interface game)
    (Thread/sleep 600)
    (if (or (over-16? dealer)
            (busted? player))
      (end-turn game)
      (recur (play-hit game :dealer)))))

(defn player-turn
  [game]
  (if (-> game :player twenty-one?)
    (end-turn game)
    (loop [game game]
      (print-interface game)
      (case (-> game move-choices get-move)
            :exit :quit
            :stay (dealer-turn game)
            :double-down (double-down game)
            :surrender (end-turn game :surrender? true)
            :hit (let [game (play-hit game :player)
                       player (:player game)]
                   (if (or (busted? player)
                           (twenty-one? player))
                     (dealer-turn game)
                     (recur game)))
            (recur game)))))

(defn -main
  ([] (-main 6 100))
  ([decks]
     (-main decks 100))
  ([decks bet-limit]
     (loop [game (new-game 500 bet-limit decks)]
       (cond (= game :quit) (println "Goodbye!")
             (broke? game)  (println "Sorry, you're out of chips!")
             :else          (recur (-> game start-turn player-turn))))))
