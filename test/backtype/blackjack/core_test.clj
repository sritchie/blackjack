(ns backtype.blackjack.core-test
  (:refer-clojure :exclude [shuffle])
  (:use backtype.blackjack.core
        clojure.test))

(deftest new-deck-test
    (is (thrown? AssertionError (new-deck 0)))
    (is (thrown? AssertionError (new-deck -2)))
    (is (= 52 (count (new-deck 1))))
    (is (= 208 (count (new-deck 4)))))

(deftest new-game-test
  (is (thrown? AssertionError (new-game 500 100 2)))
  (is (thrown? AssertionError (new-game 500 100 10)))
  (is (thrown? AssertionError (new-game -100 100 6)))
  (is (map? (new-game 500 100 6))))

(defn mk-card
  ([rank] (mk-card rank true))
  ([rank showing?]
     {:suit :spades, :rank rank, :showing? showing?}))

(def example-game (new-game 500 100 6))

(deftest set-showing-test
  (let [up-cards (map mk-card [:two :ten :ten])
        down-cards (set-showing up-cards false)]
    (is (every? true? (map :showing? up-cards)))
    (is (every? false? (map :showing? down-cards)))))

(deftest card-manipulation
  (let [game (-> example-game
                 (deal-cards 5 :player))]
    (is (= 5 (count (:player game)))
        "Checking that deal happened.")
    (is (every? true?
                (->> (show-hand game :player)
                     :player
                     (map :showing?)))
        "All hands showing, after `show-hand`?")
    (is (empty? (:player (dump-hands game)))
        ":player should be empty after dumping.")
    (is (= 5 (count (:discard (dump-hands game))))
        "Discard should have 5 cards after `dump-hands`.")))

(deftest bet-test
  (let [bet-amount 20
        old-chips (:chips example-game)
        {:keys [chips bet-limit current-bet] :as game}
        (-> example-game (make-bet bet-amount))]
    (is (= bet-amount current-bet))
    (is (= (- old-chips bet-amount)
           chips))
    (is (thrown? AssertionError (make-bet example-game
                                          (inc chips))))
    (is (thrown? AssertionError (make-bet example-game
                                          (inc bet-limit))))
    (are [outcome chip-val] (= chip-val
                               (:chips (resolve-bet game outcome)))
         :lose (- old-chips bet-amount)
         :push old-chips
         :win (+ old-chips bet-amount)
         :surrender (- old-chips (/ bet-amount 2))
         :blackjack (+ old-chips (* bet-amount (/ 3 2))))
    (are [scale] (= (- old-chips (* scale bet-amount))
                    (:chips (scale-bet game scale)))
         2, 1, 0.5, 0.2, 10)))

(deftest play-hit-test
  (let [game (-> example-game (play-hit :player))]
    (is (= 1 (count (:player game))))
    (is (= (dec (count (:deck example-game)))
           (count (:deck game))))
    (is (= 2 (count (:player (-> game (play-hit :player))))))
    (is (= 1 (count (:dealer (-> game (play-hit :dealer))))))
    (is (= 0 (count (:dealer game))))))

(deftest score-hand-test
  (are [scores cards] (= scores (score-hand (map mk-card cards)))
       [1 11] [:ace]
       [2 12] [:ace :ace]
       [11 21] [:ace :ten]
       [12] [:two :jack]
       [13 23] [:eight :four :ace])
  (are [top cards] (= top (top-score (map mk-card cards)))
       11 [:ace]
       12 [:ace :ace]
       15 [:jack :two :ace :two]
       nil [:ten :ten :ten]))

(deftest hand-predicates-test
  (are [bool func cards] (= bool (func (map mk-card cards)))
       true busted? [:ten :five :five :five]
       false busted? [:ten :five :five :ace]
       false busted? [:ace :ace]
       false over-16? [:seven :ace]
       true over-16? [:seven :ace :king]
       true twenty-one? [:ace :king]
       true twenty-one? [:ace :king :king]
       false twenty-one? [:ace :five]))

(deftest push?-test
  (are [bool a-cards b-cards] (= bool (push? (map mk-card a-cards)
                                             (map mk-card b-cards)))
       true [:ace :king] [:ace :jack]
       false [:ace :five :two] [:jack :nine]
       false [:king :king] [:ace :jack]
       false [:ace :king] [:ace :jack :two]))

(deftest broke?-test
  (are [bool game] (= bool (broke? game))
       true (-> (new-game 50 50 6)
                (make-bet 50)
                (resolve-bet :lose))
       false (-> (new-game 50 50 6)
                 (make-bet 50)
                 (resolve-bet :win))))

(deftest game-outcome-test
  (let [game-seq (fn [kwd] (iterate #(play-hit % kwd) example-game))]
    (is (= :surrender (game-outcome example-game true)))
    (is (= :win (game-outcome (nth (game-seq :dealer) 10) false)))
    (is (= :lose (game-outcome (nth (game-seq :player) 10) false)))))

(deftest score-str-test
  (are [str cards] (= str (score-str (map mk-card cards)))
       "1/11" [:ace]
       "6/16" [:ace :five]
       "11/21" [:ace :five :five]
       "13" [:ace :five :five :two]
       "21" [:ten :ten :ace]))

(deftest get-move-test
  (let [choices (move-choices example-game)]
    (are [in-str choice] (= choice (with-in-str in-str
                                     (get-move choices)))
         "hit" :hit
         "stay" :stay
         "surrender" :surrender)))

(deftest get-bet-test
  (are [chips limit in-str bet] (= bet (with-in-str in-str
                                         (get-bet chips limit)))
       500 100 "exit" :exit
       500 100 "99" 99))

(deftest special-options-test
  (is (= [:surrender :double-down] (special-options example-game)))
  (is (nil? (-> example-game (play-hit :player) special-options))))

(deftest move-choices-test
  (is (= [:hit :stay :surrender :double-down] (move-choices example-game)))
  (is (= [:hit :stay] (-> example-game (play-hit :player) move-choices))))

(deftest initial-deal-test
  (are [kwd ct] (= ct (count (kwd (initial-deal example-game))))
       :player 2
       :dealer 2)
  (is (= (- (count (:deck example-game)) 4)
         (count (:deck (initial-deal example-game))))))


