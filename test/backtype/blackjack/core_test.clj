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




