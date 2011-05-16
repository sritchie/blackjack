I'd like to map out the course of the initial game, given that we'll
be using text.

TODO:

* Docs
* splitting

* Mention early surrender allowed.
* CONSIDER what happens if someone doesn't have enough money to
  double!
* Force a quit at the end.
* Use keywords instead of strings in the case statement, since we're
* getting the move.
* Separate user input from the functions that work on it
http://www.pagat.com/banking/blackjack.html

# BLACKJACK #

### Hitting ###

When the user hits:

    (loop []
      (print-hands dealer-hand player-hand)
      (let [move (get-move)]    
        (case move
              "hit" (do (do-hit player-hand)
                        (cond (blackjack? player-hand) "automatic stay."
                        (bust? player-hand) "you busted."
                        (recur)))
               "stay" (enact-dealer-hand)
               "exit" "Goodbye!"
               "Hmm, sorry, I didn't get that.")))

### Staying ###

If I stay, here's the dealer's case:

    (defn check-winners
          [dealer-hand player-hand]
          (cond (push? dealer-hand player-hand) "Push!"
                (blackjack? player-hand) "Blackjack!"
                (beats? player-hand dealer-hand) "Player wins."
                :else "Dealer wins."))

    (defn enact-dealer
          [dealer-hand player-hand]
            (loop []
              (print-hands dealer-hand player-hand)
              (if (17-or-over? dealer-hand)
                  (check-winners dealer-hand player-hand)
                  (do (do-hit dealer-hand)
                      (recur)))))
