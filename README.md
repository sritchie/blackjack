I'd like to map out the course of the initial game, given that we'll
be using text.

# BLACKJACK #

### Hitting ###

When the user hits:
* Get a card from the deck.
* Check if I busted.
* Go back to beginning of the turn.

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

* If I stay, here's the dealer's case:

        (loop []
          (cond (blackjack? dealer-hand) (if (blackjack? player-hand) "Push!" "He wins.")
                (bust? dealer-hand) "I win."
                (under-17? dealer-hand)  (do (do-hit dealer-hand)
                                             (recur))
                (check-winners dealer-hand player-hand))
