I'd like to map out the course of the initial game, given that we'll
be using text.

TODO:

* gotta implement soft vs hard push. DONE!

* Early surrender allowed.

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


;; TODO: deal with the fact that the deck might become empty. We can't
;; just add a new deck! But once the six decks are exhausted, we're
;; going to want to replace the deck. Do we want to do that in this
;; function? Not really sure about that.
;;
;; TODO: Perhaps we should check the count of the decks -- if we play
;; a hand that gets down below three decks, or maybe below one deck,
;; after the turn we shuffle the discards back into the deck. This has
;; to reset our card counting, of course.
