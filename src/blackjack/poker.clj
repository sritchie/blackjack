(ns blackjack.poker)
;; We need a function that can evaluate a hand, based on the various
;; types of hands! This is for five-card-draw, though. We're not
;; playing poker, we're just playing blackjack.

(def hands [:royal-flush
            :straight-flush
            :four-of-a-kind
            :full-house
            :flush
            :straight
            :three-of-a-kind
            :two-pair
            :pair
            :high-card])
