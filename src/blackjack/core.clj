(ns blackjack.core)

(def suits
  #{:hearts :spades :clubs :diamonds})

(def ranks {:two 2
            :three 3
            :four 4
            :five 5
            :six 6
            :seven 7
            :eight 8
            :nine 9
            :ten 10 :jack 10 :queen 10 :king 10
            :ace 11})

(def deck
  (for [suit suits
        rank (keys ranks)]
    {:suit suit :rank rank :showing? false}))

(defn new-deck
  "Returns a new, shuffled deck."
  []
  (shuffle deck))

(defn boost
  "Adds a new deck to an existing deck."
  [deck]
  (into deck (new-deck)))

(defn rank-hand
  "Returns the two possible ranks of any given deal. "
  [card-seq]
  (reduce (fn [[r1 r2] card]
            (if (= card :ace)
              [(inc r1) (+ r1 11)]
              [(+ r1 (card ranks)) (+ r2 (card ranks))]))
          [0 0]
          (for [card card-seq]
            (:rank card))))

(defn test-hand
  "Shows the rank of an initial deal."
  [deck]
  (rank-hand (map #(assoc % :showing? true)
                  (take 2 deck))))

(defn percent-chance
  [score n]
  (let [hand-seq (filter #(= score %)
                         (flatten
                          (for [_ (range n)]
                            (->> (new-deck)
                                 (partition 2)
                                 (map (comp second rank-hand))))))
        cts (count hand-seq)]
    (/ cts (* n 26.))))

(def test-deck (ref [0 1 2 3 4]))
(def test-hand (ref []))

;; ## Game Loop Functions.

(defn valid? [input]
  (when (= input "face")
    "Excellent, paduan."))

(defn prompt [message]
  (println message)
  (read-line))

(defn get-username []
  (prompt "What is your username?"))

(defn get-move []
  (prompt "What is your move? Your choices are hit, stay, or exit."))

(defn run []
  (let [user-name (get-username)]
    (loop []
      (let [move (get-move)]
        (if (= move "exit")
          (println "Goodbye :(")
          (do (println (case move
                             "exit" 
                             "hit"  "impressive."
                             "stay" "Nice move!"
                             "Sorry, didn't understand that one."))
              (recur)))))))
