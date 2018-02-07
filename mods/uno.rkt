#lang racket

(provide uno draw-play)

(require irc)
(require db)

(require "../conf/db.rkt")

(require "../lib/userinfo.rkt")
(require "../lib/utils.rkt")
(require "../lib/structs.rkt")
(require "../lib/colours.rkt")

;;;; TODO:
;; make %uno yes/no call a procedure stored in a variable instead
;; modularize the house rules instead of defining them and their
;;  description explicitly every time.

;;; house rules (hashes #channel . bool)
(define freewd4? (make-hash))

(define (reset-homerules channel)
 (hash-set! freewd4? channel #f)
)

; key is #channel, val is an int
; 0: not playing
; 1: setting up
; 2: playing
(define channel-status (make-hash))

(define play-allowed? (make-hash))
(define challenge-threads (make-hash))
(define top-prev-colour (make-hash))

; key in #channel, val is the ID of the card that was drawn, or 0 if no card was drawn.
(define card-drawn? (make-hash))

; key is #channel, val is a (list playername ...)
(define players (make-hash))

(define (get-current-turn msg channel)
 (string-append "it's " (car (hash-ref! players channel '())) "'s turn")
)

(define (shift-players channel)
 (hash-set! players channel
  (append (cdr (hash-ref! players channel '())) (list (car (hash-ref! players channel '()))))
 )
)

(define (apply-specials msg channel player first-card?)
 (let ((type (query-value *db* "SELECT type FROM uno_topcard WHERE channel = $1" channel)))
  (cond
   ((query-value *db* "SELECT special FROM uno_topcard WHERE channel = $1" channel)
    (begin
     (query-exec *db* "UPDATE uno_topcard SET special = false WHERE channel = $1" channel)
     ((hash-ref specattrs type void) msg channel player first-card?)
    )
   )
  )
 )
)

(define (channel-enabled? channel)
 (query-maybe-value *db* "SELECT allowed FROM uno_channels WHERE channel = $1" channel)
)

(define (change-uno-status msg nick channel allowed)
 (let
  ((status (hash-ref! channel-status channel 0)))
  (cond
   ((= status 1)
    (reply msg "You can't disable uno while a game is starting. As a mod, use %uno stop to stop the game."))

   ((= status 2)
    (reply msg "You can't disable uno while there's a game going on. As a mod, use %uno stop to stop the game."))

   ((not (is-user-mod? nick channel))
    (reply msg "You need to be a channel op or hop to enable or disable uno."))
 
   (else
    (begin
     (if (query-maybe-value *db* "SELECT channel FROM uno_channels WHERE channel = $1" channel)
      (query-exec *db* "UPDATE uno_channels SET allowed = $2 WHERE channel = $1" channel allowed)
      (query-exec *db* "INSERT INTO uno_channels VALUES ( $1, $2 )" channel allowed)
     )
     (reply msg (string-append "uno is now " (if allowed "enabled" "disabled") " in " channel "."))
    )
   )
  )
 )
)

(define (init-deck channel)
 (query-exec *db* "INSERT INTO uno_decks ( channel, id ) ( SELECT $1, id FROM uno_whole_deck )" channel)
)

(define (rebuild-deck channel)
 (query-exec *db* "INSERT INTO uno_decks ( channel, id ) ( SELECT $1, id FROM uno_whole_deck WHERE id NOT IN ( SELECT id FROM uno_hands WHERE channel = $1 ) AND id NOT IN ( SELECT id FROM uno_topcard WHERE channel = $1 ) )"  channel)
)

(define (uno-cl text colour)
 (string-append
  clbold
  (if (equal? colour "wild")
   text
   (colourize text colour)
  )
  clbold
 )
)

(define (uno-cl-fromname name)
 (let ((colour (query-value *db* "SELECT colour FROM uno_whole_deck WHERE name = $1 LIMIT 1" name)))
  (uno-cl name colour)
 )
)

(define (draw-cards amount msg channel player turn?)
 (let ((text "You drew: "))
  (begin
   (for ((i (in-range amount)))
    (let*
     ((card-id (query-value *db* "SELECT id FROM uno_decks WHERE channel = $1 LIMIT 1 OFFSET floor(random()*(SELECT count(*) FROM uno_decks WHERE channel = $1))" channel))
      (card-name (query-value *db* "SELECT name FROM uno_whole_deck WHERE id = $1 LIMIT 1" card-id)))
     (begin

      (query-exec *db* "DELETE FROM uno_decks WHERE channel = $1 AND id = $2" channel card-id)
      (query-exec *db* "INSERT INTO uno_hands ( channel, player, id ) VALUES ( $1, $2, $3 )" channel player card-id)

      ; reinitialize the deck if it's empty
      (cond ((= (query-value *db* "SELECT count(*) FROM uno_decks WHERE channel = $1" channel) 0)
             (rebuild-deck channel)) 
      )

      (let ((clname (uno-cl-fromname card-name)))
       (if (= i (- amount 1))
        (set! text (string-append text clname "."))
        (set! text (string-append text clname ", "))
       )
      )

      (cond (turn? (hash-set! card-drawn? channel card-id)))

     )
    )
   )

  (notify player text)
  )
 )
)

(define (get-cardcount channel)
 (let
  ((text "")
   (players (hash-ref! players channel '())))
  (begin
   (for ((player (in-list players)))
    (let
     ((ccount (query-value *db* "SELECT count(*) FROM uno_hands WHERE player = $1 AND channel = $2" player channel)))
     (set! text
      (string-append text " " player "[" (~a ccount) "]")
     )
    )
   )
   (string-trim text)
  )
 )
)

(define (get-hand channel player prefix)
 (let* ((card-names
        (query-list *db* "SELECT name FROM uno_whole_deck WHERE EXISTS ( SELECT 1 FROM uno_hands WHERE channel = $1 AND player = $2 AND uno_hands.id = uno_whole_deck.id )" channel player))
       (text (string-append prefix " " (~a (length card-names)) " cards: ")))
  (begin
   (for ((i (in-range (length card-names))))
    (let ((card (uno-cl-fromname (list-ref card-names i))))
     (if (= i (- (length card-names) 1))
      (set! text (string-append text card "."))
      (set! text (string-append text card ", "))
     )
    )
   )
   text
  )
 )
)

(define (begin-turn-cardinfo channel)
 (let*-values
  (((player) (car (hash-ref! players channel '())))
   ((top-name top-colour) (vector->values (query-row *db* "SELECT name, colour FROM uno_topcard WHERE channel = $1" channel))))
  (begin
   (notify player (get-hand channel player "You have"))
   (string-append "top card is " (uno-cl-fromname top-name)
   (if (regexp-match? #px"wild" top-name)
    (string-append " (" (uno-cl top-colour top-colour) ")")
    ""
   )
   ".")
  )
 )
)

(define (begin-turn msg channel)
 (reply msg
  (string-append
   (get-current-turn msg channel)
   (let ((cardcount (get-cardcount channel)))
    (if (> (string-length cardcount) 250)
     (begin (reply msg cardcount) "")
     (string-append " (" cardcount ")")
    )
   )
   " | "
   (begin-turn-cardinfo channel)
  )
 )
)

(define (is-winner? player channel)
  (= (query-value *db* "SELECT count(*) FROM uno_hands WHERE channel = $1 AND player = $2" channel player) 0)
)

(define (end-turn msg channel)
 (let ((player (car (hash-ref! players channel '()))))
  (begin
   (hash-set! card-drawn? channel 0)

   (let
    ((plist (hash-ref! players channel '())))
    (apply-specials msg channel
     (if (not (null? (cdr plist)))
      (cadr plist)
      (car plist)
     ) #f
    )
   )

   (shift-players channel)

   (if (is-winner? player channel)
    (victor msg channel player)
    (begin-turn msg channel)
   )

  )
 )
)

(define (draw-two msg channel player first-card?)
 (begin
  (shift-players channel)
  (draw-cards 2 msg channel player #f)
  (reply msg (string-append player " has drawn 2 cards, and their turn will be skipped."))
 )
)

(define (wd4 msg channel player)
 (begin
  (shift-players channel)
  (draw-cards 4 msg channel player #f)
  (reply msg (string-append player " has drawn 4 cards, and their turn will be skipped."))
 )
)

(define (wd4-legal? channel player)
 (query-maybe-value *db* "SELECT true FROM uno_hands WHERE channel = $1 AND player = $2 AND EXISTS ( SELECT 1 FROM uno_whole_deck WHERE uno_hands.id = uno_whole_deck.id AND colour = $3 ) LIMIT 1" channel player (hash-ref! top-prev-colour channel "null"))
)

(define (wd4-challenge msg channel victim caller)
 (begin
  (notify victim (get-hand channel caller (string-append caller " has")))

  (if (wd4-legal? channel caller)
   (begin
    (draw-cards 4 msg channel caller #f)
    (reply msg (string-append "The challenge succeeded! " caller "'s hand was revealed to " victim " and they have drawn 4 cards!"))
   )
   (begin
    (shift-players channel)
    (draw-cards 6 msg channel victim #f)
    (reply msg (string-append "The challenge failed! " victim " has seen " caller "'s hand and drawn 6 cards, and their turn will be skipped."))
   )
  )
 )
)

(define (wd4-challenge-setup msg channel victim caller)
 (begin
  (hash-set! play-allowed? channel #f)
  (hash-set! challenge-threads channel (current-thread))
  (reply msg (string-append victim ", would you like to challenge the wd4? %uno (yes/no)"))

  (let ((challenge? (thread-receive)))
   (if challenge?
    (wd4-challenge msg channel victim caller)
    (wd4 msg channel victim)
   )
  )

  (hash-set! play-allowed? channel #t)
 )
)

(define (wild-draw-four msg channel player first-card?)
 (let ((caller (car (hash-ref! players channel '()))))
  (begin
   (cond
    ((or
      (hash-ref! freewd4? channel #f)
      (is-winner? player channel)
      first-card?)
     (wd4 msg channel player))

    (else (wd4-challenge-setup msg channel player caller))
   )
  )
 )
)

(define (reverse-order msg channel player first-card?)
 (let ((plist (hash-ref! players channel '())))
  (begin
   (if (or first-card? (= (length plist) 2))
    (hash-set! players channel (reverse plist))
    (hash-set! players channel (append (list (car plist)) (reverse (cdr plist))))
   )
   (reply msg "Turn order was reversed!")
  )
 )
)

(define (skip-turn msg channel player first-card?)
 (begin
  (shift-players channel)
  (reply msg (string-append player "'s turn was skipped."))
 )
)

; special attributes are read from the card type
; and are sent to a function that handles them.
(define specattrs
 (hash
  "s" skip-turn
  "d2" draw-two
  "wd4" wild-draw-four
  "r" reverse-order
 )
)

(define (uno-err msg text)
 (begin
  (reply msg text)
  (kill-thread (current-thread))
 )
)


(define (print-current-players msg channel)
 (let ((text "Currently playing are: ")
       (plist (hash-ref! players channel '())))
  (begin
   (for ((i (in-range (length plist))))
    (if (= i (- (length plist) 1))
     (set! text (string-append text (list-ref plist i) "."))
     (set! text (string-append text (list-ref plist i) ", "))
    )
   )
   (reply msg text)
  )
 )
)


(define (init-topcard msg channel)
 (let*-values
  (((id) (query-value *db* "SELECT id FROM uno_decks WHERE channel = $1 AND EXISTS ( SELECT 1 FROM uno_whole_deck WHERE type != 'wd4' AND uno_decks.id = uno_whole_deck.id ) LIMIT 1 OFFSET floor(random()*(SELECT count(*) FROM uno_decks WHERE channel = $1 AND EXISTS (SELECT 1 FROM uno_whole_deck WHERE type != 'wd4' AND uno_decks.id = uno_whole_deck.id )))" channel))
   ((name type colour special?) (vector->values (query-row *db* "SELECT name, type, colour, special FROM uno_whole_deck WHERE id = $1" id))))
  (begin
   (query-exec *db* "INSERT INTO uno_topcard (channel, name, type, colour, special, id) VALUES ($1, $2, $3, $4, $5, $6)" channel name type colour special? id)
   (query-exec *db* "DELETE FROM uno_decks WHERE channel = $1 AND id = $2" channel id)
   (reply msg (string-append "Top card is a " (uno-cl-fromname name) "."))
   (apply-specials msg channel (car (hash-ref! players channel '())) #f)
   (hash-set! top-prev-colour channel colour)
  )
 )
)

(define (in-players? channel nick)
 (member nick (hash-ref! players channel '()))
)

(define (show-hand msg channel player)
 (let ((status (hash-ref! channel-status channel 0)))
  (cond
   ((= status 0)
    (reply msg "There's no game going on, what hand are you trying to see?"))

   ((= status 1)
    (reply msg "You didn't even get your hand yet! Calm down!"))

   ((not (in-players? channel player))
    (reply msg (string-append "You're not in the game, " player ".")))

   ((and (= status 2)
         (in-players? channel player))
    (notify player (get-hand channel player "You have")))
  )
 )
)

(define (pass-turn msg channel)
 (cond
  ((not (= (hash-ref! card-drawn? channel 0) 0))
   (end-turn msg channel)
  )
 )
)

(define (draw-play)
 (let* ((msg (thread-receive))
        (channel (car (irc-message-parameters msg)))
        (status (hash-ref! channel-status channel 0))
        (text (cadr (irc-message-parameters msg)))
        (args (cdr (string-split (cadr (irc-message-parameters msg)))))
        (nick (get-nick msg)))
  (cond
   ((not (channel-enabled? channel))
    (reply msg "This channel has uno disabled. Use %uno on to enable it."))

   ((= status 0)
    (reply msg "Nobody's playing uno..."))

   ((= status 1)
    (reply msg "Relax, we're starting the game! Geeze!"))

   ((not (hash-ref! play-allowed? channel #t))
    (reply msg "You can't play right now!"))

   ((not (in-players? channel nick))
    (reply msg (string-append "You're not in the game, " nick ".")))

   ((not (equal? (car (hash-ref! players channel '())) nick))
    (reply msg (string-append "It's not your turn yet, " nick "!")))

   ((and (regexp-match? #px"^%d\\w*\r$" text)
         (= (hash-ref! card-drawn? channel 0) 0))
    (draw-cards 1 msg channel nick #t))

   ((or (regexp-match? #px"^%s\\w*\r$" text)
        (regexp-match? #px"^%pa\\w*\r$" text))
    (pass-turn msg channel))

   ((and (regexp-match? #px"^%p" text)
         (not (regexp-match? #px"^%pa" text))
         (not (null? args)))
    (play-card msg channel nick args))
  )
 )
)

(define (deal-hands msg channel)
 (begin
  (for ((player (hash-ref! players channel '())))
   (draw-cards 7 msg channel player #f)
  )
 )
)

(define (cleanup-db channel)
 (query-exec *db* "DELETE FROM uno_hands WHERE channel = $1" channel)
 (query-exec *db* "DELETE FROM uno_decks WHERE channel = $1" channel)
 (query-exec *db* "DELETE FROM uno_topcard WHERE channel = $1" channel)
)

(define (parse-game-args msg channel)
 (let*
  ((text (cadr (irc-message-parameters msg)))
   (args (cdr (string-split text))))

  (for ((arg (in-list args)))
   (cond

    ; Disable wd4 challenge
    ((equal? arg "freewd4")
     (begin
      (reply msg "freewd4 was enabled. wd4 can now be played at any time and can't be challenged, just like a wild card.")
      (hash-set! freewd4? channel #t)
     )
    )

   )
  )
 )
)

; Just setup initial variables.
(define (init-game msg channel)
 (begin
  (reset-homerules channel)
  (parse-game-args msg channel)
  (reply msg "Starting a game of uno. Type %uno join to join, any player who joined can type %uno begin to begin the game.")
  (hash-set! card-drawn? channel 0)
  (hash-set! channel-status channel 1)
  (add-player msg channel)
 )
)

(define (randomize-first-player channel)
 (let*
  ((plist (hash-ref! players channel '()))
   (plist-len (length plist)))
  (for ((i (in-range 0 (random plist-len))))
   (shift-players channel)
  )
 )
)

(define (init-game2 msg channel)
 (begin
  (reply msg "The game has started! Type %p(lay) <colour> <card> to play a card when it's your turn, %d(raw) to draw a card, %uno leave to leave the game. The first person to discard all of their cards wins. The game can be stopped with %uno stop.")
  (cleanup-db channel)
  (init-deck channel)
  (deal-hands msg channel)
  (randomize-first-player channel)
  (init-topcard msg channel)
  (hash-set! channel-status channel 2)
  (print-current-players msg channel)
  (begin-turn msg channel)
 )
)

(define (get-type text)
 (let
  ((matches (regexp-match #px"(0|zero|null)|(1|one)|(2|two)|(3|three)|(4|four)|(5|five)|(6|six)|(7|seven)|(8|eight)|(9|nine)|(re?v?e?r?s?e?)|(sk?i?p?)|(d.*?(?:2|tw?o?))|(wi?l?d?[^(?:.*?d.*?(?:4|four)])|(w.*?d.*?(?:4|four))" text))
   (matchv (vector #f "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "r" "s" "d2" "w" "wd4"))
   (ret #f))

   (begin
    (for ((i (in-range (length matches))) #:break ret)
     (cond
      ((and (list-ref matches i) (> i 1) (< i 17))
        (set! ret (vector-ref matchv i)))
     )
    )
    ret
   )
 )
)

(define (get-colour text)
 (let
  ((matches (regexp-match #px"((?:red|\\br|r\\b)|(?:blue|\\bb|b\\b)|(?:yellow|\\by|y\\b)|(?:green|\\bg|g\\b))" text)))
  (if matches
   (values
    (get-real-colour (cadr matches))
    (regexp-replace (regexp (cadr matches)) text "")
   )
   (values #f text)
  )
 )
)

;(matches (regexp-match #px"^(?:([bryg])\\s*?)?([^bryg\\s]+|r)(?:\\s*?([bryg]))?" text))
(define (parse-played-card msg channel args)
 (if (null? args)
  (uno-err msg "Please enter a card to play")
  (let*-values
   (((text) (string-join args " "))
    ((colour newtext) (get-colour text))
    ((type) (get-type newtext)))

   (cond
    ((or (not colour) (not type))
     (uno-err msg "Please enter a card to play"))

    ((or (equal? type "w") (equal? type "wd4"))
     (values "wild" type colour))

    (else (values colour type colour))
   )

  )
 )
)

(define (play-card msg channel player args)
 (let*-values
  (((colour type newcolour) (parse-played-card msg channel args))
   ((card-id) (query-maybe-value *db* "SELECT id FROM uno_whole_deck WHERE type = $1 AND colour = $2 AND EXISTS ( SELECT 1 FROM uno_hands WHERE channel = $3 AND player = $4 AND uno_whole_deck.id = uno_hands.id ) LIMIT 1" type colour channel player))
   ((card-name special?)
    (if card-id
     (vector->values (query-row *db* "SELECT name, special FROM uno_whole_deck WHERE id = $1" card-id))
     (values "null" #f)
    )
   )
   ((top-id top-colour top-type top-name) (vector->values (query-row *db* "SELECT id, colour, type, name FROM uno_topcard WHERE channel = $1" channel))))
  (cond

   ((not card-id)
    (uno-err msg "You don't have that card."))

   ((and (not (= (hash-ref! card-drawn? channel 0) 0))
         (not (= (hash-ref! card-drawn? channel 0) card-id)))
    (uno-err msg "You can only play the card you've just drawn, or use %skip to skip your turn."))

   ((not (or (equal? colour top-colour) (equal? type top-type)
             (equal? colour "wild") (equal? top-colour "wild")))
    (uno-err msg (string-append "You can't play this card on a " top-name ".")))

   (else

    (query-exec *db* "DELETE FROM uno_hands WHERE channel = $1 AND player = $2 AND id = $3" channel player card-id)
    (query-exec *db* "UPDATE uno_topcard SET name = $1, type = $2, colour = $3, special = $4, id = $5 WHERE channel = $6" card-name type newcolour special? card-id channel)

    (hash-set! top-prev-colour channel top-colour)
;    (reply msg
;     (string-append "Top card is now a " (uno-cl-fromname card-name)
;      (if (equal? top-colour newcolour)
;       "."
;       (string-append ", the top colour is now " (uno-cl newcolour newcolour) ".")
;      )
;     )
;    )

    (end-turn msg channel)

   )
  )
 )
)

(define (get-real-colour colour)
 (cond
  ((not colour) #f)
  ((regexp-match? #px"^r" colour) "red")
  ((regexp-match? #px"^g" colour) "green")
  ((regexp-match? #px"^y" colour) "yellow")
  ((regexp-match? #px"^b" colour) "blue")
  ((regexp-match? #px"^w" colour) "wild")
  (else #f)
 )
)

(define (add-player msg channel)
 (let ((player (get-nick msg))
       (status (hash-ref! channel-status channel 0)))
  (cond
   ((= status 0)
    (reply msg "No game is starting. Use %uno start [homerules] to start a game."))
   ((= status 2)
    (reply msg "There's already a game going on. Wait until they're done or ask them to use %uno stop to stop it."))
   ((in-players? channel player)
    (reply msg "You're already set to join the game."))
   (else
    (begin
     (hash-set! players channel
      (append (hash-ref! players channel '()) (list player)))
     (reply msg (string-append player " is joining the game."))
    )
   )
  )
 )
)

(define (rm-player msg channel player)
 (let ((status (hash-ref! channel-status channel 0)))
  (cond
   ((= status 0)
    (reply msg "There's no game to leave!"))
   ((null? (cddr (hash-ref! players channel '())))
    (stop-game msg channel player))
   (else
    (begin
     (query-exec *db* "INSERT INTO uno_decks ( channel, id ) ( SELECT $1, id FROM uno_hands WHERE player = $2 )" channel player)
     (query-exec *db* "DELETE FROM uno_hands WHERE player = $1" player)
     (hash-set! players channel (remove player (hash-ref! players channel '())))
     (reply msg (string-append player " was removed from the game."))
    )
   )
  )
 )
)

(define (init-stats channel)
 (for ((player (in-list (hash-ref! players channel '()))))
  (cond
   ((not (query-maybe-value *db* "SELECT games FROM uno_stats WHERE channel = $1 AND player = $2" channel player))
    (query-exec *db* "INSERT INTO uno_stats (channel, player, score, wins, games) VALUES ($1, $2, 0, 0, 0)" channel player)
   )
  )
 )
)

(define (victor msg channel winner)
 (begin
  (init-stats channel)
  (log-game msg channel winner)
  (stop-game msg channel winner)
 )
)

(define (calc-player-score player channel)
 (let ((score (query-maybe-value *db* "SELECT sum(value) FROM uno_whole_deck WHERE EXISTS ( SELECT 1 FROM uno_hands WHERE channel = $1 AND uno_whole_deck.id = uno_hands.id )" channel)))
  (if (sql-null? score)
   0
   score
  )
 )
)

(define (log-game msg channel winner)
 (let ((score (calc-player-score winner channel)))
  (begin
   (reply msg (string-append winner " won with " (~a score) " points!"))
   (query-exec *db* "UPDATE uno_stats SET score = score + $1, wins = wins + 1 WHERE player = $2 AND channel = $3" score winner channel)

   (for ((player (in-list (hash-ref! players channel '()))))
    (query-exec *db* "UPDATE uno_stats SET games = games + 1 WHERE channel = $1 AND player = $2" channel player)
   )
  )
 )
)

(define (stop-game msg channel nick)
 (let ((status (hash-ref! channel-status channel 0)))
  (cond

   ((= status 0)
    (reply msg "There's no game! What are you trying to stop?"))

   ((and (not (in-players? channel nick)) (not (is-user-mod? nick channel)))
    (reply msg "You can't stop a game that you're not a part of!"))

   (else
    (begin
     (reset-homerules channel)
     (hash-set! channel-status channel 0)
     (hash-remove! players channel)
     (cleanup-db channel)
     (reply msg (string-append "The game in " channel " was stopped."))
    )
   )
  )
 )
)

(define (show-scores msg channel)
 (let
  ((stats (query-rows *db* "SELECT player, wins, games, score FROM uno_stats WHERE channel = $1 AND score >= 0 ORDER BY score DESC LIMIT 10" channel))
   (text "Top players: "))
  (begin
   (for ((player-stats (in-list stats)))
    (set! text
     (string-append text
      (vector-ref player-stats 0)
      "["
      (~a (vector-ref player-stats 1))
      "/"
      (~a (vector-ref player-stats 2)) "]: "
      (~a (vector-ref player-stats 3)) " points"
      (if (not (equal? player-stats (last stats)))
             " - "
             "."
      )
     )
    )
   )
   (reply msg text)
  )
 )
)

(define (show-stats msg nick channel)
 (let*
  ((args (string-split (cadr (irc-message-parameters msg))))
   (target
    (if
     (null? (cddr args))
     nick
     (caddr args)
    )
   )
   (resultv (query-maybe-row *db* "SELECT player, wins, games, score FROM uno_stats WHERE channel = $1 AND player = $2 ORDER BY score DESC LIMIT 10" channel target))
  )

  (cond
   ((not resultv)
   (reply msg
    (string-append "No stats found for player " target " in " channel)))

   (else
    (let-values
     (((name wins games score) (vector->values resultv)))
     (reply msg
      (string-append
       "stats for " name ": "
       (~a score) " points from "
       (~a wins) " wins out of "
       (~a games) " games "
       "(" (real->decimal-string (* (/ wins games) 100) 2) "% winrate)"
       " with "
       (real->decimal-string (/ score games) 2) " points per game."
      )
     )
    )
   )
  )
 )
)

(define (show-help player)
 (begin
  (notify player "To learn to play uno: https://service.mattel.com/instruction_sheets/42001pr.pdf - The bot is 100% compliant to these rules, unless a home rule is set as a flag to %uno start.")
  (notify player "%uno start to setup the game, %uno join to join, %uno deal to deal the cards and start playing. %uno cards gives you a briefing on the cards. %uno start takes homerules arguments after the start, which can be seen in %uno homerules.")
  (notify player "When the bot announces your turn, you can spend it by either drawing a card or playing a card. To have it notify you of your hand, type %uno hand. %draw or %d draws a card, %play or %p plays a card with the syntax: %p <colour if not wild> <card> <newcolour if wild>, after drawing, %skip or %s skips your turn.")
 )
)

(define (begin-game msg channel)
 (let ((status (hash-ref! channel-status channel 0)))
  (cond
   ((= status 0) (reply msg "Please use %uno start first to setup the game."))
   ((= status 2) (reply msg "The game has already begun!"))
   ((not (in-players? channel (get-nick msg))) (reply msg "Join the game first..."))
   ((< (length (hash-ref! players channel '())) 2)
    (reply msg "You can't start a game unless there are two or more players joined in."))
   (else (init-game2 msg channel))
  )
 )
)

(define (show-cards nick)
 (notify nick "tl;dr: args are [colour number] for number cards, [colour r/s/d2] for reverse, skip and draw-two, [w/wd4 newcolour] for wild and wild draw-four. Newcolour is the colour they'll act as.")
)

(define (show-homerules msg)
 (reply msg "Home rules can be specified in the %uno start command, after the start. The home rules are - freewd4: play wd4 anytime")
)

(define (to-challenge-thread msg channel nick challenge?)
 (let*
  ((cthd (hash-ref! challenge-threads channel #f))
   (status (hash-ref! channel-status channel 0))
   (plist (hash-ref! players channel '()))
   (challenger
    (if (null? (cdr plist))
     (car plist)
     (cadr plist)
    )
   )
  )

  (cond
   ((= status 0)
    (reply msg "There's no game going on!"))
   ((= status 1)
    (reply msg "What is there to challenge or let go? We're setting up!"))

   ((or (not (thread? cthd)) (not (thread-running? cthd)))
    (reply msg "There's nothing to currently challenge or let go!"))

   ((not (equal? nick challenger))
    (reply msg "You can't challenge this!"))

   (else (thread-send cthd challenge?))
  )

 )
)

; main command parser
(define (uno)
 (let* ((msg (thread-receive))
        (channel (car (irc-message-parameters msg)))
        (nick (get-nick msg))
        (args (cdr (string-split (cadr (irc-message-parameters msg)))))
        (command (if (not (null? args))
                     (car args)
                     (kill-thread (current-thread)))))
  (cond
   ((equal? command "on")
    (change-uno-status msg nick channel #t))

   ((equal? command "off")
    (change-uno-status msg nick channel #f))

   ((not (channel-enabled? channel))
    (reply msg "This channel has uno disabled. Use %uno on to enable it."))

   ((equal? command "scoreboard")
    (show-scores msg channel))

   ((equal? command "stats")
    (show-stats msg nick channel))

   ((equal? command "hand")
    (show-hand msg channel nick))

   ((equal? command "cards")
    (show-cards nick))

   ((equal? command "homerules")
    (show-homerules msg))

   ((equal? command "help")
    (show-help nick))

   ((equal? command "leave")
    (rm-player msg channel nick))

   ((equal? command "start")
    (start-game msg))

   ((or (equal? command "begin") (equal? command "deal"))
    (begin-game msg channel))

   ((equal? command "stop")
    (stop-game msg channel nick))

   ((equal? command "join")
    (add-player msg channel))

   ((equal? command "yes")
    (to-challenge-thread msg channel nick #t))
   ((equal? command "no")
    (to-challenge-thread msg channel nick #f))
  )
 )
)

(define (start-game msg)
 (let ((channel (car (irc-message-parameters msg))))
  (cond
   ((= (hash-ref! channel-status channel 0) 1)
    (reply msg "A game is starting already! Type %uno join to play."))
   ((= (hash-ref! channel-status channel 0) 2)
    (reply msg "There's already a game! Wait until it's done."))
   (else (init-game msg channel))
  )
 )
)

;;;; EXECUTION

(hash-set! *cmds* "%uno" uno)
(hash-set! *cmds* "%p" draw-play)
(hash-set! *cmds* "%play" draw-play)
(hash-set! *cmds* "%d" draw-play)
(hash-set! *cmds* "%draw" draw-play)
(hash-set! *cmds* "%skip" draw-play)
(hash-set! *cmds* "%s" draw-play)
(hash-set! *cmds* "%pass" draw-play)
(hash-set! *cmds* "%pa" draw-play)
