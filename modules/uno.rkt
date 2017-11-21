#lang racket

(provide uno draw-play)

(require irc)
(require db)
(require "../lib/db.rkt")
(require "../lib/utils.rkt")
(require "../lib/structs.rkt")
(require "../lib/colours.rkt")

; key is #channel, val is an int
; 0: not playing
; 1: setting up
; 2: playing
(define channel-status (make-hash))

; key in #channel, val is the ID of the card that was drawn, or 0 if no card was drawn.
(define card-drawn? (make-hash))

; key is #channel, val is a (list playername ...)
(define players (make-hash))

(define (print-current-turn msg channel)
 (reply msg (string-append "It's " (car (hash-ref! players channel '())) "'s turn."))
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

(define (init-deck channel)
 (query-exec *db* "INSERT INTO uno_decks ( channel, id ) ( SELECT $1, id FROM uno_whole_deck )" channel)
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
     ((card-id (query-value *db* "SELECT id FROM uno_decks WHERE channel = $1 LIMIT 1 OFFSET floor(random()*(SELECT count(*) FROM uno_decks))" channel))
      (card-name (query-value *db* "SELECT name FROM uno_whole_deck WHERE id = $1 LIMIT 1" card-id)))
     (begin

      (query-exec *db* "DELETE FROM uno_decks WHERE channel = $1 AND id = $2" channel card-id)
      (query-exec *db* "INSERT INTO uno_hands ( channel, player, id ) VALUES ( $1, $2, $3 )" channel player card-id)

      ; reinitialize the deck if it's empty
      (cond ((= (query-value *db* "SELECT count(*) FROM uno_decks WHERE channel = $1" channel) 0)
             ;; Replace with a function that compares the whole deck with the players' hands and the topcard and builds a new deck using the missing cards (XOR)
             (init-deck channel)) 
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
     ((ccount (query-value *db* "SELECT count(*) FROM uno_hands WHERE player = $1" player)))
     (set! text
      (string-append text " " player "[" (~a ccount) "]")
     )
    )
   )
   text 
  )
 )
)

(define (get-hand channel player)
 (let* ((card-names
        (query-list *db* "SELECT name FROM uno_whole_deck WHERE EXISTS ( SELECT 1 FROM uno_hands WHERE channel = $1 AND player = $2 AND uno_hands.id = uno_whole_deck.id )" channel player))
       (text (string-append "You have " (~a (length card-names)) " cards: ")))
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

(define (print-cardcount channel)
 (let ((text (get-cardcount channel)))
  (for ((player (in-list (hash-ref! players channel '()))))
   (notify player text)
  )
 )
)

(define (print-info channel)
 (let*-values
  (((player) (car (hash-ref! players channel '())))
   ((top-name top-colour) (vector->values (query-row *db* "SELECT name, colour FROM uno_topcard WHERE channel = $1" channel))))
  (begin
   (notify player "It's your turn!")
   (notify player (string-append "The current top card is a " (uno-cl-fromname top-name) " (" (uno-cl top-colour top-colour) ")."))
   (notify player (get-hand channel player))
  )
 )
)

(define (begin-turn msg channel)
 (begin
  (print-cardcount channel)
  (print-current-turn msg channel)
  (print-info channel)
 )
)

(define (end-turn msg channel)
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

  (begin-turn msg channel)
 )
)

(define (draw-two msg channel player first-card?)
 (begin
  (shift-players channel)
  (draw-cards 2 msg channel player #f)
  (reply msg (string-append player " has drawn 2 cards, and their turn will be skipped."))
 )
)

(define (wild-draw-four msg channel player first-card?)
 (begin
  (shift-players channel)
  (draw-cards 4 msg channel player #f)
  (reply msg (string-append player " has drawn 4 cards, and their turn will be skipped."))
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
    (notify player (get-hand channel player)))
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
        (command (car (string-split (cadr (irc-message-parameters msg)))))
        (args (cdr (string-split (cadr (irc-message-parameters msg)))))
        (nick (get-nick msg)))
  (cond
   ((= status 0)
    (reply msg "Nobody's playing uno..."))

   ((= status 1)
    (reply msg "Relax, we're starting the game! Geeze!"))

   ((not (in-players? channel nick))
    (reply msg (string-append "You're not in the game, " nick ".")))

   ((not (equal? (car (hash-ref! players channel '())) nick))
    (reply msg (string-append "It's not your turn yet, " nick "!")))

   ((and (regexp-match? #px"^%d" command)
         (= (hash-ref! card-drawn? channel 0) 0))
    (draw-cards 1 msg channel nick #t))

   ((regexp-match? #px"^%pa" command)
    (pass-turn msg channel))

   ((and (regexp-match? #px"^%p" command)
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

; Just setup initial variables.
(define (init-game msg channel)
 (begin
  (hash-set! channel-status channel 1)
  (reply msg "Starting a game of uno. Type %uno join to join, any player who joined can type %uno begin to begin the game.")
  (add-player msg channel)
 )
)

(define (init-game2 msg channel)
 (begin
  (reply msg "The game has started! Type %p(lay) <colour> <card> to play a card when it's your turn, %d(raw) to draw a card, %uno leave to leave the game. The first person to discard all of their cards wins. The game can be stopped with %uno stop.")
  (cleanup-db channel)
  (init-deck channel)
  (deal-hands msg channel)
  (init-topcard msg channel)
  (hash-set! channel-status channel 2)
  (print-current-players msg channel)
  (begin-turn msg channel)
 )
)

(define (parse-played-card msg channel args)
 (if (null? args)
  (uno-err msg "Please enter a card to play")
  (let*
   (
    (colour
     ; colour is wild if the first word is w or wd4, else it's the first word.
     (if (or
          (equal? (car args) "w")
          (equal? (car args) "wd4")
         )
         "wild"
         (get-real-colour (car args))
     )
    )
    (type
     ; type is the first word if the colour is wild, else it's the second word
     (cond
      ((equal? colour "wild") (car args))
      ((null? (cdr args)) (uno-err msg "Please enter a type."))
      (else (cadr args))
     )
    )
    (newcolour
     ; newcolour is the second word if colour is wild, and the second word
     ; exists, else it's wild.
     (if (not (equal? colour "wild"))
      colour
      (cond
       ((null? (cdr args)) colour)
       (else (get-real-colour (cadr args)))
      )
     )
    )
   ) ; let* values
   (values colour type newcolour)
  ) ; let*
 ) ; if
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
    (uno-err msg "You can only play the card you've just drawn, or use %pass to pass your turn."))

   ((not (or (equal? colour top-colour) (equal? type top-type)
             (equal? colour "wild") (equal? top-colour "wild")))
    (uno-err msg (string-append "You can't play this card on a " top-name ".")))

   (else

    (query-exec *db* "DELETE FROM uno_hands WHERE channel = $1 AND player = $2 AND id = $3" channel player card-id)
    (query-exec *db* "UPDATE uno_topcard SET name = $1, type = $2, colour = $3, special = $4, id = $5 WHERE channel = $6" card-name type newcolour special? card-id channel)

    (reply msg
     (string-append "Top card is now a " (uno-cl-fromname card-name)
      (if (equal? top-colour newcolour)
       "."
       (string-append ", the top colour is now " (uno-cl newcolour newcolour) ".")
      )
     )
    )

    (let
     ((cardsleft (query-value *db* "SELECT count(*) FROM uno_hands WHERE player = $1" player)))
     (if (= cardsleft 0)
      (victor msg channel player)
      (end-turn msg channel)
     )
    )

   )
  )
 )
)

(define (get-real-colour colour)
 (cond
  ((regexp-match? #px"^r" colour) "red")
  ((regexp-match? #px"^g" colour) "green")
  ((regexp-match? #px"^y" colour) "yellow")
  ((regexp-match? #px"^b" colour) "blue")
  ((regexp-match? #px"^w" colour) "wild")
  (else "wild")
 )
)

(define (add-player msg channel)
 (let ((player (get-nick msg))
       (status (hash-ref! channel-status channel 0)))
  (cond
   ((= status 0)
    (reply msg "No game is starting. Use %uno start to start a game."))
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
   (else
    (begin
     (query-exec *db* "DELETE FROM uno_hands WHERE player = $1" player)
     (hash-set! players channel (remove player (hash-ref! players channel '())))
     (reply msg (string-append player " was removed from the game."))
    )
   )
  )
 )
)

(define (init-scoreboard channel)
 (for ((player (in-list (hash-ref! players channel '()))))
  (cond
   ((not (query-maybe-value *db* "SELECT games FROM uno_scoreboard WHERE channel = $1 AND player = $2" channel player))
    (query-exec *db* "INSERT INTO uno_scoreboard (channel, player, score, games) VALUES ($1, $2, 0, 0)" channel player)
   )
  )
 )
)

(define (victor msg channel winner)
 (begin
  (init-scoreboard channel)
  (log-game msg channel winner)
  (stop-game msg channel winner)
 )
)

(define (calc-player-score player channel)
 (query-maybe-value *db* "SELECT sum(value) FROM uno_whole_deck WHERE EXISTS ( SELECT 1 FROM uno_hands WHERE channel = $1 AND uno_whole_deck.id = uno_hands.id )" channel)
)

(define (log-game msg channel winner)
 (let ((score (calc-player-score winner channel)))
  (begin
   (reply msg (string-append winner " won with " (~a score) " points!"))
   (query-exec *db* "UPDATE uno_scoreboard SET score = score + $1 WHERE player = $2 AND channel = $3" score winner channel)

   (for ((player (in-list (hash-ref! players channel '()))))
    (query-exec *db* "UPDATE uno_scoreboard SET games = games + 1 WHERE channel = $1 AND player = $2" channel player)
   )
  )
 )
)

(define (stop-game msg channel nick)
 (let ((status (hash-ref! channel-status channel 0)))
  (cond

   ((= status 0)
    (reply msg "There's no game! What are you trying to stop?"))

   ((not (in-players? channel nick))
    (reply msg "You can't stop a game that you're not a part of!"))

   (else
    (begin
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
  ((scores (query-rows *db* "SELECT player, games, score FROM uno_scoreboard WHERE channel = $1 AND score >= 0 ORDER BY score DESC LIMIT 10" channel))
   (text "Top players: "))
  (begin
   (for ((score (in-list scores)))
    (set! text
     (string-append text
      (vector-ref score 0)
      "[" (~a (vector-ref score 1)) "]: "
      (~a (vector-ref score 2)) " points"
      (if (not (equal? score (last scores)))
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

(define (show-help player)
 (begin
  (notify player "To learn to play uno: https://service.mattel.com/instruction_sheets/42001pr.pdf")
  (notify player "When the bot announces your turn, you can spend it by either drawing a card or playing a card. To have it notify you of your hand, type %uno hand. To draw a card, simply type %draw or %d. To play a card, type %play or %p. If you still have nothing after drawing, type %pass or %pa.")
  (notify player "The %play command works as follows: %play colour type new-colour?. Colour is the colour of your card. The first letter is enough. For wild cards, type wild. The type is either the number of your card, d2 for a draw-two card, wd4 for a wild draw-four card, s for a skip card, or w for a regular wild card. If you played a wild card, you have to specify a new colour the same way you did your first colour for the deck's top card.")
 )
)

(define (begin-game msg channel)
 (let ((status (hash-ref! channel-status channel 0)))
  (cond
   ((= status 0) (reply msg "Please use %uno start first to setup the game."))
   ((= status 2) (reply msg "The game has already begun!"))
   (else (init-game2 msg channel))
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
   ((equal? command "scoreboard")
    (show-scores msg channel))

   ((equal? command "hand")
    (show-hand msg channel nick))

   ((equal? command "help")
    (show-help nick))

   ((equal? command "leave")
    (rm-player msg channel nick))

   ((equal? command "start")
    (start-game msg))

   ((equal? command "begin")
    (begin-game msg channel))

   ((equal? command "stop")
    (stop-game msg channel nick))

   ((equal? command "join")
    (add-player msg channel))
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
(hash-set! *cmds* "%pass" draw-play)
(hash-set! *cmds* "%pa" draw-play)
