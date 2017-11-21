#!/usr/bin/env racket
#lang racket

(require irc)
(require racket/async-channel)

; globals defining everything related to the IRC protocol
(require "lib/info.rkt")
; data structures containing what the bot should respond to
(require "lib/structs.rkt")
; functions/shortcuts useful to interact with IRC without having to mention *irc*
(require "lib/utils.rkt")

;;; importing modules

(require "modules/channels.rkt")
(require "modules/pingpong.rkt")
(require "modules/intensify.rkt")
(require "modules/uno.rkt")

;;; core functions

;; take a (struct irc-message) and parse it
(define (parse-msg msg)
 (let* ((params (irc-message-parameters msg))
        (text (cadr (irc-message-parameters msg)))
        (val (cond
              ((not (null? (string-split text)))
               (hash-ref! *cmds* (car (string-split text)) #f))
              (else #f)
             )
        ))

  (cond
   ((procedure? val)
    (thread-send (thread val) msg))

   ((not (null? *rgxlst*))
    (for ((rgx *rgxlst*))
     (cond ((regexp-match? (regex-cmd-regex rgx) (string-trim (cadr params)))
            (thread-send (thread (regex-cmd-func rgx)) msg))
     )
    )
   )
  )
 )
)

;;; initial setup

(sleep 10)

(identify)

(sleep 10)

(join *channels*)

(define *msgchan*
 (irc-connection-incoming *irc*)
)

;;; execution

(define (main)
 (let ((msg (async-channel-get *msgchan*)))
  (begin
   (cond ((equal? (irc-message-command msg) "PRIVMSG")
          (parse-msg msg))
   )
   (main)
  )
 )
)

(main)
