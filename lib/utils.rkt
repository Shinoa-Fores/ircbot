#lang racket

(require irc)
(require "info.rkt")
(provide join part reply notify get-nick)

;; join an arbitrary list of channels
(define (join chans)
 (cond
  ((null? chans) chans)
  (else
   (begin
    (irc-join-channel *irc* (car chans))
    (join (cdr chans))
   )
  )
 )
)

(define (part chans)
 (cond
  ((null? chans) chans)
  (else
   (begin
    (irc-part-channel *irc* (car chans))
    (part (cdr chans))
   )
  )
 )
)

;; reply to what's assumed to be a privmsg according to its params
(define (reply msg text)
 (irc-send-message *irc* (car (irc-message-parameters msg)) text)
)

(define (notify target text)
 (irc-send-notice *irc* target text)
)

(define (get-nick msg)
 (cadr (regexp-match #px"(\\S+)!\\S+@" (irc-message-prefix msg)))
)
