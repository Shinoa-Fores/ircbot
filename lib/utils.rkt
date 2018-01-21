#lang racket

(require irc)
(require "../conf/irc.rkt")
(provide join part reply irc-send notify get-nick ellipsize-string)

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
(define (reply msg text (mod-allowed? #f))
 (let
  ((dangerous? (string-prefix? text "!")))
  (if (or (not dangerous?) (and mod-allowed? dangerous?))
   (irc-send-message *irc* (car (irc-message-parameters msg)) (string-trim text))
   (irc-send-message *irc* (car (irc-message-parameters msg)) "no")
  )
 )
)

(define (ellipsize-string string max)
 (if (> (string-length string) max)
  (string-append (substring string 0 max) "...")
  string
 )
)

(define (irc-send target text (mod-allowed? #f))
 (let
  ((dangerous? (string-prefix? text "!")))
  (if (or (not dangerous?) (and mod-allowed? dangerous?))
   (irc-send-message *irc* target text)
   (irc-send-message *irc* target "no")
  )
 )
)

(define (notify target text)
 (irc-send-notice *irc* target text)
)

(define (get-nick msg)
 (cadr (regexp-match #px"(\\S+)!\\S+@" (irc-message-prefix msg)))
)
