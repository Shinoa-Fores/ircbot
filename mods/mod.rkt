#lang racket

(provide mod-reply)

(require irc)
(require "../lib/utils.rkt")
(require "../lib/structs.rkt")
(require "../lib/userinfo.rkt")

(define (mod-reply)
 (let*
  ((msg (thread-receive))
   (nick (get-nick msg))
   (channel (car (irc-message-parameters msg)))
  )
  (if (is-user-mod? (get-nick msg) channel)
   (reply msg "yup.")
   (reply msg "nope.")
  )
 )
)

(hash-set! *cmds* "%mod?" mod-reply)
