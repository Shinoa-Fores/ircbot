#lang racket

(provide joinfunc partfunc)
(require irc)
(require "../lib/structs.rkt")
(require "../lib/utils.rkt")

(define (joinfunc)
 (let ((msg (thread-receive)))
  (join (cdr (string-split (cadr (irc-message-parameters msg)))))
 )
)
(define (partfunc)
 (let ((msg (thread-receive)))
  (part (list (car (irc-message-parameters msg))))
 )
)

(hash-set! *cmds* "%join" joinfunc)
(hash-set! *cmds* "%part" partfunc)
