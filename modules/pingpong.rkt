#lang racket

(provide ping pong)
(require "../lib/utils.rkt")
(require "../lib/structs.rkt")

(define (ping)
 (let ((msg (thread-receive)))
  (reply msg "%pong")
 )
)

(define (pong)
 (let ((msg (thread-receive)))
  (reply msg "%ping")
 )
)

(hash-set! *cmds* "%ping" ping)
(hash-set! *cmds* "%pong" pong)
