#lang racket

(provide mk-hook run-hooks)
(require "threads.rkt")

; "IRCCOMMAND" -> (list)
(define hooks (make-hash))

(define (mk-hook irccmd func)
 (hash-set! hooks irccmd
  (append
   (hash-ref! hooks irccmd '())
   (list func)
  )
 )
)

(define (run-hooks irccmd msg)
 (for ((func (in-list (hash-ref! hooks irccmd '()))))
  (cmd-thread func msg)
 )
)
