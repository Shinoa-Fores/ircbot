#lang racket
(require racket/sandbox)
(require racket/format)

(require irc)
(require "../lib/structs.rkt")
(require "../lib/utils.rkt")

(define sandy-evaluators (make-hash))

(define (irc-racket-make-evaluator channel)
 (parameterize ((sandbox-eval-limits '(5 #f)) (sandbox-memory-limit #f))
  (let ((new-evaluator (make-module-evaluator '(module sandbox racket))))
   (begin
    (hash-set! sandy-evaluators channel new-evaluator)
    new-evaluator
   )
  )
 )
)

(define (irc-racket-eval)
 (let*
  ((msg (thread-receive))
   (channel (car (irc-message-parameters msg)))
   (chan-eval? (hash-ref! sandy-evaluators channel #f))
   (chan-eval
    (if (and chan-eval? (evaluator-alive? chan-eval?))
     chan-eval?
     (irc-racket-make-evaluator channel)
    )
   )
   (input (string-join (cdr (string-split (cadr (irc-message-parameters msg)))) " "))
   (output (string-trim (~a (chan-eval input)))))

  (reply msg
   (if (> (string-length output) 420)
    (string-append (substring output 0 420) "...")
    output
   )
  )
 )
)

(define (irc-racket-eval-break)
 (let*
  ((msg (thread-receive))
   (channel (car (irc-message-parameters msg)))
   (chan-eval (hash-ref! sandy-evaluators channel #f)))
  (cond
   ((and chan-eval (evaluator-alive? chan-eval))
    (break-evaluator chan-eval))
  )
 )
)

(define (irc-racket-eval-reset)
 (let*
  ((msg (thread-receive))
   (channel (car (irc-message-parameters msg)))
   (chan-eval (hash-ref! sandy-evaluators channel '())))
  (begin
   (cond
    ((and (not (null? chan-eval)) (evaluator-alive? chan-eval))
     (kill-evaluator chan-eval))
   )
   (hash-set! sandy-evaluators channel (irc-racket-make-evaluator channel))
  )
 )
)

(hash-set! *cmds* "%eval" irc-racket-eval)
(hash-set! *cmds* "%break" irc-racket-eval-break)
(hash-set! *cmds* "%reset" irc-racket-eval-reset)
