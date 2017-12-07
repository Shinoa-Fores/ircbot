#lang racket
(provide parse-msg on-invite)
(require irc)
(require "threads.rkt")
(require "structs.rkt")
(require "utils.rkt")

(define (on-invite msg)
 (join (irc-message-parameters msg))
)

;; take a (struct irc-message) and parse it
(define (parse-msg msg)
 (let*
  ((params (irc-message-parameters msg))
   (text (cadr (irc-message-parameters msg)))
   (val
    (cond
     ((not (null? (string-split text)))
      (hash-ref! *cmds* (car (string-split text)) #f))
     (else #f)
    )
   )
  )

  (cond
   ((procedure? val)
    (cmd-thread val msg))

   ((not (null? *rgxlst*))
    (for ((rgx *rgxlst*))
     (cond ((regexp-match? (regex-cmd-regex rgx) (string-trim (cadr params)))
            (cmd-thread (regex-cmd-func rgx) msg))
     )
    )
   )
  )
 )
)
