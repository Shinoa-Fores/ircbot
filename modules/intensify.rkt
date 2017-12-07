#lang racket

(provide intensify)
(require irc)
(require "../lib/colours.rkt")
(require "../lib/utils.rkt")
(require "../lib/structs.rkt")

(define (intensify)
 (let ((msg (thread-receive)))
  (let ((text (string-trim (cadr (irc-message-parameters msg)))))
   (reply msg
    (string-append clbold "["
     (string-locale-upcase
      (string-trim (string-trim text "[" #:right? #f) "]" #:left? #f)
     )
    " INTENSIFIES" clpref "]" clbold
    )
   )
  )
 )
)

(hash-set! *cmds* "intensitest" intensify)
(add-regex-cmd #px"^\\[.+\\]$" intensify)
