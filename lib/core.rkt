#lang racket

(provide on-privmsg on-invite)
(require irc)
(require "../conf/irc.rkt")
(require "hooks.rkt")
(require "threads.rkt")
(require "structs.rkt")
(require "utils.rkt")

(define (on-invite)
 (let ((msg (thread-receive)))
  (join (irc-message-parameters msg))
 )
)

;; take a (struct irc-message) and parse it
(define (on-privmsg)
 (let*
  ((msg (thread-receive))
   (params (irc-message-parameters msg))
   (text (string-trim (cadr (irc-message-parameters msg))))
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

(define (on-client-registered)
 (if identify?
  (identify)
  (join *channels*)
 )
)

(define (on-logged-in)
 (join *channels*)
)

(mk-hook "INVITE" on-invite)
(mk-hook "PRIVMSG" on-privmsg)
(mk-hook "001" on-client-registered)
(mk-hook "900" on-logged-in)
