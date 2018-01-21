#lang racket

; userinfo.rkt
; watches the channels we're in and maintains a list of
; users and their info, and makes these informations
; available to modules.

(provide is-user-mod?
 ui-on-part ui-on-kick
 ui-on-names ui-on-mode ui-on-nick
)

(require irc)
(require "hooks.rkt")
(require "utils.rkt")
(require "../conf/irc.rkt")

(struct irc-user (nick modes))

; "#channel" -> (hash "nick" "modes")
(define chans (make-hash))

(define (is-user-mod? nick channel)
 (hash-ref! (hash-ref! chans channel (make-hash)) nick #f)
)

(define (is-mode-mod? modes)
 (let
  ((mod?
    (or
     (equal? modes #t)
     (regexp-match? #px"^\\+.*?h" modes)
     (regexp-match? #px"^\\+.*?o" modes)
    )
   )
  )
  mod?
 )
)

(define (mod-user nick channel (modes #f))
 (let*
  (
   (user-hash (hash-ref! chans channel (make-hash)))
  )
  (begin
   (cond ((null? user-hash) (hash-set! chans channel user-hash)))
   (if (or (equal? modes #t) (string-contains? (~a modes) "h") (string-contains? (~a modes) "o"))
    (hash-set! user-hash nick (is-mode-mod? modes))
    (void)
   )
  )
 )
)

(define (rm-user nick channel)
 (hash-remove! (hash-ref! chans channel (make-hash)) nick)
)

(define (rm-chan channel)
 (begin
  (hash-clear! (hash-ref! chans channel (make-hash)))
  (hash-remove! chans channel)
 )
)

; inspect a nick formatted as something like %user and return
; their channel mode as +v, +h, +o, or #f if it has nothing special
(define (grab-mode mnick)
 (let*
  ((ret (regexp-match #px"^(.)(.+)" mnick))
   (prefix (cadr ret))
   (nick (caddr ret))
  )
  (cond
   ((equal? prefix "@")
    (values #t nick))
   ((equal? prefix "%")
    (values #t nick))
   ((equal? prefix "+")
    (values "+v" nick))
   (else (values #f mnick))
  )
 )
)

(define (ui-on-part)
 (let*
  ((msg (thread-receive))
   (params (irc-message-parameters msg))
   (nick (get-nick msg))
   (channel (car (irc-message-parameters msg)))
  )
  (if (equal? nick *nick*)
   (rm-chan channel)
   (rm-user nick channel)
  )
 )
)

(define (ui-on-kick)
 (let*
  ((msg (thread-receive))
   (params (irc-message-parameters msg))
   (channel (string-trim (car (irc-message-parameters msg))))
   (nick (string-trim (cadr (irc-message-parameters msg))))
  )
  (if (equal? nick *nick*)
   (rm-chan channel)
   (rm-user nick channel)
  )
 )
)

(define (ui-on-nick)
 (let*
  ((msg (thread-receive))
   (params (irc-message-parameters msg))
   (oldnick (get-nick msg))
   (newnick (string-trim (car params)))
  )

  (for ((channel (hash-keys chans)))
   (if (hash-ref! chans channel #f)
    (let ((mod? (is-user-mod? oldnick channel)))
     (begin
      (rm-user oldnick channel)
      (mod-user newnick channel mod?)
     )
    )
    (void)
   )
  )
 )
)

(define (ui-on-names)
 (let*
  ((msg (thread-receive))
   (params (irc-message-parameters msg))
   (channel (string-trim (caddr params)))
   (nicks (string-split (cadddr params)))
  )
  (for ((mnick nicks))
   (let-values (((modes nick) (grab-mode mnick)))
    (mod-user nick channel modes)
   )
  )
 )
)

(define (ui-on-mode)
 (let*
  ((msg (thread-receive))
   (params (irc-message-parameters msg))
   (channel (car params))
   (mode (cadr params))
   (nick
    (if (null? (cddr params))
     (string-trim (car params))
     (string-trim (caddr params))
    )
   )
  )
  (cond
   ((regexp-match? #px"^\\#" channel)
    (mod-user nick channel mode))
  )
 )
)

(mk-hook "PART" ui-on-part)
(mk-hook "KICK" ui-on-kick)
(mk-hook "NICK" ui-on-nick)
(mk-hook "353" ui-on-names)
(mk-hook "NAMES" ui-on-names)
(mk-hook "MODE" ui-on-mode)
