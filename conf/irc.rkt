#lang racket

(require irc)
(provide
 identify
 *nick*
 *server*
 *channels*
 *irc*
)

;;; Global IRC variables

(define *nick* "ircbot")
(define *password* '())
(define *realname* "ircbot")
(define *server* "irc.snoonet.org")
(define *channels* (list "#bsdmasterrace"))
(define-values (*irc* *irc-ready*)
 (irc-connect
  *server*
  6697
  *nick*
  *nick*
  *realname*
  #:ssl 'tls
 )
)

(define (identify)
 (cond ((string? *password*)
       (irc-send-message *irc* "nickserv" (string-append "identify " *password*)))
 )
)
