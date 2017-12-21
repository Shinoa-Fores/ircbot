#lang racket

(provide *db*)
(require db)

(define *db*
 (postgresql-connect
  #:user "ircbot"
  #:password "ircbot"
  #:database "ircbot"
  #:socket "/tmp/.s.PGSQL.5432"
 )
)
