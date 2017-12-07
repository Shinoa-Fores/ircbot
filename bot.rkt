#!/usr/bin/env racket
#lang racket

(require irc)
(require racket/async-channel)

(require "lib/core.rkt")
(require "lib/info.rkt")
(require "lib/structs.rkt")
(require "lib/utils.rkt")
(require "lib/threads.rkt")

;;; importing modules

(require "modules/pingpong.rkt")

;;; initial setup

(sleep 10)

(identify)

(sleep 10)

(join *channels*)

(define *msgchan*
 (irc-connection-incoming *irc*)
)

(thread thread-watcher)

;;; execution

(define (main)
 (let*
  ((msg (async-channel-get *msgchan*))
   (irccmd (irc-message-command msg)))
  (begin

   (cond
    ((equal? irccmd "PRIVMSG")
     (parse-msg msg))

    ((equal? irccmd "INVITE")
     (on-invite msg))
   )

   (main)
  )
 )
)

(main)
