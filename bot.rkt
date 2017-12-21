#!/usr/bin/env racket
#lang racket

(require irc)
(require racket/async-channel)

(require "conf/irc.rkt")

(require "lib/core.rkt")
(require "lib/structs.rkt")
(require "lib/utils.rkt")
(require "lib/threads.rkt")
(require "lib/hooks.rkt")

;;; importing modules

(require "mods/pingpong.rkt")

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
   (run-hooks irccmd msg)
   (main)
  )
 )
)

(main)
