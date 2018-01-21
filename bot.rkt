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

(require "scripts.rkt")
(require "mods/intensify.rkt")

(define *msgchan*
 (irc-connection-incoming *irc*)
)

(void (thread thread-watcher))

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
