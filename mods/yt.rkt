#lang racket

(provide yt)
(require irc)
(require (planet gh/gapi/macro))
(require-gapi-doc youtube.v3.js)

(require "../lib/colours.rkt")
(require "../lib/structs.rkt")
(require "../lib/utils.rkt")

(define (query-yt text)
 (youtube-search-list
  #:key "AIzaSyCUYi5V6lrqKrgb9AfXeCl_NUj9xPk9nSs"
  #:part "snippet"
  #:q text
  #:maxResults "1")
)

(define (get-channel ytret)
 (hash-ref
  (hash-ref
   (car
    (hash-ref ytret 'items #f)
   ) 'snippet #f
  ) 'channelTitle #f
 )
)

(define (get-title ytret)
 (hash-ref
  (hash-ref
   (car
    (hash-ref ytret 'items #f)
   ) 'snippet #f
  ) 'title #f
 )
)

(define (get-id ytret)
 (hash-ref
  (hash-ref
   (car
    (hash-ref ytret 'items #f)
   ) 'id #f
  ) 'videoId #f
 )
)

(define (fmt-reply id title channel)
 (string-append
  clbold title clbold ": "
  "https://youtu.be/" id
  " by " clbold channel clbold "."
 )
)

(define (get-fmt-text text)
 (let*
  ((ytret (query-yt text))
   (id (get-id ytret))
   (title (get-title ytret))
   (channel (get-channel ytret)))
  (fmt-reply id title channel)
 )
)

(define (yt)
 (let*
  ((msg (thread-receive))
   (text
    (string-join
     (cdr
      (string-split
       (cadr (irc-message-parameters msg))
      )
     ) " "
    )
   )
  )
  (reply msg (get-fmt-text text))
 )
)

(hash-set! *cmds* "%yt" yt)
