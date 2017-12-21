#lang racket

(provide ddg)
(require irc)
(require net/http-client)
(require net/uri-codec)
(require json)

(require "../lib/colours.rkt")
(require "../lib/structs.rkt")
(require "../lib/utils.rkt")

(define (handle-search ddg-json)
 (let
  ((redirect? (hash-ref ddg-json 'Redirect))
   (abstURL?  (hash-ref ddg-json 'AbstractURL))
   (heading?  (hash-ref ddg-json 'Heading)))
  (cond
   ((non-empty-string? redirect?)
    redirect?)

   ((non-empty-string? abstURL?)
    (string-append clbold heading? clbold ": " abstURL?))

   (else (string-append clbold "[NO RESULT]" clbold))
  )
 )
)


(define (handle-article ddg-json)
 (let
  ((abst-url  (hash-ref ddg-json 'AbstractURL))
   (abst-text (hash-ref ddg-json 'AbstractText))
   (heading   (hash-ref ddg-json 'Heading)))

  (string-append
   clbold heading clbold
   ": " abst-url " "
   (if (> (string-length abst-text) 100)
    (string-append (substring abst-text 0 100) "...")
    abst-text
   )
  )
 )
)

(define (handle-definition ddg-json)
 (let
  ((definition (hash-ref ddg-json 'Definition))
   (defsource  (hash-ref ddg-json 'DefinitionSource))
   (defURL     (hash-ref ddg-json 'DefinitionURL)))

  (string-append
   clbold defsource clbold
   ": "
   (if (> definition 250)
    (string-append (substring definition 0 250) "...")
    definition
   )
   " (" defURL ")"
  )
 )
)

(define typehash
 (hash
  "A" handle-article
  "D" handle-definition
  "C" handle-search
  "N" handle-search
  "E" handle-search
 )
)

(define (get-ddg-json text)
 (let*-values
  (((ddg-url)
    (string-append 
     "http://api.duckduckgo.com/?q="
     (string-join (string-split text) "+")
     "&format=json&no_redirect=1"
    )
   )
   ((ddg-conn) (http-conn-open "api.duckduckgo.com"))
   ((httpok httpstat portret) (http-conn-sendrecv! ddg-conn ddg-url))
   ((ddg-json) (read-json portret))
  )
  (begin
   (http-conn-close! ddg-conn)
   ddg-json
  )
 )
)

(define (ddg-fmt type ddg-json)
 (cond
  ((non-empty-string? type)
   ((hash-ref typehash type) ddg-json))

  (else (handle-search ddg-json))
 )
)

(define (get-ddg-fmt text)
 (let
  ((ddg-json (get-ddg-json text)))
  (ddg-fmt (hash-ref ddg-json 'Type) ddg-json)
 )
)

(define (ddg)
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
  (reply msg (get-ddg-fmt text))
 )
)

(hash-set! *cmds* "%ddg" ddg)
