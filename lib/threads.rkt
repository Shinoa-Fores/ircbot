#lang racket
(provide thread-watcher cmd-thread)

(struct watched-thread (creation-time tid))
(define thread-list (list))
(define max-time 600)

(define (thread-watcher)
 (let
  ((cthd
    (if (null? thread-list)
        (begin (sleep 300) (thread-watcher))
        (car thread-list)
    )
   )
  )
  (begin
   (cond
    ((thread-dead? (watched-thread-tid cthd))
     (set! thread-list (cdr thread-list)))

    ((>= (- (current-seconds) (watched-thread-creation-time cthd)) max-time)
     (begin
      (kill-thread (watched-thread-tid cthd))
      (set! thread-list (cdr thread-list))
     )
    )

    (else (sleep 60))
   )

   (thread-watcher)
  )
 )
)

; Start a thread, append to watched list, send it its message
(define (cmd-thread func msg)
 (let ((tid (thread func)))
  (begin
   (thread-send tid msg)
   (append thread-list (watched-thread (current-seconds) tid))
  )
 )
)
