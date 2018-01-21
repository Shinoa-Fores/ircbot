#lang racket

(provide run-script update-scripts)

(require irc)
(require "lib/structs.rkt")
(require "lib/utils.rkt")

(define hooked (list))
(define script-running? (make-hash))

; directory of the scripts to watch relatively to the bot.rkt executable
(define bindir "bin/")

;;; TODO:
; maybe find a way to let scripts hook to irc commands

(define (exec-script msg script nick channel text)
 (let*-values
  (((script-proc script-out script-in script-err)
    (parameterize ((current-directory bindir))
     (subprocess
      #f
      #f
      #f
      script
      nick channel text
     )
    )
   )

   ((lines) (string-split (read-string 5000 script-out) "\n"))
  )

  (begin
   (for ((line lines))
    (reply msg (ellipsize-string line 400))
   )

   (close-input-port script-err)
   (close-input-port script-out)
   (close-output-port script-in)
  )
 )
)

(define (run-script)
 (let*
  ((msg (thread-receive))
   (nick (get-nick msg))
   (channel (car (irc-message-parameters msg)))

   (whole-text (cadr (irc-message-parameters msg)))
   (script (car (string-split whole-text)))
   (text (string-join (cdr (string-split whole-text)) " ")))
  (exec-script msg script nick channel text)
 )
)

; update the scripts in the hash table
(define (update-scripts)
 (begin
  (unhook-scripts)
  (hook-scripts)
 )
)

; remove every hook in hooked from *cmds*
(define (unhook-scripts)
 (for ((hook hooked))
  (begin
   (hash-remove! *cmds* hook)
   (set! hooked (remove hook hooked))
  )
 )
)

; set a command for each file name in ./mods
(define (hook-scripts)
 (let ((scripts (directory-list bindir)))
  (for ((script scripts))
   (cond
    ((member 'execute
      (file-or-directory-permissions (string-append bindir (~a script))))
     (let ((scriptname (~a script)))
      (begin
       (hash-set! *cmds* scriptname run-script)
       (set! hooked (append hooked (list scriptname)))
      )
     )
    )
   )
  )
 )
)

(hook-scripts)
(hash-set! *cmds* "%update-scripts" update-scripts)
