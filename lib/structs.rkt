#lang racket

(provide
 add-regex-cmd
 *cmds*
 *rgxlst*
 (struct-out regex-cmd)
)

;; Hash table containing commands => function
(define *cmds* (make-hash))

;; List containing regexes => function
(struct regex-cmd (regex func))
(define *rgxlst* (list))

(define (add-regex-cmd regex func)
 (cond
  ((and (regexp? regex) (procedure? func))
   (set! *rgxlst* (append *rgxlst* (list (regex-cmd regex func)))))
 )
)
