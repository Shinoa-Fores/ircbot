#!/usr/bin/env racket
#lang racket
;;;; Create the tables in the database and fill them.

(require db)
(require "../conf/db.rkt")

(define num-cards 0)

(query-exec *db*
 "CREATE TABLE IF NOT EXISTS uno_whole_deck ( name varchar(20), type varchar(5), colour varchar(10), special boolean, value int, id int, UNIQUE (id) )"
)
(query-exec *db*
 "CREATE TABLE IF NOT EXISTS uno_decks ( channel varchar(100), id int )"
)
(query-exec *db*
 "CREATE TABLE IF NOT EXISTS uno_hands ( channel varchar(100), player varchar(50), id int )"
)
(query-exec *db*
 "CREATE TABLE IF NOT EXISTS uno_topcard ( channel varchar(100), name varchar(20), type varchar(5), colour varchar(10), special boolean, id int, UNIQUE(channel) )"
)
(query-exec *db*
 "CREATE TABLE IF NOT EXISTS uno_scoreboard ( channel varchar(100), player varchar(50), score int, games int )"
)

(define (add-card amount name type colour special? value)
 (for [(i (in-range amount))]
  (begin
   (set! num-cards (+ num-cards 1))
   (query-exec *db* "INSERT INTO uno_whole_deck VALUES ($1, $2, $3, $4, $5, $6)"
    name type colour special? value num-cards)
  )
 )
)

(for [(colour (list "red" "yellow" "green" "blue"))]
 (begin
  (add-card 1 (string-append colour " 0") (~a 0) colour #f 0)
  (add-card 2 (string-append colour " draw-two") "d2" colour #t 20)
  (add-card 2 (string-append colour " reverse") "r" colour #t 20)
  (add-card 2 (string-append colour " skip") "s" colour #t 20)
  (for [(num (in-range 1 10))]
   (add-card 2 (string-append colour " " (~a num)) (~a num) colour #f num)
  )
 )
)

(add-card 4 "wild card" "w" "wild" #f 50)
(add-card 4 "wild draw-four" "wd4" "wild" #t 50)
