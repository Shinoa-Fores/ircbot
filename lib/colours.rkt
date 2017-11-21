#lang racket

(provide clpref clbold clhash colourize)

(define clpref "")
(define clbold "")

(define clhash
 (hash
  "white"   "00"
  "black"   "01"
  "navy"    "02"
  "green"   "03"
  "red"     "04"
  "maroon"  "05"
  "purple"  "06"
  "olive"   "07"
  "yellow"  "08"
  "lgreen"  "09"
  "teal"    "10"
  "cyan"    "11"
  "blue"    "12"
  "magenta" "13"
  "grey"    "14"
  "lgrey"   "15"
 )
)

(define (colourize text fg)
 (string-append clpref (hash-ref clhash fg "")
  text
  clpref
 )
)
