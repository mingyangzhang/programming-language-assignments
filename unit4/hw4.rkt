#lang racket

;; programming language hw4
;; author: Mingyang Zhang
(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; return number sequence
(define (sequence low high stride)
  (cond
     [(> low high) null]
     [#t (cons low (sequence (+ low stride) high stride))]))

;; append string with suffix
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; return ith element of list
(define (list-nth-mod xs n)
  (cond
    [(null? xs) (error "list-nth-mod: empty list")]
    [(< n 0) (error "list-nth-mod: negative number")]
    [(= (remainder n (length xs)) 0) (car xs)]
    [#t (list-nth-mod (cdr xs) (- n 1))]))
