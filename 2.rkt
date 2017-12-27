#lang typed/racket

(require typed/rackunit)

(define (diff-list [n : (Listof Number)])  : Number
  (- (apply max (cast n (Listof Real))) (apply min (cast n (Listof Real)))))

(define (get-list [s : String]) : (Listof Number)
  (define sl (string-split s "\t"))
  (map (lambda ([s : String]) : Number (assert (string->number s) exact-integer?)) sl))

(define (diff-all [all : (Listof (Listof Number))]) : Number
  (apply + (map diff-list all)))
  
(check-eq? 1 (diff-list (list 4 5)))
(check-eq? 1 (diff-list (list 4 4 5)))
(check-eq? 7 (diff-list (list 1 2 2 8 3)))
(check-eq? 9 (diff-all (list (list 1 4 5) (list 3 4 8))))

(diff-all (map get-list (file->lines "input/2")))

; part b

(define (get-evenly-divisible [n : Number] [l : (Listof Number)]) : (Listof Number)
  (filter (λ ([x : Number]) : Boolean (and (not (eq? x n)) (eq? (modulo (cast n Integer) (cast x Integer) ) 0))) l))

(define (get-dividend [l : (Listof Number)]) : Number
  (define even-numbers (map (lambda ([x : Number]) : (Listof Number) (cons x (get-evenly-divisible x l))) l))
  (define valid-list (first (filter (lambda ([x : (Listof Number)]) : Boolean (eq? 2 (length x))) even-numbers)))
  (/ (first valid-list) (second valid-list)))

(apply + (map get-dividend (map get-list (file->lines "input/2"))))