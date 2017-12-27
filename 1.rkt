#lang typed/racket
(require typed/rackunit)
(require racket/flonum)
(require readline)

(define (check-first-last [captcha : (Listof Number)]) : Number
  (cond
    [(eq? (first captcha) (last captcha)) (first captcha)]
    [else 0]))

(define (solve-captcha [captcha : (Listof Number)] [sum : Number]) : Number
  (match captcha
    [(cons f r)
     (cond
       [(eq? r '()) sum]
       [(eq? f (first r))
        (solve-captcha r (+ f sum))]
       [else (solve-captcha r sum)])]
    [else sum]))

(define (main [captcha : String]) : Number
  (define captcha-list (string->list captcha))
  (define captcha-nums
    (map (lambda ([n : Char]) : Number (assert (string->number (string n)) exact-integer?))
         captcha-list))
  (solve-captcha captcha-nums (check-first-last captcha-nums)))

(define (solve-captcha-2 [indices : (Listof Integer)] [captcha : (Listof Number)] [step : Integer] [len : Integer] [sum : Number]) : Number
  (match indices
    [(cons f r)
     (define other-index (modulo (+ f step) len))
     (cond
       [(eq? (list-ref captcha f) (list-ref captcha other-index))
        (solve-captcha-2 r captcha step len (+ sum (list-ref captcha f)))]
       [else (solve-captcha-2 r captcha step len sum)])]
    [else sum]))

(define (main_2 [captcha : String]) : Number
  (define captcha-list (string->list captcha))
  (define captcha-nums
    (map (lambda ([n : Char]) : Number (assert (string->number (string n)) exact-integer?))
         captcha-list))
  (define indices (range 0 (length captcha-nums)))
  (define len (length captcha-nums))
  (define step (inexact->exact (floor (/ len 2))))
  (solve-captcha-2 indices captcha-nums step (length captcha-nums) 0))


(check-eq? (check-first-last (list 1 1)) 1)
(check-eq? (check-first-last (list 2)) 2)

(check-eq? (main "1122") 3)
(check-eq? (main "1111") 4)
(check-eq? (main "91212129") 9)
(check-eq? (main_2 "123123") 12)
(check-eq? (main_2 "12131415") 4)
(check-eq? (main_2 "1212") 6)

(define input (cast (read-line (open-input-file "input/1")) String))
(main input)
(main_2 input)
