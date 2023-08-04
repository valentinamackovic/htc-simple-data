;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname odd-from-n) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; odd-from-n-starter.rkt

;  PROBLEM:
;  
;  Design a function called odd-from-n that consumes a natural number n, and produces a list of all 
;  the odd numbers from n down to 1. 
;  
;  Note that there is a primitive function, odd?, that produces true if a natural number is odd.
;  


;; Natural -> ListOfNatural
;; consumes a natural number n, and produces a list of all the odd numbers from n down to 1

(check-expect (odd-from-n 2) (cons 1 empty))
(check-expect (odd-from-n 1) (cons 1 empty))
(check-expect (odd-from-n 3) (cons 3 (cons 1 empty)))

;; (define (odd-from-n n) (cons 1 empty)) ;stub

(define (odd-from-n n)
  (cond
    [(= n 1) (cons 1 empty)]
    [(odd? n) (cons n (odd-from-n (sub1 n)))]
    [else (odd-from-n (sub1 n))]))

