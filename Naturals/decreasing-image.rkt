;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname decreasing-image) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; decreasing-image-starter.rkt

;  PROBLEM:
;  
;  Design a function called decreasing-image that consumes a Natural n and produces an image of all the numbers 
;  from n to 0 side by side. 
;  
;  So (decreasing-image 3) should produce .


(define TEXT-SIZE 16)
(define TEXT-COLOR "black")

;; Natural -> Image
;; consumes number n, produces an image of all the numbers from n to 0 side by side

(check-expect (decreasing-image 0) (text "0" TEXT-SIZE TEXT-COLOR))
(check-expect (decreasing-image 2) (beside (text "2" TEXT-SIZE TEXT-COLOR) (text "1" TEXT-SIZE TEXT-COLOR) (text "0" TEXT-SIZE TEXT-COLOR)))

;; (define (decreasing-image n) (text "0" TEXT-SIZE TEXT-COLOR)) ;stub

(define (decreasing-image n)
  (cond
    [(zero? n) (text "0" TEXT-SIZE TEXT-COLOR)]
    [else  (beside (text (number->string n) TEXT-SIZE TEXT-COLOR) (decreasing-image (- n 1)))]))


