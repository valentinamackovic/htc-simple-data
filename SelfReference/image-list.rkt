;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname image-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; image-list-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; Design a data definition to represent a list of images. Call it ListOfImage. 
; 



;; ListOfImages is one of:
;;  - empty
;;  - (cons Image ListOfImages)
;; interp. a list of images
(define LOI1 empty)
(define LOI2 (cons (square 10 "solid" "red") (cons (circle 10 "solid" "red") empty)))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Image ListOfImages)
;;  - self-reference: (rest loi) is ListOfImages


;; =================
;; Functions:

; 
; PROBLEM B:
; 
; Design a function that consumes a list of images and produces a number 
; that is the sum of the areas of each image. For area, just use the image's 
; width times its height.
; 


;; ListOfImages -> Number
;; consumes list of images, produces sum of the areas of each image (image width * image height), for empty list return 0

(check-expect (area-sum empty) 0)
(check-expect (area-sum (cons (square 10 "solid" "red") empty)) 100)
(check-expect (area-sum (cons (square 10 "solid" "red") (cons (square 2 "solid" "red") empty))) 104)
(check-expect (area-sum (cons (rectangle 10 3 "solid" "red") empty)) 30)
(check-expect (area-sum (cons (rectangle 10 2 "solid" "red") (cons (rectangle 2 12 "solid" "red") empty))) 44)

;; (define (area-sum loi) 0) stub

(define (area-sum loi)
  (cond [(empty? loi) 0]
        [else
         (+ (* (image-height (first loi)) (image-width (first loi)))
              (area-sum (rest loi)))]))


