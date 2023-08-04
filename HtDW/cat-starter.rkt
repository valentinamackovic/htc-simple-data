;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cat-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; cat-starter.rkt

; 
; PROBLEM:
; 
; Use the How to Design Worlds recipe to design an interactive
; program in which a cat starts at the left edge of the display 
; and then walks across the screen to the right. When the cat
; reaches the right edge it should just keep going right off 
; the screen.
; 
; Once your design is complete revise it to add a new feature,
; which is that pressing the space key should cause the cat to
; go back to the left edge of the screen. When you do this, go
; all the way back to your domain analysis and incorporate the
; new feature.
; 
; To help you get started, here is a picture of a cat, which we
; have taken from the 2nd edition of the How to Design Programs 
; book on which this course is based.
; 
; .
; 

(require 2htdp/image)
(require 2htdp/universe)



;; My world program  (make this more specific)

;; =================
;; Constants:
(define WIDTH 300)
(define HEIGHT 200)
(define CAT-Y (/ HEIGHT 2))
(define CANVAS (rectangle WIDTH HEIGHT "solid" "white"))
(define CAT .)
(define SPEED 3)


;; =================
;; Data definitions:

;; Cat is a Number
;; interp as the position of cat on x axis

(define C1 3)
(define C2 9)

#;
(define (fn-for-cat c)
  (... c))

;; Template rules used:
;;  - atomic non-distinct: Number

;; =================
;; Functions:

;; Cat -> Cat
;; start the world with cat position equal to 0

(define (main c)
  (big-bang c                             ; Cat
            (on-tick   next-position)     ; Cat -> Cat
            (to-draw   cat->image)        ; Cat -> Image
            (on-key    reset-position)))  ; Cat, KeyEvent -> Cat


;; Cat -> Cat

;; Cat -> Cat
;; consumes current position of the cat, produces the next position (Cat + Speed)

;; (define (next-position c ) 9) ;tsub

(check-expect (next-position 3 ) (+ 3 SPEED))
(check-expect (next-position 9 ) (+ 9 SPEED))

#;
(define (next-position c )
  (... c SPEED)) ;template

(define (next-position c)
  (+ c SPEED))

;; Cat -> Image
;; consumes current position of the cat, returns an image placed on x axis according to the consumed position

;; (define (cat->image c) (place-image CAT 3 150 CANVAS)) ;stub

(check-expect (cat->image 3) (place-image CAT 3 100 CANVAS))
(check-expect (cat->image 48) (place-image CAT 48 100 CANVAS))

#;
(define (cat->image c)
  (... c)) ;template

(define (cat->image c)
  (place-image CAT c CAT-Y CANVAS))

;; Cat, HandlerResult -> Cat
;; consumes cats current position adn HandlerResult, produces the new cat position

;; check expect, stub and template??
;; (define (reset-position c ke) 0) ;stub

(check-expect (reset-position 10 " ") 0)
(check-expect (reset-position 10 "a") 10)

(define (reset-position c a-key)
  (cond
    [(key=? a-key " ") 0]
    [else c]))

(main 0)
