;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname countdown-animation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; countdown-animation starter.rkt

; 
; PROBLEM:
; 
; Design an animation of a simple countdown. 
; 
; Your program should display a simple countdown, that starts at ten, and
; decreases by one each clock tick until it reaches zero, and stays there.
; 
; To make your countdown progress at a reasonable speed, you can use the 
; rate option to on-tick. If you say, for example, 
; (on-tick advance-countdown 1) then big-bang will wait 1 second between 
; calls to advance-countdown.
; 
; Remember to follow the HtDW recipe! Be sure to do a proper domain 
; analysis before starting to work on the code file.
; 
; Once you are finished the simple version of the program, you can improve
; it by reseting the countdown to ten when you press the spacebar.
; 


(require 2htdp/image)
(require 2htdp/universe)

;; Animation of a simple countdown

;; =================
;; Constants:

(define HEIGHT 200)
(define WIDTH HEIGHT)
(define TEXT-SIZE 20)
(define TEXT-COLOR "black")
(define CY (/ HEIGHT 2))
(define CX (/ WIDTH 2))
(define CANVAS (rectangle HEIGHT WIDTH "solid" "white"))


;; =================
;; Data definitions:

;; Countdown is Natural[0,10]
;; interp. the number of seconds remaining to liftoff
(define C1 10)  ; start
(define C2 5)   ; middle
(define C3 0)   ; end
 
#;
(define (fn-for-countdown c)
  (... c))

;; Template rules used:
;;  - atomic non-distinct: Natural[0, 10]

;; =================
;; Functions:

;; Countdown -> Countdown
;; start the world with (main 10)

(define (main c)
  (big-bang c                               ; Countdown
            (on-tick  countdown 1)          ; Countdown -> Countdown
            (to-draw  countdown->image)))   ; Countdown -> Image

;; Countdown -> Countdown
;; consumes countdown, produces countdown reduced 1

;; (define (countdown c) 9) ;stub

(check-expect (countdown 10) 9)
(check-expect (countdown 0) 0)

;; <use template for countdown>

(define (countdown c)
  (if (= c 0)
      0
      (- c 1)))

;; Countdown -> Image
;; render countdown number as an image

;; (define (countdown->image c) 9) ;stub

(check-expect (countdown->image 10) (place-image (text "10" TEXT-SIZE TEXT-COLOR) CX CY CANVAS))

(define (countdown->image c)
  (place-image (text (number->string c) TEXT-SIZE TEXT-COLOR) CX CY CANVAS))

(main 10)