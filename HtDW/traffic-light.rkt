;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; traffic-light-starter.rkt

; 
; PROBLEM:
; 
; Design an animation of a traffic light. 
; 
; Your program should show a traffic light that is red, then green, 
; then yellow, then red etc. For this program, your changing world 
; state data definition should be an enumeration.
; 
; Here is what your program might look like if the initial world 
; state was the red traffic light:
; .
; Next:
; .
; Next:
; .
; Next is red, and so on.
; 
; To make your lights change at a reasonable speed, you can use the 
; rate option to on-tick. If you say, for example, (on-tick next-color 1) 
; then big-bang will wait 1 second between calls to next-color.
; 
; Remember to follow the HtDW recipe! Be sure to do a proper domain 
; analysis before starting to work on the code file.
; 
; Note: If you want to design a slightly simpler version of the program,
; you can modify it to display a single circle that changes color, rather
; than three stacked circles. 
; 


;; Animation of a traffic light.

;; =================
;; Constants:
(define (render ws) ...)
(define CW 50)
(define CW/2 (/ 50 2))
(define HEIGHT (+ (* CW 3) 40))
(define WIDTH (+ CW 20))
(define CANVAS (rectangle WIDTH HEIGHT "solid" "black"))
(define CTRY (/ HEIGHT 2))
(define CTRX (/ WIDTH 2))

(define RED-OUTLINE (circle CW/2 "outline" "red"))
(define RED-LIGHT (circle CW/2 "solid" "red"))

(define YELLOW-OUTLINE (circle CW/2 "outline" "yellow"))
(define YELLOW-LIGHT (circle CW/2 "solid" "yellow"))

(define GREEN-OUTLINE (circle CW/2 "outline" "green"))
(define GREEN-LIGHT (circle CW/2 "solid" "green"))

;; =================
;; Data definitions:

;; TrafficLight is one of:
;;  - "red"
;;  - "yellow"
;;  - "green"
;; interp. the color of a traffic light

;; <examples are redundant for enumerations>
 
#;
(define (fn-for-traffic-light tl)
  (cond [(string=? "red" tl) (...)]
        [(string=? "yellow" tl) (...)]
        [(string=? "green" tl) (...)]))

;; Template rules used:
;;  - one of: 3 cases
;;  - atomic distinct: "red"
;;  - atomic distinct: "yellow"
;;  - atomic distinct: "green"

;; =================
;; Functions:

;; TrafficLight -> TrafficLight
;; start the world with (main "red")

(define (main tl)
  (big-bang tl                      ; TrafficLight
            (on-tick next-light 1)  ; TrafficLight -> TrafficLight
            (to-draw render-lights)))        ; TrafficLight -> Image

;; TrafficLight -> TrafficLight
;; produce the next traffic light in order: red -> yellow -> green -> red

;; (define (next-light tl) "red") ;stub

(check-expect (next-light "red") "yellow")
(check-expect (next-light "yellow") "green")
(check-expect (next-light "green") "red")

;; <took template from TrafficLight>

(define (next-light tl)
  (cond [(string=? "red" tl) "yellow"]
        [(string=? "yellow" tl) "green"]
        [(string=? "green" tl) "red"]))

;; TrafficLight -> Image
;; render image of a traffic light with only the light currently on should be respresented as a solid circle

;; (define (render-lights tl) ...) ; stub

(check-expect (render-lights "red") (overlay (above RED-LIGHT YELLOW-OUTLINE GREEN-OUTLINE) CANVAS))
(check-expect (render-lights "yellow") (overlay (above RED-OUTLINE YELLOW-LIGHT GREEN-OUTLINE) CANVAS))
(check-expect (render-lights "green") (overlay (above RED-OUTLINE YELLOW-OUTLINE GREEN-LIGHT) CANVAS))

;; <took template from TrafficLight>

(define (render-lights tl)
  (cond [(string=? "red" tl) (overlay (above RED-LIGHT YELLOW-OUTLINE GREEN-OUTLINE) CANVAS)]
        [(string=? "yellow" tl) (overlay (above RED-OUTLINE YELLOW-LIGHT GREEN-OUTLINE) CANVAS)]
        [(string=? "green" tl) (overlay (above RED-OUTLINE YELLOW-OUTLINE GREEN-LIGHT) CANVAS)]))