;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cowabunga-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; 
; PROBLEM:
; 
; As we learned in the cat world programs, cats have a mind of their own. When they 
; reach the edge they just keep walking out of the window.
; 
; Cows on the other hand are docile creatures. They stay inside the fence, walking
; back and forth nicely.
; 
; Design a world program with the following behaviour:
;    - A cow walks back and forth across the screen.
;    - When it gets to an edge it changes direction and goes back the other way
;    - When you start the program it should be possible to control how fast a
;      walker your cow is.
;    - Pressing space makes it change direction right away.
;    
; To help you here are two pictures of the right and left sides of a lovely cow that 
; was raised for us at Brown University.
; 
; .     .
; 
; Once your program works here is something you can try for fun. If you rotate the
; images of the cow slightly, and you vary the image you use as the cow moves, you
; can make it appear as if the cow is waddling as it walks across the screen.
; 
; Also, to make it look better, arrange for the cow to change direction when its
; nose hits the edge of the window, not the center of its body.
; 


(require 2htdp/image)
(require 2htdp/universe)

;; Cow walking back and forth in the window

;; =================
;; Constants:
(define WIDTH 400)
(define HEIGHT 250)
(define CANVAS (rectangle WIDTH HEIGHT "solid" "white"))
(define CTRY (/ HEIGHT 2))
(define SPEED 3) ;in pixels

(define RCOW .)
(define LCOW .)


;; =================
;; Data definitions:

;; Direction is one of:
;;  - "left"
;;  - "right"
;; interp. the direction in which cow is facing

;; <examples are redundant for enumerations>
 
#;
(define (fn-for-direction d)
  (cond [(string=? "left" d) (...)]
        [(string=? "right" d) (...)]))
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: "left"
;;  - atomic distinct: "right"

(define-struct cow (position direction))
;; Cow is (make-cow Natural[0, WIDTH] Direction)
;; interp. a cow at position x and direction (facing left or right)

(define COW-1 (make-cow 100 "right"))
(define COW-2 (make-cow 100 "left"))

#;
(define (fn-for-cow c)
  (... (cow-position c)      ;Natural[0, WIDTH]
       (cow-direction c)))   ;Direction

;; Template rules used:
;;  - compound: 2 fields

;; =================
;; Functions:

;; Cow -> Cow
;; start the world with (main (make-cow 0 "right"))
;; 
(define (main c)
  (big-bang c                            ; Cow
            (on-tick   advance)          ; Cow -> Cow
            (to-draw   render)           ; Cow -> Image
            (on-key    handle-on-key)))  ; Cow KeyEvent -> Cow

;; Cow -> Cow
;; produce the next Cow position, if the Cow got to the edge of the window turn it around (change the direction)

(check-expect (advance (make-cow 100 "right")) (make-cow 103 "right"))
(check-expect (advance (make-cow 100 "left")) (make-cow 97 "left"))

(check-expect (advance (make-cow WIDTH "right")) (make-cow WIDTH "left")) ;gets to the right edge of window and turns

(check-expect (advance (make-cow 0 "left")) (make-cow 0 "right")) ;gets to the left edge of window and turns

;; (define (advance c) (make-cow 0 "right")) ;stub

(define (advance c)
  (cond [(string=? (cow-direction c) "right")
         (if (>= (+ (cow-position c) SPEED) WIDTH)
             (make-cow WIDTH "left")
             (make-cow (+ (cow-position c) SPEED) "right"))]
        [(string=? (cow-direction c) "left")
         (if (<= (- (cow-position c) SPEED) 0)
             (make-cow 0 "right")
             (make-cow (- (cow-position c) SPEED) "left"))]))

;; Cow -> Image
;; produces LCOW if direction of cow is "left" and RCOW if it's "right"

(check-expect (choose-image (make-cow 100 "right")) RCOW)
(check-expect (choose-image (make-cow 100 "left")) LCOW)

;; (define (choose-image c) RCOW) ;stub

(define (choose-image c)
  (cond [(string=? (cow-direction c) "left") LCOW]
        [(string=? (cow-direction c) "right") RCOW]))


;; Cow -> Image
;; render cow image, taking into consideration the direction it's facing

(check-expect (render (make-cow 100 "right")) (place-image RCOW 100 CTRY CANVAS))
(check-expect (render (make-cow 50 "left")) (place-image LCOW 50 CTRY CANVAS))

;; (define (render c) (place-image RCOW 100 CTRY CANVAS)) ;stub

(define (render c)
  (place-image (choose-image c) (cow-position c) CTRY CANVAS))


;; Cow KeyEvent -> Cow
;; when ke is equal to " " produces Cow reset to the intial state with position zero and direction "right", otherwise returns the current state

(check-expect (handle-on-key (make-cow 100 "left") " ") (make-cow 0 "right"))
(check-expect (handle-on-key (make-cow 50 "right") " ") (make-cow 0 "right"))
(check-expect (handle-on-key (make-cow 50 "right") "c") (make-cow 50 "right"))

;; (define (handle-on-key c ke) (make-cow 0 "right")) ;stub

(define (handle-on-key c ke)
  (cond [(string=? ke " ") (make-cow 0 "right")]
        [else c]))

