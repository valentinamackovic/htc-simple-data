;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spinning-bears) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; spinning-bears-starter.rkt

(require 2htdp/image)
(require 2htdp/universe)

; PROBLEM:
; 
; In this problem you will design another world program. In this program the changing 
; information will be more complex - your type definitions will involve arbitrary 
; sized data as well as the reference rule and compound data. But by doing your 
; design in two phases you will be able to manage this complexity. As a whole, this problem 
; will represent an excellent summary of the material covered so far in the course, and world 
; programs in particular.
; 
; This world is about spinning bears. The world will start with an empty screen. Clicking
; anywhere on the screen will cause a bear to appear at that spot. The bear starts out upright,
; but then rotates counterclockwise at a constant speed. Each time the mouse is clicked on the 
; screen, a new upright bear appears and starts spinning.
; 
; So each bear has its own x and y position, as well as its angle of rotation. And there are an
; arbitrary amount of bears.
; 
; To start, design a world that has only one spinning bear. Initially, the world will start
; with one bear spinning in the center at the screen. Clicking the mouse at a spot on the
; world will replace the old bear with a new bear at the new spot. You can do this part 
; with only material up through compound. 
; 
; Once this is working you should expand the program to include an arbitrary number of bears.
; 
; Here is an image of a bear for you to use: .


;; =================
;; Constants:
(define BEAR .)
(define WIDTH 800)
(define HEIGHT 650)
(define CANVAS (rectangle WIDTH HEIGHT "solid" "white"))
(define ROTATION-STEP 3)

;; =================
;; Data definitions:

(define-struct bear (x y r))
;; Bear is (make-bear Number[0, WIDTH] Number[0, HEIGHT] Number[0, 360])
;; interp. a bear at position x, y with rotation r

(define BEAR-1 (make-bear 100 150 0))
(define BEAR-2 (make-bear 300 350 100))

#;
(define (fn-for-bear b)
  (... (bear-x b)       ;Number[0, WIDTH]
       (bear-y b)       ;Number[0, HEIGHT]
       (bear-r b)))     ;Number[0, 360]

;; Template rules used:
;;  - compound: 3 fields


;; ListOfBear is one of:
;;  - empty
;;  - (cons String ListOfBear)
;; interp. a list of bears

(define LOB-1 empty)
(define LOB-2 (cons BEAR-1 empty))
(define LOB-3 (cons BEAR-1 (cons BEAR-2 empty)))

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]                           ;BASE CASE
        [else (fn-for-bear (first lob)                 ;Bear
                           (fn-for-lob (rest lob)))])) ;NATURAL RECURSION
;;             /
;;            /
;;       COMBINATION
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Bear ListOfBear)
;;  - self-reference: (rest lob) is ListOfBear
;;  - reference: (first lob) is Bear


;; =================
;; Functions:

;; ListOfBear -> ListOfBear
;; start the world with (main (cons (make-bear (/ WIDTH 2) (/ HEIGHT 2) 0) empty))

(define (main ws)
  (big-bang ws                        ; ListOfBear
            (on-tick   rotate-bears)  ; ListOfBear -> ListOfBear
            (to-draw   bears->image)  ; ListOfBear -> Image
            (on-mouse  spawn-bear)))  ; ListOfBear Integer Integer MouseEvent -> ListOfBear   ;"button-down"

;; Bear -> Bear
;; produce the next bear rotation

(check-expect (rotate-bear (make-bear 100 150 0)) (make-bear 100 150 (+ (bear-r (make-bear 100 150 0)) ROTATION-STEP)))
(check-expect (rotate-bear (make-bear 100 150 360)) (make-bear 100 150 (modulo (+ (bear-r (make-bear 100 150 360)) ROTATION-STEP) 360)))
(check-expect (rotate-bear (make-bear 100 150 180)) (make-bear 100 150 (+ (bear-r (make-bear 100 150 180)) ROTATION-STEP)))

;; (define (rotate-bear b) (make-bear 100 150 5)) ;stub

(define (rotate-bear b)
  (make-bear (bear-x b) (bear-y b) (modulo (+ (bear-r b) ROTATION-STEP) 360)))

;; ListOfBear -> ListOfBear
;; produce the next bear rotation for all bears in ListOfBear

(check-expect (rotate-bears empty) empty)
(check-expect (rotate-bears (cons BEAR-1 empty)) (cons (rotate-bear BEAR-1) empty))
(check-expect (rotate-bears (cons BEAR-1 (cons BEAR-2 empty))) (cons (rotate-bear BEAR-1) (cons (rotate-bear BEAR-2) empty)))

;; (define (rotate-bears lob) empty) ;stub

(define (rotate-bears lob)
  (cond [(empty? lob) empty]
        [else (cons (rotate-bear (first lob)) (rotate-bears (rest lob)))]))

;; Bear, Image -> Image
;; render image of a bear in x, y positions and rotation r on top of image that has been provided

(check-expect (bear->image BEAR-1 CANVAS) (place-image (rotate (bear-r BEAR-1) BEAR) (bear-x BEAR-1) (bear-y BEAR-1) CANVAS))

;; (define (bear->image b) (place-image (rotate 180 BEAR) 100 100 CANVAS)) ;stub

(define (bear->image b i)
  (place-image (rotate (bear-r b) BEAR) (bear-x b) (bear-y b) i))

;; ListOfBear -> ListOfImage
;; consumes list of bears, produces list of images that have corresponding x,y positions and rotation

(check-expect (bears->image empty) CANVAS)
(check-expect (bears->image (cons BEAR-1 empty)) (bear->image BEAR-1 CANVAS))
(check-expect (bears->image (cons BEAR-1 (cons BEAR-2 empty))) (bear->image BEAR-2 (bear->image BEAR-1 CANVAS)))

;; (define (bears->image b) empty) ;stub

(define (bears->image lob)
  (cond [(empty? lob) CANVAS]
        [else (bear->image (first lob)
                           (bears->image (rest lob)))]))

;; ListOfBear Integer Integer MouseEvent -> ListOfBear
;; create a new bear with x and y positions set to the position of mouse click event and rotation set to 0 and add it to the existing list of bears
;; !!!

(check-expect (spawn-bear empty 200 300 "button-down") (cons (make-bear 200 300 0) empty))
(check-expect (spawn-bear (cons BEAR-1 empty) 250 300 "button-down") (cons (make-bear 250 300 0) (cons BEAR-1 empty)))
(check-expect (spawn-bear (cons BEAR-1 empty) 250 300 "button-up") (cons BEAR-1 empty))

;; (define (spawn-bear lob x y me) (cons BEAR-1 lob)) ;stub

(define (spawn-bear lob x y me)
  (cond [(string=? me "button-down") (cons (make-bear x y 0) lob)]
        [else lob]))


