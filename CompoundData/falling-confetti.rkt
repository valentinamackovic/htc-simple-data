;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname falling-confetti) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/abstraction)
(require 2htdp/universe)

;; Animation of a single confetti falling from the position of mouse click. On every click a new confetti is created while the old one disappears.

;; =================
;; Constants:
(define WIDTH 600)
(define HEIGHT 400)
(define N 200)
(define COLORS (list "tomato" "indianred" "lightcoral" "saddlebrown" "sienna" "salmon" "goldenrod" "darkkhaki" "olivedrab" "aquamarine" "lightseagreen" "mediumturquoise" "dodgerblue" "steelblue" "cadetblue" "darkslategray" "mediumslateblue" "mediumorchid" "plum"))
(define SIZES (list 10 16 14 12 18))
(define S 3)
(define R 5)

(define CANVAS (rectangle WIDTH HEIGHT "solid" "white"))


;; =================
;; Data definitions:

(define-struct confetti (x y w h c r))
;; Confetti is (make-confetti Natural[0, WIDTH] Natural[0, HEIGHT] Natural Natural Color Number[0, 360))
;; interp. a rectangle in position x and y with randomised width and heigh, color and rotation

(define C-1 (make-confetti 10 12 4 16 "red" 60))

#;
(define (fn-for-confetti c)
  (... (confetti-w b)     ;Natural[0, WIDTH]
       (confetti-h b)     ;Natural[0, HEIGHT]
       (confetti-x b)     ;Natural
       (confetti-y b)     ;Natural
       (confetti-c b)     ;Color
       (confetti-r b)))   ;Number[0, 360)
;; Template rules used:
;;  - compound: 6 fields

;; =================
;; Functions:

;; Confetti -> Confetti
;; start the world with (main (make-confetti 50 50 10 8 "red" 10))
;; 
(define (main ws)
  (big-bang ws                               ; Confetti
            (on-tick   tock)                 ; Confetti -> Confetti
            (to-draw   render)               ; Confetti -> Image
            (on-mouse  handle-on-mouse)))    ; Confetti Integer Integer MouseEvent -> Confetti

;; Confetti -> Confetti
;; produce the next confetti falling down (speed S) and rotating (rotation R)

(check-expect (tock (make-confetti 50 50 10 8 "red" 10)) (make-confetti 50 53 10 8 "red" 15))

;; (define (tock c) (make-confetti 50 50 10 8 "red" 10)) ;stub

(define (tock c)
  (make-confetti (confetti-x c) (+ (confetti-y c) S) (confetti-w c) (confetti-h c) (confetti-c c) (+ (confetti-r c) R)))

;; Confetti -> Image
;; render a single confetti over the white background

(check-expect (render (make-confetti 50 50 10 8 "red" 10)) (place-image (rotate (modulo 10 360) (rectangle 10 8 "solid" "red")) 50 50 CANVAS))

;; (define (render c) ...) ;stub

(define (render c)
  (place-image (rotate (modulo (confetti-r c) 360) (rectangle (confetti-w c) (confetti-h c) "solid" (confetti-c c))) (confetti-x c) (confetti-y c) CANVAS))

;; Confetti Integer Integer MouseEvent -> Confetti
;; create a new confetti in the place of origination of mouse click

(check-random (handle-on-mouse (make-confetti 50 50 10 8 "red" 10) 1 1 "enter") (make-confetti 50 50 10 8 "red" 10))
(check-random (handle-on-mouse (make-confetti 50 50 10 8 "red" 10) 1 1 "button-up") (make-confetti 1 1 (list-ref SIZES (random (length SIZES))) (list-ref SIZES (random (length SIZES))) (list-ref COLORS (random (length COLORS))) (random 360)))

;;(define (handle-on-mouse c x y me) (make-confetti 50 50 10 8 "red" 10)) ;stub

(define (handle-on-mouse c x y me)
  (cond [(string=? me "button-up") (make-confetti x y (list-ref SIZES (random (length SIZES))) (list-ref SIZES (random (length SIZES))) (list-ref COLORS (random (length COLORS))) (random 360))]
        [else c]))

;; =================
;; A LOT of confetti
(define confettis-list
  (for/list ([i (in-range N)])
    (make-confetti (random WIDTH) (random HEIGHT) (list-ref SIZES (random (length SIZES))) (list-ref SIZES (random (length SIZES))) (list-ref COLORS (random (length COLORS))) (random 360))))

(define positions (for/list ([i (in-range N)])
    (make-posn (random WIDTH) (random HEIGHT))))

(define (confettis-list->image l)
    (for/list ([i (in-range N)])
        (rotate (confetti-r (list-ref l i)) (rectangle (confetti-w (list-ref l i)) (confetti-h (list-ref l i)) "solid" (confetti-c (list-ref l i))))))

(place-images
   (confettis-list->image confettis-list)
   positions
   CANVAS)

;; =================
;; Functions:

(define-struct confettis (c p))

;; Confetti -> Confetti
;; start the world with (testmain (make-confettis confettis-list positions))
;; 
(define (testmain confettis)
  (big-bang confettis    
            (on-tick   falling-confetti)       
            (to-draw   render-falling-confetti)))   

;; Confetti -> Confetti

(define (falling-confetti c)
  (make-confettis
   (for/list ([i (in-range N)])
    (make-confetti (confetti-x (list-ref (confettis-c c) i)) (+ (confetti-h (list-ref (confettis-c c) i)) S) (confetti-w (list-ref (confettis-c c) i)) (confetti-h (list-ref (confettis-c c) i)) (confetti-c (list-ref (confettis-c c) i)) (+ (confetti-r (list-ref (confettis-c c) i)) (random 30))))
   (for/list ([i (in-range N)])
     (make-posn  (posn-x (list-ref (confettis-p c) i)) (+ (posn-y (list-ref (confettis-p c) i)) (random 30))))))
  ;(make-confetti (confetti-x c) (+ (confetti-y c) S) (confetti-w c) (confetti-h c) (confetti-c c) (+ (confetti-r c) R)))

;; Confetti -> Image
(define (render-falling-confetti c)
  (place-images
   (confettis-list->image (confettis-c c))
   (confettis-p c)
   CANVAS))

