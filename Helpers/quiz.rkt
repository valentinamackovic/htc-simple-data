;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname quiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; SPD2-Design-Quiz-1.rkt


;; ======================================================================
;; Constants
(define COOKIES .)

;; ======================================================================
;; Data Definitions

;; Natural is one of:
;;  - 0
;;  - (add1 Natural)
;; interp. a natural number
(define N0 0)         ;0
(define N1 (add1 N0)) ;1
(define N2 (add1 N1)) ;2

#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... n   ; n is added because it's often useful                   
              (fn-for-natural (sub1 n)))]))

;; Template rules used:
;;  - one-of: two cases
;;  - atomic distinct: 0
;;  - compound: 2 fields
;;  - self-reference: (sub1 n) is Natural


; PROBLEM 1:
; 
; Complete the design of a function called pyramid that takes a natural
; number n and an image, and constructs an n-tall, n-wide pyramid of
; copies of that image.
; 
; For instance, a 3-wide pyramid of cookies would look like this:
; 
; .


;; Natural Image -> Image
;; produce an n-wide pyramid of the given image
(check-expect (pyramid 0 COOKIES) empty-image)
(check-expect (pyramid 1 COOKIES) COOKIES)
(check-expect (pyramid 3 COOKIES)
              (above COOKIES
                     (beside COOKIES COOKIES)
                     (beside COOKIES COOKIES COOKIES)))

; (define (pyramid n i) empty-image) ; stub

(define (pyramid n i )
  (pyramid-image (create-loi n i)))


;; Natural, Image -> ListOfImage
;; produces a list of images i and length n

(check-expect (create-loi 0 COOKIES) empty)
(check-expect (create-loi 2 COOKIES) (cons COOKIES (cons COOKIES empty)))

; (define (create-loi n i) (cons i empty))

(define (create-loi n i)
  (cond [(= n 0) empty]
        [else (cons i (create-loi (- n 1) i))]))

;; LisOfImage -> Image
;; consumes list of image that represents the first base row of the pyramid, produces one image n-tall and n-wide -> n being the length of loi, each row should have one image less

(check-expect (pyramid-image empty) empty-image)
(check-expect (pyramid-image (cons COOKIES empty)) COOKIES)
(check-expect (pyramid-image (cons COOKIES (cons COOKIES empty))) (above COOKIES
                                                                         (beside COOKIES COOKIES)))

; (define (pyramid-image loi) (above COOKIES empty-image))

(define (pyramid-image loi)
  (cond [(empty? loi) empty-image]
        [else (above (pyramid-image (rest loi)) (pyramid-row loi))]))


;; ListOfImage -> Image
;; produces one row of images beside each other

(check-expect (pyramid-row empty) empty-image)
(check-expect (pyramid-row (cons COOKIES empty)) COOKIES)
(check-expect (pyramid-row (cons COOKIES (cons COOKIES empty))) (beside COOKIES COOKIES))

; (define (pyramid-row loi) empty-image)

(define (pyramid-row loi)
  (cond [(empty? loi) empty-image]
        [else (beside (first loi) (pyramid-row (rest loi)))]))


; Problem 2:
; Consider a test tube filled with solid blobs and bubbles.  Over time the
; solids sink to the bottom of the test tube, and as a consequence the bubbles
; percolate to the top.  Let's capture this idea in BSL.
; 
; Complete the design of a function that takes a list of blobs and sinks each
; solid blob by one. It's okay to assume that a solid blob sinks past any
; neighbor just below it.
; 
; To assist you, we supply the relevant data definitions.


;; Blob is one of:
;; - "solid"
;; - "bubble"
;; interp.  a gelatinous blob, either a solid or a bubble
;; Examples are redundant for enumerations
#;
(define (fn-for-blob b)
  (cond [(string=? b "solid") (...)]
        [(string=? b "bubble") (...)]))

;; Template rules used:
;; - one-of: 2 cases
;; - atomic distinct: "solid"
;; - atomic distinct: "bubble"


;; ListOfBlob is one of:
;; - empty
;; - (cons Blob ListOfBlob)
;; interp. a sequence of blobs in a test tube, listed from top to bottom.
(define LOB0 empty) ; empty test tube
(define LOB2 (cons "solid" (cons "bubble" empty))) ; solid blob above a bubble

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-blob (first lob))
              (fn-for-lob (rest lob)))]))

;; Template rules used
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: 2 fields
;; - reference: (first lob) is Blob
;; - self-reference: (rest lob) is ListOfBlob

;; ListOfBlob -> ListOfBlob
;; produce a list of blobs that sinks the given solid blobs by one (by one spot down)

(check-expect (sink empty) empty)

(check-expect (sink (cons "bubble" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "bubble" (cons "solid" empty))))

(check-expect (sink (cons "solid" (cons "solid" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))

(check-expect (sink (cons "solid" (cons "bubble" (cons "bubble" empty))))
              (cons "bubble" (cons "solid" (cons "bubble" empty))))

(check-expect (sink (cons "solid" (cons "bubble" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))

(check-expect (sink (cons "bubble" (cons "solid" (cons "solid" empty))))
              (cons "bubble" (cons "solid" (cons "solid" empty))))

(check-expect (sink (cons "solid"
                          (cons "solid"
                                (cons "bubble" (cons "bubble" empty)))))
              (cons "bubble" (cons "solid" 
                                   (cons "solid" (cons "bubble" empty)))))

; (define (sink lob) empty) ; stub

; template taken from ListOfBlob

(define (sink lob)
  (cond [(empty? lob) lob]
        [else
         (if (should-sink? (first lob))
             (sink-first (sink (rest lob)))
             (cons (first lob) (sink (rest lob))))]))

;; Blob -> boolean
;; should return true if blob is 'solid', otherwise should return false

; template taken from Blob

(check-expect (should-sink? "solid") true)
(check-expect (should-sink? "bubble") false) 

;; (define (should-sink? b) true) ; stub

(define (should-sink? b)
  (string=? "solid" b))


;; ListOfBlob -> ListOfBlob
;; put the blob in the second spot inside of provided list

(check-expect (sink-first empty) (cons "solid" empty))
(check-expect (sink-first (cons "bubble" empty)) (cons "bubble" (cons "solid" empty)))
(check-expect (sink-first (cons "bubble" (cons "bubble" empty))) (cons "bubble" (cons "solid" (cons "bubble" empty))))
(check-expect (sink-first (cons "solid" (cons "bubble" empty))) (cons "solid" (cons "bubble" (cons "solid" empty))))

; (define (sink-first lob) lob) ;stub

; template taken from ListOfBlob

(define (sink-first lob)
  (cond [(empty? lob) (cons "solid" empty)]
        [(string=? "bubble" (first lob)) (cons "bubble" (cons "solid" (rest lob)))]
        [else (cons (first lob) (sink-first (rest lob)))]))








