;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tuition-graph) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; tuition-graph-starter.rkt  (just the problem statements)

; 
; PROBLEM:
; 
; Eva is trying to decide where to go to university. One important factor for her is 
; tuition costs. Eva is a visual thinker, and has taken Systematic Program Design, 
; so she decides to design a program that will help her visualize the costs at 
; different schools. She decides to start simply, knowing she can revise her design
; later.
; 
; The information she has so far is the names of some schools as well as their 
; international student tuition costs. She would like to be able to represent that
; information in bar charts like this one:
; 
; 
;         .
;         
; (A) Design data definitions to represent the information Eva has.
; (B) Design a function that consumes information about schools and their
;     tuition and produces a bar chart.
; (C) Design a function that consumes information about schools and produces
;     the school with the lowest international student tuition.
; 


(define BAR-COLOR "lightblue")
(define BAR-WIDTH 30)
(define TEXT-COLOR "black")
(define TEXT-SIZE 24)
(define Y-SCALING 1/100)

;; Data definitiond:

(define-struct school (name tuition))
;; School is (make-school String Number)
;; interp. a school with a name and it's internation student tuition cost

(define SCHOOL-1 (make-school "FTN" 10000))

#;
(define (fn-for-school s)
  (... (school-name s)        ;Number
       (school-tuition s)))   ;Number

;; Template rules used:
;;  - compound: 2 fields

;; ------------------------------------------------------------------------------

;; ListOfSchool is one of:
;;  - empty
;;  - (cons School ListOfSchool)
;; interp. a list of School

(define LOS-1 empty)
(define LOS-2 (cons (make-school "FTN" 10000) empty))
(define LOS-3 (cons (make-school "FTN" 10000) (cons (make-school "ETF" 20000) empty)))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]                   ;BASE CASE
        [else (... (fn-for-school (first los))                 ;School
                   (fn-for-los (rest los)))])) ;NATURAL RECURSION
;;             /
;;            /
;;       COMBINATION
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons School ListOfSchool)
;;  - referense (first los) is School
;;  - self-reference: (rest los) is ListOfSchool



;; Functions:

; (B) Design a function that consumes information about schools and their
;     tuition and produces a bar chart.


;; ListOfSchool -> Image
;; consumes list of schools, produces a bar chart with a bar for each school that includes the name and has a certain height accroding to the tuition

(check-expect (school-bar empty) (rectangle 0 0 "solid" "white"))
(check-expect (school-bar (cons (make-school "FTN" 10000) empty))
              (overlay/align "middle" "bottom" (rotate 90 (text "FTN" TEXT-SIZE TEXT-COLOR)) (rectangle BAR-WIDTH (* 10000 Y-SCALING) "solid" BAR-COLOR)))
(check-expect (school-bar (cons (make-school "FTN" 10000) (cons (make-school "ETF" 20000) empty)))
              (beside/align "bottom" (overlay/align "middle" "bottom" (rotate 90 (text "FTN" TEXT-SIZE TEXT-COLOR)) (rectangle BAR-WIDTH (* 10000 Y-SCALING) "solid" BAR-COLOR))
                            (overlay/align "middle" "bottom" (rotate 90 (text "ETF" TEXT-SIZE TEXT-COLOR)) (rectangle BAR-WIDTH (* 20000 Y-SCALING) "solid" BAR-COLOR))))

;; (define (school-bar los) (rectangle 0 0 "solid" "white")) ;stub

(define (school-bar los)
  (cond [(empty? los) (rectangle 0 0 "solid" "white")]                   
        [else (beside/align "bottom" (overlay/align "middle" "bottom" (rotate 90 (text (school-name (first los)) TEXT-SIZE TEXT-COLOR)) (rectangle BAR-WIDTH (* (school-tuition (first los)) Y-SCALING) "solid" BAR-COLOR)) ;; ovo sve treba da bude funkcija make-bar, ukazatelj na to nam je bila onaj fn-for-school template funkcija
                             (school-bar (rest los)))]))

(school-bar (cons (make-school "FTN" 10000) (cons (make-school "ETF" 20000) (cons (make-school "FON" 15000) empty))))


; (C) Design a function that consumes information about schools and produces
;     the school with the lowest international student tuition.


;; ListOfSchool -> School
;; consumes list of schools, produces school with lowest tuition, for empty list return strin 'There are no schools in the list!'

(check-expect (lowest-tuition empty) "There are no schools in the list!")
(check-expect (lowest-tuition (cons (make-school "FTN" 10000) empty)) (make-school "FTN" 10000))
(check-expect (lowest-tuition (cons (make-school "FTN" 10000) (cons (make-school "ETF" 20000) empty))) (make-school "FTN" 10000))
(check-expect (lowest-tuition (cons (make-school "FTN" 20000) (cons (make-school "ETF" 10000) empty))) (make-school "ETF" 10000))
(check-expect (lowest-tuition (cons (make-school "FTN" 20000) (cons (make-school "ETF" 10000) (cons (make-school "WHAT" 5000) empty)))) (make-school "WHAT" 5000))
(check-expect (lowest-tuition (cons (make-school "FTN" 20000) (cons (make-school "ETF" 5000) (cons (make-school "WHAT" 100000) empty)))) (make-school "ETF" 5000))

;; (define (lowest-tuition los) "There are no schools in the list!") ;stub

(define (lowest-tuition los)
  (cond [(empty? los) "There are no schools in the list!"]                  
        [else (if (string? (lowest-tuition (rest los)))
                  (first los)
                  (if (< (school-tuition (first los)) (school-tuition (lowest-tuition (rest los))))
                      (first los)
                      (lowest-tuition (rest los))))]))

