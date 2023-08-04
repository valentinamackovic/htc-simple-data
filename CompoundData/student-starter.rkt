;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname student-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; student-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; Design a data definition to help a teacher organize their next field trip. 
; On the trip, lunch must be provided for all students. For each student, track 
; their name, their grade (from 1 to 12), and whether or not they have allergies.
; 


;; Grade is Natural[1,12]
;; interp. as grade a student is in

(define G1 1)
(define G2 6)
(define G3 12)

#;
(define (fn-for-grade g)
  (... g))

;; Template rules used:
;;  - atomic non-distinct: Natural[1, 12]

(define-struct student (name grade allergies))
;; Student is (make-student String Grade Boolean)
;; interp. as student with a name, grade they belong to and whether or not they have allergies 

(define ST-1 (make-student "Valentina" 10 true))

#;
(define (fn-for-student s)
  (... (student-name b)         ;String
       (student-grade (... g))  ;Grade
       (student-allergies b)))  ;Boolean

;; Template rules used:
;;  - compound: 3 fields

;; =================
;; Functions:

; 
; PROBLEM B:
; 
; To plan for the field trip, if students are in grade 6 or below, the teacher 
; is responsible for keeping track of their allergies. If a student has allergies, 
; and is in a qualifying grade, their name should be added to a special list. 
; Design a function to produce true if a student name should be added to this list.
; 


;; Student -> Boolean
;; consumes student, produces true if the student is in grade 6 or below and has allergies, otherwise produce false

;; (define (add-to-list? s) true) ;stub

(check-expect (add-to-list? (make-student "Valentina" 10 true)) false)
(check-expect (add-to-list? (make-student "Valentina" 10 false)) false)
(check-expect (add-to-list? (make-student "Valentina" 5 false)) false)
(check-expect (add-to-list? (make-student "Valentina" 5 true)) true)

;; <template used from struct definition above>

(define (add-to-list? s)
  (and (student-allergies s) (<= (student-grade s) 6)))




