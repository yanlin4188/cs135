;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname horner) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *****************************************************************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 04
;; *****************************************************************************
;;

;;
;; Problem 3
;;
;; Calculate the result of the given polynomial
;; eval-poly: (listof Num) Num -> Num
;; Requires: a_n != 0
(define (eval-poly lst x)
  (cond [(empty? lst) 0]
        [else (+ (first lst) (* x (eval-poly (rest lst) x)))]))
;; testing
(check-expect (eval-poly (cons -4 (cons 7 (cons 0 (cons 2 empty)))) 3) 71)
(check-expect (eval-poly (list 5) 2) 5)
(check-expect (eval-poly (list 1 2) 3) 7)
(check-expect (eval-poly (list 1 2 3) 2) 17)
(check-expect (eval-poly (list 0 0 1) 4) 16)
(check-expect (eval-poly (list 3 0 2) 2) 11)
(check-expect (eval-poly (list 1 2 3 4) 0) 1)
(check-expect (eval-poly (list 1 2 3) 1) 6)
(check-expect (eval-poly (list 1 2 3) -1) 2)
(check-expect (eval-poly (list -2 3 -1) 2) 0)
(check-expect (eval-poly (list 2 -3 4) -2) 24)
(check-expect (eval-poly (list 0 0 -5) 3) -45)
(check-expect (eval-poly (list 1/2 1/3 1/4) 2) 13/6)
(check-expect (eval-poly (list 2 0 0 0 1) 3) 83)