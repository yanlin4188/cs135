;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname curves) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *****************************************************************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 04
;; *****************************************************************************
;;

;;
;; Calculate the result of the given polynomial
;; eval-poly: (listof Num) Num -> Num
;; Requires: a_n != 0
(define (eval-poly lst x)
  (cond [(empty? lst) 0]
        [else (+ (first lst) (* x (eval-poly (rest lst) x)))]))

;;
;; Problem 4 Part a
;;
;; Check if a given curve is elliptic
;; elliptic?: (list Int Int Int Int) -> Bool
;; consumes a curve polynomial of the form (list b a 0 1)
(define (elliptic? lst)
  (not (zero? (+ (eval-poly (list 0 0 0 4) (first (rest lst)))
                 (eval-poly (list 0 0 27) (first lst))))))
;; testing
(check-expect (elliptic? (list 7 0 0 1)) true)
(check-expect (elliptic? (list 2 -3 0 1)) false)
(check-expect (elliptic? (list 0 0 0 1)) false)
;;

;;
;; Problem 4 Part b
;;
;; Produces the list of the polynomial's values at each x-coordinates
;; curve-values: (listof Int Int Int Int) (listof Int) -> (anyof (listof Int) Bool)
(define (curve-values curve lst-x)
  (cond [(not (elliptic? curve)) false]
        [(empty? lst-x) empty]
        [else (cons (eval-poly curve (first lst-x))
                    (curve-values curve (rest lst-x)))]))
;; testing
(check-expect (curve-values (list 7 0 0 1) (list -2 -1 0 1 2))
              (list -1 6 7 8 15))
(check-expect (curve-values (list 7 0 0 1) empty) empty)
(check-expect (curve-values (list 2 -3 0 1) (list 1)) false)
(check-expect (curve-values (list 2 -3 0 1) empty) false)

;;
;; Problem 4 Part c
;;
;; Produces the list of b that makes valid elliptic curve
;; usable-b: Int (listof Int) -> (listof Int)
(define (usable-b a list-b)
  (cond [(empty? list-b) empty]
        [(elliptic? (list (first list-b) a 0 1)) (cons (first list-b) (usable-b a (rest list-b)))]
        [else (usable-b a (rest list-b))]))
;; testing
(check-expect (usable-b 0 (list 0 7 -1)) (list 7 -1))
(check-expect (usable-b -3 (list 2 0 -2)) (list 0))
(check-expect (usable-b 5 empty) empty)












