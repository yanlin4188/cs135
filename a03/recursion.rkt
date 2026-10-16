;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *******************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 03
;; *******************************
;;

;;
;; Problem 2 Part a
;; Find the sum of the first n harmonic numbers
;; harmonic: Nat -> Rat
(define (harmonic n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (/ 1 n) (harmonic (sub1 n)))]))
;; testing
(check-expect (harmonic 0) 0)
(check-expect (harmonic 1) 1)
(check-expect (harmonic 2) 3/2)
(check-expect (harmonic 3) 11/6)

;;
;; Problem 2 Part b
;; Find the sum of the squares of the natural numbers using the given formula
;; ss-closed: Nat -> Nat
(define (ss-closed n)
  (/ (* n (add1 n) (+ (* 2 n) 1)) 6))
;; Find the sum of the squares of the natural numbers using recursion
;; ss-recursive: Nat -> Nat
(define (ss-recursive n)
  (cond [(zero? n) 0]
        [else (+ (sqr n) (ss-recursive (sub1 n)))]))
;; testing
(check-expect (ss-closed 0) (ss-recursive 0))
(check-expect (ss-closed 1) (ss-recursive 1))
(check-expect (ss-closed 2) (ss-recursive 2))
(check-expect (ss-closed 3) (ss-recursive 3))
(check-expect (ss-closed 4) (ss-recursive 4))