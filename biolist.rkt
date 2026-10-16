;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname biolist) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *****************************************************************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 04
;; *****************************************************************************
;;

;;
;; Problem 2, Part a
;;
;; Produce the list of tomorrow's counts
;; next-day: Nat (listof Nat) -> (listof Nat)
;; Requires: factor > 0
(define (next-day factor lst)
  (cond [(empty? lst) empty]
        [else (cons (+ (* factor (first lst)) 1) (next-day factor (rest lst)))]))
;; testing
(check-expect (next-day 2 (list 5 0 12))
              (list 11 1 25))
(check-expect (next-day 3 empty) empty)
;;

;;
;; Problem 2, Part b
;;
;; Predict the counts several days ahead
;; after-days: Nat (listof Nat) Nat -> (listof Nat)
;; Requires: factor > 0
(define (after-days factor lst days)
  (cond [(zero? days) lst]
        [else (after-days factor (next-day factor lst) (sub1 days))]))
;; testing
(check-expect (after-days 2 (list 5 0 12) 2)
              (list 23 3 51))
(check-expect (after-days 3 (list 4) 0)
              (list 4))
(check-expect (after-days 3 empty 2)
              empty)
