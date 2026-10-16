;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname weekday) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *******************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 02
;; *******************************
;;

;;
;; Problem 5

;; find the day
;; day: Nat -> Nat
(define (day date)
  (modulo date 100))

;; find the month(1 = Jan, ..., 10 = Oct, 11 = Nov, 12 = Dec)
;; month-start-with-Jan: Nat -> Nat
(define (month-start-with-Jan date)
  (floor (/ (modulo date 10000) 100)))

;; find the month(1 = March, ..., 10 = Dec, 11 = Jan, 12 = Feb)
;; month: Nat -> Nat
(define (month date)
  (cond [(< (month-start-with-Jan date) 3) (+ (month-start-with-Jan date) 10)]
        [else (- (month-start-with-Jan date) 2)]))

;; find the year
;; year: Nat -> Nat
(define (year date)
  (floor (/ (modulo date 1000000) 10000)))

;; find the century
;; century Nat -> Nat
(define (century date)
  (floor (/ date 1000000)))

;; find the number representing the week day(0 = Sunday, ..., 6 = Saturday)
;; week-num: Nat -> Nat
(define (week-num date)
  (modulo (+ (day date)
             (floor (- (* 2.6 (month date)) 0.2))
             (* 2 (- (century date))) (year date)
             (floor (/ (year date) 4))
             (floor (/ (century date) 4))) 7))

;; find the day in week from a given date
;; date->day-of-week: Nat -> Week-Day(anyof 'Monday 'Tuesday 'Wednesday
;;                                          'Thursday 'Friday 'Saturday 'Sunday)
(define (date->day-of-week date)
  (cond [(= (week-num date) 0) 'Sunday]
        [(= (week-num date) 1) 'Monday]
        [(= (week-num date) 2) 'Tuesday]
        [(= (week-num date) 3) 'Wednesday]
        [(= (week-num date) 4) 'Thursday]
        [(= (week-num date) 5) 'Friday]
        [(= (week-num date) 6) 'Saturday]))
(check-expect (date->day-of-week 20260927) 'Sunday)
(check-expect (date->day-of-week 20260928) 'Monday)
(check-expect (date->day-of-week 20260929) 'Tuesday)
(check-expect (date->day-of-week 20260930) 'Wednesday)
(check-expect (date->day-of-week 20261001) 'Thursday)
(check-expect (date->day-of-week 20261002) 'Friday)
(check-expect (date->day-of-week 20261003) 'Saturday)