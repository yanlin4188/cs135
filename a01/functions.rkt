;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *******************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 01
;; *******************************
;;

;;
;; Problem 2, Part a
;;
;; Calculate the surface area of a doughnut
(define (doughnut-surface-area r z)
  (* 4 (sqr pi) r z))


;;
;; Problem 2, Part b
;;
;; Calculate the pressure
(define (pressure n T V)
  (* n 8.3144626 T (/ 1 V)))

;;
;; Problem 2, Part c
;;
;; Calculate the exponential decay of a radioactive substance
(define (q s d t)
  (* s (expt e (* -1 d t))))

;;
;; Problem 2, Part d
;;
;; Calculate the frequency
(define intervals-per-octave 12)
(define (freq base-frequency interval)
  (* base-frequency (expt 2 (/ interval intervals-per-octave))))