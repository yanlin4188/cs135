;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *******************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 02
;; *******************************
;;

;;
;; Problem 4 Part a
;; Determining if the blood type is acceptable using cond expressions
;; can-donate-to/cond?: Blood-Type Blood-Type -> Bool
;; A Blood-Type(donor/recipient) is (anyof 'O- 'O+ 'A- 'A+ 'B- 'B+ 'AB- 'AB+)
(define (can-donate-to/cond? donor recipient)
  (cond [(symbol=? donor 'O-) true]
        [(symbol=? recipient 'AB+) true]
        [(symbol=? donor recipient) true]
        [(symbol=? donor 'O+)
         (cond [(symbol=? recipient 'A+) true]
               [(symbol=? recipient 'B+) true]
               [else false])]
        [(symbol=? donor 'A-)
         (cond [(symbol=? recipient 'A+) true]
               [(symbol=? recipient 'AB-) true]
               [else false])]
        [(symbol=? donor 'B-)
         (cond [(symbol=? recipient 'B+) true]
               [(symbol=? recipient 'AB-) true]
               [else false])]
        [else false]))
(check-expect (can-donate-to/cond? 'O- 'O-) true)
(check-expect (can-donate-to/cond? 'O- 'AB+) true)
(check-expect (can-donate-to/cond? 'A+ 'AB+) true)
(check-expect (can-donate-to/cond? 'A+ 'A+) true)
(check-expect (can-donate-to/cond? 'O+ 'A+) true)
(check-expect (can-donate-to/cond? 'O+ 'B+) true)
(check-expect (can-donate-to/cond? 'O+ 'A-) false)
(check-expect (can-donate-to/cond? 'A- 'A+) true)
(check-expect (can-donate-to/cond? 'A- 'AB-) true)
(check-expect (can-donate-to/cond? 'A- 'B+) false)
(check-expect (can-donate-to/cond? 'B- 'B+) true)
(check-expect (can-donate-to/cond? 'B- 'AB-) true)
(check-expect (can-donate-to/cond? 'B- 'A+) false)
(check-expect (can-donate-to/cond? 'AB+ 'O-) false)

;;
;; Problem 4 Part b
;; Determine if the blood type is acceptable using a boolean expression
;; can-donate-to/bool?: Blood-Type Blood-Type -> Bool
;; A Blood-Type(donor/recipient) is (anyof 'O- 'O+ 'A- 'A+ 'B- 'B+ 'AB- 'AB+)
(define (can-donate-to/bool? donor recipient)
  (or (symbol=? donor 'O-)
      (symbol=? recipient 'AB+)
      (symbol=? donor recipient)
      (and (symbol=? donor 'O+)
           (or (symbol=? recipient 'A+) (symbol=? recipient 'B+)))
      (and (symbol=? donor 'A-)
           (or (symbol=? recipient 'A+) (symbol=? recipient 'AB-)))
      (and (symbol=? donor 'B-)
           (or (symbol=? recipient 'B+) (symbol=? recipient 'AB-)))))
(check-expect (can-donate-to/bool? 'O- 'O-) true)
(check-expect (can-donate-to/bool? 'O- 'AB+) true)
(check-expect (can-donate-to/bool? 'A+ 'AB+) true)
(check-expect (can-donate-to/bool? 'A+ 'A+) true)
(check-expect (can-donate-to/bool? 'O+ 'A+) true)
(check-expect (can-donate-to/bool? 'O+ 'B+) true)
(check-expect (can-donate-to/bool? 'O+ 'A-) false)
(check-expect (can-donate-to/bool? 'A- 'A+) true)
(check-expect (can-donate-to/bool? 'A- 'AB-) true)
(check-expect (can-donate-to/bool? 'A- 'B+) false)
(check-expect (can-donate-to/bool? 'B- 'B+) true)
(check-expect (can-donate-to/bool? 'B- 'AB-) true)
(check-expect (can-donate-to/bool? 'B- 'A+) false)
(check-expect (can-donate-to/bool? 'AB+ 'O-) false)