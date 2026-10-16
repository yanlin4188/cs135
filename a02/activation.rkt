;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname activation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *******************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 02
;; *******************************
;;

;;
;; Problem 2, Part a
;; The sigmoid activation function
;; sigmoid: Num -> Num
(define (sigmoid x)
  (/ 1 (+ 1 (expt e (- x)))))
(check-within (sigmoid 0) 0.5 0.0001)
(check-within (sigmoid 1) 0.7311 0.0001)
(check-within (sigmoid -1) 0.2689 0.0001)

;;
;; Problem 2, Part b
;; The rectified linear unit
;; relu: Num -> Num
(define (relu x)
  (cond [(< x 0) 0]
        [else x]))
(check-expect (relu -5) 0)
(check-expect (relu 0) 0)
(check-expect (relu 5) 5)

;;
;; Problem 2, Part c
;; Swish activation function
;; swish: Num -> Num
(define (swish x) (* x (sigmoid x)))
(check-within (swish -1) -0.2689 0.0001)
(check-within (swish 0) 0 0.0001)
(check-within (swish 1) 0.7311 0.0001)

;;
;; Problem 2, Part d
;; hard sigmoid function
;; hard-sigmoid: Num -> Num
(define (hard-sigmoid x)
  (cond [(<= x -3) 0]
        [(>= x 3) 1]
        [else (/ (+ x 3) 6)]))
(check-expect (hard-sigmoid -4) 0)
(check-expect (hard-sigmoid -3) 0)
(check-expect (hard-sigmoid -1) 1/3)
(check-expect (hard-sigmoid 0) 1/2)
(check-expect (hard-sigmoid 1) 2/3)
(check-expect (hard-sigmoid 3) 1)
(check-expect (hard-sigmoid 4) 1)

;;
;; Problem 2, Part e
;; hard swish activation function
;; hard-swish: Num -> Num
(define (hard-swish x) (* x (hard-sigmoid x)))
(check-expect (hard-swish -4) 0)
(check-expect (hard-swish -3) 0)
(check-expect (hard-swish -1) -1/3)
(check-expect (hard-swish 0) 0)
(check-expect (hard-swish 1) 2/3)
(check-expect (hard-swish 3) 3)
(check-expect (hard-swish 4) 4)