;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rgb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *******************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 03
;; *******************************
;;

;;
;; Problem 3 Part a
;;
;; An RGB Triplet (RGB) is a (cons Nat (cons Nat (cons Nat empty)))
;; Requires: each element of the triplet must be <= 255
;;
;; mk-rgb consumes three natural numnbers within a appropriate range and produces an RGB
;; mk-rgb: Nat Nat Nat -> RGB
(define (mk-rgb r g b)
  (cons r (cons g (cons b empty))))
;; get-r gets the red components
;; get-r: RGB -> Nat
(define (get-r RGB)
  (first RGB))
;; get-g gets the green components
;; get-g: RGB -> Nat
(define (get-g RGB)
  (first (rest RGB)))
;; get-b gets the blue components
;; get-b: RGB -> Nat
(define (get-b RGB)
  (first (rest (rest RGB))))
;; testing
(define test0
  (cons 10 (cons 11 (cons 12 empty))))
(check-expect (mk-rgb 10 11 12) test0)
(check-expect (get-r test0) 10)
(check-expect (get-g test0) 11)
(check-expect (get-b test0) 12)

;;
;; Problem 3 Part b
;;
;; An RGB Triplet (RGB) is a (cons Nat (cons Nat (cons Nat empty)))
;; Requires: each element of the triplet must be <= 255
;;
;; A Colour is (anyof 'red, 'green 'blue 'yellow 'cyan
;;                    'magenta 'white 'black 'unknown)
;;
;; rgb->name produces the colour name for the RGB value
;; rgb->name: RGB -> Colour
(define (rgb->name RGB)
  (cond [(= (get-r RGB) 255)
         (cond [(= (get-g RGB) 255)
                (cond [(= (get-b RGB) 255) 'white]
                      [(= (get-b RGB) 0) 'yellow]
                      [else 'unknown])]
               [(= (get-g RGB) 0)
                (cond [(= (get-b RGB) 255) 'magenta]
                      [(= (get-b RGB) 0) 'red]
                      [else 'unknown])]
               [else 'unknown])]
         [(= (get-r RGB) 0)
          (cond [(= (get-g RGB) 255)
                 (cond [(= (get-b RGB) 255) 'cyan]
                       [(= (get-b RGB) 0) 'green]
                       [else 'unknown])]
                [(= (get-g RGB) 0)
                 (cond [(= (get-b RGB) 255) 'blue]
                       [(= (get-b RGB) 0) 'black]
                       [else 'unknown])]
                [else 'unknown])]
         [else 'unknown]))
(define test1 (cons 255 (cons 0 (cons 0 empty))))
(define test2 (cons 0 (cons 255 (cons 0 empty))))
(define test3 (cons 0 (cons 0 (cons 255 empty))))
(define test4 (cons 0 (cons 0 (cons 0 empty))))
(define test5 (cons 255 (cons 255 (cons 255 empty))))
(define test6 (cons 255 (cons 255 (cons 0 empty))))
(define test7 (cons 255 (cons 0 (cons 255 empty))))
(define test8 (cons 0 (cons 255 (cons 255 empty))))
(define test9 (cons 255 (cons 255 (cons 1 empty))))
(define test10 (cons 255 (cons 1 (cons 255 empty))))
(define test11 (cons 1 (cons 255 (cons 255 empty))))
(define test12 (cons 255 (cons 1 (cons 1 empty))))
(define test13 (cons 1 (cons 255 (cons 1 empty))))
(define test14 (cons 1 (cons 1 (cons 255 empty))))
(define test15 (cons 255 (cons 1 (cons 1 empty))))
(define test16 (cons 0 (cons 0 (cons 1 empty))))
(define test17 (cons 0 (cons 1 (cons 0 empty))))
(define test18 (cons 1 (cons 0 (cons 0 empty))))
(define test19 (cons 0 (cons 1 (cons 1 empty))))
(define test20 (cons 1 (cons 0 (cons 1 empty))))
(define test21 (cons 1 (cons 1 (cons 0 empty))))
(define test22 (cons 1 (cons 1 (cons 1 empty))))
(define test23 (cons 1 (cons 0 (cons 255 empty))))
(define test24 (cons 1 (cons 255 (cons 0 empty))))
(define test25 (cons 0 (cons 1 (cons 255 empty))))
(define test26 (cons 255 (cons 1 (cons 0 empty))))
(check-expect (rgb->name test1) 'red)
(check-expect (rgb->name test2) 'green)
(check-expect (rgb->name test3) 'blue)
(check-expect (rgb->name test4) 'black)
(check-expect (rgb->name test5) 'white)
(check-expect (rgb->name test6) 'yellow)
(check-expect (rgb->name test7) 'magenta)
(check-expect (rgb->name test8) 'cyan)
(check-expect (rgb->name test9) 'unknown)
(check-expect (rgb->name test10) 'unknown)
(check-expect (rgb->name test11) 'unknown)
(check-expect (rgb->name test12) 'unknown)
(check-expect (rgb->name test13) 'unknown)
(check-expect (rgb->name test14) 'unknown)
(check-expect (rgb->name test15) 'unknown)
(check-expect (rgb->name test16) 'unknown)
(check-expect (rgb->name test17) 'unknown)
(check-expect (rgb->name test18) 'unknown)
(check-expect (rgb->name test19) 'unknown)
(check-expect (rgb->name test20) 'unknown)
(check-expect (rgb->name test21) 'unknown)
(check-expect (rgb->name test22) 'unknown)
(check-expect (rgb->name test23) 'unknown)
(check-expect (rgb->name test24) 'unknown)
(check-expect (rgb->name test25) 'unknown)
(check-expect (rgb->name test26) 'unknown)

;;
;; Problem 3 Part c
;; 
;; An RGB Triplet (RGB) is a (cons Nat (cons Nat (cons Nat empty)))
;; Requires: each element of the triplet must be <= 255
;;
;; A Colour is (anyof 'red, 'green 'blue 'yellow 'cyan
;;                    'magenta 'white 'black 'unknown)
;;
;; within-range? checks if an integer is within [0, 255]
;; within-range?: Int -> Bool
(define (within-range num)
  (and (>= num 0) (<= num 255)))
;; valid-rgb? consumes anything and checks if it is a valid RGB Triplet
;; valid-rgb?: Any -> Bool
(define (valid-rgb? any)
  (and (list? any)
       (not (empty? any))
       (integer? (first any))
       (within-range (first any))
       (not (empty? (rest any)))
       (integer? (first (rest any)))
       (within-range (first (rest any)))
       (not (empty? (rest (rest any))))
       (integer? (first (rest (rest any))))
       (within-range (first (rest (rest any))))
       (empty? (rest (rest (rest any))))))
;; testing
(check-expect (valid-rgb? 'red) false)
(check-expect (valid-rgb? empty) false)
(check-expect (valid-rgb? (cons 255 (cons 0 empty))) false)
(check-expect (valid-rgb? (cons 255 (cons 0 (cons 0 (cons 100 empty))))) false)
(check-expect (valid-rgb? (cons 255 (cons 0 (cons 0 empty)))) true)
(check-expect (valid-rgb? (cons 0 (cons 255 (cons 128 empty)))) true)
(check-expect (valid-rgb? (cons 256 (cons 0 (cons 0 empty)))) false)
(check-expect (valid-rgb? (cons 0 (cons -1 (cons 0 empty)))) false)
(check-expect (valid-rgb? (cons 0 (cons 0 (cons 'blue empty)))) false)





























