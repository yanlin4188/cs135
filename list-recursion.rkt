;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname list-recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *****************************************************************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 03
;; *****************************************************************************
;;

;;
;; Problem 5 Part a
;;
;; A (listof Int) is one of:
;; * empty
;; * (cons Int (listof Int))
;;
;; odd-sum produces the sum of all the odd integers in a list of integers
;; odd-sum: (listof Int) -> Int
(define (odd-sum lst)
  (cond [(empty? lst) 0]
        [(= (remainder (first lst) 2) 0) (odd-sum (rest lst))]
        [else (+ (first lst)(odd-sum (rest lst)))]))
;; testing
(check-expect (odd-sum empty) 0)
(check-expect (odd-sum (cons 2 (cons 4 (cons 6 empty)))) 0)
(check-expect (odd-sum (cons 1 (cons 3 (cons 5 empty)))) 9)
(check-expect (odd-sum (cons 2 (cons 3 (cons 4 (cons 5 empty))))) 8)
(check-expect (odd-sum (cons 9 empty)) 9)
(check-expect (odd-sum (cons 10 empty)) 0)
(check-expect (odd-sum (cons -1 (cons -2 (cons -3 empty)))) -4)
(check-expect (odd-sum (cons 0 (cons 1 (cons 2 (cons 3 empty))))) 4)
;;

;;
;; Problem 5 Part b
;;
;; A (listof Num) is one of:
;; * empty
;; * (cons Num (listof Num))
;;
;; count-within-range produces the number of elements within a range(inclusive)
;; count-within-range: (listof Num) Num Num -> Int
(define (count-within-range lst lower-bound upper-bound)
  (cond [(empty? lst) 0]
        [(and (>= (first lst) lower-bound) (<= (first lst) upper-bound)) (+ 1 (count-within-range (rest lst) lower-bound upper-bound))]
        [else (count-within-range (rest lst) lower-bound upper-bound)]))
;; testing
(define test1
  (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
(check-expect (count-within-range test1 1 4) 4)
(check-expect (count-within-range test1 1.1 3.1) 2)
(check-expect (count-within-range test1 1 3) 3)
(check-expect (count-within-range test1 0.1 0.2) 0)
(check-expect (count-within-range test1 5 6) 0)
;;

;;
;; Problem 5 Part c
;;
;; strictly-increasing? checks if a list of numbers are strictly increasing
;; strictly-increasing?: (listof Num) -> Bool
(define (strictly-increasing? lst)
  (cond [(or (empty? lst) (empty? (rest lst))) true]
        [(>= (first lst) (first (rest lst))) false]
        [else (strictly-increasing? (rest lst))]))
;; testing
(define test2
  (cons 1 (cons 1 (cons 2 (cons 3 empty)))))
(define test3
  (cons 1 (cons 2 (cons 1 (cons 3 empty)))))
(check-expect (strictly-increasing? test1) true)
(check-expect (strictly-increasing? test2) false)
(check-expect (strictly-increasing? test3) false)
;;

;;
;; Problem 5 Part d
;;
;; A (listof Any) is one of:
;; * empty
;; * Num
;; * Sym
;; * Bool
;;
;; weird-sum produces the sum of the number of numbers and twice the number of symbols in a list
;; weird-sum: (listof Any) -> Int
(define (weird-sum lst)
  (cond [(empty? lst) 0]
        [(number? (first lst)) (+ 1 (weird-sum (rest lst)))]
        [(symbol? (first lst)) (+ 2 (weird-sum (rest lst)))]
        [else (weird-sum (rest lst))]))
(check-expect (weird-sum empty) 0)
(check-expect (weird-sum (cons 3 empty)) 1)
(check-expect (weird-sum (cons 'hello empty)) 2)
(check-expect (weird-sum (cons true empty)) 0)
(check-expect
 (weird-sum (cons 1 (cons 'a (cons false (cons 2.5 (cons 'b empty))))))
 6)
;;

;;
;; Problem 5 Part e
;;
;; count-types counts the occurance of numbers, symbols, and boolean values in a list
;; count-types: (listof Any) -> (cons Int (cons Int (cons Int empty)))
(define (count-types lst)
  (cons (count-numbers lst) (cons (count-symbols lst) (cons (count-boolean lst) empty))))

(define (count-numbers lst)
  (cond [(empty? lst) 0]
        [(number? (first lst)) (add1 (count-numbers (rest lst)))]
        [else (count-numbers (rest lst))]))

(define (count-symbols lst)
  (cond [(empty? lst) 0]
        [(symbol? (first lst)) (add1 (count-symbols (rest lst)))]
        [else (count-symbols (rest lst))]))

(define (count-boolean lst)
  (cond [(empty? lst) 0]
        [(boolean? (first lst)) (add1 (count-boolean (rest lst)))]
        [else (count-boolean (rest lst))]))
;; testing
(check-expect (count-types empty)
              (cons 0 (cons 0 (cons 0 empty))))

(check-expect (count-types
               (cons 1 (cons 'a (cons true empty))))
              (cons 1 (cons 1 (cons 1 empty))))

(check-expect (count-types
               (cons 1 (cons 2 (cons 3 empty))))
              (cons 3 (cons 0 (cons 0 empty))))

(check-expect (count-types
               (cons 'a (cons 'b (cons false (cons true empty)))))
              (cons 0 (cons 2 (cons 2 empty))))

(check-expect (count-types
               (cons 1 (cons 'a (cons false
               (cons 2.5 (cons 'b (cons true empty)))))))
              (cons 2 (cons 2 (cons 2 empty))))














