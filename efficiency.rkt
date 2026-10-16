;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname efficiency) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *****************************************************************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 04
;; *****************************************************************************
;;

;;
;; Problem 5 Part a
;;
;; Karina's solution
;;
;; Sum up the numbers from the front
;; sum-k: (listof Num) -> Num
(define (sum-k lon)
  (cond [(empty? lon) 0]
        [else (+ (first lon) (sum-k (rest lon)))]))
;;
;; 
;; Claire's solution
;;
;; Appends lon2 to the end of lon1.
;; my-append: (listof Num) (listof Num) -> (listof Num)
(define (my-append lon1 lon2)
  (cond [(empty? lon1) lon2]
        [else (cons (first lon1) (my-append (rest lon1) lon2))]))
;;
;; Reverse the list
;; my-reverse: (listof Num) -> (listof Num)
(define (my-reverse lon)
  (cond [(empty? lon) empty]
        [else (my-append (my-reverse (rest lon)) (list (first lon)))]))
;;
;; Sum up all the numbers from the front
;; add-up: (listof Num) ->  Num
(define (add-up lon)
  (cond [(empty? lon) 0]
        [else (+ (first lon) (add-up (rest lon)))]))
;;
;; Sum up all the numbers in reverse order
;; sum-c: (listof Num) -> Num
(define (sum-c lon)
  (add-up (my-reverse lon)))
;; testing
;(define test0 empty)
;(define test1 (list 1))
;(define test2 (list 1 2 3))
;(define test3 (list 3 2 1))
;(define test4 (list -1 -2 -3))
;(define test5 (list -1 2 -3))
;(check-expect (sum-k test0) 0)
;(check-expect (sum-k test1) 1)
;(check-expect (sum-k test2) 6)
;(check-expect (sum-k test3) 6)
;(check-expect (sum-k test4) -6)
;(check-expect (sum-k test5) -2)
;(check-expect (sum-c test0) 0)
;(check-expect (sum-c test1) 1)
;(check-expect (sum-c test2) 6)
;(check-expect (sum-c test3) 6)
;(check-expect (sum-c test4) -6)
;(check-expect (sum-c test5) -2)


;; Report:
;; The tests reveals that  Karina's program is correct, but Claire's add-up function
;; incorrectly returned 1 for the empty list. The error is fixed by replacing 1 with 0.
;;
;; Documentation and testing must come before step-counting because efficiency
;; without correctness is meaningless.

;;
;; Problem 5 Part b
;;
(define lst1 (list 7))
(define lst2 (list 7 3))
(define lst3 (list 7 3 5))
(define lst4 (list 7 3 5 2))
;(sum-k lst1);; 12
;(sum-k lst2);; 19
;(sum-k lst3);; 26
;(sum-k lst4);; 33
;(sum-c lst1);; 25
;(sum-c lst2);; 48
;(sum-c lst3);; 78
;(sum-c lst4);; 115
(define karina-steps (list 12 19 26 33))
(define claire-steps (list 25 48 78 115))

;;
;; Problem 5 Part c
;;
;;
;; Karina's code:
;; f(n) = 7n + 5
;; Find the number of steps for Karina's functions
;; f-karina: Nat -> Nat
(define (f-karina n)
  (+ (* 7 n) 5))
(define f-karina-list (list (f-karina 1) (f-karina 2) (f-karina 3) (f-karina 4)))
;; (define karina-steps (list 12 19 26 33))
;;
;;
;; Claire's code:
;; f(n) = 7n^2 + 9n + 9
;; Find the number of steps for Claire's functions
;; f-claire: Nat -> Nat
(define (f-claire n)
  (+ (* 7 (sqr n)) (* 9 n) 9))
(define f-claire-list (list (f-claire 1) (f-claire 2) (f-claire 3) (f-claire 4)))
;;(define claire-steps (list 25 48 78 115))
;;
;; Problem 5 Part d
;;
;; Karina:
;; f(100) = 7 * 100 + 5 = 705
;;
;; Claire:
;; f(100) = 7 * (100)^2 + 9 * 100 + 9 = 70909
;;
;; Karina's solution is more efficient because her code's linear time complexity if faster than
;; Claire's quadratic time complexity.
;; In Claire's code, the line "[else (my-append (my-reverse (rest lon)) (list (first lon)))]"
;; is responsible for the difference because it repeatedly goes over the list to append the two lists.




