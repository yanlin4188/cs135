;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *****************************************************************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 05
;; *****************************************************************************
;;

;;
;; Problem 1 Part a
;;
;; A Set is a (listof Sym)
;; Requires: symbols must be unique
;;
;; Check if an element is in a set
;; in?: Sym Set -> Bool
(define (in? sym set)
  (cond [(empty? set) false]
        [(symbol=? sym (first set)) true]
        [else (in? sym (rest set))]))
;; testing
(check-expect (in? 'hello empty) false)
(check-expect (in? 'hello (list 'hello)) true)
(check-expect (in? 'hello (list 'world)) false)
(check-expect (in? 'hello (list 'world 'hello)) true)
(check-expect (in? 'hello (list 'print 'world)) false)

;;
;; Problem 1 Part b
;;
;; Add an element to a set if it is not in the set
;; add: Sym Set -> Set
(define (add sym set)
  (cond [(in? sym set) set]
        [else (cons sym set)]))
;; testing
(check-expect (add 'hello empty) (list 'hello))
(check-expect (add 'hello (list 'hello)) (list 'hello))
(check-expect (add 'hello (list 'world)) (list 'hello 'world))
(check-expect (add 'hello (list 'hello 'world)) (list 'hello 'world))

;;
;; Problem 1 Part c
;;
;; Produces the union of two sets
;; union: Set Set -> Set
(define (union set1 set2)
  (cond [(empty? set1) set2] 
        [(in? (first set1) set2) (union (rest set1) set2)]
        [else (cons (first set1) (union (rest set1) set2))]))
;; testing
(check-expect (union empty empty) empty)
(check-expect (union empty (list 'hello)) (list 'hello))
(check-expect (union (list 'hello) empty) (list 'hello))
(check-expect (union (list 'hello) (list 'world)) (list 'hello 'world))
(check-expect (union (list 'hello 'world) (list 'hello 'world)) (list 'hello 'world))
(check-expect (union (list 'hello 'print) (list 'print 'world)) (list 'hello 'print 'world))

;;
;; Problem 1 Part d
;;
;; Produces the intersection of two sets
;; intersection: Set Set -> Set
(define (intersection set1 set2)
  (cond [(empty? set1) empty]
        [(in? (first set1) set2) (cons (first set1) (intersection (rest set1) set2))]
        [else (intersection (rest set1) set2)]))
;; testing
(check-expect (intersection empty empty) empty)
(check-expect (intersection empty (list 'hello)) empty)
(check-expect (intersection (list 'hello) empty) empty)
(check-expect (intersection (list 'hello) (list 'hello)) (list 'hello))
(check-expect (intersection (list 'hello 'world) (list 'hello)) (list 'hello))
(check-expect (intersection (list 'print 'hello) (list 'hello 'world)) (list 'hello))
(check-expect (intersection (list 'print 'hello 'world) (list 'hello 'world)) (list 'hello 'world))

;;
;; Problem 1 Part e
;;
;; Determines if two sets are equal
;; set=? Set Set -> Bool
(define (set=? set1 set2)
  (and (set-in? set1 set2) (set-in? set2 set1)))
;; testing
(check-expect (set=? empty empty) true)
(check-expect (set=? empty (list 'hello)) false)
(check-expect (set=? (list 'hello) empty) false)
(check-expect (set=? (list 'hello) (list 'hello)) true)
(check-expect (set=? (list 'hello 'world) (list 'hello 'world)) true)
(check-expect (set=? (list 'hello) (list 'world)) false)
(check-expect (set=? (list 'hello 'world) (list 'world 'print)) false)
(check-expect (set=? (list 'hello 'world) (list 'hello)) false)
;;
;; Determine if a set is inside of the other
;; set-in? Set Set -> Bool
(define (set-in? set1 set2)
  (cond [(empty? set1) true]
        [(in? (first set1) set2) (set-in? (rest set1) set2)]
        [else false]))

;;
;; Problem 1 Part f
;;
;; Determines if an input is a Set
;; set?: Any -> Bool
(define (set? any)
  (cond [(not (list? any)) false]
        [else (set?-helper any empty)]))
;; testing
(check-expect (set? 5) false)
(check-expect (set? 'a) false)
(check-expect (set? false) false)
(check-expect (set? empty) true)
(check-expect (set? (list 'a)) true)
(check-expect (set? (list 'a 'b 'c)) true)
(check-expect (set? (list 1 2 3)) false)
(check-expect (set? (list 'a 1 'b)) false)
(check-expect (set? (list 'a 'b 1)) false)
(check-expect (set? (list (list 'a))) false)
(check-expect (set? (list 'a empty)) false)
(check-expect (set? (list 'a 'a)) false)
(check-expect (set? (list 'a 'b 'a)) false)
(check-expect (set? (list 'a 'b 'c 'b)) false)
(check-expect (set? (list 'a 'b 'c 'd)) true)
(check-expect (set? (list 1)) false)
(check-expect (set? (list 'a true)) false)
;;
;; Determines if a list is a Set with visited elements stored for checking uniqueness
;; set?-helper: (listof Any) Set -> bool
(define (set?-helper set1 set2)
  (cond [(empty? set1) true]
        [(not (symbol? (first set1))) false]
        [(in? (first set1) set2) false]
        [else (set?-helper (rest set1) (cons (first set1) set2))]))












