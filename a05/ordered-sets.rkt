;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ordered-sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *****************************************************************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 05
;; *****************************************************************************
;;

;;
;; Problem 2
;;
;; An IntSet is a (listof Int)
;; Requires: elements must be unique
;;           elements must be in ascending order
;;
;; Part a
;;
;; Determines if an element is in a set
;; in? Int IntSet -> Bool
(define (in? int intset)
  (cond [(empty? intset) false]
        [(= int (first intset)) true]
        [(< int (first intset)) false]
        [else (in? int (rest intset))]))
;; testing
(check-expect (in? 0 empty) false)
(check-expect (in? 0 (list 0)) true)
(check-expect (in? 1 (list 0 1 2)) true)
(check-expect (in? 2 (list 0 1 2)) true)
(check-expect (in? 3 (list 0 1 2)) false)
(check-expect (in? 2 (list 0 1 3)) false)

;;
;; Part b
;;
;; Add an element to a set
;; add: Int IntSet -> IntSet
(define (add int intset)
  (cond [(empty? intset) (list int)]
        [(= int (first intset)) intset]
        [(< int (first intset)) (cons int intset)]
        [else (cons (first intset) (add int (rest intset)))]))
;; testing
(check-expect (add 1 empty) (list 1))
(check-expect (add 1 (list 1)) (list 1))
(check-expect (add 1 (list 1 2)) (list 1 2))
(check-expect (add 2 (list 0 1 3)) (list 0 1 2 3))
(check-expect (add 3 (list 0 1 2)) (list 0 1 2 3))

;;
;; Part c
;;
;; Produce the union of two sets
;; union: IntSet IntSet -> IntSet
(define (union set1 set2)
  (cond [(empty? set1) set2]
        [(empty? set2) set1]
        [(= (first set1) (first set2)) (cons (first set1) (union (rest set1) (rest set2)))]
        [(> (first set1) (first set2)) (cons (first set2) (unino set1 (rest set2)))]
        [else (cons (first set1) (union))]))