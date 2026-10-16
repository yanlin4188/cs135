;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname median2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define (median-of-3-simple a b c)
  (cond [(not (boolean=? (> a b) (> c b))) b]
        [(not (boolean=? (> a b) (> a c))) a]
        [else c]))

(define (median-of-3-simple a b c)
  (cond [> (- a )]))

(check-expect (median-of-3-simple 1 2 3) 2)
(check-expect (median-of-3-simple 1 3 2) 2)
(check-expect (median-of-3-simple 2 1 3) 2)
(check-expect (median-of-3-simple 2 3 1) 2)
(check-expect (median-of-3-simple 3 1 2) 2)
(check-expect (median-of-3-simple 3 2 1) 2)

(check-expect (median-of-3-simple 1 1 2) 1)
(check-expect (median-of-3-simple 1 2 1) 1)
(check-expect (median-of-3-simple 2 1 1) 1)
(check-expect (median-of-3-simple 1 1 1) 1)

(check-expect (median-of-3-simple -3 -2 -1) -2)
(check-expect (median-of-3-simple -1 -3 -2) -2)
(check-expect (median-of-3-simple -1 0 1) 0)