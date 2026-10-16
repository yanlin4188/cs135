;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; *****************************************************************************
;; Yan Lin (21244699)
;; CS 135 Fall 2026
;; Assignment 03
;; *****************************************************************************
;;

;;
;; Problem 4 Part a
;;
;; A Direction is the direction that the robot is facing
;; A Direction is (anyof 'North, 'South, 'East, 'West)
;;
;; A State is defined by its position(x, y) and Direction
;; A State is (cons x (cons y (cons Direction empty)))
;; requires: 0 <= x <= 10, 0 <= y <= 10
;;

;;
;; Problem 4 Part b
;;
;; mk-state consumes the position(x and y) and Direction to produce the State.
;; mk-state: Int Int Direction -> State
;; requires: 0 <= x <= 10, 0 <= y <= 10
(define (mk-state x y Direction)
  (cons x (cons y (cons Direction empty))))
;; testing
(define test0 (cons 0 (cons 10 (cons 'North empty))))
(check-expect (mk-state 0 10 'North) test0)
;;
;; get-x consumes the State and returns the x-coordinate
;; get-x: State -> Int
(define (get-x State) (first State))
;; testing
(check-expect (get-x test0) 0)
;;
;; get-y consumes the State and returns the y-coordinate
;; get-y: State -> Int
(define (get-y State) (first (rest State)))
;; testing
(check-expect (get-y test0) 10)
;;
;; get-direction consumes the State and returns the Direction
;; get-direction: State -> Direction
(define (get-direction State) (first (rest (rest State))))
;; testing
(check-expect (get-direction test0) 'North)
;;

;;
;; Problem 4 part c
;;
;; A Command is (anyof 'forward 'turn-left 'turn-right)
;;
;; robot-ctl consumes a State and a Command to produce a new State
;; robot-ctl: State Command -> State
(define (robot-ctl State Command)
  (cond [(symbol=? Command 'turn-left)
         (cond [(symbol=? (get-direction State) 'North) (change-direction State 'West)]
               [(symbol=? (get-direction State) 'West) (change-direction State 'South)]
               [(symbol=? (get-direction State) 'South) (change-direction State 'East)]
               [else (change-direction State 'North)])]
        [(symbol=? Command 'turn-right)
         (cond [(symbol=? (get-direction State) 'North) (change-direction State 'East)]
               [(symbol=? (get-direction State) 'East) (change-direction State 'South)]
               [(symbol=? (get-direction State) 'South) (change-direction State 'West)]
               [else (change-direction State 'North)])]
        [(or (and (= (get-x State) 0) (symbol=? (get-direction State) 'West))
             (and (= (get-x State) 10) (symbol=? (get-direction State) 'East))
             (and (= (get-y State) 0) (symbol=? (get-direction State) 'South))
             (and (= (get-y State) 10) (symbol=? (get-direction State) 'North)))
         State]
        [(symbol=? (get-direction State) 'North)
         (change-y State 1)]
        [(symbol=? (get-direction State) 'West)
         (change-x State -1)]
        [(symbol=? (get-direction State) 'South)
         (change-y State -1)]
        [else
         (change-x State 1)]))
;; change-direction changes the direction of State
;; change-direction: State Direction -> State
(define (change-direction State new-direction)
  (cons (get-x State) (cons (get-y State) (cons new-direction empty))))
;; change-x changes the x coordinate of State
;; change-x: State Nat -> State
(define (change-x State x-diff)
  (cons (+ x-diff (get-x State)) (cons (get-y State) (cons (get-direction State) empty))))
;; change-y: changes the y coordinate of State
;; change-y: State Nat -> State
(define (change-y State y-diff)
  (cons (get-x State) (cons (+ y-diff (get-y State)) (cons (get-direction State) empty))))
;; testing
(check-expect (robot-ctl (cons 5 (cons 5 (cons 'North empty))) 'turn-left)
              (cons 5 (cons 5 (cons 'West empty))))
(check-expect (robot-ctl (cons 5 (cons 5 (cons 'West empty))) 'turn-left)
              (cons 5 (cons 5 (cons 'South empty))))
(check-expect (robot-ctl (cons 5 (cons 5 (cons 'South empty))) 'turn-left)
              (cons 5 (cons 5 (cons 'East empty))))
(check-expect (robot-ctl (cons 5 (cons 5 (cons 'East empty))) 'turn-left)
              (cons 5 (cons 5 (cons 'North empty))))

(check-expect (robot-ctl (cons 5 (cons 5 (cons 'North empty))) 'turn-right)
              (cons 5 (cons 5 (cons 'East empty))))
(check-expect (robot-ctl (cons 5 (cons 5 (cons 'East empty))) 'turn-right)
              (cons 5 (cons 5 (cons 'South empty))))
(check-expect (robot-ctl (cons 5 (cons 5 (cons 'South empty))) 'turn-right)
              (cons 5 (cons 5 (cons 'West empty))))
(check-expect (robot-ctl (cons 5 (cons 5 (cons 'West empty))) 'turn-right)
              (cons 5 (cons 5 (cons 'North empty))))

(check-expect (robot-ctl (cons 5 (cons 5 (cons 'North empty))) 'forward)
              (cons 5 (cons 6 (cons 'North empty))))
(check-expect (robot-ctl (cons 5 (cons 5 (cons 'West empty))) 'forward)
              (cons 4 (cons 5 (cons 'West empty))))
(check-expect (robot-ctl (cons 5 (cons 5 (cons 'South empty))) 'forward)
              (cons 5 (cons 4 (cons 'South empty))))
(check-expect (robot-ctl (cons 5 (cons 5 (cons 'East empty))) 'forward)
              (cons 6 (cons 5 (cons 'East empty))))

(check-expect (robot-ctl (cons 0 (cons 5 (cons 'West empty))) 'forward)
              (cons 0 (cons 5 (cons 'West empty))))
(check-expect (robot-ctl (cons 10 (cons 5 (cons 'East empty))) 'forward)
              (cons 10 (cons 5 (cons 'East empty))))
(check-expect (robot-ctl (cons 5 (cons 0 (cons 'South empty))) 'forward)
              (cons 5 (cons 0 (cons 'South empty))))
(check-expect (robot-ctl (cons 5 (cons 10 (cons 'North empty))) 'forward)
              (cons 5 (cons 10 (cons 'North empty))))


















