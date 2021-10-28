#lang racket

;;; Title: LinearRegression.rkt
;;; Explanation: This file contains basic method for implementing LinearRegression
;;; which is applicable to a single-dimensional input and output.
;;; Author : Hiroyuki Shiono
;;; Date : Oct 28th 2021

;;; proc : initialize
;;; input:
;;; x : a list of numbers
;;; y : a list of numbers
;;; output:
;;; list of x, y, and list of theta0 and theta1

(define initialize
  (lambda (x y)
    (list x y (cons 0 0))))

;;; proc : update_coeffs
;;; input:
;;; init : a list of x, y, and thetas
;;; y_predict : a list of numbers
;;; learning_rate : an integer, 0 < learning_rate < 1
;;; m : an integer, a size of y
;;; output:
;;; a pair of theta0 and theta1

(define update_coeffs
  (lambda (init y_predict learning_rate m)
    (let* ([pred (predict init y_predict)]
           [x (list-ref init 0)]
           [y (list-ref init 1)]
           [b (list-ref init 2)])
      (let ([m (length x)]
            [theta_0 (car b)]
            [theta_1 (cdr b)])
        (update_coeffs_helper learning_rate theta_0 theta_1 x y pred m)))))

;;; proc : update_coeffs_helper
;;; input:
;;; learning_rate : an integer, 0 < learning_rate < 1
;;; theta_0 : a double number
;;; theta_1 : a double number
;;; x : a list of numbers
;;; y : a list of numbers
;;; pred : a list of numbers
;;; m  : an integer, a size of y
;;; output:
;;; a pair of theta0 and theta1

(define update_coeffs_helper
  (lambda (learning_rate theta_0 theta_1 x y pred m)
    (cons (- theta_0 (update_theta_0 y pred 0 learning_rate m))
          (- theta_1 (update_theta_1 x y pred 0 learning_rate m)))))

;;; proc : update_theta_0
;;; input:
;;; y : a list of numbers
;;; pred : a list of numbers
;;; sum : an integer, initially defined as a 0
;;; learning_rate : an integer, 0 < learning_rate < 1
;;; m  : an integer, a size of y
;;; output:
;;; a double number, an updated version of theta0

(define update_theta_0
  (lambda (y pred sum learning_rate m)
    (if (null? y)
        (* learning_rate (/ sum m))
        (update_theta_0 (cdr y) (cdr pred)
                        (+ sum (- (car pred) (car y)))
                        learning_rate m))))

;;; proc : update_theta_1
;;; input:
;;; x : a list of numbers
;;; y : a list of numbers
;;; pred : a list of numbers
;;; sum : an integer, initially defined as a 0
;;; learning_rate : an integer, 0 < learning_rate < 1
;;; m  : an integer, a size of y
;;; output:
;;; a double number, an updated version of theta1

(define update_theta_1
  (lambda (x y pred sum learning_rate m)
    (if (null? y)
        (* learning_rate (/ sum m))
        (update_theta_1 (cdr x) (cdr y) (cdr pred)
                        (+ sum (* (- (car pred) (car y)) (car x)))
                        learning_rate m))))
                        

;;; proc : predict
;;; input:
;;; init : a list of x, y, and (list theta0 theta1)
;;; y_predict : a list of numbers,initially defined as an empty list
;;; output:
;;; a list of numbers with predicted values

(define predict
  (lambda (init y_predict)
    (let* ([x (list-ref init 0)]
           [b (list-ref init 2)])
      (let* ([theta_0 (car b)]
             [theta_1 (cdr b)])
        (predict_helper x theta_0 theta_1 y_predict)))))

;;; proc : predict_helper
;;; input:
;;; x : a list of numbers
;;; theta_0 : a double number
;;; theta_1 : a double number
;;; y_predict : a list of numbers, initially defined as an empty list
;;; output:
;;; a list of numbers with predicted values

(define predict_helper
  (lambda (x theta_0 theta_1 y_predict)
    (if (null? x)
        y_predict
        (predict_helper (cdr x) theta_0 theta_1
                        (append y_predict (list (+ theta_0 (* theta_1 (car x)))))))))

;;; proc : compute_cost
;;; input:
;;; y_pred : a list of numbers, storing predicted values
;;; y : a list of numbers
;;; m : an integer, a size of y
;;; sum : a double number or an integer
;;; output:
;;; a list of numbers with predicted values

(define compute_cost
  (lambda (y_pred y m sum)
    (if (null? y_pred)
        (/ sum (* 2 m))
        (compute_cost (cdr y_pred)(cdr y)
                      m (+ sum (sqr (- (car y_pred)(car y))))))))

;;; proc : LinearRegression
;;; input:
;;; x : a list of numbers
;;; y : a list of numbers
;;; learning_rate : a double number, 0 < learning_rate < 1
;;; epochs : an integer, epochs > 0
;;; output:
;;; a pair containining theta0 and theta1.

(define LinearRegression
  (lambda (x y learning_rate epochs)
    (let ([init (initialize x y)])
      (let ([thetas (list-ref init 2)])
        (LinearRegression-helper x y thetas learning_rate 0 epochs)))))

;;; proc : LinearRegression-helper
;;; input:
;;; x : a list of numbers
;;; y : a list of numbers
;;; thetas : a pair of double numbers, a pair of theta0 and theta1
;;; learning_rate : a double number, 0 < learning_rate < 1
;;; epochs : an integer, epochs > 0
;;; output:
;;; a pair containining theta0 and theta1.
;;; preconditions :
;;; x and y has to be the same length

(define LinearRegression-helper
  (lambda (x y thetas learning_rate counter epochs)
    (let* ([pred (predict (list x y thetas) '())]
           [m (length y)]
           [sum 0])
      (let* ([costs (compute_cost pred y m sum)]
             [new_coeffs (update_coeffs (list x y thetas) pred learning_rate m)])
        (if (equal? counter epochs)
            thetas
            (LinearRegression-helper x y new_coeffs learning_rate (+ counter 1) epochs))))))

;;; sample input
;;; (define x (list 1 2 3 4 5 6 7 8 9 10))
;;; (define y (list 2 4 6 8 10 12 14 16 18 20))
;;; (LinearRegression x y 0.01 3000)
;;; sample output
;;; '(0.0005138142882716239 . 1.9999261954325986)
           

            
    
          
