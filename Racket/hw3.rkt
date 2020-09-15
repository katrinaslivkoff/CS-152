;;; Smart Spartans

;;; Function all-but-last: list -> list
;;; Parameters: xs
;;; Returns all elements of the list except the last one
;;; Expected types: takes in a list and returns a list
(define (all-but-last xs)
  (if (null? (cdr xs))
      '()
      (cons(car xs)(all-but-last(cdr xs)))))
     
;;; Function common?: list list -> Boolean
;;; Uses the member?  predicate defined in the lecture
;;; Parameters: lst1 and lst2
;;; Returns true if there is at least one element from lst1 in common with one element in lst2
;;; Expected types: takes in two lists and returns a Boolean
(define (member? x xs)
  (cond ((null? xs) #f)
        ((equal? x (car xs)) #t)
         (else(member? x (cdr xs)))))

(define (common? lst1 lst2)
  (cond ((null? lst1) #f)
        ((member? (car lst1) lst2))
         (else (common? (cdr lst1) lst2))))

;;; Function make-list: integer integer integer -> list
;;; Parameters: start, stop, and step
;;; Returns start as the first element and steps 'n' times until it reaches stop
;;; Expected types: takes in 3 integers and returns a list
(define (make-list start stop step)
  (cond ((equal? start stop) '())
        ((> start stop) '())
        (else (cons start (make-list (+ start step) stop step))))) 

;;; Function mix: list list -> list
;;; Parameters: lst1 and lst2
;;; Returns alternating elements of both lists and if one list is longer,
;;; it returns the remaining elements at the end.
;;; Expected types: takes in 2 lists and returns a combination of 2 lists
(define (mix lst1 lst2)
  (cond ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else (cons (car lst1) (mix lst2 (cdr lst1))))))

;;; Function take-while: predicate list -> list
;;; Paremeters: pred and lst
;;; Returns first few elements that satisfy the critera made by predicate
;;; Expected types: takes in a predicate and a list and returns a new list
(define (take-while pred lst)
    (cond ((null? lst) '()) ;base case
          ;if car lst satisfy the predicate, then include car & filter cdr 
          ((pred (car lst))(cons (car lst)(take-while pred (cdr lst))))
          (else '())))  

;;; Function at-least-two?: list -> Boolean
;;; Parameters: xs
;;; Returns true if the numbers are in order and the difference between each number is at least 2
;;; Expected types: takes in a list and returns a Boolean
(define (at-least-two? xs)
  (cond ((null? xs) #t)
        ((null? (cdr xs)) #t)
        ((> (car xs) (cadr xs)) #f)
        ((>= (- (cadr xs)(car xs)) 2) (at-least-two? (cdr xs)))
        (else (< (- (cadr xs)(car xs)) 2) #f)))
       
;;; Function lowest: list -> number
;;; Parameters: xs
;;; Returns the lowest number in the list
;;; Expected types: takes in a list and returns number
(define (lowest xs)
  (cond ((null? (cdr xs)) (car xs))
        ((< (car xs)(lowest (cdr xs)))(car xs))
        (else (lowest (cdr xs))))) 
