#lang racket

;2.17
(define (last-pair l)
  (cond ((= (length l) 0) l)
        ((= (length l) 1) (car l))
        (else (last-pair (cdr l)))))

;(last-pair (list 23 72 149 34))
;(34)

;2.18
#;(define (append-item ls a)
  (if (null? ls)
      (cons a ls)
      (cons (car ls) (append-item (cdr ls) a))))
#;(define (append l1 l2)
  (if (null? l2)
      l1
      (append (append-item l1 (car l2))
              (cdr l2))))
;(append '(1 2 3) '(4 5))
;'(1 2 3 4 5)
(define (reverse ls)
  (if (null? ls)
      ls
      (append (reverse (cdr ls)) (car ls))))
;(reverse (list 1 4 9 16 25))
;'(25 16 9 4 1)


;2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define first-denomination
  (lambda (ls)
    (car ls)))
(define except-first-denomination
  (lambda (ls)
    (cdr ls)))
(define no-more?
  (lambda (ls)
    (null? ls)))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))
;(cc 100 us-coins)
;292


;2.20
(define (same-parity . ls)
  (define (same? a)
    (or (and (even? (car ls))(even? a))
        (and (odd? (car ls)) (odd? a))))
  (define (filter l)
    (cond ((null? l) l)
          ((same? (car l)) (cons (car l) (filter (cdr l))))
          (else (filter (cdr l)))))
  (filter ls))
   

;(same-parity 1 2 3 4 5 6 7)
;'(1 3 5 7)
;(same-parity 2 3 4 5 6 7)
;'(2 4 6)


;2.21
(define square (lambda (x) (* x x)))
#;(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items))
            (square-list (cdr items)))))
#;(define (square-list items)
  (map square items))

;(square-list (list 1 2 3 4 5))
;'(1 4 9 16 25)

;2.22
#;(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

#;(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))
;(square-list (list 1 2 3 4 5))
;'(25 16 9 4 1)
;'(((((() . 1) . 4) . 9) . 16) . 25)


;2.23
(define (for-each proc items)
  (cond ((null? items) '())
        (else (proc (car items))(for-each proc (cdr items)))))
#;(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;2.24
;(list 1 (list 2 (list 3 4)))
;'(1 (2 (3 4)))

;2.25
;(define a '(1 3 (5 7) 9))
;(define b '((7)))
;(define c '(1 (2 (3 (4 (5 (6 7)))))))
;(car (cdr (car (cdr (cdr a)))))
;(car (car b))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))


;2.26
;(define x (list 1 2 3))
;(define y (list 4 5 6))
;(append x y)
;'(1 2 3 (4 5 6))
;(cons x y)
;'((1 2 3) 4 5 6)
;(list x y)
;'((1 2 3) (4 5 6))

;2.27
;(define x (list (list 1 2) (list 3 4)))
(define (deep-reverse x)
  (if (not (pair? x))
      x
      (let ((new-x (reverse x)))
        (cons (deep-reverse (car new-x))
              (deep-reverse (cdr new-x))))))
;(deep-reverse x)
;'((4 3) (2 1))

;2.28
;(define x (list (list 1 2) (list 3 4)))
(define (fringe x)
  (define (append-to l1 l2)
    (if (null? l2)
        l1
        (append-to (append l1 (car l2)) (cdr l2))))
  (cond ((not (pair? x)) x)
        ((not (pair? (car x))) (cons (car x) (fringe (cdr x))))
        (else (append-to (fringe (car x))
                   (fringe (cdr x))))))
;(fringe x)

;2.29
#;(define (make-mobile left right)
  (list left right))
#;(define (make-branch length structure)
  (list length structure))
;a
(define branch-length
  (lambda (branch)
    (car branch)))
#;(define branch-structure
  (lambda (branch)
    (car (cdr branch))))

(define left-branch
  (lambda (mobile)
    (car mobile)))
#;(define right-branch
  (lambda (mobile)
    (car (cdr mobile))))
;(define a (make-branch 1 2))
;(define b (make-branch 2 3))
;(define c (make-mobile a b))
;(define d (make-mobile a c))
;(define e (make-mobile c d))
;(left-branch a)
;1
;(left-branch e)
;'((1 2) (2 3))
;(right-branch d)
;'((1 2) (2 3))
;(branch-length a)
;1
;(branch-structure a)
;2
;b
(define total-weight
  (lambda (mobile)
    (cond ((number? (right-branch mobile)) (branch-structure mobile))
          ((number? (left-branch mobile)) (total-weight (right-branch mobile)))
          (else (+ (total-weight (left-branch mobile))
                   (total-weight (right-branch mobile)))))))
;(total-weight e)
;12
(define torque
  (lambda (mobile)
    (cond((number? (left-branch mobile))
          (* (branch-length mobile) (total-weight mobile)))
         (else (+ (torque (left-branch mobile))
                  (torque (right-branch mobile))))
         )))
(define balanced
  (lambda (mobile)
    (if (number? (left-branch mobile))
        #f
        (= (torque (right-branch mobile))
           (torque (left-branch mobile))))))
;(balanced e)
;d
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define right-branch
  (lambda (mobile)
    (cdr mobile)))
(define branch-structure
  (lambda (branch)
    (cdr branch)))
;(define a (make-branch 1 2))
;(define b (make-branch 2 3))
;(define c (make-mobile a b))
;(define d (make-mobile a c))
;(define e (make-mobile c d))
;(left-branch a)
;1
;(left-branch e)
;'((1 2) (2 3))
;(right-branch d)
;'((1 2) (2 3))
;(branch-length a)
;1
;(branch-structure a)
;2

;2.30
#;(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))
#;(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;'(1 (4 (9 16) 25) (36 49))

;2.31
(define (tree-map fuc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fuc sub-tree)
             (fuc sub-tree)))
       tree))
(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))
#;(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))

;(subsets '(1 2 3))
;'(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))







   
