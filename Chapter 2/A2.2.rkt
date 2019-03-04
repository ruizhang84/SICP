#lang racket

;2.17
(define (last-pair l)
  (cond ((= (length l) 0) l)
        ((= (length l) 1) (car l))
        (else (last-pair (cdr l)))))

;(last-pair (list 23 72 149 34))
;(34)

;2.18
(define (append ls a)
  (if (null? ls)
      (cons a ls)
      (cons (car ls) (append (cdr ls) a))))
;(append '(1 2 3) '4)
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













