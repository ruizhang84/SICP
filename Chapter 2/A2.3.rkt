#lang racket

;2.53
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
;(list 'a 'b 'c)
;'(a b c)
;(list (list 'george))
;'((george))
;(cdr '((x1 x2) (y1 y2)))
;'((y1 y2))
;(cadr '((x1 x2) (y1 y2)))
;'(y1 y2)
;(pair? (car '(a short list)))
;#f
;(memq 'red '((red shoes) (blue socks)))
;#f
;(memq 'red '(red shoes blue socks))
;'(red shoes blue socks)

;2.54
(define equal?
  (lambda (a b)
    (cond ((and (pair? a) (pair? b)) (and (eq? (car a) (car b))
                                          (equal? (cdr a) (cdr b))))
          ((and (null? a) (null? b)) #t)
          (else #f))))
#;(equal? '(this is a list) '(this is a list))
;#t
#;(equal? '(this is a list) '(this (is a) list))
;#f

;2.55
;(car ''abracadabra)
;(list 'quote 'abracadabra)
;''abracadabra








(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bit bit" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))









 
