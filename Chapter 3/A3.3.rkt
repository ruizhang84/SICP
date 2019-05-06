#lang racket
(require rnrs/mutable-pairs-6)
#;(define (cons x y)
    (let ((new (get-new-pair)))
      (set-car! new x)
      (set-cdr! new y)
      new))


;3.12
(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (mcdr x)) x (last-pair (mcdr x))))

;(define x (list 'a 'b))
;(define y (list 'c 'd))
;(define z (append x y))
;(cdr x)
;'(b)

;(define x (mcons 'a (mcons 'b '())))
;(define y (mcons 'c (mcons 'd '())))
;(define w (append! x y))
;(mcdr x)
;(mcons 'b (mcons 'c (mcons 'd '())))


(define x (mcons 'a 'b))
(define z1 (mcons x x))
(define z2 (mcons (mcons 'a 'b) (mcons 'a 'b)))

(define (set-to-wow! x) (set-car! (mcar x) 'wow) x)
;(set-to-wow! z1)
;(mcons (mcons 'wow 'b) (mcons 'wow 'b))
;(set-to-wow! z2)
;(mcons (mcons 'wow 'b) (mcons 'a 'b))

;3.16
#;(define (count-pairs x)
  (if (not (mpair? x))
      0
      (+ (count-pairs (mcar x))
         (count-pairs (mcdr x))
         1)))
(define x1 (mcons 'a 'b))
(define x2 (mcons 'c 'd))
(define x3 'c)
(define y1 (mcons x1 x2))
(define y2 (mcons x3 (mcons x1 x1)))
(define y3 (mcons (mcons x1 x2) (mcons x2 x1)))
;y1
;(mcons (mcons 'a 'b) (mcons 'c 'd))
;(count-pairs y1)
;3
;y2
;(mcons 'c (mcons (mcons 'a 'b) (mcons 'a 'b)))
;(count-pairs y2)
;4
;y3
;(mcons (mcons (mcons 'a 'b) (mcons 'c 'd)) (mcons (mcons 'c 'd) (mcons 'a 'b)))
;(count-pairs y3)
;7
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define y4 (make-cycle (mcons 'a (mcons 'b (mcons 'c '())))))
;(count-pairs y4)
;
;3.17
(define memory-pairs
  '())
(define (equal-pairs l1 l2)
  (if (and (eq? (mcar l1) (mcar l2))
           (eq? (mcdr l1) (mcdr l2)))
      #t
      #f))
(define (seen-pairs ls)
  (define (have-seen mem)
    (cond
      ((not (mpair? mem)) #f)
      ((equal-pairs (mcar mem) ls) #t)
      (else (have-seen (mcdr mem)))))
  (have-seen memory-pairs))

;(equal-pairs x1 x1)
;#t
;(set! memory-pairs (mcons x1 memory-pairs))
;(mcons (mcons 'a 'b) '())
;memory-pairs
;(seen-pairs x1)
;#t
(define (count-pairs x)
  (cond
    ((not (mpair? x)) 0)
    ((seen-pairs x) 0)
    (else (begin
            (set! memory-pairs (mcons x memory-pairs))
            (+ (count-pairs (mcar x))
               (count-pairs (mcdr x))
               1)))))
;(count-pairs y1)
;3
;(count-pairs y2)
;2
;(count-pairs y3)
;2
;(count-pairs y4)
;3

;3.18
(define (has-cycle? x)
  (cond
    ((not (mpair? x)) #f)
    ((seen-pairs x) #t)
    (else (begin
            (set! memory-pairs (mcons x memory-pairs))
            (or (has-cycle? (mcar x)) (has-cycle? (mcdr x)))
            ))))
(has-cycle? y4)








