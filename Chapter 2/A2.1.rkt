#lang racket
;2.4
#;(define (cons x y)
  (lambda (m) (m x y)))
#;(define (car z)
  (z (lambda (p q) p)))
#;(define (cdr z)
  (z (lambda (p q) q)))

;(car (cons 1 2))
;1
;(car (cdr (cons (cons 1 2) (cons 3 4))))
;3

;2.5
#;(define (cons a b)
  (* (expt 2 a) (expt 3 b)))
#;(define (car a)
  (define (car-iter result)
    (if (= (modulo result 3) 0)
        (car-iter (/ result 3))
        result))
  (log (car-iter a) 2))
#;(define (cdr a)
  (define (car-iter result)
    (if (= (modulo result 2) 0)
        (car-iter (/ result 2))
        result))
  (log (car-iter a) 3))

;(car (cons 1 2))
;1
;(car (cdr (cons (cons 1 2) (cons 2 2))))
;2


;2.6
(define zero
  (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define incr
  (lambda (x) (+ x 1)))

;((zero incr) 0)
;0
;(((add-1 zero) incr) 0)
;1
;(((add-1 (add-1 zero)) incr) 0)
;2

(define one
  (lambda (f) (lambda (x) (f x))))
;(((add-1 one) incr) 0)
;2

(define two
  (lambda (f) (lambda (x) (f (f x)))))
;((two incr) 0)
;2

(define (add-3 n)
  (lambda (f)
    (lambda (x)
      (f (f (f ((n f) x)))))))
;(((add-3 zero) incr) 0)
;3
#;(define (add m n)
  (lambda (f)
    (lambda (x)
      (let ((op ((n f) x)))
        (define (add-iter result k)
          (if (= k 0)
              result
              (add-iter (f result) (- k 1))))
        (add-iter op m)))))
;(((add 3 zero) incr) 0)
;(((add 1 zero) incr) 0)





;2.7
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y)))
        )
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
#;(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

#;(define (make-interval a b)
  (cons a b))
#;(define (upper-bound x)
  (cdr x))
#;(define (lower-bound x)
  (car x))

#;(add-interval
 (make-interval 1 5)
 (make-interval 2 3))
; '(3 . 8)
#;(mul-interval
 (make-interval 1 5)
 (make-interval 2 3))
;(2, 15)

;2.8
#;(define (sub-interval x y)
  (add-interval
   x
   (make-interval (- (upper-bound y)) (- (lower-bound y)))))

(define (sub-interval x y)
  (let ((p1 (- (upper-bound x) (upper-bound y)))
        (p2 (- (upper-bound x) (lower-bound y)))
        (p3 (- (lower-bound x) (upper-bound y)))
        (p4 (- (lower-bound x) (lower-bound y))))
    (make-interval (min p1 p3) (max p2 p4))))

#;(sub-interval
 (make-interval 1 5)
 (make-interval 2 3))
;'(-2 . 3)

;2.9
#;(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))
;(width (make-interval 2 4))
;1
#;(width
 (add-interval
 (make-interval 1 6)
 (make-interval 2 3)))
;3
#;(define (width-add-interval w1 w2)
  (+ w1 w2))
#;(width-add-interval
 (width (make-interval 1 6))
 (width (make-interval 2 3)))
;3

;2.10
(define (div-interval x y)
  (if (or (= (upper-bound y) 0) (= (lower-bound y) 0))
      (error "divided by 0")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))
#;(div-interval
 (make-interval 1 5)
 (make-interval 2 3))
;'(0.3333333333333333 . 2.5)
#;(div-interval
 (make-interval 1 5)
 (make-interval 0 3))


;2.11
#;(define (mul-interval x y)
  (cond ((and (< (lower-bound x) 0)
             (< (lower-bound y) 0)
             (< (upper-bound x) 0)
             (< (upper-bound y) 0))
         (make-interval
          (* (upper-bound x) (upper-bound y))
          (* (lower-bound x) (lower-bound y))))
        ((and (< (lower-bound x) 0)
              (< (lower-bound y) 0)
              (> (upper-bound x) 0)
              (> (upper-bound y) 0))
         (make-interval
          (min (* (upper-bound x) (lower-bound y))(lower-bound x) (upper-bound y))
          (max (* (upper-bound x) (upper-bound y))(lower-bound x) (lower-bound y))))
        ((and (< (lower-bound x) 0)
              (< (lower-bound y) 0)
              (> (upper-bound x) 0)
              (< (upper-bound y) 0))
         (make-interval
          (* (upper-bound x) (upper-bound y))
          (* (lower-bound x) (upper-bound y))))
        ((and (< (lower-bound x) 0)
              (< (lower-bound y) 0)
              (< (upper-bound x) 0)
              (> (upper-bound y) 0))
         (make-interval
          (* (upper-bound x) (upper-bound y))
          (* (upper-bound x) (lower-bound y))))
        ((and (> (lower-bound x) 0)
              (> (lower-bound y) 0)
              (> (upper-bound x) 0)
              (> (upper-bound y) 0))
         (make-interval
          (* (lower-bound x) (lower-bound y))
          (* (upper-bound x) (upper-bound y))))
        ((and (< (lower-bound x) 0)
              (> (lower-bound y) 0)
              (> (upper-bound x) 0)
              (> (upper-bound y) 0))
         (make-interval
          (* (lower-bound x) (upper-bound y))
          (* (upper-bound x) (upper-bound y))))
        ((and (> (lower-bound x) 0)
              (< (lower-bound y) 0)
              (> (upper-bound x) 0)
              (> (upper-bound y) 0))
         (make-interval
          (* (upper-bound x) (lower-bound y))
          (* (upper-bound x) (upper-bound y))))
        ((and (or (= (lower-bound x) 0)
                  (= (lower-bound y) 0))
              (> (upper-bound x) 0)
              (> (upper-bound y) 0))
         (make-interval
          0
          (* (upper-bound x) (upper-bound y))))
        ((and (< (lower-bound x) 0)
              (< (lower-bound y) 0)
              (or (= (upper-bound x) 0)
                  (= (upper-bound y) 0)))
         (make-interval
          (0
          (* (lower-bound x) (lower-bound y)))))))

      
(define (make-interval a b)
  (cons a b))
(define (upper-bound x)
  (cdr x))
(define (lower-bound x)
  (car x))


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

#;(mul-interval
 (make-interval 1 5)
 (make-interval 2 3))
;(2, 15)

;2.12
(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))
(define (percent x)
  (/ (width x) (center x) ))
#;(make-center-percent (center (make-interval 1 5))
                     (percent (make-interval 1 5)))
;(1 . 5)


;2.13
#;(define (mul-interval x y)
  (make-center-percent
   (* (center x) (center y))
   (+ (percent x) (percent y))))
#;(mul-interval (make-interval 1.1 1.2)
              (make-interval  2.3 2.4))
;'(2.53 . 2.88)
;'(2.5274999999999994 . 2.877499999999999)


;2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval (div-interval one r1)
                   (div-interval one r2)))))
;(par1 (make-interval 2 3) (make-interval 4 5))
;'(1.0 . 2.5)
;(par2 (make-interval 2 3) (make-interval 4 5))
;'(1.3333333333333333 . 1.875)


;(par1 (make-center-width 2.5 0.001) (make-center-width 4.5 0.001))
;'(1.6056842330762637 . 1.6086026007430696)
;(par1 (make-center-percent 2.5 0.01) (make-center-percent 4.5 0.01))
;'(1.5595650636492222 . 1.6560064935064938)
;(par2 (make-center-width 2.5 0.001) (make-center-width 4.5 0.001))
;'(1.606602029151186 . 1.6076836618109112)











