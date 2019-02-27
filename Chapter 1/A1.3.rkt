#lang racket
;1.29
#;(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
#;(define (integeral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(define cube
  (lambda (x) (* x x x)))

(define (incr n)
  (+ n 1))
(define (integeral f a b n)
  (let ([h (/ (- b a) n)])
    (define yk
      (lambda (k)
        (cond ((= k 0) (f (+ a (* k h))))
              ((= k n) (f (+ a (* k h))))
              ((= (modulo k 2) 0) (* 4 (f (+ a (* k h)))))
              (else (* 2 (f (+ a (* k h))))))))
    (* (/ h 3)
       (sum yk 0 incr n))))


;(integeral cube 0 1 100000)
;0.24998750000000042
;0.24999666671666668


;1.30
#;(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
;(integeral cube 0 1 100000)


;1.32
#;(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
#;(define (product term a next b)
  (define (product-iter a result)
    (if (> a b)
        result
        (product-iter (next a) (* result (term a)))))
  (product-iter a 1))
  
(define (factorial n)
  (product * 1 incr n))

(define (incr-2 a)
  (+ a 2))
(define (mul a)
  (* a (+ a 2)))
(define (square a)
  (* a a))
(define (Pi n)
  (/ (product mul 2 incr-2 (+ 2 (* 2 n)))
     (product square 3 incr-2 (+ 3 (* 2 n)) )))

; 1.33
#;(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter combiner result a)
    (if (> a b)
        result
        (accumulate-iter combiner (combiner result (term a))
                         (next a))))
  (accumulate-iter combiner null-value a))


;(sum (lambda (x) x) 1 incr 100)
;5050

; 1.34
(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                              (filtered-accumulate filter combiner null-value
                                                   term (next a) next b)))
        (else (filtered-accumulate filter combiner null-value
                                   term (next a) next b))))

;(filtered-accumulate even? + 0 (lambda (x) x) 1 incr 100)
;2550

;(factorial 7)
;(* (Pi 10000) 4.0)
;3.1416711786826412

;1.34
#;(define f
  (lambda (g)
    (g 2)))
;(f f)

;1.35
(define toloerance 0.00001)
#;(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       toloerance))
  (define (try guess)
    ;(display guess)
    ;(newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;(fixed-point cos 1.0)
;0.7390822985224023

;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;1.6180327868852458

;1.36
;(fixed-point (lambda (x) (log 1000 x)) 10)
;4.555532257016376
;(define average (lambda (x y) (/ (+ x y) 2)))
;(fixed-point (lambda (x) (average x (log 1000 x))) 10)
;4.555536206185039

;1.37
#;(define cont-frac
  (lambda (n d k)
    (if (= k 0)
        (/ (n k) (d k))
        (/ (n k) (+ (d k) (cont-frac n d (- k 1)))))))

(define cont-frac
  (lambda (n d k)
    (define (cont-frac-iter result i)
      (if (= i 0)
          result
          (cont-frac-iter (/ (n i) (+ (d i) result)) (- i 1))))
    (cont-frac-iter (/ (n k) (d k)) k)))

#;(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1000)
;0.6180339887498948

;1.38
#;(+  2
    (cont-frac (lambda (i) 1.0)
           (lambda (i)
             (if (= (modulo (- i 2) 3) 0)
                 (* (+ 1 (/ (- i 2) 3)) 2.0)
                 1.0))
           1000))
;2.7182818284590453

;1.39
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (* x x))))
             (lambda (i) (- (* i 2) 1))
             k))

;(tan-cf 0.7853981633974483 1000)
;1.0


;1.40
(define average
  (lambda (x y) (/ (+ x y) 2)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (derive g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x) dx))))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((derive g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

#;(define (sqrt x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))
    
;(newtons-method (cubic 1 1 1) 1)
;(sqrt 12)

;1.41
(define double
  (lambda (f)
    (lambda (x)
      (f (f x)))))
;((double incr) 1)
;(((double (double double)) incr) 5)
;21

;1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))
;((compose square incr) 6)


;1.43
(define (repeated f n)
  (cond ((= n 0) f)
        ((even? n) (repeated (compose f f) (/ n 2)))
        (else (repeated (lambda (x) (f x)) (- n 1)))))
;((repeated square 2) 5)
; 625

;1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f x) (f (+ x dx)) (f (- x dx)))
       3)))
;((smooth sqrt) 3)
;1.7320508075672734
(define n-fold-smooth
  (lambda (f n)
    (repeated (smooth f) n)))

;((n-fold-smooth sqrt 2) 3)
;1.3160740129482273

;1.45
(define (nqrt x n)
  (fixed-point
   ((repeated average-damp (- n 2)) (lambda (y) (/ x (expt y (- n 1)))))
   1.0))

;(nqrt 9 2)
;3.0
;(nqrt 27 3)
;2.9999972321057697

;1.46
(define iterative-improve
  (lambda (enough? improve)
    (lambda (guess)
      (if (enough? guess)
          guess
          ((iterative-improve enough? improve)
           (improve guess))))))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- guess (f guess)))
         0.00001))
    f)
   first-guess))
;(nqrt 256 4)
;4.000000130140986
;(fixed-point cos 1.0)
;0.7390822985224023