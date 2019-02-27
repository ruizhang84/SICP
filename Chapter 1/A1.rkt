#lang racket

(define (square x)
  (* x x))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))

#;(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

#;(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess new-guess)
  (< (abs (- guess new-guess)) 0.01))

(define (sqrt-iter guess x)
  (let ([new-guess (improve guess x)])
    (if (good-enough? guess new-guess)
        guess
        (sqrt-iter (improve guess x) x))))
(define (sqrt x)
  (sqrt-iter 1.0 x))

;(sqrt 9)
;(sqrt 137)
;(sqrt 100101000023131)

(define (improve-cube y x)
  (/ (+ (* 2 y)
        (/ x (square y)))
     3))

(define (cube-root-iter guess x)
  (let ([new-guess (improve-cube guess x)])
    (if (good-enough? guess new-guess)
        guess
        (cube-root-iter new-guess x))))

(define (cube-root x)
  (cube-root-iter 1 x))

;(cube-root 27)
;(cube-root 526)



(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;(A 1 1)
;(A 1 10)
;(A 2 4)
;(A 3 2)

#;(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (f (- n 2))
                 (f (- n 3))
                 ))))

#;(define (f n)
  (define (f-iter r1 r2 r3 count)
    (cond ((= 0 count) r1)
          ((= 1 count) r2)
          ((= 2 count) r3)
          (else (f-iter r2 r3 (+ r1 r2 r3) (- count 1)))
          ))      
  (f-iter 0 1 2 n))

;(f 12)
(define (transfer row)
  (if (null? (cdr row))
      row
      (cons (+ (car row)
               (car (cdr row)))
            (transfer (cdr row)))))
      

(define (transfer-row row)
  (cons (car row) (transfer row)))

;(transfer-row '(1 3 3 1))
(define (pascal row)
  (if (= row 1)
      '(1)
      (transfer-row (pascal (- row 1)))))
;(pascal 5)

#;(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
#;(* 2 3)

(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))

#;(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
;(fast-expt 2 12) ; 4096
;(* 2 2)

#;(define (fast-expt a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt a (square b) (/ n 2)))
        (else (fast-expt (* a b) b (- n 1)))))
#;(fast-expt 1 2 8)

#;(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (* a (halve b))))
        (else (+ a (* a (- b 1))))))
;(* 2 8)

(define (*-iter a b c)
  (cond ((= b 0) c)
        ((even? b) (*-iter (double a) (halve b) c))
        (else (*-iter a (- b 1) (+ c a)))))
;(*-iter 2 12 0)

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;(fib 12)

#;(define (smallest-divisor n)
  (define (smallest-divisor-iter n i)
    (if (= (modulo n i) 0)
        i
        (smallest-divisor-iter n (+ i 1))))
  (smallest-divisor-iter n 2))

;(smallest-divisor 199)
;(smallest-divisor 1999)
;(smallest-divisor 19999)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                   m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

#;(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
#;(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
#;(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 3)))

(define (divides? a b)( = (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (* 1.0 (current-milliseconds))))

(define (start-prime-test n start-time)
  (cond ((prime? n) (report-prime (- (current-milliseconds) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display (* 1.0 elapsed-time)))

#;(timed-prime-test 1999)

(define (search-for-primes start)
  (cond ((fast-prime? start 12) start)
        ((even? start) (search-for-primes (+ start 1)))
        (else (search-for-primes (+ start 2)))))

(define (expmod-1 a n m)
  (cond ((= n 0) 1)
        ((even? n)
         (let* ([number (expmod-1 a (/ n 2) m)]
                [y (remainder (square number)  m)])
         (cond ((= number 1) y)
               ((= number (- m 1)) y)
               ((= y 1) 0)
               (else y))))
        (else (remainder (* a (expmod-1 a (- n 1) m)) m))))

(define (fast-fermat-test n)
  (let* ([num  (+ 2 (random (- n 2)))]
        [y (expmod-1 num (- n 1) n)])
  (= y 1)))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fast-fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(timed-prime-test
 (search-for-primes 1000))

(timed-prime-test
 (search-for-primes 10000000))

(timed-prime-test
 (search-for-primes 1000000000))