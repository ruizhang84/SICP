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
#;(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
;(define y4 (make-cycle (mcons 'a (mcons 'b (mcons 'c '())))))
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
;(has-cycle? y4)


#;(define (cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined operation: CONS" m))))
  dispatch)
;(define (car z) (z 'car))
;(define (cdr z) (z 'cdr))


(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else
           (error "Undefined operation: CONS" m))))
  dispatch)


(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value) z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value) z)

(define a (cons 1 2))
;(set-car! a 3)
;(car (set-car! a 3))


;(define (front-ptr queue) (car queue))
;(define (rear-ptr queue) (cdr queue))
#;(define (set-front-ptr! queue item)
  (set-car! queue item))
#;(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

#;(define (empty-queue? queue)
  (null? (front-ptr queue)))

;(define (make-queue) (cons '() '()))

#;(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

#;(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

#;(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

;3.21
#;(define (print-queue queue)
  (define (recur-queue q)
    (if (null? q)
        '()
        (begin
          (display (car q))
          (recur-queue (cdr q)))))
  (recur-queue (front-ptr queue)))

;(define q1 (make-queue))
;(insert-queue! q1 'a)
;(car (front-ptr q1))
;(print-queue q1)
;(insert-queue! q1 'b)
;(print-queue q1)
;(delete-queue! q1)
;(print-queue q1)
;(delete-queue! q1)
;(print-queue q1)

;3.22
(define (make-queue)
  (let ((front-ptrx '())
        (rear-ptrx '()))
    (define (empty-queue?) (null? front-ptrx))
    (define (insert-queue item)
      (cond
        ((empty-queue?)
            (set! front-ptrx (cons item '()))
            (set! rear-ptrx (cons item '())))
        (else
         (set-cdr! rear-ptrx (cons item '()))
         (set! rear-ptrx (cons item '())))))
    (define (delete-queue)
      (set! front-ptrx (cdr front-ptrx)))
    (define (dispatch m)
      (cond
        ((eq? m 'empty-queue?) (empty-queue?))
        ((eq? m 'insert-queue) insert-queue)
        ((eq? m 'delete-queue) delete-queue)
        ((eq? m 'front-ptr) front-ptrx)
        ((eq? m 'rear-ptr)  rear-ptrx)
        (else
         (error "Undefined operation: CONS" m))))
    dispatch))



(define (front-ptr queue)
  (queue 'front-ptr))
(define (rear-ptr queue)
  (queue 'rear-ptr))
(define (insert-queue! queue item)
  ((queue 'insert-queue) item))
(define (delete-queue! queue item)
  ((queue 'delete-queue) item))
(define (empty-queue? queue)
  (queue 'empty-queue?))


;(define q1 (make-queue))
;(front-ptr q1)
;(insert-queue! q1 'a)
;(car (front-ptr q1))
;'a



(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))


(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

#;(define (make-table)
  (list '*table*))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))









