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
#;(define equal?
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

#;(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;(define (make-sum a1 a2) (list '+ a1 a2))
;(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
;(define (addend s) (cadr s))
;(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
;(define (multiplier p) (cadr p))
;(define (multiplicand p) (caddr p))

;(deriv '(+ x 3) 'x)
;'(+ 1 0)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;(deriv '(+ x 3) 'x)
;1
;(deriv '(* (* x y) (+ x 3)) 'x)
;(+ (* x y) (* y (+ x 3)))

;2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((exponentiation? exp) (make-exponentiation
                                (base exp)
                                (exponent exp)
                                (deriv (base exp) var)))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (base exp)
  (cadr exp))
(define (exponent exp)
  (caddr exp))
(define (make-exponentiation u n du)
  (let ((power (list '** u (- n 1))))
    (list '* n power du)))

;(deriv '(** x 3) 'x)
;'(* 3 (** x 2) 1)

;2.57
#;(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (addend s) (cadr s))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (append (list '+) (cddr s))))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (append (list '*) (cddr p))))

;(deriv '(* x y (+ x 3)) 'x)
;'(+ (* x y) (* y (+ x 3)))


;2.59
#;(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

#;(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

#;(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


#;(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))


;(union-set (list 1 2 3) (list 2 3 4))
;'(1 2 3 4)

;2.60
#;(define (remove-duplicate set)
  (cond ((null? set) set)
        ((element-of-set? (car set) (cdr set))
         (remove-duplicate (cdr set)))
        (else (cons (car set) (remove-duplicate (cdr set))))))

#;(define (intersection-set set1 set2)
  (let ((set11 (remove-duplicate set1)))
    (cond ((or (null? set11) (null? set2)) '())
          ((element-of-set? (car set11) set2)
           (cons (car set11) (intersection-set (cdr set11) set2)))
          (else (intersection-set (cdr set11) set2)))))


#;(define (union-set set1 set2)
  (let ((set11 (remove-duplicate set1))
        (set22 (remove-duplicate set2)))
    (cond ((null? set11) set22)
          ((element-of-set? (car set11) set22)
           (union-set (cdr set11) set22))
          (else (cons (car set11)
                      (union-set (cdr set11) set22))))))


;(union-set (list 1 2 1 3) (list 2 3 2 4))
;'(1 3 2 4)

;2.61
#;(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

#;(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

#;(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))
        (else (cons x set))))

;(adjoin-set '3 '(1 2 4))
;'(1 2 3 4)

;2.62
#;(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
        (else (cons (car set2) (union-set set1 (cdr set2))))))

;(union-set '(1 2 3) '(2 3 4))
;'(1 2 3 4)

;2.63
#;(define (entry tree) (car tree))
;(define (left-branch tree) (cadr tree))
;(define (right-branch tree) (caddr tree))
#;(define (make-tree entry left right)
  (list entry left right))

#;(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

#;(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))

#;(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))
#;(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

#;(define a-tree (adjoin-set 11 (adjoin-set 1 (adjoin-set 7 (adjoin-set 5 (adjoin-set 9 (adjoin-set 3 '())))))))
;(tree->list-1 a-tree)
;(tree->list-2 a-tree)
;'(1 3 5 7 9 11)

;2.64
#;(define (list->tree elements)
  (car (partial-tree elements (length elements))))

#;(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

#;(define (union-set set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (define (union-set-ls l1 l2)
      (cond ((null? l1) l2)
            ((null? l2) l1)
            ((= (car l1) (car l2)) (cons (car l1) (union-set-ls (cdr l1) (cdr l2))))
            ((< (car l1) (car l2)) (cons (car l1) (union-set-ls (cdr l1) l2)))
            (else (cons (car l2) (union-set-ls l1 (cdr l2))))))
    (list->tree (union-set-ls list1 list2))))


;(define a-tree (adjoin-set 11 (adjoin-set 1 (adjoin-set 7 (adjoin-set 5 (adjoin-set 9 (adjoin-set 3 '())))))))
;(define b-tree (adjoin-set 1 (adjoin-set 2 (adjoin-set 4 (adjoin-set 5 (adjoin-set 9 (adjoin-set 3 '())))))))
;(union-set a-tree b-tree)
;'(1 2 3 4 5 7 9 11)
;'(4 (2 (1 () ()) (3 () ())) (7 (5 () ()) (9 () (11 () ()))))

;2.66
#;(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

#;(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (entry set-of-records)) (entry set-of-records))
        ((< given-key (entry set-of-records)) (given-key (left-branch set-of-records)))
        (else (given-key (right-branch set-of-records)))))



;2.67
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
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
;(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;(decode sample-message sample-tree)
;'(A D A B B C A)


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-set-? x set)
  (cond ((null? set) #f)
        ((eq? (car set) x) #t)
        (else (element-set-? x (cdr set)))))

(define (encode-symbol s tree)
  (cond ((leaf? tree) '())
        ((not (element-set-? s (symbols tree))) (error "wrong"))
        ((element-set-? s (symbols (left-branch tree))) (cons 0 (encode-symbol s (left-branch tree))))
        (else (cons 1 (encode-symbol s (right-branch tree))))))

;(define sample-message '(A D A B B C A))
;(encode sample-message sample-tree)
;'(0 1 1 0 0 1 0 1 0 1 1 1 0)

;2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge pair-set)
  (define (successive-merge-iter pairs result)
    (if (= (length pairs) 1)
        (make-code-tree (car pairs) result)
        (successive-merge-iter (cdr pairs) (make-code-tree (car pairs) result))))
  (successive-merge-iter (cdr pair-set) (car pair-set)))

;(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
;'((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))
;(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
;sample-tree
;'((leaf A 4) ((leaf B 2) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 4) (A B C D) 8)

;2.70
(define a-tree (generate-huffman-tree '((a 2) (get 2) (sha 3) (wah 1) (boom 1) (job 2) (na 16) (yip 9))))

#;(encode '(get a job
sha na na na na na na na na
get a job
sha na na na na na na na na
wah yip yip yip yip yip yip yip yip yip
sha boom) a-tree)

  



