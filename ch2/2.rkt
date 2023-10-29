#lang sicp
;rational number
;2.1
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d)))
        (nn (if (or (and (<= n 0) (<= d 0))
                    (and (>= n 0) (>= d 0)))
                (abs n)
                (if (> n 0)
                    (- n)
                    n))))
    (cons (/ nn g) (/ (abs d) g))))
        

(define numer car)
(define denom cdr)
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;2.2
(define (sqrt x)
  (define (sqrt-iter res)
    (if (< (abs (- (square res) x) 0.00000001))
        res
        (sqrt-iter (/ (+ res (/ x res)) 2))))
  (sqrt-iter 1.0))
(define (square x) (* x x))
           
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (x-dis s)
  (/ (abs (- (x-point (start-segment s))
             (x-point (end-segment s))))
     2.0))
(define (y-dis s)
  (/ (abs (- (y-point (start-segment s))
             (y-point (end-segment s))))
     2.0))
                                   
(define (distance s)
  (sqrt (+ (square (x-dis s))
           (square (y-dis s)))))
(define (mid-dis s)
  (/ (distance s) 2))
(define (min-x s)
  (if (< (x-point (start-segment s))
         (x-point (end-segment s)))
      (x-point (start-segment s))
      (x-point (end-segment s))))
(define (min-y s)
  (if (< (y-point (start-segment s))
         (y-point (end-segment s)))
      (y-point (start-segment s))
      (y-point (end-segment s))))
         
(define (mid-point s)
  (make-point (+ (min-x s) (x-dis s))
              (+ (min-y s) (y-dis s))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;2.3
(define (make-rect p len width)
  (cons p (cons len width)))
(define (rect-len r)
  (car (cdr r)))
(define (rect-width r)
  (cdr (cdr r)))
  
(define (rect-area r)
  (* (rect-len r)
     (rect-width r)))
(define (rect-per r)
  (* (+ (rect-len r)
        (rect-width r))
     2))
(define (make-rect2 ul lr)
  (cons ul lr))
(define (rect2-get-ul r)
  (car r))
(define (rect2-get-lr r)
  (cdr r))
(define (rect2-len r)
  (abs (- (x-point (rect2-get-ul r))
          (x-point (rect2-get-lr r)))))
(define (rect2-width r)
  (abs (- (y-point (rect2-get-ul r))
          (y-point (rect2-get-lr r)))))
(define (rect2-area r)
  (* (rect2-len r)
     (rect2-width r)))
(define (rect2-per r)
  (* (+ (rect2-len r)
        (rect2-width r))
     2))
  
;2.4
(define (cons2.4 x y)
  (lambda (m) (m x y)))
(define (car2.4 z)
  (z (lambda (p q) p)))
(define (cdr2.4 z)
  (z (lambda (p q) q)))

;2.5
(define (cons2.5 x y)
  (* (expt 2 (+ x 1)) (expt 3 (+ y 1))))
(define (getpart n d)
  (if (= (remainder n d) 0)
      (+ 1 (getpart (/ n d) d))
      0))
(define (car2.5 z)
  (- (getpart z 2) 1))
(define (cdr2.5 z)
  (- (getpart z 3) 1))

;2.6
(define (inc n) (+ n 1))
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add-church n1 n2)
  (lambda (f) (lambda (x) ((n2 f) ((n1 f) x)))))
;interval
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))
;2.7
(define (make-interval a b) (cons a b))
(define (get-start x) (car x))
(define (get-end x) (cdr x))
(define (upper-bound x)
  (if (> (get-start x) (get-end x))
      (get-start x)
      (get-end x)))
(define (lower-bound x)
  (if (< (get-start x) (get-end x))
      (get-start x)
      (get-end x)))
;2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
;2.10
(define (div-interval-new x y)
  (if (not (and (= (upper-bound y) 0) (= (lower-bound y) 0)))
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))
      (error "0")))
;make center width
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
;2.12
(define (make-center-percent c p)
  (make-interval (* c (/ (- 100.0 p) 100.0))
                 (* c (/ (+ 100.0 p) 100.0))))
(define (center-interval i)
  (/ (+ (upper-bound i) (lower-bound i)) 2))
(define (percent-interval i)
  (* (- (/ (upper-bound i) (center-interval i)) 1) 100))
;2.14
(define inter-one (make-interval 1 1))
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1 )))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))
;2.17
(define (last-pair ls)
  (if (null? (cdr ls))
      ls
      (last-pair (cdr ls))))
;2.18
(define (reverse ls)
  (define (rev-iter xs res)
    (if (null? xs)
        res
        (rev-iter (cdr xs) (cons (car xs) res))))
  (rev-iter ls nil))
;2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
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
(define (no-more? ls)
  (null? ls))
(define (except-first-denomination ls)
  (cdr ls))
(define (first-denomination ls)
  (car ls))
;2.20
(define (same-parity x . y)
  (cons x
        (if (= (remainder x 2) 0)
            (take-parity 0 y)
            (take-parity 1 y)
            )))
(define (take-parity n ls)
  (if (null? ls)
      ls
      (if (= (remainder  (car ls) 2) n)
          (cons (car ls)(take-parity n (cdr ls)))
          (take-parity n (cdr ls)))))
;map
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
;2.21
(define (sq-list-1 items)
  (if (null? items)
      nil
      (cons (car items) (sq-list-1 (cdr items)))))
(define (sq-list-2 items)
  (map square items))
;2.23
(define (for-each f xs)
  (cond ((not (null? xs))
         (f (car xs)) (for-each f (cdr xs)))))
;2.25
(define ex2.25a (list 1 3 (list 5 7) 9))
;(cdr (car (cdr (cdr ex2.25a))))
(define ex2.25b (list (list 7)))
;(car (car ex2.25b))
(define ex2.25c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr ex2.25c))))))))))))

;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

;2.27
(define (deep-reverse xs)
  (define (deep-reverse-iter ls res)
    (if (null? ls)
        res
        (if (pair? (car ls))
            (deep-reverse-iter
             (cdr ls)
             (cons (deep-reverse (car ls)) res))
            (deep-reverse-iter
             (cdr ls)
             (cons (car ls) res)))))
  (deep-reverse-iter xs nil))
;2.28
(define (fringe xs)
  (cond ((null? xs) nil)
        (else
         (if (not (pair? (car xs)))
             (cons (car xs)
                   (fringe (cdr xs)))
             (append (fringe (car xs))
                     (fringe (cdr xs)))))))
;2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch m)
  (if (or (not (pair? (branch-structure m))) (null? m))
      nil
      (car m)))
(define (right-branch m)
  (if (or (not (pair? (branch-structure m))) (null? m))
      nil
      (car (cdr m))))
(define (branch-length b)
  (if (or (not (pair? b)) (null? b))
      0
      (car b)))
(define (branch-structure b)
  (if (or (not (pair? b)) (null? b))
      0
      (car (cdr b))))
;b
(define (total-weight m)
  (cond ((null? m) 0)
        ((not (pair? m)) m)
        (else
         (+ (if (null? (left-branch m))
                0
                (total-weight (branch-structure (left-branch m))))
            (if (null? (right-branch m))
                0
                (total-weight (branch-structure (right-branch m))))))))
;c
(define (balanced? m)
  (if (or (null? m) (not (pair? m)))
      #t
      (and (= (* (branch-length (left-branch m))
                 (total-weight (branch-structure (left-branch m))))
              (* (branch-length (right-branch m))
                 (total-weight (branch-structure (right-branch m)))))
           (balanced? (branch-structure (left-branch m)))
           (balanced? (branch-structure (right-branch m))))))

;2.30
(define (square-tree t)
  (cond ((and (not (null? t)) (not (pair? (car t))))
         (cons (square (car t)) (square-tree (cdr t))))
        ((and (not (null? t)) (pair? (car t)))
         (cons (square-tree (car t)) (square-tree (cdr t))))
        (else nil)))
(define (square-tree-map t)
  (define (help xs)
    (cond ((pair? xs) (square-tree-map xs))
          (else (square xs))))
  (map help t))
;2.31
(define (square-tree-ab tree) (tree-map square tree))
(define (tree-map f t)
  (cond ((null? t) nil)
        ((pair? (car t)) (cons (tree-map f (car t))
                               (tree-map f (cdr t))))
        (else (cons (f (car t)) (tree-map f (cdr t))))))
;2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;filter
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
;accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;2.33
(define (map-acc p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate + 0 sequence))
;2.34
(define (horner-eval x co-sq)
  (accumulate (lambda (this-co higher-terms)
                (+ (* higher-terms x) this-co))
              0
              co-sq))
;2.35
(define (count-leaves t)
  (accumulate + 0
              (map (lambda (x) (if (pair? x)
                                   (count-leaves x)
                                   1))
                   t)))
;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (car seqs))
            (accumulate-n op init (cdr seqs)))))
;2.37
(define (map-2 f a b)
  (if (or (null? a) (null? b))
      nil
      (cons (f (car a) (car b)) (map-2 f (cdr a) (cdr b)))))
(define (dot-product v w)
  (accumulate + 0 (map-2 * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x)
         (dot-product x v))
       m))
(define (repeat n x)
  (if (= n 0)
      nil
      (cons x (repeat (- n 1) x))))
      
(define (transpose mat)
  (accumulate-n (map list) (repeat (length mat) nil) mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map list m)))
;2.38
(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial seq))
;2.39
(define fold-right accumulate)
(define (reverse-fr seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))
(define (reverse-fl seq)
  (fold-left (lambda (x y) (append (list y) x)) nil seq))
;2.40
(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (unique-pairs n)
  (accumulate
   append
   nil
   (map (lambda (i)
          (map (lambda (j) (list i j))
               (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n))))
;2.41
(define (triple n s)
  (filter
   (lambda (x) (not (null? x)))
   (flatmap
    (lambda (i)
      (flatmap
       (lambda (j)
         (map (lambda (k)
                (cond ((= (+ i j k) s)
                       (list k j i))
                      (else nil)))
              (enumerate-interval 1 (- j 1))))
       (enumerate-interval 2 (- i 1))))
    (enumerate-interval 3 n))))
;2.42
(define empty-board nil)
(define (adjoin-position new-row k rest-of-queens)
  (if (= 0 (length (filter
                    (lambda (x) (= x new-row))
                    rest-of-queens)))
      (append rest-of-queens (list new-row))
      nil))
(define (get x ls)
  (if (= x 1)
      (car ls)
      (get (- x 1) (cdr ls))))
(define (drop-last ls)
  (if (= 1 (length ls))
      nil
      (cons (car ls) (drop-last (cdr ls)))))
(define (neighbour-check xs)
  (define (nc x y)
    (if (or (= -1 (car y))
            (and (cdr y)
                 (not (= x (car y)))
                 (not (= x (- (car y) 1)))
                 (not (= x (+ (car y) 1)))))
        (cons x #t)
        (cons x #f)))
  (fold-right nc (cons -1 #t) xs))
;(define (diag-check ls)
; (define (dc x y)
;    (if (or (= -1 (car y))
;           ))))
(define (safe? k positions)
  (or (= 1 (length positions))
      (and (> (length positions) 0)
           (let ((x (get k positions)))
             (> x 0)))))
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
