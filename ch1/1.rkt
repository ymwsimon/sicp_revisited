#lang sicp
;1.2
(define 1.2eq (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
                 (* 3 (- 6 2) (- 2 7))))
;1.3
(define (large2 a b c)
  (if (or (>= a b)
          (>= a c))
      a
      0))
(define (square x)  (* x x))

(define (threenums a b c)
  (+ (square (large2 a b c))
     (square (large2 b a c))
     (square (large2 c a b))))
;1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
;1.7
(define (sqrt x) (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (/ (abs (- (* guess guess) x)) x) 0.0000000001))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))

;1.8
(define (cuberoot x)
  (cuberoot-iter 1.0 x))

(define (cuberoot-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cuberoot-iter (improve-cube guess x) x)))

(define (good-enough-cube? guess x)
  (< (/ (abs (- (* guess guess guess) x)) x) 0.0000001))

(define (improve-cube guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
;1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
;coin change
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;fib iter
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
;1.11
(define (f1.11r n)
  (if (< n 3)
      n
      (+ (f1.11r (- n 1))
         (* 2 (f1.11r (- n 2)))
         (* 3 (f1.11r (- n 3))))))
(define (f1.11re n)
  (if (< n 3)
      n
      (+ (f1.11re (- n 1))
         (f1.11re (- n 2))
         (f1.11re (- n 3)))))

(define (f1.11ie n)
  (if (< n 3)
      n
      (f1.11ie-iter 2 1 0 n)))
(define (f1.11ie-iter x y z n)
  (cond ((= n 0) x)
        (else (f1.11ie-iter (+ x y z) x y (- n 1)))))
   

(define (f1.11i n)
  (if (< n 3)
      n
      (f1.11-iter 2 1 0 n)))
(define (f1.11-iter x y z n)
  (cond ((= n 0)
         z)
        (else
         (f1.11-iter (+ x (* 2 y) (* 3 z)) x y (- n 1)))))
;1.12
(define (pt row)
  (if (>= row 1)
      (pt-pr 1 row)))

(define (pt-pr r nrow)
  (padding r nrow)
  (pt-pc 1 r)
  (newline)
  (if (< r nrow)
      (pt-pr (+ r 1) nrow)))

(define (pt-pc c r)
  (display (pt-val c r))
  (display " ")
  (if (< c r)
      (pt-pc (+ c 1) r)))

(define (pt-val c r)
  (if (or (= c 1) (= c r))
      1
      (+ (pt-val (- c 1) (- r 1)) (pt-val c (- r 1)))))

(define (padding r nrow)
  (if (< r nrow)
      (display " "))
  (if (< r nrow)
      (padding (+ r 1) nrow)))
;1.15
(define (cube x) (* x x x))
(define (p1.15 x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p1.15 (sine (/ angle 3.0)))))

;1.17
(define (even? n)
  (= (remainder n 2) 0))

(define (mul a b)
  (cond ((or (= a 0) (= b 0)) 0)
        ((even? b) (double (mul a (halve b))))
        (else (+ a (mul a (- b 1))))))

(define (double x) (+ x x))
(define (halve x) (/ x 2))
      
;1.19
(define (fib1.19 n)
  (fib1.19-iter 1 0 0 1 n))
(define (fib1.19-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib1.19-iter a
                       b
                       (+ (* p p) (* q q))
                       (+ (* 2 p q) (* q q))
                       (/ count 2)))
        (else (fib1.19-iter (+ (* b q) (* a q) (* a p))
                            (+ (* b p) (* a q))
                            p
                            q
                            (- count 1)))))
;smallest divisor
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (and (= n (smallest-divisor n)) (not (= n 1))))
;Fermat test
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
;1.23
(define (timed-prime-test23 n)
  (newline)
  (display n)
  (start-prime-test23 n (runtime)))
(define (start-prime-test23 n start-time)
  (if (prime23? n)
      (report-prime (- (runtime) start-time))))
(define (smallest-divisor23 n) (find-divisor23 n 2))
(define (find-divisor23 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor23 n (if (= test-divisor 2)
                                    (+ test-divisor 1)
                                    (+ test-divisor 2))))))

(define (prime23? n)
  (= n (smallest-divisor23 n)))
;1.24
(define (timed-prime-test24 n)
  (newline)
  (display n)
  (start-prime-test24 n (runtime)))
(define (start-prime-test24 n start-time)
  (if (fermat-test n)
      (report-prime (- (runtime) start-time))))
;1.25
(define (expmod25 base exp m)
  (remainder (fast-expt base exp) m))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (fermat-test25 n)
  (define (try-it a)
    (= (expmod25 a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;integral
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
          
;1.29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (t k)
    (cond ((or (= k 0) (= k n)) (f a))
          ((even? k) (* 2 (f (+ a (* k h)))))
          (else (* 4 (f (+ a (* k h)))))))
  (* (/ h 3)
     (sum t 0 inc n)))

;1.30
(define (sum30 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (define (id x) x)
  (product id 1 inc n))

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))
(define (fac-rec n)
  (define (id x) x)
  (product-rec id 1 inc n))
(define (pi n)
  (define (t x)
    (if (even? x)
        (/ (+ x 2) (+ x 1))
        (/ (+ x 1) (+ x 2))))
  (* 4 (product t 1 inc n)))
;1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (term a)
       (accumulate combiner null-value term (next a) next b))))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))
(define (sum-acc term a next b)
  (accumulate + 0 term a next b))
(define (id x) x)
(define (fac-acc n)
  (product-acc id 1 inc n))
(define (acc-iter combiner n-v term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (combiner res (term a)))))
  (iter a n-v))
(define (fac-acc-iter n)
  (pro-acc-iter id 1 inc n))
(define (pro-acc-iter term a next b)
  (acc-iter * 1 term a next b))

;1.33
(define (filtered-accumulate comb n-v term a next b p)
  (if (> a b)
      n-v
      (if (p (term a))
          (comb (term a) (filtered-accumulate comb n-v term (next a) next b p))
          (filtered-accumulate comb n-v term (next a) next b p))))

(define (sum-prime-range a b)
  (filtered-accumulate + 0 id a inc b prime?))

(define (product-rel-prime n)
  (define (rel-prime a)
    (= (gcd a n) 1))
  (filtered-accumulate * 1 id 1 inc n rel-prime))
;1.34
(define (f g) (g 2))

;1.35
(define tolerance 0.0000000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
;1.36
(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
;1.37
(define (cont-frac n d k)
  (define (cont-frac-rec a)
    (if (> a k)
        0
        (/ (n a) (+ (d a) (cont-frac-rec (+ a 1))))))
  (cont-frac-rec 1))

(define (cont-frac-iter n d k)
  (define (cont-frac-i a result)
    (if (<= a 1)
        (/ (n a) (+ (d a) result))
        (cont-frac-i (- a 1) (/ (n a) (+ (d a) result)))))
  (cont-frac-i (- k 1) (/ (n k) (d k))))

;1.38
(define (e k)
  (define (di x)
    (cond ((= x 2) 2)
          ((= (remainder (- x 2) 3) 0) (- x (/ (- x 2) 3)))
          (else 1)))
  (+ 2 (cont-frac (lambda (y) 1.0) di k)))

;1.39
(define (tan-cf x k)
  (cont-frac
   (lambda (y) (if (> y 1)
                   (- (* x x))
                   x))
   (lambda (y) (+ (* (- y 1) 2) 1.0))
   k))
;Newton's method
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.0000000001)
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (sqrt-newton x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
;1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
;1.41
(define (double-f f) (lambda (x) (f (f x))))
;1.42
(define (compose f g) (lambda (x) (f (g x))))
;1.43
(define (repeated f n)
  (lambda (x) (if (= n 1)
                  (f x)
                  ((compose f (repeated f (- n 1))) x))))
;1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))
(define (n-smooth f n)
  (repeated f n))
;1.45
(define (average x y)
  (/ (+ x y) 2.0))
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (n-root n x)
  (fixed-point
   (repeated (average-damp (lambda(z) (/ x (expt z (- n 1)))))
             (* n 2)) 1.0))
;1.46
(define (iterative-improve ge? improve)
  (lambda (x) (if (ge? x)
                  x
                  ((iterative-improve ge? improve)(improve x)))))
(define (sqrt-iter-improve x)
  ((iterative-improve (lambda (y)
                       (< (abs (- (* y y) x )) 0.00001))
                     (average-damp (lambda (z) (/ x z))))
  1.0))
