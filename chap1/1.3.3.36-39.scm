;;;SECTION 1.3.3

;; Half-interval method

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))


;: (half-interval-method sin 2.0 4.0)

;: (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
;:                       1.0
;:                       2.0)


;; Fixed points

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;: (fixed-point cos 1.0)

;: (fixed-point (lambda (y) (+ (sin y) (cos y)))
;:              1.0)


(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;;EXERCISE 1.36
;;计算x^x=1000的一个根
(define (my-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;;EXERCISE 1.37
;;计算无穷连分式的递归函数
(define (recursive-cont-frac n d k)
  (define (cf i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i) (cf (+ i 1))))))
  (cf 1))
;;计算无穷连分式的循环函数
(define (loop-cont-frac n d k)
  (define (iter i v)
    (if (= i 0)
        v
        (iter (- i 1) 
              (/ (n i) (+ (d i) v)))))
  (iter k 0))

;;EXERCISE 1.38
;;利用连分式计算e
(define (e-cf k)
  (define n (lambda (i) 1.0))
  (define d (lambda (i)
    (if (= (remainder i 3) 2)
        (+ (/ (- i 2) 3) 1.0)
        1.0)))
  (loop-cont-frac n d k))

;;EXERCISE 1.39
;;利用连分式计算tan(x)
(define (tan-cf x k)
  (define n (lambda (i) 
                    (if (= i 1)
                        x
                        (- (* x x)))))
  (define d (lambda (i) (- (* 2 i) 1)))
  (loop-cont-frac n d k))
