;;;SECTION 2.1.4

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
  (mul-interval x
                (if (and (> (upper-bound y) 0)
                         (< (lower-bound y) 0))
                    (raise "interval crosses zero")
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; EXERCISE 2.7
;; complete the implemention
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;; EXERCISE 2.8
;; 实现区间的差
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; EXERCISE 2.10
;; 处理除以横跨0的区间


;;;SECTION 2.1.4 again

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; EXERCISE 2.12
;; 处理百分数的误差
(define (make-center-percent c w)
  (let ((p (* (/ w 100.0) c)))
    (make-interval (- c p) (+ c p))))

(define (percent x)
  (let ((c (/ (+ (upper-bound x)
                 (lower-bound x)) 2.0))
        (p (/ (- (upper-bound x)
                 (lower-bound x)) 2.0)))
    (* (/ p c) 100)))

;; parallel resistors

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
