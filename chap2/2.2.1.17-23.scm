;;;SECTION 2.2.1

;: (cons 1
;:       (cons 2
;:             (cons 3
;:                   (cons 4 nil))))


;: (define one-through-four (list 1 2 3 4))
;: 
;: one-through-four
;: (car one-through-four)
;: (cdr one-through-four)
;: (car (cdr one-through-four))
;: (cons 10 one-through-four)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;: (define squares (list 1 4 9 16 25))

;: (list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;: (define odds (list 1 3 5 7))

;: (length odds)

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

;: (append squares odds)
;: (append odds squares)


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


;; EXERCISE 2.17
;: (last-pair (list 23 72 149 34))
(define (last-pair x)
  (cond ((null? x) (error "empty list has no last one."))
        ((null? (cdr x)) (car x))
        (else (last-pair (cdr x)))))

;; EXERCISE 2.18
;: (reverse (list 1 4 9 16 25))
(define (my-reverse x)
  (define (iter s rs)
    (if (null? s)
        rs
        (iter (cdr s) (cons (car s) rs))))
  (iter x '()))

      

;; 19.20就不做了，19之前就没怎么看，20看起来没有什么做的意义
;; EXERCISE 2.19
(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;: (cc 100 us-coins)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;; EXERCISE 2.20
;: (same-parity 1 2 3 4 5 6 7)
;: (same-parity 2 3 4 5 6 7)

;; Mapping over lists

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

;: (scale-list (list 1 2 3 4 5) 10)

;: (map + (list 1 2 3) (list 40 50 60) (list 700 800 900))

;: (map (lambda (x y) (+ x (* 2 y)))
;:      (list 1 2 3)
;:      (list 4 5 6))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;: (map abs (list -10 2.5 -11.6 17))

;: (map (lambda (x) (* x x))
;:      (list 1 2 3 4))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))


;; EXERCISE 2.21
;: (square-list (list 1 2 3 4))

;; EXERCISE 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))


;; EXERCISE 2.23

;: (for-each (lambda (x) (newline) (display x))
;:           (list 57 321 88))
(define (foreach f x)
  (if (null? (x))
      #t
      ((f (car x))
       (foreach f (cdr x)))))


