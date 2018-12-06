;;;SECTION 2.2.2
;: (cons (list 1 2) (list 3 4))
;: 
;: (define x (cons (list 1 2) (list 3 4)))
;: (length x)
;: (count-leaves x)
;: 
;: (list x x)
;: (length (list x x))
;: (count-leaves (list x x))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; EXERCISE 2.24
;: (list 1 (list 2 (list 3 4)))

;; EXERCISE 2.25
;: (1 3 (5 7) 9)
;: ((7))
;: (1 (2 (3 (4 (5 (6 7))))))

;; EXERCISE 2.26
;: (define x (list 1 2 3))
;: (define y (list 4 5 6))
;: 
;: (append x y)
;: (cons x y)
;: (list x y)

;; EXERCISE 2.27
;; 深度反序
(define (my-reverse x)
  (define (iter s rs)
    (if (null? s)
        rs
        (iter (cdr s) (cons (car s) rs))))
  (iter x '()))
;; 二叉树叶的深度反序
(define (deep-reverse-bst x)
  (cond ((null? x) '())
        ((not (pair? x)) x)
        (else (list (deep-reverse (cadr x))
                    (deep-reverse (car x))))))
;; 多叉树的深度反序
(define (deep-reverse x)
  (define (iter s rs)
    (cond ((null? s) rs)
          ((not (pair? (car s))) (iter (cdr s) (cons (car s) rs)))
          (else (iter (cdr s) (cons (deep-reverse (car s)) rs)))))
  (iter x '()))

;; EXERCISE 2.28
;; 二叉树叶节点的表生成
(define (fringe x)
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cadr x))))))

;; EXERCISE 2.29
;; 二叉活动体
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; 二叉活动体的分支选择过程，分支成分过程
(define (left-branch x) (car x))
(define (right-branch x) (cadr x))
(define (branch-length x) (car x))
(define (branch-structure x) (cadr x))

;; 二叉活动体的总重量过程
(define (total-weight x)
  (define (branch-weight b)
    (if (pair? (branch-structure b))
        (total-weight (branch-structure b))
        (branch-structure b)))
  (cond ((null? x) 0)
        ((not (pair? x)) x)
        (else (+ (branch-weight (left-branch x))
              (branch-weight (right-branch x))))))

;; 二叉活动体平衡判断过程
(define (balance? x)
  (define (branch-moment b)
    (* (branch-length b)
       (total-weight (branch-structure b))))
  (cond ((null? x) error "empty binary mobile")
        ((not (pair? x)) #t)
        (else (and (= (branch-moment (left-branch x))
                      (branch-moment (right-branch x)))
                   (balance? (branch-structure (left-branch x)))
                   (balance? (branch-structure (right-branch x)))))))

;; 对二叉树活动体的实现变化后，做出的修正
;(define (make-mobile left right) (cons left right))
;(define (make-branch length structure) (cons length structure))
;(define (left-branch x) (car x))
;(define (right-branch x) (cdr x))
;(define (branch-length x) (car x))
;(define (branch-structure x) (cdr x))

;; Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))


;: (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
;:             10)


(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))


;; EXERCISE 2.30
;: (square-tree
;:  (list 1
;:        (list 2 (list 3 4) 5)
;:        (list 6 7)))


;; EXERCISE 2.31
(define (square-tree tree) (tree-map square tree))


;; EXERCISE 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map ??FILL-THIS-IN?? rest)))))

