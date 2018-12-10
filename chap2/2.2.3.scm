;;;SECTION 2.2.3

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;; Sequence operations

;: (map square (list 1 2 3 4 5))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;: (filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;: (accumulate + 0 (list 1 2 3 4 5))
;: (accumulate * 1 (list 1 2 3 4 5))
;: (accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;: (enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;: (enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

;: (list-fib-squares 10)


(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

;: (product-of-squares-of-odd-elements (list 1 2 3 4 5))

(define (salary-of-highest-paid-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))

;; EXERCISE 2.33
;; 用积累的角度看待各种操作
(define (square x) (* x x))

;(define (map p sequence)
;  (accumulate (lambda (i j) (cons (p i) j)) '() sequence))
;(define (append seq1 seq2)
;  (accumulate cons seq2 seq1))
;(define (length sequence)
;  (accumulate (lambda (i j) (+ j 1)) 0 sequence))

;; EXERCISE 2.34
;; 用Horner法则计算多项式和
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

;: (horner-eval 2 (list 1 3 0 5 0 1))

;; EXERCISE 2.36
;; 更高阶的积累抽象
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;: (accumulate-n + 0 s)

;; EXERCISE 2.37
;; 矩阵运算
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (i) (dot-product i v))  m))
(define (transpose  mat) ; 不支持向量转置
  (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) 
           (map (lambda (col) 
                  (dot-product row col)) cols)) m)))

;; EXERCISE 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;: (fold-right / 1 (list 1 2 3))
;: (fold-left / 1 (list 1 2 3))
;: (fold-right list nil (list 1 2 3))
;: (fold-left list nil (list 1 2 3))

;; EXERCISE 2.39
;; 实现left-fold和right-fold
(define fold-right accumulate)
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
;用append省事一些
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

;;Nested mappings

;:(accumulate append
;:             nil
;:             (map (lambda (i)
;:                    (map (lambda (j) (list i j))
;:                         (enumerate-interval 1 (- i 1))))
;:                  (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


(define (permutations s)
  (if (null? s)                         ; empty set?
      (list nil)                        ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; EXERCISE 2.40
;; 暴露了太多细节，应该把p83的代码定义为unique-pairs，提高可读性

;; EXERCISE 2.42
;; k皇后问题，wish thinking
(define empty-board '())

(define (safe? k positions)
  ()

(define adjoin-position)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; EXERCISE 2.43
;; Louis's version of queens
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
	 ;; next expression changed
         (flatmap
	  (lambda (new-row)
	    (map (lambda (rest-of-queens)
		   (adjoin-position new-row k rest-of-queens))
		 (queen-cols (- k 1))))
	  (enumerate-interval 1 board-size)))))
  (queen-cols board-size))
