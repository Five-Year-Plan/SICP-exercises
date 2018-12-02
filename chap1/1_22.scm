;;EXERCISE 1.22

;;
;;可以分成下面的子问题
(define (next-odd n)
  (if (odd? n)
      (+ n 2)
      (+ n 1)))

(define (search-primes n count)
  (cond ((= count 0)
         (display "are primes"))
        ((prime? n)
         (display n)
         (newline)
         (search-primes (next-odd n) (- count 1)))
        (else
         (search-primes (next-odd n) count))))

(define (search-for-primes n count)
  (let ((start-time (real-time-clock)))
    (search-primes n count)
    (- (real-time-clock) start-time)))

