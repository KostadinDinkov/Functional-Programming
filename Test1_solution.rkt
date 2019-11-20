(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (filter p l)
  (if (eqv? l '()) '()
      (if (p (car l))
          (cons (car l) (filters p (cdr l)))
          (filters p (cdr l)))))

(define (foldr operation null-value l)
  (if (null? l)
      null-value
      (operation (car l)
                 (foldr operation null-value (cdr l)))))

(define (foldl operation null-value l)
  (if (null? l)
      null-value
      (foldl operation
             (operation null-value (car l))
             (cdr l))))




(define (product-digits n)
  (if (< n 10) n
      (* (remainder n 10) (product-digits (quotient n 10)))))

(define (for g gg)
      (if (> g gg) 0
          (max (- (- g (product-digits g)) (- gg (product-digits gg))) (- (- gg (product-digits gg)) (- g (product-digits g))) (for (+ g 1) gg))))

(define (largest-diff a b)
  (if (> a b) 0
      (max (for a b) (largest-diff a (- b 1)))))

(define (sum-of-l l)
  (foldl + 0 l))

(define (metr m l1)
  (if (null? l1) '()
      (cons (m (car l1)) (metr m (cdr l1)))))

(define (help ml ll)
    (if (null? ml) '()
          (cons (cons (sum-of-l (metr (car ml) ll)) (car ml)) (help (cdr ml) ll))))

(define (helpp ml ll)
    (if (null? ml) '(0 ())
          (cons (max (sum-of-l (metr (car ml) ll)) (car (helpp (cdr ml) ll))) '())))

(define (max-metric ml ll)
  (define mylist (help ml ll))
  (define maxfound (helpp ml ll))
  (define (for mylist)
    (if (null? mylist) '()
      (if (= (car maxfound) (car (car mylist))) (cdr (car mylist))
          (for (cdr mylist)))))
  (for (help ml ll)))

(define (list-repeat s k)
  (if (= k 0) '()
      (cons s (list-repeat s (- k 1)))))

(define (deep-repeat l)
  (define (level i ll)
   (if (null? ll) '()
       (if (number? (car ll)) (append (car (cons (list-repeat (car ll) i) (list-repeat (cdr ll) i))) (level i (cdr ll)))
           (cons (level (+ i 1) (car ll)) (level i (cdr ll))))))
  (level 1 l))
        
  