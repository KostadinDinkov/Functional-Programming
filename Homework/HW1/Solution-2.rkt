(define (progress toPrint remaining)
  (if (null? remaining)
      (list toPrint)
      (cons toPrint
            (progress (append toPrint
                         (list (car remaining)))
                 (cdr remaining)))))

(define (prefixes xs)
  (progress '() xs)
)