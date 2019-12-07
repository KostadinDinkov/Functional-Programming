#lang racket/base

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

;unit-testing
(require rackunit rackunit/text-ui)

(define sol2-tests
  (test-suite "Prefixes tests"
    (test-case "should return only '(()) as a list of the prefixes of '()" (check-equal? (prefixes '()) '(())))
    (test-case "should return '(() (1)) as a list of the prefixes of '(1))" (check-equal? (prefixes '(1))'(() (1))))
    (test-case "should return '(() (1) (1 a)) as a list of the prefixes of '(1 a))" (check-equal? (prefixes '(1 a))'(() (1) (1 a))))
    (test-case "should return '(() (1) (1 2) (1 2 3)) as a list of the prefixes of '(1 2 3))" (check-equal? (prefixes '(1 2 3))'(() (1) (1 2) (1 2 3))))
    (test-case "should return '(() ((1 2)) ((1 2) (3 4))) as a list of the prefixes of '((1 2) (3 4)))" (check-equal? (prefixes '((1 2) (3 4))) '(() ((1 2)) ((1 2) (3 4)))))
    (test-case "should return '(() ((1 a)) ((1 a) (2 b))) as a list of the prefixes of '((1 a) (2 b)))" (check-equal? (prefixes '((1 a) (2 b))) '(() ((1 a)) ((1 a) (2 b)))))
  )
)

(run-tests sol2-tests 'verbose)