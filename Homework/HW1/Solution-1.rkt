#lang racket/base

(define one
  (lambda f
    (if (null? f)
        1
        ((car f) 1))))

(define two
  (lambda f
    (if (null? f)
        2
        ((car f) 2))))

(define three
  (lambda f
    (if (null? f)
        3
        ((car f) 3))))

(define four
  (lambda f
    (if (null? f)
        4
        ((car f) 4))))

(define five
  (lambda f
    (if (null? f)
        5
        ((car f) 5))))

(define six
  (lambda f
    (if (null? f)
        6
        ((car f) 6))))

(define seven
  (lambda f
    (if (null? f)
        7
        ((car f) 7))))

(define eight
  (lambda f
    (if (null? f)
        8
        ((car f) 8))))

(define nine
  (lambda f
    (if (null? f)
        9
        ((car f) 9))))

(define (plus digit)
  (lambda (ret) (+ ret digit)))

(define (minus digit)
  (lambda (ret) (- ret digit)))

(define (times digit)
  (lambda (ret) (* ret digit)))

(define (div digit)
  (lambda (ret) (/ ret digit)))


;unit-testing
(require rackunit rackunit/text-ui)

(define sol1-tests
  (test-suite "Sol1 tests"
    (test-case "should return 1+x for addition with 1" (check-eq? (one (plus (one))) 2))
    (test-case "should return 2+x for addition with 2" (check-eq? (two (plus (two))) 4))
    (test-case "should return 4+x for addition with 4" (check-eq? (three (plus (one))) 4))
    (test-case "should return 5+x for addition with 5" (check-eq? (four (plus (five))) 9))
    (test-case "should return 9+x for addition with 9" (check-eq? (seven (plus (nine))) 16))
    (test-case "should return 6+x for addition with 6" (check-eq? (nine (plus (six))) 15))

    (test-case "should return x-1 for substraction with 1" (check-eq? (one (minus (one))) 0))
    (test-case "should return x-2 for substraction with 2" (check-eq? (two (minus (two))) 0))
    (test-case "should return x-4 for substraction with 4" (check-eq? (three (minus (one))) 2))
    (test-case "should return x-5 for substraction with 5" (check-eq? (four (minus (five))) -1))
    (test-case "should return x-9 for substraction with 9" (check-eq? (seven (minus (nine))) -2))
    (test-case "should return x-6 for substraction with 6" (check-eq? (nine (minus (six))) 3))
    
    (test-case "should return x for multiplication with 1" (check-eq? (one (times (one))) 1))
    (test-case "should return 2x for multiplication with 2" (check-eq? (two (times (two))) 4))
    (test-case "should return x for multiplication with 1" (check-eq? (three (times (one))) 3))
    (test-case "should return 5x for multiplication with 5" (check-eq? (four (times (five))) 20))
    (test-case "should return 9x for multiplication with 9" (check-eq? (seven (times (nine))) 63))
    (test-case "should return 6x for multiplication with 6" (check-eq? (nine (times (six))) 54))

    (test-case "should return x for division with 1" (check-eqv? (one (div (one))) 1))
    (test-case "should return x/2 for division with 2" (check-eqv? (two (div (two))) 1))
    (test-case "should return x for division with 1" (check-eqv? (three (div (one))) 3))
    (test-case "should return x/5 for division with 5" (check-eqv? (four (div (five))) 4/5))
    (test-case "should return x/9 for division with 9" (check-eqv? (seven (div (nine))) 7/9))
    (test-case "should return x/6 for division with 6" (check-eqv? (nine (div (six))) 9/6))
  )
)

(run-tests sol1-tests 'verbose)