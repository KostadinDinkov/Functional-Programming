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
  (lambda (ret) (quotient ret digit)))