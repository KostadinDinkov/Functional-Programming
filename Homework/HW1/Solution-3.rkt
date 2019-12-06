(define (make-date day month year)
  (list day month year))

(define (day date)
  (car date))

(define (month date)
  (cadr date))

(define (year date)
  (caddr date))

(define (leap? year)
  (or (and (= (remainder year 4) 0)
           (not (= (remainder year 100) 0)))
      (= (remainder year 400) 0)))

(define (long-month? month)
  (or (= month 1)
      (= month 3)
      (= month 5)
      (= month 7)
      (= month 8)
      (= month 10)
      (= month 12)))

(define (date? date)
  (if (or (< (month date) 1) (> (month date) 12))
      #f
      (cond ((= (month date) 2) (if (leap? (year date))
                                    (and (< (day date) 30)
                                         (> (day date) 0))
                                    (and (< (day date) 29)
                                         (> (day date) 0))))
            ((long-month? (month date)) (if (and (< (day date) 32)
                                                 (> (day date) 0))
                                            #t
                                            #f))
            (else (if (and (< (day date) 31)
                           (> (day date) 0))
                      #t
                      #f)))))

(define (date->string date)
  (string-append (number->string (car date))
                 "."
                 (number->string (cadr date))
                 "."
                 (number->string (caddr date))))

(define (next-day date)
  (if (date? (cons (+ (day date) 1) (cdr date)))
      (cons (+ (day date) 1) (cdr date))
      (if (date? (list 1 (+ (month date) 1) (year date)))
          (list 1 (+ (month date) 1) (year date))
          (list 1 1 (+ (year date) 1)))))