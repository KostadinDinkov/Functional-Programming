#lang racket

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

(define (date< date1 date2)
  (cond ((< (year date1) (year date2)) #t)
        ((> (year date1) (year date2)) #f)
        (else (cond ((< (month date1) (month date2)) #t)
                    ((> (month date1) (month date2)) #f)
                    (else (if (< (day date1) (day date2))
                              #t
                              #f))))))

;(define (weekday date)
 ; (define jdn (- (+ (* 367 (year date))
  ;               (/ (* 275 (month date)) 9)
   ;              (day date)
    ;             1729777)
     ;         (* (/ 7 4) (+ (year date)
      ;                      5001
       ;                     (/ (- (month date) 9) 7)))))
;  jdn)

;(define (weekday date)
 ; (define jdn (- (+ (day date)
  ;                  (* (/ 367 12) (- (month date) 2 (* 12 (/ (- (month date) 14) 12))))
   ;                 (* (/ 1461 4) (+ (year date) 4800 (/ (- (month date) 14) 12))))
    ;             (* (/ 3 4) (/ (+ (year date) 4900 (/ (- (month date) 14) 12)) 100))
     ;            32075))
;  jdn)

(define (weekday date)
  (define a (quotient (year date) 100))
  (define b (quotient a 4))
  (define c (- (+ 2 b) a))
  (define e (quotient (numerator (* 365.25 (+ (year date) 4716))) (denominator (* 365.25 (+ (year date) 4716)))))
  (define f (quotient (numerator (* 30.6001 (+ (month date) 1))) (denominator (* 30.6001 (+ (month date) 1)))))
  (define jdn (- (+ c (day date) e f) 1524))
  (define day-of-week (remainder (numerator jdn) 7))
  (cond ((= day-of-week 0) (string->symbol "Monday"))
        ((= day-of-week 1) (string->symbol "Tuesday"))
        ((= day-of-week 2) (string->symbol "Wednesday"))
        ((= day-of-week 3) (string->symbol "Thursday"))
        ((= day-of-week 4) (string->symbol "Friday"))
        ((= day-of-week 5) (string->symbol "Saturday"))
        ((= day-of-week 6) (string->symbol "Sunday"))))

(define (next-weekday wday date)
  (if (eq? (weekday (next-day date)) wday)
      (next-day date)
      (next-weekday wday (next-day date))))

(define (events-for-day date cal)
  (filter (lambda cal-date (equal? (caar cal-date) date)) cal))

