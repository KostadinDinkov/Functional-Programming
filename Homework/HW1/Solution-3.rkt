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
  ;jdn)

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

(define (calendar-sort cal)
  (if (null? cal) '()
      (if (null? (filter (lambda el (date< (caar el) (caar cal))) cal))
          (cons (list (caar cal) (cdar cal)) (calendar-sort (cdr cal)))
          (calendar-sort (append (filter (lambda el (date< (caar el) (caar cal))) cal) (filter (lambda el (not (date< (caar el) (caar cal)))) cal))))))

(define (uniq alist)
  (if (null? alist) '()
      (if (assoc (caar alist) (cdr alist))
          (cons (append (car alist)
                        (cdr (assoc (caar alist) (cdr alist))))
                (uniq (cddr alist)))
          (cons (car alist) (uniq (cdr alist))))))

(define (calendar cal)
  (define sorted (calendar-sort cal))
  (uniq sorted))

;unit-testing
(require rackunit rackunit/text-ui)

(define sol3-tests
  (test-suite "Date tests"
    (test-case "should return 21 as day of 21.11.2019" (check-eq? (day (make-date 21 11 2019)) 21))
    (test-case "should return 30 as day of 30.08.2019" (check-eq? (day (make-date 30 8 2019)) 30))
    (test-case "should return 11 as month of 21.11.2019" (check-eq? (month (make-date 21 11 2019)) 11))
    (test-case "should return 8 as month of 21.11.2019" (check-eq? (month (make-date 30 8 2019)) 8))
    (test-case "should return 2019 as year of 21.11.2019" (check-eq? (year (make-date 21 11 2019)) 2019))
    (test-case "should return 2019 as year of 21.11.2019" (check-eq? (year (make-date 30 8 2019)) 2019))

    (test-case "should return true for date 21.11.2019" (check-eq? (date? (make-date 21 11 2019)) #t))
    (test-case "should return true for date 21.11.-2019" (check-eq? (date? (make-date 21 11 -2019)) #t))
    (test-case "should return false for date 51.11.2019" (check-eq? (date? (make-date 51 11 2019)) #f))
    (test-case "should return false for date 21.13.2019" (check-eq? (date? (make-date 21 13 2019)) #f))
    (test-case "should return true for date 28.2.2000" (check-eq? (date? (make-date 29 2 2000)) #t))
    (test-case "should return false for date 28.2.2100" (check-eq? (date? (make-date 29 2 2100)) #f))
    
    (test-case "should return \"21.11.2019\" for date 21.11.2019" (check-equal? (date->string (make-date 21 11 2019)) "21.11.2019"))
    (test-case "should return \"1.2.1239\" for date 1.2.-1239" (check-equal? (date->string (make-date 1 2 -1239)) "1.2.-1239"))
    (test-case "should return \"6.12.2019\" for date 6.12.2019" (check-equal? (date->string (make-date 6 12 2019)) "6.12.2019"))
    (test-case "should return \"3.3.2000\" for date 3.3.2000" (check-equal? (date->string (make-date 3 3 2000)) "3.3.2000"))
    (test-case "should return \"1.1.681\" for date 1.1.681" (check-equal? (date->string (make-date 1 1 681)) "1.1.681"))

    (test-case "should return \"22.11.2019\" for date 21.11.2019" (check-equal? (date->string (next-day (make-date 21 11 2019))) "22.11.2019"))
    (test-case "should return \"2.2.1239\" for date 1.2.-1239" (check-equal? (date->string (next-day (make-date 1 2 -1239))) "2.2.-1239"))
    (test-case "should return \"7.12.2019\" for date 6.12.2019" (check-equal? (date->string (next-day (make-date 6 12 2019))) "7.12.2019"))
    (test-case "should return \"1.1.2000\" for date 31.12.1999" (check-equal? (date->string (next-day (make-date 31 12 1999))) "1.1.2000"))
    (test-case "should return \"29.2.2000\" for date 28.2.2000" (check-equal? (date->string (next-day (make-date 28 2 2000))) "29.2.2000"))
    (test-case "should return \"29.2.2004\" for date 28.2.2004" (check-equal? (date->string (next-day (make-date 28 2 2004))) "29.2.2004"))
    (test-case "should return \"1.3.2100\" for date 28.2.2100" (check-equal? (date->string (next-day (make-date 28 2 2100))) "1.3.2100"))
    (test-case "should return \"1.3.2019\" for date 28.2.2019" (check-equal? (date->string (next-day (make-date 28 2 2019))) "1.3.2019"))
    
    (test-case "should return true if the first date is before the second date, false else" (check-eq? (date< (make-date 21 11 2019) (make-date 1 1 2020)) #t))
    (test-case "should return true if the first date is before the second date, false else" (check-eq? (date< (make-date 3 5 681) (make-date 1 1 2010)) #t))
    (test-case "should return true if the first date is before the second date, false else" (check-eq? (date< (make-date 30 3 2014) (make-date 31 1 2014)) #f))
    (test-case "should return true if the first date is before the second date, false else" (check-eq? (date< (make-date 21 11 2019) (make-date 6 12 2020)) #t))

    (test-case "should return 'Thursday for date 21.11.2019" (check-eq? (weekday (make-date 21 11 2019)) 'Thursday))
    (test-case "should return 'Friday for date 22.11.2019" (check-eq? (weekday (make-date 22 11 2019)) 'Friday))
    (test-case "should return 'Monday for date 9.12.2019" (check-eq? (weekday (make-date 9 12 2019)) 'Monday))
    (test-case "should return 'Wednesday for date 1.1.2020" (check-eq? (weekday (make-date 31 12 2009)) 'Thursday))

    (test-case "should return \"28.11.2019\" for the first Thursday after 21.11.2019" (check-equal? (date->string (next-weekday 'Thursday (make-date 21 11 2019))) "28.11.2019"))
    (test-case "should return \"26.11.2019\" for the first Tuesday after 21.11.2019" (check-equal? (date->string (next-weekday 'Tuesday (make-date 21 11 2019))) "26.11.2019"))
    (test-case "should return \"9.5.2008\" for the first Friday after 6.5.2008" (check-equal? (date->string (next-weekday 'Friday (make-date 6 5 2008))) "9.5.2008"))
    (test-case "should return \"10.5.2008\" for the first Saturday after 8.5.2008" (check-equal? (date->string (next-weekday 'Saturday (make-date 8 5 2008))) "10.5.2008"))

    (test-case "should return all the event for a date" (check-equal? (events-for-day (make-date 27 11 2019)
                                                                                      (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                                                                                            (cons (make-date 27 11 2019) "Спират водата в Младост")
                                                                                            (cons (make-date 28 11 2019) "Спират водата в Лозенец")))

                                                                      '(((27 11 2019) . "Първа лекция за Хаскел")
                                                                        ((27 11 2019) . "Спират водата в Младост"))))
    (test-case "should return all the event for a date" (check-equal? (events-for-day (make-date 28 11 2019)
                                                                                      (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                                                                                            (cons (make-date 27 11 2019) "Спират водата в Младост")
                                                                                            (cons (make-date 28 11 2019) "Спират водата в Лозенец")))

                                                                      '(((28 11 2019) . "Спират водата в Лозенец"))))

    (test-case "should return all the event for a date" (check-equal? (calendar (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                                                                                      (cons (make-date 25 12 2019) "Коледа")
                                                                                      (cons (make-date 27 11 2019) "Спират водата в Младост")
                                                                                      (cons (make-date 23 3 2018) "Концерт на Лепа Брена")))

                                                                      '(((23 3 2018) "Концерт на Лепа Брена")
                                                                        ((27 11 2019) "Първа лекция за Хаскел" "Спират водата в Младост")
                                                                        ((25 12 2019) "Коледа"))))
  )
)

(run-tests sol3-tests 'verbose)