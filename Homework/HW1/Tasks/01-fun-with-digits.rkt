#lang racket

; Искаме да дефинираме следните имена: one, two, three, ..., nine, plus, minus, times, div,
; така че извиквания от типа на (one (plus (three))) (операция с точно две операнди) да връщат легитимни числови стойности (в този случай - 4)
; Още малко примери:
; (three (times (five))) -> 15
; (nine (div (three))) -> 3
; (eight (minus (four))) -> 4
;

