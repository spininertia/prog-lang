
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; q1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; q2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; q3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; q4
(define (stream-for-n-steps s n)
  (let ([hd (s)])
  (if (= 0 n)
      null
      (cons (car hd) (stream-for-n-steps (cdr hd) (- n 1))))))

; q5
(define funny-number-stream
  (letrec ([f (lambda (x) 
               (if (= (remainder x 5) 0)
                (cons (- x) (lambda () (f (+ x 1))))
                (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

; q6
(define dan-then-dog
  (letrec ([f (lambda (state) 
                (if (= state 0)
                    (cons "dan.jpg" (lambda() (f 1)))
                    (cons "dog.jpg" (lambda() (f 0)))))])
    (lambda() (f 0))))

; q7
(define (stream-add-zero s)
  (let ([hd (s)])
    (lambda () (cons (cons 0 (car hd)) (stream-add-zero (cdr hd))))))

; q8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda() (f (+ n 1)))))])
    (lambda() (f 0))))
   
; q9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n) 
              (cond [(>= n (vector-length vec)) #f]
                    [(and (pair? (vector-ref vec n)) (equal? (car (vector-ref vec n)) v)) (vector-ref vec n)]
                    [else (f (+ 1 n))]))])
    (f 0)))

; q10
(define (cached-assoc xs n)
  (let ([next-pos 0]
        [cache (make-vector n 0)])
    (lambda (v) 
      (cond [(let ([cell (vector-assoc v cache)])
               cell
               cell)]
            [(let ([cell (assoc v xs)])
               cell 
               (vector-set! cache next-pos cell) 
               (set! next-pos (remainder (+ next-pos 1) n))
               cell)]
            [else #f]))))
    