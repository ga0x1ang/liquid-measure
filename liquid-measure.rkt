#lang racket

; USAGE: (solve small_vol big_vol target_vol) 

(define count 0)
(define big_status 0)
(define small_status 0)
(define steps (stream "make-etat(0 0)")) ; this output is nothing special

(define (log_step num1 num2)
  (set! steps (stream-append steps (stream (format "make-etat(~a ~a)" num1 num2)))))

(define (solve small big final)
  (define (fill) ; fill the big cup (when it is empty)
    (set! big_status big)
    (log_step big_status small_status))
  
  (define (pour) ; pour from big cup to small cup
    (cond
      [(> big_status (- small small_status)) (set! big_status (- big_status (- small small_status)))
                                             (set! small_status small)]
      [else (set! small_status big_status)
            (set! big_status 0)])
    (log_step big_status small_status))
  
  (define (dump) ; dump the small cup (when it is full)
    (set! small_status 0)
    (log_step big_status small_status))
  
  ; main procedure
  (cond
    [(equal? big_status 0) (fill)]
    [else (cond
            [(equal? small_status small) (dump)]
            [else (pour)])])
  (set! count (+ count 1))
  (cond
    ;[(equal? (compute small big count) final)]
    [(or (equal? big_status final) (equal? small_status final))]
    [else (solve small big final)])
  (stream->list steps))