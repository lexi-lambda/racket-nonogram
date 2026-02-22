#lang racket/base

(require pict
         toolbox/format
         toolbox/logging)

(provide (logger-out nonogram)
         define-nonogram-logger
         make-nonogram-log-receiver

         timing-start
         time-pict)

;; -----------------------------------------------------------------------------

(define-root-logger nonogram #:parent #f)
(define-nonogram-logger nonogram:timing)

(define (make-nonogram-log-receiver #:level [level 'info]
                                    #:timing? [timing? #f])
  (make-log-receiver nonogram-logger level #f
                     (if timing? 'debug 'none) 'nonogram:timing))

;; -----------------------------------------------------------------------------

(define (timing-start name)
  (define start-time (current-inexact-monotonic-milliseconds))
  (λ ()
    (define end-time (current-inexact-monotonic-milliseconds))
    (define elapsed-millis (- end-time start-time))
    (log-nonogram:timing-debug "[~a] finished in ~a ms" name (~r* elapsed-millis #:precision '(= 1)))))

(define (time-pict p name)
  (define draw (make-pict-drawer p))
  (struct-copy
   pict (unsafe-dc
         (λ (dc x y)
           (define timing-end (timing-start name))
           (draw dc x y)
           (timing-end))
         (pict-width p)
         (pict-height p)
         (pict-ascent p)
         (pict-descent p))
   [children (list (child p 0 0 1 1 0 0))]))
