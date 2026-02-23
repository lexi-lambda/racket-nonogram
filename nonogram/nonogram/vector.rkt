#lang racket/base

(require racket/contract
         racket/vector)

(provide (all-from-out racket/vector)
         (contract-out
          [vector-fill! (->* [vector?
                              any/c]
                             [exact-nonnegative-integer?
                              exact-nonnegative-integer?]
                             void?)]))

;; -----------------------------------------------------------------------------

(define (vector-fill! vec value [start-i 0] [end-i (vector-length vec)])
  (for ([i (in-range start-i end-i)])
    (vector-set! vec i value)))
