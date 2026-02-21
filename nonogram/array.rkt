#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/math
         racket/vector

         (only-in racket/base
                  [vector-length array-length]
                  [vector-ref array-ref]
                  [vector->immutable-vector vector->array]
                  [vector->list array->list]
                  [in-vector in-array])
         (only-in racket/mutability [immutable-vector? array?])
         (only-in racket/unsafe/ops [unsafe-vector*->immutable-vector! unsafe-vector*->array!])
         (only-in racket/vector
                  [vector-copy array->vector]))

(provide array?
         arrayof
         make-array
         array-length
         array-ref
         array-set

         in-array
         for/array
         for*/array

         vector->array
         array->vector
         list->array
         array->list)

;; -----------------------------------------------------------------------------

(define (arrayof ctc)
  (vectorof ctc #:immutable #t))

(define (make-array size v)
  (unsafe-vector*->array! (make-vector size v)))

(define (array-set arr i v)
  (unsafe-vector*->array! (vector-set/copy arr i v)))

(define (list->array lst)
  (apply vector-immutable lst))

;; -----------------------------------------------------------------------------

(define-syntaxes [for/array for*/array]
  (let ()
    (define (make for-id)
      (syntax-parser
        [(_ . more)
         #`(unsafe-vector*->array!
            #,(quasisyntax/loc this-syntax
                (#,for-id . more)))]))

    (values (make #'for/vector) (make #'for*/vector))))
