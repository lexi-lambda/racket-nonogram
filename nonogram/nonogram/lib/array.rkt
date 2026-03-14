#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/transformer)
         racket/contract
         racket/match
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
         array/c
         array
         make-array
         array-length
         array-ref
         array-set

         array-map
         in-array
         for/array
         for*/array

         vector->array
         unsafe-vector*->array!
         array->vector
         list->array
         array->list)

;; -----------------------------------------------------------------------------

(define (arrayof ctc)
  (vectorof ctc #:immutable #t))

(define (array/c . ctcs)
  (apply vector/c ctcs #:immutable #t))

(define-match-expander array
  (syntax-parser
    [(_ pat ...)
     #`(? array? #,(syntax/loc this-syntax
                     (vector pat ...)))])
  (make-variable-like-transformer #'vector-immutable))

(define (make-array size v)
  (unsafe-vector*->array! (make-vector size v)))

(define (array-set arr i v)
  (unsafe-vector*->array! (vector-set/copy arr i v)))

(define (list->array lst)
  (apply vector-immutable lst))

(define (array-map proc arr)
  (unsafe-vector*->array! (vector-map proc arr)))

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
