#lang racket/base

(require (for-syntax racket/base
                     racket/provide-transform
                     syntax/parse)
         racket/contract)

(provide maybe-contract-out)

;; -----------------------------------------------------------------------------

(begin-for-syntax
  (define use-contracts? #f)

  (define-syntax-class contract-out-clause
    #:description #f
    #:attributes [no-ctc {ctc-e 1}]
    #:literals [struct]
    (pattern (struct ~! struct-id:id ([_:id ctc-e:expr] ...))
      #:attr no-ctc (syntax/loc this-syntax
                      (struct-out struct-id)))
    (pattern [id ctc:expr]
      #:attr no-ctc #'id
      #:attr {ctc-e 1} (list #'ctc))))

(define-syntax maybe-contract-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-parse stx
       #:track-literals
       [(_ clause:contract-out-clause ...)
        (pre-expand-export
         (cond
           [use-contracts?
            (syntax/loc this-syntax
              (contract-out clause ...))]
           [else
            ; Lift all the contracts so they’re still expanded and evaluated,
            ; both for Check Syntax and to make sure they evaluate without errors.
            (syntax-local-lift-module-end-declaration
             #'(define-values [] (begin clause.ctc-e ... ... (values))))
            (syntax/loc this-syntax
              (combine-out clause.no-ctc ...))])
         modes)]))))
