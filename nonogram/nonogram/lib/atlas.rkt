#lang racket/base

(require racket/class
         racket/contract
         racket/draw
         racket/list
         racket/match
         racket/math
         threading
         toolbox/pict
         toolbox/who
         "contract.rkt"
         "geometry.rkt")

(provide (maybe-contract-out
          [atlas? predicate/c]
          [atlas-pict (-> atlas? pict?)]
          [atlas-bitmap (-> atlas? (is-a?/c bitmap%))]
          [current-atlas (parameter/c (or/c atlas? #f))]
          [empty-atlas atlas?]
          [pack-atlas (->* [(listof pict?)]
                           [#:scale exact-positive-integer?]
                           atlas?)]
          [atlas-child-rect (->* [atlas? pict?]
                                 [#:normalize? any/c
                                  #:fail failure-result/c
                                  #:who symbol?]
                                 any)]

          [next-power-of-two (-> natural? natural?)]))

;; -----------------------------------------------------------------------------

(define not-given (gensym 'not-given))

(struct atlas (pict bitmap child-rect-cache) #:transparent)

(define current-atlas (make-parameter #f))

(define empty-atlas
  (atlas (blank) (make-bitmap 1 1) (make-hash)))

(define/who (pack-atlas ps #:scale [atlas-scale 1])
  (cond
    [(empty? ps)
     empty-atlas]
    [else
     (define scaled-ps
       (for/list ([p (in-list ps)])
         (define scaled-p (scale (clip p) atlas-scale))
         (unless (and (integer? (pict-width scaled-p))
                      (integer? (pict-height scaled-p)))
           (raise-arguments-error who "pict has non-integral bounds"
                                  "pict" p
                                  "width" (pict-width scaled-p)
                                  "height" (pict-height scaled-p)))
         (inset scaled-p 1)))

     (define max-size
       (for/fold ([size 0])
                 ([p (in-list scaled-ps)])
         (max size
              (+ (pict-width p) 2)
              (+ (pict-height p) 2))))

     (define initial-size (~> (ceiling max-size)
                              inexact->exact
                              next-power-of-two))

     (define packed-p
       (let size-loop ([size initial-size])
         (let loop ([lines (blank)]
                    [ps scaled-ps])
           (match ps
             ['()
              (lt-superimpose (blank size size) lines)]
             [_
              (let line-loop ([line (blank)]
                              [ps ps])

                (define (finish-line)
                  (define lines* (vl-append lines line))
                  (if (> (pict-height lines*) size)
                      (size-loop (* size 2))
                      (loop lines* ps)))

                (match ps
                  ['() (finish-line)]
                  [(cons p ps)
                   (define line* (ht-append line p))
                   (if (> (pict-width line*) size)
                       (finish-line)
                       (line-loop line* ps))]))]))))

     (define bmp (pict->bitmap packed-p 'smoothed))
     (for ([p (in-list ps)])
         (define r (pict-child-rect packed-p p))

         (define x1 (inexact->exact (rect-x r)))
         (define x2 (inexact->exact (rect-x2 r)))
         (define y1 (inexact->exact (rect-y r)))
         (define y2 (inexact->exact (rect-y2 r)))

         (define row-width (inexact->exact (rect-width r)))
         (define col-height (inexact->exact (+ (rect-height r) 2)))
         (define buffer (make-bytes (* (max row-width col-height) 4)))

         (define (extend-row! y dy)
           (send bmp get-argb-pixels
                 x1
                 y
                 row-width
                 1
                 buffer)
           (send bmp set-argb-pixels
                 x1
                 (+ y dy)
                 row-width
                 1
                 buffer))

         (define (extend-col! x dx)
           (send bmp get-argb-pixels
                 x
                 (sub1 y1)
                 1
                 col-height
                 buffer)
           (send bmp set-argb-pixels
                 (+ x dx)
                 (sub1 y1)
                 1
                 col-height
                 buffer))

         (extend-row! y1 -1)
         (extend-row! (sub1 y2) 1)
         (extend-col! x1 -1)
         (extend-col! (sub1 x2) 1))

     (atlas packed-p bmp (make-hash))]))

(define/who (atlas-child-rect atl child
                              #:normalize? [normalize? #t]
                              #:fail [fail not-given]
                              #:who [who who])
  (define rect-cache (atlas-child-rect-cache atl))
  (define cache-key (cons child normalize?))
  (hash-ref
   rect-cache
   cache-key
   (λ ()
     (define parent (atlas-pict atl))
     (define r (pict-child-rect parent child #:fail #f))
     (cond
       [r
        (define r* (if normalize?
                       (rect-scale-all r
                                       (/ (pict-width parent))
                                       (/ (pict-height parent)))
                       r))
        (hash-set! rect-cache cache-key r*)
        r*]
       [(eq? fail not-given)
        (raise-arguments-error who "pict not found in atlas"
                               "pict" child
                               "atlas" atlas)]
       [(procedure? fail) (fail)]
       [else fail]))))

;; -----------------------------------------------------------------------------

(define (next-power-of-two x)
  (arithmetic-shift 1 (integer-length (sub1 x))))
