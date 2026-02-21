#lang racket/base

(require pict
         racket/contract
         racket/match)

(provide integer-point?
         (contract-out
          (struct point ([x real?] [y real?]))
          [truncate-point (-> point? integer-point?)])

         tf:identity
         tf*
         (contract-out
          (struct transformation ([xx real?]
                                  [yx real?]
                                  [x0 real?]
                                  [xy real?]
                                  [yy real?]
                                  [y0 real?]))
          [tf:translate (-> real? real? transformation?)]
          [tf:scale (->* [real?] [real?] transformation?)]
          [world-to-child (-> pict? pict? transformation?)]))

;; -----------------------------------------------------------------------------

(struct point (x y) #:transparent)

(define (integer-point? v)
  (and (point? v)
       (exact-integer? (point-x v))
       (exact-integer? (point-y v))))

(define (truncate-point p)
  (point (inexact->exact (truncate (point-x p)))
         (inexact->exact (truncate (point-y p)))))

;; -----------------------------------------------------------------------------

(struct transformation (xx yx x0
                        xy yy y0)
  #:transparent)

(define tf:identity
  (transformation 1.0 0.0 0.0
                  0.0 1.0 0.0))

(define (tf:translate dx dy)
  (transformation 1.0 0.0 dx
                  0.0 1.0 dy))

(define (tf:scale sx [sy sx])
  (transformation sx 0.0 0.0
                  0.0 sy 0.0))

(define (tf* . ts0)
  (match-define (cons t ts) (reverse ts0))
  (for/fold ([t2 t])
            ([t1 (in-list ts)])
    (match-define (transformation xx1 yx1 x01 xy1 yy1 y01) t1)
    (match t2
      [(point x2 y2)
       (point (+ (* xx1 x2) (* yx1 y2) x01)
              (+ (* xy1 x2) (* yy1 y2) y01))]
      [(transformation xx2 yx2 x02 xy2 yy2 y02)
       (transformation (+ (* xx1 xx2) (* yx1 xy2))
                       (+ (* xx1 yx2) (* yx1 yy2))
                       (+ (* xx1 x02) (* yx1 y02) x01)
                       (+ (* xy1 xx2) (* yy1 xy2))
                       (+ (* xy1 yx2) (* yy1 yy2))
                       (+ (* xy1 x02) (* yy1 y02) y01))])))

(define (world-to-child world child)
  (define-values [x1 y1] (lt-find world child))
  (define-values [x2 y2] (rb-find world child))
  (define world-width (exact->inexact (- x2 x1)))
  (define world-height (exact->inexact (- y2 y1)))
  (tf* (tf:scale (/ (pict-width child) world-width)
                 (/ (pict-height child) world-height))
       (tf:translate (- x1) (- y1))))
