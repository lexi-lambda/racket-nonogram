#lang racket/base

(require data/gvector
         pict
         racket/contract
         racket/match
         threading
         toolbox/print)

(provide integer-point?
         (contract-out
          (struct point ([x real?] [y real?]))
          [truncate-point (-> point? integer-point?)]

          (struct rect ([x real?] [y real?] [width real?] [height real?]))
          [rect-x2 (-> rect? real?)]
          [rect-y2 (-> rect? real?)]
          [rect-empty? (-> rect? boolean?)]
          [normalize-rect (-> rect? rect?)]
          [rect-union (-> rect? rect? ... rect?)]
          [rect-contains? (-> rect? point? boolean?)]
          [rect-intersects? (-> rect? rect? boolean?)]

          [dirty-rects? predicate/c]
          [make-dirty-rects (-> dirty-rects?)]
          [dirty-rects-add! (-> dirty-rects? rect? void?)]
          [dirty-rects->list (-> dirty-rects? list?)])

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

(struct rect (x y width height) #:transparent)

(define (rect-x2 r) (+ (rect-x r) (rect-width r)))
(define (rect-y2 r) (+ (rect-y r) (rect-height r)))

(define (rect-empty? r)
  (or (zero? (rect-width r))
      (zero? (rect-height r))))

(define (normalize-rect r)
  (match-define (rect x y w h) r)
  (define w-neg? (< w 0))
  (define h-neg? (< h 0))
  (if (or w-neg? h-neg?)
      (rect (if w-neg? (- x w) x)
            (if h-neg? (- y h) y)
            (if w-neg? (- w) w)
            (if h-neg? (- h) h))
      r))

(define (rect-union r . rs)
  (for/fold ([r1 (normalize-rect r)])
            ([r2 (in-list rs)])
    (match-define (rect x1 y1 w1 h1) r1)
    (match-define (rect x2 y2 w2 h2) (normalize-rect r2))
    (define x3 (min x1 x2))
    (define y3 (min y1 y2))
    (rect x3
          y3
          (- (max (+ x1 w1) (+ x2 w2)) x3)
          (- (max (+ y1 h1) (+ y2 h2)) y3))))

(define (rect-contains? r p)
  (match-define (rect x0 y0 w h) (normalize-rect r))
  (match-define (point x y) p)
  (and (>= x x0)
       (>= y y0)
       (< x (+ x0 w))
       (< y (+ y0 h))))

(define (rect-intersects? r1 r2)
  (match-define (and r1* (rect x1 y1 _ _)) (normalize-rect r1))
  (match-define (and r2* (rect x2 y2 _ _)) (normalize-rect r2))
  (or (rect-contains? r1* (point x2 y2))
      (rect-contains? r2* (point x1 y1))))

;; -----------------------------------------------------------------------------

(struct dirty-rects (vec)
  #:property prop:custom-write
  (make-constructor-style-printer
   #:expression? #f
   (λ (self) 'dirty-rects)
   (λ (self) (printing-sequence (dirty-rects->list self)))))

(define (make-dirty-rects)
  (dirty-rects (make-gvector)))

(define (dirty-rects-add! dr r)
  (unless (rect-empty? r)
    (define vec (dirty-rects-vec dr))
    (define r* (normalize-rect r))
    (let again ([r r*])
      (define r*
        (for/first ([(ri i) (in-gvector vec)]
                    #:when (rect-intersects? r ri))
          (gvector-remove! vec i)
          (rect-union r ri)))
      (if r*
          (again r*)
          (gvector-add! vec r)))))

(define (dirty-rects->list dr)
  (gvector->list (dirty-rects-vec dr)))

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
