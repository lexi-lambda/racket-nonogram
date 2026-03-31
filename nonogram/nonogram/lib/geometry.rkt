#lang racket/base

(require data/gvector
         racket/contract
         racket/match
         racket/math
         racket/serialize
         racket/string
         toolbox/pict
         toolbox/print
         toolbox/who
         "contract.rkt")

(provide integer-point?
         (maybe-contract-out
          (struct point ([x real?] [y real?]))
          [zero-point integer-point?]
          [point- (-> point? point? ... point?)]
          [truncate-point (-> point? integer-point?)]
          [floor-point (-> point? integer-point?)]
          [pict-child-point (->* [pict? pict?] [pict-finder/c] point?)]

          (struct size ([width real?] [height real?]))
          [pict-size (-> pict? size?)]
          [size-scaling-factor-to-fit (-> size? size? real?)]

          (struct rect ([x real?] [y real?] [width real?] [height real?]))
          [zero-rect rect?]
          [rect-x2 (-> rect? real?)]
          [rect-y2 (-> rect? real?)]
          [rect-tl (-> rect? point?)]
          [rect-tr (-> rect? point?)]
          [rect-bl (-> rect? point?)]
          [rect-br (-> rect? point?)]
          [rect-empty? (-> rect? boolean?)]
          [normalize-rect (-> rect? rect?)]
          [rect-scale-size (->* [rect? real?] [real?] rect?)]
          [rect-scale-all (->* [rect? real?] [real?] rect?)]
          [rect-transform/bounds (-> rect? transformation? rect?)]
          [rect-union (-> rect? rect? ... rect?)]
          [rect-contains? (-> rect? point? boolean?)]
          [rect-intersects? (-> rect? rect? boolean?)]
          [pict-child-rect (->* [pict? pict?] [#:fail failure-result/c] any)]

          [dirty-rects? predicate/c]
          [make-dirty-rects (-> dirty-rects?)]
          [dirty-rects-add! (-> dirty-rects? rect? void?)]
          [dirty-rects->list (-> dirty-rects? list?)]

          [turns (-> real? real?)])

         tf:identity
         tf*
         (maybe-contract-out
          (struct transformation ([xx real?]
                                  [yx real?]
                                  [x0 real?]
                                  [xy real?]
                                  [yy real?]
                                  [y0 real?]))
          [tf:translate (case->
                         (-> point? transformation?)
                         (-> real? real? transformation?))]
          [tf:scale (->* [real?] [real?] transformation?)]
          [tf:rotate (-> real? transformation?)]
          [tf-invert (-> transformation? transformation?)]
          [tf:world-to-child (-> pict? pict? transformation?)]
          [tf:child-to-world (-> pict? pict? transformation?)]))

;; -----------------------------------------------------------------------------

(define not-given (gensym 'not-given))

;; -----------------------------------------------------------------------------

(serializable-struct point (x y) #:transparent)

(define zero-point (point 0 0))

(define point-
  (case-lambda
    [(p)
     (point (- (point-x p))
            (- (point-y p)))]
    [(p . ps)
     (for ([p1 p]
           [p2 (in-list ps)])
       (point (- (point-x p1) (point-x p2))
              (- (point-y p1) (point-y p2))))]))

(define (integer-point? v)
  (and (point? v)
       (exact-integer? (point-x v))
       (exact-integer? (point-y v))))

(define (truncate-point p)
  (point (inexact->exact (truncate (point-x p)))
         (inexact->exact (truncate (point-y p)))))

(define (floor-point p)
  (point (inexact->exact (floor (point-x p)))
         (inexact->exact (floor (point-y p)))))

(define (pict-child-point parent child [find lt-find])
  (define-values [x y] (find parent child))
  (point x y))

;; -----------------------------------------------------------------------------

(struct size (width height) #:transparent)

(define (pict-size p)
  (size (pict-width p)
        (pict-height p)))

(define (size-scaling-factor-to-fit sz bounds-sz)
  (match-define (size w h) sz)
  (match-define (size bw bh) bounds-sz)
  (define w-fac (/ bw w))
  (if (> (* h w-fac) bh)
      (/ bh h)
      w-fac))

;; -----------------------------------------------------------------------------

(struct rect (x y width height) #:transparent)

(define zero-rect (rect 0 0 0 0))

(define (rect-x2 r) (+ (rect-x r) (rect-width r)))
(define (rect-y2 r) (+ (rect-y r) (rect-height r)))

(define (rect-tl r)
  (define r* (normalize-rect r))
  (point (rect-x r*) (rect-y r*)))
(define (rect-tr r)
  (define r* (normalize-rect r))
  (point (rect-x2 r*) (rect-y r*)))
(define (rect-bl r)
  (define r* (normalize-rect r))
  (point (rect-x r*) (rect-y2 r*)))
(define (rect-br r)
  (define r* (normalize-rect r))
  (point (rect-x2 r*) (rect-y2 r*)))

(define (rect-size r)
  (size (rect-width r) (rect-height r)))

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

(define (rect-scale-size r sx [sy sx])
  (match-define (rect x y w h) r)
  (rect x y (* w sx) (* h sy)))

(define (rect-scale-all r sx [sy sx])
  (match-define (rect x y w h) r)
  (rect (* x sx) (* y sy) (* w sx) (* h sy)))

(define (rect-transform/bounds r tf)
  (match-define (rect x y w h) r)
  (define-values [min-x min-y max-x max-y]
    (for*/fold ([min-x +inf.0]
                [min-y +inf.0]
                [max-x -inf.0]
                [max-y -inf.0])
               ([x (in-list (list x (+ x w)))]
                [y (in-list (list y (+ y h)))])
      (match-define (point x* y*) (tf* tf (point x y)))
      (values (min min-x x*)
              (min min-y y*)
              (max max-x x*)
              (max max-y y*))))
  (rect min-x min-y (- max-x min-x) (- max-y min-y)))

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

(define/who (pict-child-rect parent child #:fail [fail not-given])
  (with-handlers* ([(λ (exn)
                      (and (exn:fail? exn)
                           (string-contains? (exn-message exn) "sub-pict not found")))
                    (λ (exn)
                      (if (eq? fail not-given)
                          (raise-arguments-error who "child pict not found"
                                                 "child" child
                                                 "parent" parent)
                          (if (procedure? fail) (fail) fail)))])
    (define-values [x1 y1] (lt-find parent child #:nth 'unique))
    (define-values [x2 y2] (rb-find parent child #:nth 'unique))
    (rect x1 y1 (- x2 x1) (- y2 y1))))

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

(define (turns x)
  (* pi 2 x))

(struct transformation (xx yx x0
                        xy yy y0)
  #:transparent)

(define tf:identity
  (transformation 1.0 0.0 0.0
                  0.0 1.0 0.0))

(define tf:translate
  (case-lambda
    [(p) (tf:translate (point-x p) (point-y p))]
    [(dx dy)
     (transformation 1.0 0.0 dx
                     0.0 1.0 dy)]))

(define (tf:scale sx [sy sx])
  (transformation sx 0.0 0.0
                  0.0 sy 0.0))

(define (tf:rotate theta)
  (define a (cos (- theta)))
  (define b (sin (- theta)))
  (transformation a (- b) 0.0
                  b a     0.0))

(define/who (tf-invert t)
  (match-define (transformation xx yx x0 xy yy y0) t)
  (define a (- (* yx y0) (* yy x0)))
  (define b (- (* xy x0) (* xx y0)))
  (define d (- (* xx yy) (* yx xy)))
  (when (zero? d)
    (raise-arguments-error who "transformation is not invertible"
                           "transformation" t))
  (transformation (/ yy d) (- (/ yx d)) (/ a d)
                  (- (/ xy d)) (/ xx d) (/ b d)))

(define tf*
  (case-lambda
    [() tf:identity]
    [(t) t]
    [(t1 t2)
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
                        (+ (* xy1 x02) (* yy1 y02) y01))])]
    [(t1 t2 t3)
     (tf* t1 (tf* t2 t3))]
    [ts0
     (match-define (cons t ts) (reverse ts0))
     (for/fold ([t2 t])
               ([t1 (in-list ts)])
       (tf* t1 t2))]))

(define (tf:world-to-child world child)
  (define-values [x1 y1] (lt-find world child))
  (define-values [x2 y2] (rb-find world child))
  (define world-width (exact->inexact (- x2 x1)))
  (define world-height (exact->inexact (- y2 y1)))
  (tf* (tf:scale (/ (pict-width child) world-width)
                 (/ (pict-height child) world-height))
       (tf:translate (- x1) (- y1))))

(define (tf:child-to-world world child)
  (define-values [x1 y1] (lt-find world child))
  (define-values [x2 y2] (rb-find world child))
  (define world-width (exact->inexact (- x2 x1)))
  (define world-height (exact->inexact (- y2 y1)))
  (tf* (tf:translate x1 y1)
       (tf:scale (/ world-width (pict-width child))
                 (/ world-height (pict-height child)))))
