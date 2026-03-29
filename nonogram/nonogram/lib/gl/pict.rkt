#lang racket/base

(require (prefix-in p: toolbox/pict)
         opengl
         pict/convert
         racket/class
         racket/contract
         racket/gui/base
         racket/match
         toolbox/color
         toolbox/who
         "../atlas.rkt"
         "../contract.rkt"
         "../geometry.rkt"
         "core.rkt"
         "dc.rkt")

(provide (maybe-contract-out
          (struct pict ([width real?]
                        [height real?]
                        [draw (-> gl-dc? any)]
                        [children (listof (cons/c pict? transformation?))]))
          [pict-size (-> pict? size?)]
          [pict-bounds (-> pict? rect?)]

          [blank (->* [] [real? real?] pict?)]
          [ghost (-> pict? pict?)]
          [launder (-> pict? pict?)]
          [rectangle (->* [real?]
                          [real?
                           #:color color?
                           #:texture (or/c rect? p:pict? #f)
                           #:texture-mode texture-mode/c
                           #:atlas (or/c atlas? #f)]
                          pict?)]
          [sprite (->* [p:pict?]
                       [#:color (or/c color? #f)
                        #:texture-mode texture-mode/c
                        #:atlas (or/c atlas? #f)]
                       pict?)]
          [scale (->* [pict? real?] [real?] pict?)]
          [scale-to-fit (->* [pict?]
                             [#:direction (or/c 'up 'down 'both)]
                             #:rest (or/c (list/c (or/c size? pict?))
                                          (list/c (or/c real? #f)
                                                  (or/c real? #f)))
                             pict?)]
          [inset (case->
                  (-> pict? real? pict?)
                  (-> pict? real? real? pict?)
                  (-> pict? real? real? real? real? pict?))]
          [refocus (-> pict? pict? pict?)]
          [rotate (-> pict? real? pict?)]

          [ht-append append/c]
          [hc-append append/c]
          [hb-append append/c]
          [vl-append append/c]
          [vc-append append/c]
          [vr-append append/c]

          [lt-superimpose superimpose/c]
          [lc-superimpose superimpose/c]
          [lb-superimpose superimpose/c]
          [ct-superimpose superimpose/c]
          [cc-superimpose superimpose/c]
          [cb-superimpose superimpose/c]
          [rt-superimpose superimpose/c]
          [rc-superimpose superimpose/c]
          [rb-superimpose superimpose/c]

          [pin (->* [pict? pict? (or/c point? (-> pict? point?))]
                    [#:over? any/c
                     #:hole (or/c point? pict-finder/c)
                     #:extend? any/c]
                    pict?)]

          [tf:child-to-parent (->* [pict? pict?] [#:fail failure-result/c] any)]
          [tf:parent-to-child (->* [pict? pict?] [#:fail failure-result/c] any)]
          [tf:child-to-parent* (-> pict? pict? (listof transformation?))]
          [tf:parent-to-child* (-> pict? pict? (listof transformation?))]

          [lt-find pict-finder*/c]
          [lc-find pict-finder*/c]
          [lb-find pict-finder*/c]
          [ct-find pict-finder*/c]
          [cc-find pict-finder*/c]
          [cb-find pict-finder*/c]
          [rt-find pict-finder*/c]
          [rc-find pict-finder*/c]
          [rb-find pict-finder*/c]
          [bounds-find pict-finder*/c]

          [pict->bitmap (->* [pict?]
                             [#:atlas (or/c atlas? #f)
                              #:gl-config (is-a?/c gl-config%)]
                             (is-a?/c bitmap%))]
          [draw-pict (->* [pict? (is-a?/c dc<%>)]
                          [real? real?
                           #:atlas (or/c atlas? #f)
                           #:gl-config (is-a?/c gl-config%)]
                          void?)]))

;; -----------------------------------------------------------------------------

(define not-given (gensym 'not-given))

(struct pict
  (width
   height
   draw
   children)
  #:property prop:pict-convertible
  (λ (self)
    (p:unsafe-dc
     (λ (dc x y)
       (draw-pict self dc x y))
     (pict-width self)
     (pict-height self))))

(define (pict-size p)
  (size (pict-width p) (pict-height p)))

(define (pict-bounds p)
  (rect 0.0 0.0 (pict-width p) (pict-height p)))

;; -----------------------------------------------------------------------------

(define (blank [width 0] [height width])
  (pict width height void '()))

(define (ghost p)
  (pict (pict-width p)
        (pict-height p)
        void
        (list (cons p tf:identity))))

(define (launder p)
  (struct-copy pict p
    [children '()]))

(define/who (rectangle width [height width]
                       #:color [color transparent-color]
                       #:texture [texture-spec #f]
                       #:texture-mode [texture-mode (if texture-spec 'texture 'solid)]
                       #:atlas [atlas (current-atlas)]
                       #:who [who who])

  (when (and atlas (p:pict? texture-spec))
    (atlas-child-rect atlas texture-spec #:who who))

  (cond
    [(or (zero? width) (zero? height))
     (blank width height)]
    [else
     (define color* (->rgb color))
     (pict width
           height
           (λ (dc)
             (gl-dc-add-rectangle! dc (rect 0 0 width height)
                                   #:color color*
                                   #:texture texture-spec
                                   #:texture-mode texture-mode))
           '())]))

(define/who (sprite texture-p
                    #:color [color #f]
                    #:texture-mode [texture-mode (if color 'mask 'texture)]
                    #:atlas [atlas (current-atlas)])
  (rectangle (p:pict-width texture-p)
             (p:pict-height texture-p)
             #:color (or color transparent-color)
             #:texture texture-p
             #:texture-mode texture-mode
             #:atlas atlas
             #:who who))

(define (transform-draw draw tf)
  (λ (dc)
    (parameterize ([current-gl-dc-transform
                    (tf* (current-gl-dc-transform) tf)])
      (draw dc))))

(define (scale p x-fac [y-fac x-fac])
  (define tf (tf:scale x-fac y-fac))
  (pict (* (pict-width p) x-fac)
        (* (pict-height p) y-fac)
        (transform-draw (pict-draw p) tf)
        (list (cons p tf))))

(define (scale-to-fit p
                      #:direction [direction 'both]
                      . args)
  (define-values [target-w target-h]
    (match args
      [(list (size w h))
       (values w h)]
      [(list (? pict? size-p))
       (values (pict-width size-p)
               (pict-height size-p))]
      [(list w h)
       (values w h)]))

  (define x-fac (and target-w (/ target-w (pict-width p))))
  (define y-fac (and target-h (/ target-h (pict-height p))))
  (define fac (if (and x-fac y-fac)
                  (min x-fac y-fac)
                  (or x-fac y-fac)))

  (cond
    [(or (not fac) (= fac 1.0)) p]
    [(> fac 1.0)
     (if (eq? direction 'down)
         p
         (scale p fac))]
    [else
     (if (eq? direction 'up)
         p
         (scale p fac))]))

(define inset
  (case-lambda
    [(p v)   (inset p v v v v)]
    [(p h v) (inset p h v h v)]
    [(p l t r b)
     (define tf (tf:translate l t))
     (pict (+ (pict-width p) l r)
           (+ (pict-height p) t b)
           (transform-draw (pict-draw p) tf)
           (list (cons p tf)))]))

(define/who (refocus p child)
  (define r (bounds-find p child #:who who))
  (define tf (tf:translate (point- (rect-tl r))))
  (pict (rect-width r)
        (rect-height r)
        (transform-draw (pict-draw p) tf)
        (list (cons p tf))))

(define (rotate p theta)
  (define cx (/ (pict-width p) 2.0))
  (define cy (/ (pict-height p) 2.0))
  (define base-tf (tf* (tf:translate cx cy)
                       (tf:rotate theta)
                       (tf:translate (- cx) (- cy))))

  (match-define (rect bx by bw bh)
    (rect-transform/bounds (pict-bounds p) base-tf))

  (define tf (tf* (tf:translate (- bx) (- by)) base-tf))
  (pict bw bh
        (transform-draw (pict-draw p) tf)
        (list (cons p tf))))

;; -----------------------------------------------------------------------------

(define (make-append vertical? get-alignment-point)
  (define maybe-swap
    (if vertical?
        (λ (x y) (values y x))
        values))

  (define (pict-size p)
    (maybe-swap (pict-width p) (pict-height p)))

  (define (tf:translate* x y)
    (define-values [x* y*] (maybe-swap x y))
    (tf:translate x* y*))

  (λ (#:gap [gap 0] . ps)
    (match ps
      ['() (blank)]
      [(list p) p]
      [(cons p ps)
       (for/fold ([p1 p])
                 ([p2 (in-list ps)])
         (define-values [w1 h1] (pict-size p1))
         (define-values [w2 h2] (pict-size p2))
         (define ap1 (get-alignment-point p1))
         (define ap2 (get-alignment-point p2))

         (define dx2 (+ w1 gap))
         (define-values [dy1 dy2]
           (if (>= ap1 ap2)
               (values 0 (- ap1 ap2))
               (values (- ap2 ap1) 0)))

         (define-values [w h]
           (maybe-swap (+ dx2 w2)
                       (max (+ h1 dy1) (+ h2 dy2))))

         (define tf1 (tf:translate* 0 dy1))
         (define tf2 (tf:translate* dx2 dy2))
         (define draw1
           (if (zero? dy1)
               (pict-draw p1)
               (transform-draw (pict-draw p1) tf1)))
         (define draw2
           (transform-draw (pict-draw p2) tf2))

         (pict w h
               (λ (dc) (draw1 dc) (draw2 dc))
               (list (cons p1 tf1) (cons p2 tf2))))])))

(define append/c
  (->* [] [#:gap real?] #:rest (listof pict?) pict?))

(define ht-append (make-append #f (λ (p) 0.0)))
(define hc-append (make-append #f (λ (p) (/ (pict-height p) 2.0))))
(define hb-append (make-append #f pict-height))
(define vl-append (make-append #t (λ (p) 0.0)))
(define vc-append (make-append #t (λ (p) (/ (pict-width p) 2.0))))
(define vr-append (make-append #t pict-width))

;; -----------------------------------------------------------------------------

(define (make-superimpose align-x align-y)
  (case-lambda
    [() (blank)]
    [(p) p]
    [(p . ps)
     (for/fold ([p1 p])
               ([p2 (in-list ps)])
       (define w1 (pict-width p1))
       (define w2 (pict-width p2))
       (define h1 (pict-height p1))
       (define h2 (pict-height p2))

       (define ax1 (* w1 align-x))
       (define ax2 (* w2 align-x))
       (define ay1 (* h1 align-y))
       (define ay2 (* h2 align-y))

       (define-values [dx1 dx2]
         (if (>= ax1 ax2)
             (values 0 (- ax1 ax2))
             (values (- ax2 ax1) 0)))
       (define-values [dy1 dy2]
         (if (>= ay1 ay2)
             (values 0 (- ay1 ay2))
             (values (- ay2 ay1) 0)))

       (define tf1 (tf:translate dx1 dy1))
       (define tf2 (tf:translate dx2 dy2))
       (define draw1 (transform-draw (pict-draw p1) tf1))
       (define draw2 (transform-draw (pict-draw p2) tf2))

       (pict (max (+ w1 dx1) (+ w2 dx2))
             (max (+ h1 dy1) (+ h2 dy2))
             (λ (dc) (draw1 dc) (draw2 dc))
             (list (cons p1 tf1) (cons p2 tf2))))]))

(define superimpose/c
  (-> pict? ... pict?))

(define lt-superimpose (make-superimpose 0.0 0.0))
(define lc-superimpose (make-superimpose 0.0 0.5))
(define lb-superimpose (make-superimpose 0.0 1.0))
(define ct-superimpose (make-superimpose 0.5 0.0))
(define cc-superimpose (make-superimpose 0.5 0.5))
(define cb-superimpose (make-superimpose 0.5 1.0))
(define rt-superimpose (make-superimpose 1.0 0.0))
(define rc-superimpose (make-superimpose 1.0 0.5))
(define rb-superimpose (make-superimpose 1.0 1.0))

;; -----------------------------------------------------------------------------

(define (pin base-p p posn-spec
             #:over? [over? #t]
             #:hole [hole-spec (point 0.0 0.0)]
             #:extend? [extend-bb? #f])
  (define posn
    (match posn-spec
      [(? point?) posn-spec]
      [(? procedure?) (posn-spec base-p)]))
  (define hole
    (match hole-spec
      [(? point?) hole-spec]
      [(? procedure?) (hole-spec p p)]))

  (define dx (- (point-x posn) (point-x hole)))
  (define dy (- (point-y posn) (point-y hole)))

  (define (combine-draw draw1 draw2)
    (if over?
        (λ (dc) (draw1 dc) (draw2 dc))
        (λ (dc) (draw2 dc) (draw1 dc))))

  (cond
    [extend-bb?
     (define-values [dx1 dx2]
       (if (< dx 0) (values (- dx) 0) (values 0 dx)))
     (define-values [dy1 dy2]
       (if (< dy 0) (values (- dy) 0) (values 0 dy)))

     (define tf1 (tf:translate dx1 dy1))
     (define tf2 (tf:translate dx2 dy2))
     (define draw1 (transform-draw (pict-draw base-p) tf1))
     (define draw2 (transform-draw (pict-draw p) tf2))

     (pict (max (+ (pict-width base-p) dx1) (+ (pict-width p) dx2))
           (max (+ (pict-height base-p) dy1) (+ (pict-height p) dy2))
           (combine-draw draw1 draw2)
           (list (cons base-p tf1) (cons p tf2)))]
    [else
     (define tf (tf:translate dx dy))
     (define draw1 (pict-draw base-p))
     (define draw2 (transform-draw (pict-draw p) tf))
     (pict (pict-width base-p)
           (pict-height base-p)
           (combine-draw draw1 draw2)
           (list (cons base-p tf:identity) (cons p tf)))]))

;; -----------------------------------------------------------------------------

(define/who (tf:child-to-parent parent child
                                #:fail [fail not-given]
                                #:who [who who])
  (match (tf:child-to-parent* parent child)
    [(list tf) tf]
    ['()
     (cond
       [(eq? fail not-given)
        (raise-arguments-error who "could not find child pict"
                               "child" child
                               "parent" parent)]
       [(procedure? fail) (fail)]
       [else fail])]
    [tfs
     (raise-arguments-error who "child appears multiple times within parent"
                            "transformations" tfs
                            "child" child
                            "parent" parent)]))

(define (tf:child-to-parent* parent child)
  (let loop ([parent parent]
             [tf tf:identity]
             [tfs '()])
    (if (equal? parent child)
        (cons tf tfs)
        (for/fold ([tfs tfs])
                  ([child+tf (in-list (pict-children parent))])
          (match-define (cons child1 tf1) child+tf)
          (loop child1 (tf* tf tf1) tfs)))))

(define/who (tf:parent-to-child parent child #:fail [fail not-given])
  (cond
    [(tf:child-to-parent parent child
                         #:fail (if (eq? fail not-given) fail #f)
                         #:who who)
     => tf-invert]
    [(procedure? fail) (fail)]
    [else fail]))

(define (tf:parent-to-child* parent child)
  (map tf-invert (tf:child-to-parent* parent child)))

(define (make-find who x-fac y-fac)
  (λ (parent [child parent] #:fail [fail not-given])
    (define tf (tf:child-to-parent parent child
                                   #:fail (if (eq? fail not-given) fail #f)
                                   #:who who))
    (cond
      [tf
       (tf* tf
            (point (* (pict-width child) x-fac)
                   (* (pict-height child) y-fac)))]
      [(procedure? fail) (fail)]
      [else fail])))

(define pict-finder/c
  (-> pict? pict? point?))
(define pict-finder*/c
  (->* [pict?] [pict? #:fail failure-result/c] any))

(define/who lt-find (make-find who 0.0 0.0))
(define/who lc-find (make-find who 0.0 0.5))
(define/who lb-find (make-find who 0.0 1.0))
(define/who ct-find (make-find who 0.5 0.0))
(define/who cc-find (make-find who 0.5 0.5))
(define/who cb-find (make-find who 0.5 1.0))
(define/who rt-find (make-find who 1.0 0.0))
(define/who rc-find (make-find who 1.0 0.5))
(define/who rb-find (make-find who 1.0 1.0))

(define/who (bounds-find parent [child parent]
                         #:fail [fail not-given]
                         #:who [who who])
  (define tf (tf:child-to-parent parent child
                                 #:fail (if (eq? fail not-given) fail #f)
                                 #:who who))
  (cond
    [tf (rect-transform/bounds (pict-bounds child) tf)]
    [(procedure? fail) (fail)]
    [else fail]))

;; -----------------------------------------------------------------------------

(define (pict->bitmap p
                      #:atlas [atlas (current-atlas)]
                      #:gl-config [gl-config (current-gl-config)])
  (define width (max 1 (inexact->exact (ceiling (pict-width p)))))
  (define height (max 1 (inexact->exact (ceiling (pict-height p)))))
  (define dc (make-object bitmap-dc% (make-gl-bitmap 1 1 (current-gl-config))))
  (define ctx (send dc get-gl-context))
  (define pixels (make-bytes (* width height 4)))
  (send ctx call-as-current
        (λ ()
          (define prog (link-gl-dc-program))

          ;; XXX: multisampling
          (glBindFramebuffer GL_FRAMEBUFFER (gl-gen-framebuffer))
          (define tex (gl-gen-texture))
          (glBindTexture GL_TEXTURE_2D tex)
          (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 width height 0 GL_BGRA reverse-pixel-format #f)
          (glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D tex 0)
          (glViewport 0 0 width height)

          (define atl (or atlas empty-atlas))
          (define atl-tex (make-gl-texture (atlas-bitmap atl)))
          (define gdc (make-gl-dc #:atlas atl #:texture atl-tex))
          (parameterize ([current-gl-dc-transform tf:identity])
            ((pict-draw p) gdc))

          (glClearColor 0.0 0.0 0.0 0.0)
          (glClear GL_COLOR_BUFFER_BIT)
          (glEnable GL_BLEND)
          (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
          (use-gl-dc-program!
           prog
           #:transform (tf* (tf:translate -1.0 -1.0)
                            (tf:scale (/ 2.0 width) (/ 2.0 height))))
          (gl-dc-draw gdc)

          (glBindTexture GL_TEXTURE_2D tex)
          (glGetTexImage GL_TEXTURE_2D 0 GL_BGRA reverse-pixel-format pixels)))

  (define bmp (make-bitmap width height))
  (send bmp set-argb-pixels 0 0 width height pixels #f #t)
  bmp)

(define (draw-pict p dc [x 0] [y 0]
                   #:atlas [atlas (current-atlas)]
                   #:gl-config [gl-config (current-gl-config)])
  (define old-transform (send dc get-transformation))
  (send dc scale 1 1)

  (match-define (vector (vector xx yx xy yy _ _) _ _ _ _ _)
    (send dc get-transformation))
  (match-define (point sx sy)
    (tf* (tf:scale (send dc get-backing-scale))
         (transformation xx yx 0.0 xy yy 0.0)
         (point 1.0 1.0)))

  (send dc scale (/ sx) (/ sy))
  (send dc draw-bitmap
        (pict->bitmap (scale p sx sy)
                      #:atlas atlas
                      #:gl-config gl-config)
        (* x sx)
        (* y sy))

  (send dc set-transformation old-transform))
