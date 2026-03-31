#lang racket/base

(require racket/class
         racket/contract
         racket/draw
         racket/list
         racket/match
         racket/math
         threading
         toolbox/color
         toolbox/pict
         "../core.rkt"
         "../lib/array.rkt"
         "../lib/atlas.rkt"
         "../lib/contract.rkt"
         "../lib/geometry.rkt"
         "../lib/gl/dc.rkt"
         (prefix-in gl: "../lib/gl/pict.rkt")
         "constants.rkt")

(provide (maybe-contract-out
          [blank-tile gl:pict?]
          [tile (-> tile? gl:pict?)]

          [board-background (-> natural? natural? gl:pict?)]
          [tf:tile-to-scene (-> gl:pict? gl:pict? transformation?)]
          [tf:scene-to-tile (-> gl:pict? gl:pict? transformation?)]
          [board-grid (-> natural? natural? gl:pict?)]
          [board-border (-> natural? natural? gl:pict?)]

          [cursor (-> natural? gl:pict?)]

          (rename gl:text text (->* [string?]
                                    [#:color color?
                                     #:texture-mode texture-mode/c]
                                    gl:pict?))
          [clue (-> clue? #:color color? gl:pict?)]
          [mega-clue (-> axis? clue? #:color color? gl:pict?)]
          [clue-underlay (-> axis? real? #:color color? gl:pict?)]

          [sprite-atlas (->* [] [#:scale (and/c exact-integer? (>=/c 2))] atlas?)]))

;; -----------------------------------------------------------------------------

(define (line dx dy)
  (unsafe-dc (λ (dc x y) (send dc draw-line x y (+ x dx) (+ y dy)))
             dx
             dy))

(define (path p
              #:color [color "black"]
              #:pen [pen (make-pen #:style 'transparent)]
              #:brush [brush (make-brush #:color color)])
  (define-values [x y w h] (send p get-bounding-box))
  (unsafe-dc
   (λ (dc x y)
     (define old-pen (send dc get-pen))
     (define old-brush (send dc get-brush))
     (send dc set-pen pen)
     (send dc set-brush brush)
     (send dc draw-path p x y)
     (send dc set-brush old-brush)
     (send dc set-pen old-pen))
   (+ x w)
   (+ y h)))

(define (make-bounds-integral p #:align [align cc-superimpose])
  (align (blank (ceiling (pict-width p))
                (ceiling (pict-height p)))
         p))

;; -----------------------------------------------------------------------------

(define blank-tile (gl:blank TILE-SIZE))

(define (tile-rectangle color)
  (gl:rectangle TILE-SIZE #:color (->rgb color)))

(define empty-tile-1 (tile-rectangle TILE-EMPTY-COLOR-1))
(define empty-tile-2 (tile-rectangle TILE-EMPTY-COLOR-2))
(define full-tile (tile-rectangle TILE-FULL-COLOR))

(define sprite:cross
  (~> (cc-superimpose
       (line (/ TILE-SIZE 2) (/ TILE-SIZE 2))
       (line (/ TILE-SIZE 2) (/ TILE-SIZE -2)))
      (colorize TILE-CROSS-COLOR)
      (linewidth TILE-SYMBOL-THICKNESS _)
      (inset TILE-SYMBOL-THICKNESS)
      make-bounds-integral))

(define sprite:mark
  (~> (rectangle (/ TILE-SIZE 2.5) (/ TILE-SIZE 2.5))
      (rotate (turns 1/8))
      (colorize TILE-MARK-COLOR)
      (linewidth TILE-SYMBOL-THICKNESS _)
      (inset TILE-SYMBOL-THICKNESS)
      make-bounds-integral))

(define cross-tile (gl:cc-superimpose blank-tile (gl:sprite sprite:cross)))
(define mark-tile (gl:cc-superimpose blank-tile (gl:sprite sprite:mark)))

(define (tile t)
  (match t
    ['empty blank-tile]
    ['full  full-tile]
    ['cross cross-tile]
    ['mark  mark-tile]))

;; -----------------------------------------------------------------------------

(define (board-background width height)
  (define (build-row offset)
    (for/fold ([line-p (gl:blank)])
              ([x (in-range width)])
      (gl:ht-append line-p
                    (if (even? (+ x offset))
                        empty-tile-1
                        empty-tile-2))))

  (define even-row (build-row 0))
  (define odd-row (build-row 1))

  (gl:launder
   (for/fold ([board-p (gl:blank)])
             ([y (in-range height)])
     (gl:vl-append board-p
                   (if (even? y) even-row odd-row)))))

(define tf:tile-to-background
  (tf:scale TILE-SIZE TILE-SIZE))

(define (tf:tile-to-scene scene-p bg-p)
  (tf* (gl:tf:child-to-parent scene-p bg-p)
       tf:tile-to-background))

(define (tf:scene-to-tile scene-p bg-p)
  (tf-invert (tf:tile-to-scene scene-p bg-p)))

(define (path:rounded-square-outline outer-size outer-radius
                                     inner-size inner-radius)
  (define path (new dc-path%))
  (send path rounded-rectangle 0 0 outer-size outer-size outer-radius)
  (define inner-offset (/ (- outer-size inner-size) 2))
  (send path rounded-rectangle
        inner-offset inner-offset inner-size inner-size inner-radius)
  path)

(define TILE-INNER-SIZE (- TILE-SIZE GRID-LINE-WIDTH))
(define GRID-REAL-BORDER-WIDTH (/ (+ GRID-BORDER-WIDTH GRID-LINE-WIDTH) 2))

(define sprite:minor-grid-tile
  (path (path:rounded-square-outline
         TILE-SIZE 0
         TILE-INNER-SIZE GRID-TILE-RADIUS)
        #:color GRID-MINOR-COLOR))

(define minor-grid-tile (gl:sprite sprite:minor-grid-tile))

(define (board-grid width height)
  (define minor-row (apply gl:ht-append (make-list width minor-grid-tile)))
  (define minor-grid (apply gl:vl-append (make-list height minor-row)))
  (define major-h-line (gl:rectangle (* width TILE-SIZE) GRID-LINE-WIDTH
                                     #:color GRID-MAJOR-COLOR))
  (define major-v-line (gl:rectangle GRID-LINE-WIDTH (* height TILE-SIZE)
                                     #:color GRID-MAJOR-COLOR))
  (define grid
    (for/fold ([grid minor-grid])
              ([x (in-range GRID-MAJOR-INTERVAL width GRID-MAJOR-INTERVAL)])
      (gl:pin grid major-v-line (point (* x TILE-SIZE) 0) #:hole gl:ct-find)))
  (gl:launder
   (for/fold ([grid grid])
             ([y (in-range GRID-MAJOR-INTERVAL height GRID-MAJOR-INTERVAL)])
     (gl:pin grid major-h-line (point 0 (* y TILE-SIZE)) #:hole gl:lc-find))))

(define-values [sprite:border-corner/integral sprite:border-corner]
  (let ()
    (define outer-size (+ TILE-SIZE GRID-BORDER-WIDTH))
    (define outer-radius (+ GRID-TILE-RADIUS (/ (- outer-size TILE-INNER-SIZE) 2)))
    (define sprite:border-tile
      (path (path:rounded-square-outline
             outer-size
             outer-radius
             TILE-INNER-SIZE
             GRID-TILE-RADIUS)
            #:color GRID-BORDER-COLOR))
    (define corner-window (blank outer-radius))
    (values (~> (lt-superimpose sprite:border-tile corner-window)
                (refocus corner-window)
                (make-bounds-integral #:align lt-superimpose)
                clip)
            corner-window)))

(define (board-border board-width board-height)
  (define border-corner (gl:sprite sprite:border-corner))
  (define width (* board-width TILE-SIZE))
  (define height (* board-height TILE-SIZE))
  (define h-line (gl:rectangle (- width (* GRID-TILE-RADIUS 2) GRID-LINE-WIDTH)
                               GRID-REAL-BORDER-WIDTH
                               #:color GRID-BORDER-COLOR))
  (define v-line (gl:rectangle GRID-REAL-BORDER-WIDTH
                               (- height (* GRID-TILE-RADIUS 2) GRID-LINE-WIDTH)
                               #:color GRID-BORDER-COLOR))
  (define top-border
    (gl:ht-append border-corner
                  h-line
                  (gl:rotate border-corner (turns -1/4))))
  (gl:launder
   (gl:rc-superimpose
    (gl:vl-append top-border v-line (gl:rotate top-border (turns 1/2)))
    v-line)))

(define sprite:cursor-mask
  (let ()
    (define outer-size (+ TILE-SIZE GRID-BORDER-WIDTH))
    (define inner-size (- TILE-SIZE GRID-BORDER-WIDTH))
    (path (path:rounded-square-outline
           outer-size
           (+ GRID-TILE-RADIUS (/ (- outer-size TILE-INNER-SIZE) 2))
           inner-size
           (- GRID-TILE-RADIUS (/ (- TILE-INNER-SIZE inner-size) 2))))))

(define (cursor client-id)
  (define color-i (modulo client-id (array-length CURSOR-COLORS)))
  (gl:sprite sprite:cursor-mask #:color (->rgb (array-ref CURSOR-COLORS color-i))))

;; -----------------------------------------------------------------------------

(define CLUE-FONT
  (make-font #:size (max CLUE-SIZE MEGA-CLUE-SIZE)
             #:size-in-pixels? #t
             #:font-list the-font-list
             #:hinting 'unaligned))

(define sprites:glyphs
  (for/hasheqv ([i (in-inclusive-range 32 126)])
    (define ch (integer->char i))
    (values ch (text (string ch) CLUE-FONT))))

(define (sprite:glyph ch)
  (hash-ref sprites:glyphs ch))

(define (gl:text str
                 #:color [color "black"]
                 #:texture-mode [texture-mode 'mask])
  (for/fold ([p (gl:blank)])
            ([ch (in-string str)])
    (gl:hb-append p (gl:sprite (sprite:glyph ch)
                               #:color color
                               #:texture-mode texture-mode))))

(define (integer->digits n)
  (if (zero? n)
      '(0)
      (let loop ([n n]
                 [digits '()])
        (define-values [q r] (quotient/remainder n 10))
        (if (zero? q)
            (cons r digits)
            (loop q (cons r digits))))))

(define (clue n #:color color)
  (~> (gl:text (number->string n) #:color color)
      (gl:scale-to-fit #f CLUE-SIZE #:direction 'down)
      gl:launder))

(define sprite:mega-clue-corner-mask
  (let ()
    (~> (disk (* MEGA-CLUE-RADIUS 2)
              #:draw-border? #f)
        (inset 0 0 (- MEGA-CLUE-RADIUS) (- MEGA-CLUE-RADIUS))
        clip)))

(define (mega-clue axis n #:color color)
  (define corner-p (gl:sprite sprite:mega-clue-corner-mask #:color color))

  (define text-size (if (or (eq? axis 'column) (= n 2))
                        CLUE-SIZE
                        MEGA-CLUE-SIZE))
  (define text-p
    (~> (gl:text (number->string n)
                 #:color color
                 #:texture-mode 'inverted-mask)
        (gl:scale-to-fit #f text-size #:direction 'down)))

  (define text-w (gl:pict-width text-p))
  (define text-h (gl:pict-height text-p))

  (gl:launder
   (match axis
     ['row
      (define padding (if (= n 2) MEGA-CLUE-TWO-PADDING MEGA-CLUE-PADDING))
      (define total-w (+ text-w (* padding 2)))
      (define total-h (* (- TILE-SIZE MEGA-CLUE-MARGIN) 2))
      (define text-y (/ (- total-h text-h) 2))

      (define top-p
        (gl:vl-append
         (gl:ht-append corner-p
                       (gl:rectangle (- total-w (* MEGA-CLUE-RADIUS 2))
                                     MEGA-CLUE-RADIUS
                                     #:color color)
                       (gl:rotate corner-p (turns -1/4)))
         (gl:rectangle total-w (- text-y MEGA-CLUE-RADIUS) #:color color)))

      (define edge-p (gl:rectangle padding text-h #:color color))
      (define center-p (gl:ht-append edge-p text-p edge-p))
      (gl:vl-append top-p
                    center-p
                    (gl:rotate top-p (turns 1/2)))]
     ['column
      (define total-w (* (- TILE-SIZE MEGA-CLUE-MARGIN) 2))
      (define total-h text-h)
      (define text-x (/ (- total-w text-w) 2))

      (define left-p
        (gl:ht-append
         (gl:vl-append corner-p
                       (gl:rectangle MEGA-CLUE-RADIUS
                                     (- total-h (* MEGA-CLUE-RADIUS 2))
                                     #:color color)
                       (gl:rotate corner-p (turns 1/4)))
         (gl:rectangle (- text-x MEGA-CLUE-RADIUS) total-h #:color color)))

      (gl:ht-append left-p
                    text-p
                    (gl:rotate left-p (turns 1/2)))])))

;; -----------------------------------------------------------------------------

(define sprite:clue-underlay-cap
  (~> (filled-rounded-rectangle (* CLUE-UNDERLAY-RADIUS 2)
                                TILE-SIZE
                                CLUE-UNDERLAY-RADIUS
                                #:draw-border? #f)
      (inset 0 0 (- CLUE-UNDERLAY-RADIUS) 0)
      clip))

(define (clue-underlay axis size #:color color)
  (gl:launder
   (match axis
     ['row
      (~> (gl:ht-append (gl:sprite sprite:clue-underlay-cap #:color color)
                        (gl:rectangle (+ (- size (pict-width sprite:clue-underlay-cap))
                                         GRID-BORDER-WIDTH)
                                      (pict-height sprite:clue-underlay-cap)
                                      #:color color))
          (gl:inset 0 0 (- (/ GRID-BORDER-WIDTH 2)) 0))]
     ['column
      (gl:rotate (clue-underlay 'row size #:color color) (turns -1/4))])))

;; -----------------------------------------------------------------------------

(define all-sprites
  (append (list sprite:cross
                sprite:mark
                sprite:minor-grid-tile
                sprite:border-corner/integral
                sprite:cursor-mask
                sprite:mega-clue-corner-mask
                sprite:clue-underlay-cap)
          (map make-bounds-integral
               (hash-values sprites:glyphs #t))))

(define (sprite-atlas #:scale [scale 2])
  (pack-atlas all-sprites #:scale scale))

;; -----------------------------------------------------------------------------

(module+ test
  (void (sprite-atlas #:scale 2)))

(module+ main
  (atlas-pict (sprite-atlas #:scale 2))
  (current-atlas (sprite-atlas #:scale 8))

  (define bw 5)
  (define bh 5)

  (define bg-p (board-background bw bh))

  (define ((tile-cc-find x y) p)
    (tf* (gl:tf:child-to-parent p bg-p)
         (point (* (+ x 0.5) TILE-SIZE)
                (* (+ y 0.5) TILE-SIZE))))

  (~> (vl-append (text "21" CLUE-FONT)
                 (clue 21 #:color (->rgb CLUE-DONE-COLOR))
                 (~> (for/list ([i (in-inclusive-range 2 20)])
                       (mega-clue 'row i #:color (->rgb CLUE-PENDING-COLOR)))
                     (apply gl:hb-append _ #:gap 1))
                 (~> (for/list ([i (in-inclusive-range 2 20)])
                       (mega-clue 'column i #:color (->rgb CLUE-ERROR-COLOR)))
                     (apply gl:hb-append _ #:gap 1)))
      (scale 2)
      (freeze #:scale 2))

  (~> (gl:cc-superimpose
       bg-p
       (board-grid bw bh)
       (board-border bw bh))
      (gl:pin (apply gl:vr-append
                     (for/list ([i (in-range bh)])
                       (clue-underlay 'row 50 #:color (tile-empty-color i))))
              (λ~> (gl:lb-find bg-p)) #:hole gl:rb-find
              #:over? #f #:extend? #t)
      (gl:pin (apply gl:hb-append
                     (for/list ([i (in-range bw)])
                       (clue-underlay 'column 50 #:color (tile-empty-color i))))
              (λ~> (gl:rt-find bg-p)) #:hole gl:rb-find
              #:over? #f #:extend? #t)
      (gl:pin (cursor 3) (tile-cc-find 1 2) #:hole gl:cc-find)
      (gl:scale 4)
      (freeze #:scale 2)))
