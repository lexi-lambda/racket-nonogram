#lang racket/base

(require racket/draw
         toolbox/color
         "../lib/array.rkt")

(module+ main
  (require toolbox/pict))

(provide (all-defined-out))

;; -----------------------------------------------------------------------------

(define (make-color* n)
  (make-color (bitwise-bit-field n 16 24)
              (bitwise-bit-field n 8 16)
              (bitwise-bit-field n 0 8)))

(define (hsv* hue-deg sat val [alpha 1.0])
  (hsv (/ hue-deg 360.0) sat val alpha))

(define CLUE-SIZE 16)
(define CLUE-GAP 6)
(define CLUE-BOARD-GAP 4)
(define CLUE-PENDING-COLOR (hsv* 0 0 0.19))
(define CLUE-HINT-COLOR (hsv* 217 0.94 0.89))
(define CLUE-DONE-ALPHA 0.3)
(define CLUE-ERROR-COLOR (->rgb "red"))
(define CLUE-UNDERLAY-RADIUS 3)

(define MEGA-CLUE-SIZE 20)
(define MEGA-CLUE-GAP 4)
(define MEGA-CLUE-TWO-PADDING 1)
(define MEGA-CLUE-MARGIN 2)
(define MEGA-CLUE-PADDING 3)
(define MEGA-CLUE-RADIUS 2)

(define TILE-SIZE 20)
(define TILE-EMPTY-COLOR-1 "white smoke")
(define TILE-EMPTY-COLOR-2 "white")
(define TILE-FULL-COLOR "black")
(define TILE-SYMBOL-THICKNESS 2)
(define TILE-CROSS-COLOR "dark orange")
(define TILE-MARK-COLOR "dim gray")
(define TILE-ERROR-MARK-COLOR (hsv* 7 0.90 1.0))
(define TILE-ERROR-OVERLAY-COLOR (hsv* 17 0.90 1.0 0.4))

(define GRID-MAJOR-INTERVAL 5)
(define GRID-BORDER-COLOR (make-color* #x484848))
(define GRID-MAJOR-COLOR (make-color* #x07d918))
(define GRID-MINOR-COLOR (make-color* #x808080))
(define GRID-LINE-WIDTH 1.5)
(define GRID-BORDER-WIDTH 2)
(define GRID-TILE-RADIUS 2)

(define CURSOR-STRIPES 4)
(define CURSOR-COLORS
  (array (hsv* 217 0.73 0.88)
         (hsv*   0 0.73 0.88)
         (hsv*  58 0.80 0.94)
         (hsv* 287 0.83 0.88)))

(define CLUE-UNDERLAY-HIGHLIGHT-COLORS
  (array (hsv* 217 0.40 1.0)
         (hsv*   0 0.40 1.0)
         (hsv*  58 0.40 1.0)
         (hsv* 287 0.40 1.0)))

(define CLUE-UNDERLAY-SECONDARY-HIGHLIGHT-COLORS
  (array (hsv* 217 0.15 1.0)
         (hsv*   0 0.15 1.0)
         (hsv*  58 0.15 1.0)
         (hsv* 287 0.15 1.0)))

(module+ main
  (define (visualize-colors colors)
    (for/fold ([p (blank)])
              ([color (in-array colors)])
      (hc-append
       p
       (filled-rectangle 20 20 #:color color #:draw-border? #f))))

  (visualize-colors (array TILE-EMPTY-COLOR-1
                           TILE-EMPTY-COLOR-2
                           TILE-FULL-COLOR
                           TILE-CROSS-COLOR
                           TILE-MARK-COLOR
                           TILE-ERROR-MARK-COLOR
                           TILE-ERROR-OVERLAY-COLOR))

  (vl-append
   (visualize-colors CURSOR-COLORS)
   (visualize-colors CLUE-UNDERLAY-HIGHLIGHT-COLORS)
   (visualize-colors CLUE-UNDERLAY-SECONDARY-HIGHLIGHT-COLORS)))

;; -----------------------------------------------------------------------------

(define (tile-empty-color i)
  (if (even? i)
      TILE-EMPTY-COLOR-1
      TILE-EMPTY-COLOR-2))

(define (array-ref/wrap arr i)
  (array-ref arr (modulo i (array-length arr))))

(define (cursor-color i)
  (array-ref/wrap CURSOR-COLORS i))
(define (clue-underlay-highlight-color i)
  (array-ref/wrap CLUE-UNDERLAY-HIGHLIGHT-COLORS i))
(define (clue-underlay-secondary-highlight-color i)
  (array-ref/wrap CLUE-UNDERLAY-SECONDARY-HIGHLIGHT-COLORS i))
