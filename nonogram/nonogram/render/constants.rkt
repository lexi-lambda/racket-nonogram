#lang racket/base

(require racket/draw
         "../lib/array.rkt")

(provide (all-defined-out))

;; -----------------------------------------------------------------------------

(define CLUE-SIZE 16)
(define CLUE-GAP 6)
(define CLUE-BOARD-GAP 4)
(define CLUE-PENDING-COLOR (make-color #x30 #x30 #x30))
(define CLUE-DONE-COLOR "gray")
(define CLUE-ERROR-COLOR "red")

(define MEGA-CLUE-SIZE 20)
(define MEGA-CLUE-GAP 4)
(define MEGA-CLUE-TWO-PADDING 1)
(define MEGA-CLUE-MARGIN 2)
(define MEGA-CLUE-PADDING 3)
(define MEGA-CLUE-RADIUS 2)

(define TILE-SIZE 20)
(define TILE-EMPTY-COLOR-1 "white")
(define TILE-EMPTY-COLOR-2 "white smoke")
(define TILE-FULL-COLOR "black")

(define TILE-SYMBOL-THICKNESS 2)
(define TILE-CROSS-COLOR "dark orange")
(define TILE-MARK-COLOR "dim gray")

(define GRID-MAJOR-INTERVAL 5)
(define GRID-BORDER-COLOR (make-color #x48 #x48 #x48))
(define GRID-MAJOR-COLOR (make-color #x07 #xd9 #x18))
(define GRID-MINOR-COLOR (make-color #x80 #x80 #x80))
(define GRID-LINE-WIDTH 1.5)
(define GRID-BORDER-WIDTH 2)
(define GRID-TILE-RADIUS 2)

(define CURSOR-COLORS
  (array (make-color #x3d #x7b #xe0)
         (make-color #xe0 #x3d #x3d)
         (make-color #xf0 #xe9 #x30)
         (make-color #xb5 #x19 #xe0)))
