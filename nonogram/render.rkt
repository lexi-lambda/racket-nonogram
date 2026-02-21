#lang racket/base

(require pict
         racket/class
         racket/contract
         racket/list
         racket/match
         racket/math
         threading
         "analyze.rkt"
         "array.rkt"
         "core.rkt")

(provide TILE-SIZE
         (contract-out
          [line (-> real? real? pict?)]
          [set-smoothing (->* [pict?] [(or/c 'unsmoothed 'smoothed 'aligned)] pict?)]

          (struct rendered-puzzle ([pict pict?]
                                   [board-pict pict?]))
          [render-puzzle (-> puzzle? board-analysis? rendered-puzzle?)]))

;; -----------------------------------------------------------------------------

(define (line dx dy)
  (dc (λ (dc x y) (send dc draw-line x y (+ x dx) (+ y dy)))
      dx
      dy))

(define (set-smoothing p [smoothing 'smoothed])
  (define draw-p (make-pict-drawer p))
  (struct-copy
   pict (dc (λ (dc x y)
              (define old-smoothing (send dc get-smoothing))
              (send dc set-smoothing smoothing)
              (draw-p dc x y)
              (send dc set-smoothing old-smoothing))
            (pict-width p)
            (pict-height p)
            (pict-ascent p)
            (pict-descent p))
   [children (list (child p 0 0 1 1 0 0))]))

;; -----------------------------------------------------------------------------

(define CLUE-SIZE 16)
(define CLUE-GAP 6)
(define CLUE-BOARD-GAP 4)
(define CLUE-DONE-COLOR "gray")
(define CLUE-ERROR-COLOR "red")
(define TILE-SIZE 20)
(define TILE-BORDER-COLOR "dim gray")
(define TILE-CROSS-COLOR "dark orange")
(define TILE-MARK-COLOR "dim gray")

;; render-tile : tile? -> pict?
(define (render-tile tile)
  (match tile
    ['empty (rectangle TILE-SIZE TILE-SIZE #:border-color TILE-BORDER-COLOR)]
    ['full  (filled-rectangle TILE-SIZE TILE-SIZE #:border-color TILE-BORDER-COLOR)]
    ['cross
     (define cross-p
       (linewidth
        (/ TILE-SIZE 10)
        (colorize
         (cc-superimpose
          (line (/ TILE-SIZE 2) (/ TILE-SIZE 2))
          (line (/ TILE-SIZE 2) (/ TILE-SIZE -2)))
         TILE-CROSS-COLOR)))
     (cc-superimpose (render-tile 'empty) cross-p)]
    ['mark
     (define mark-p
       (linewidth
        (/ TILE-SIZE 10)
        (colorize
         (rotate (rectangle (/ TILE-SIZE 2.5) (/ TILE-SIZE 2.5)) (/ pi 4))
         TILE-MARK-COLOR)))
     (cc-superimpose (render-tile 'empty) mark-p)]))

;; render-board : board? -> pict?
(define (render-board pb)
  (~> (for/list ([row (in-array (board-rows pb))])
        (~> (for/list ([tile (in-array row)])
              (render-tile tile))
            (apply hc-append _)))
      (apply vc-append _)))

;; render-clue : clue? (or/c clue-analysis? 'error) -> pict?
(define (render-clue clue [analysis 'pending])
  (define p (text (number->string clue) '() CLUE-SIZE))
  (match analysis
    ['pending p]
    ['done (colorize p CLUE-DONE-COLOR)]
    ['error (colorize p CLUE-ERROR-COLOR)]))

;; render-clue : axis? clue-line? clue-line-analysis? -> pict?
(define (render-line-clues axis line-clues line-analysis)
  (define clue-picts
    (for/list ([clue (in-list (if (empty? line-clues)
                                  '(0)
                                  line-clues))]
               [analysis (match line-analysis
                           ['done (in-cycle '(done))]
                           ['error (in-cycle '(error))]
                           [_ (in-list line-analysis)])])
      (render-clue clue analysis)))
  (match axis
    ['row    (hc-append (apply hc-append CLUE-GAP clue-picts)
                        (blank CLUE-BOARD-GAP TILE-SIZE))]
    ['column (vc-append (apply vc-append clue-picts)
                        (blank TILE-SIZE 0))]))

;; render-clue-axis : axis? axis-clues? clue-axis-analysis? -> pict?
(define (render-axis-clues axis axis-clues axis-analysis)
  (define line-picts
    (for/list ([clue-line (in-vector axis-clues)]
               [line-analysis (in-vector axis-analysis)])
      (render-line-clues axis clue-line line-analysis)))
  (match axis
    ['row    (apply vr-append line-picts)]
    ['column (apply hb-append line-picts)]))

(struct rendered-puzzle (pict board-pict) #:transparent)

;; render-puzzle : puzzle? board-analysis? -> pict?
(define (render-puzzle p analysis)
  (define clues (puzzle-clues p))
  (define board-pict (render-board (puzzle-board p)))
  (define pict
    (hb-append
     (render-axis-clues 'row
                        (board-clues-row-clues clues)
                        (board-analysis-row-analysis analysis))
     (vr-append
      (render-axis-clues 'column
                         (board-clues-column-clues clues)
                         (board-analysis-column-analysis analysis))
      board-pict)))

  (rendered-puzzle pict board-pict))
