#lang racket/base

(require pict
         racket/class
         racket/contract
         racket/draw
         racket/list
         racket/match
         racket/math
         threading
         toolbox/who
         "analyze.rkt"
         "array.rkt"
         "core.rkt"
         "geometry.rkt"
         "logger.rkt")

(provide TILE-SIZE
         (contract-out
          [line (-> real? real? pict?)]
          [set-smoothing (->* [pict?] [(or/c 'unsmoothed 'smoothed 'aligned)] pict?)]

          [get-base-puzzle-size (-> puzzle? size?)]
          [puzzle-renderer%
           (class/c
            (init-field [puzzle puzzle?]
                        [board-analysis (or/c board-analysis? #f)]
                        [output-scale real?]
                        [backing-scale real?])

            [get-output-scale (->m real?)]
            [get-backing-scale (->m real?)]

            [update! (->m puzzle? (or/c board-analysis? #f) void?)]
            [get-render (->m pict?)]
            [get-tile-at (->m point? (or/c integer-point? #f))])]))

;; -----------------------------------------------------------------------------

(define (turns x)
  (* pi 2 x))

(define (line dx dy)
  (unsafe-dc (λ (dc x y) (send dc draw-line x y (+ x dx) (+ y dy)))
             dx
             dy))

(define (set-smoothing p [smoothing 'smoothed])
  (define draw-p (make-pict-drawer p))
  (struct-copy
   pict (unsafe-dc
         (λ (dc x y)
           (define old-smoothing (send dc get-smoothing))
           (send dc set-smoothing smoothing)
           (draw-p dc x y)
           (send dc set-smoothing old-smoothing))
         (pict-width p)
         (pict-height p)
         (pict-ascent p)
         (pict-descent p))
   [children (list (child p 0 0 1 1 0 0))]))

(define/who (freeze-to p0 bm-box
                       #:scale [backing-scale 1.0]
                       #:allow-size-change? [allow-size-change? #f])
  (define p (scale p0 backing-scale))
  (define bm-width (max 1 (inexact->exact (ceiling (pict-width p)))))
  (define bm-height (max 1 (inexact->exact (ceiling (pict-height p)))))

  (define (install-new-bitmap!)
    (define bm (make-bitmap bm-width bm-height))
    (set-box! bm-box bm)
    bm)

  (define bm
    (match (unbox bm-box)
      [#f (install-new-bitmap!)]
      [bm
       (cond
         [(and (= (send bm get-width) bm-width)
               (= (send bm get-height) bm-height))
          bm]
         [allow-size-change?
          (install-new-bitmap!)]
         [else
          (raise-arguments-error who "pict to freeze has different size from previously-frozen pict")])]))

  (define dc (new bitmap-dc% [bitmap bm]))
  (send dc clear)
  (send dc set-smoothing 'smoothed)
  (draw-pict p dc 0 0)
  (struct-copy pict (scale (bitmap bm) (/ backing-scale))
               [children (list (child p0 0 0 1 1 0 0))]))

;; -----------------------------------------------------------------------------

(define CLUE-SIZE 16)
(define CLUE-GAP 6)
(define CLUE-BOARD-GAP 4)
(define CLUE-DONE-COLOR "gray")
(define CLUE-ERROR-COLOR "red")

(define TILE-SIZE 20)
(define TILE-EMPTY-COLOR-1 "white")
(define TILE-EMPTY-COLOR-2 "white smoke")
(define TILE-FULL-COLOR "black")

(define TILE-SYMBOL-THICKNESS 2)
(define TILE-CROSS-COLOR "dark orange")
(define TILE-MARK-COLOR "dim gray")

(define GRID-MAJOR-INTERVAL 5)
(define GRID-MAJOR-COLOR (make-color #x48 #x48 #x48))
(define GRID-MINOR-COLOR (make-color #x80 #x80 #x80))
(define GRID-LINE-WIDTH 1.5)
(define GRID-BORDER-WIDTH 2)
(define GRID-TILE-RADIUS 2)

;; -----------------------------------------------------------------------------

(define renderer%
  (class object%
    (init-field [backing-scale 1.0])

    (define/public (freeze* p)
      (~> (set-smoothing p)
          (freeze #:scale backing-scale)))

    (define/public (freeze-to* p bm-box)
      (freeze-to p bm-box #:scale backing-scale))

    (define/public (freeze/dc proc #:width width #:height height)
      (define bm (make-bitmap (max 1 (inexact->exact (ceiling (* width backing-scale))))
                              (max 1 (inexact->exact (ceiling (* height backing-scale))))))
      (define dc (make-object bitmap-dc% bm))
      (send dc set-smoothing 'smoothed)
      (send dc scale backing-scale backing-scale)
      (proc dc)
      (scale (bitmap bm) (/ backing-scale)))

    (super-new)))

;; -----------------------------------------------------------------------------
;; board

(define (cross-symbol)
  (~> (cc-superimpose
       (line (/ TILE-SIZE 2) (/ TILE-SIZE 2))
       (line (/ TILE-SIZE 2) (/ TILE-SIZE -2)))
      (colorize TILE-CROSS-COLOR)
      (linewidth TILE-SYMBOL-THICKNESS _)
      (inset 1)))

(define (mark-symbol)
  (~> (rectangle (/ TILE-SIZE 2.5) (/ TILE-SIZE 2.5))
      (rotate (turns 1/8))
      (colorize TILE-MARK-COLOR)
      (linewidth TILE-SYMBOL-THICKNESS _)
      (inset 1)))

(define board-renderer%
  (class renderer%
    (init-field [{board-w board-width}]
                [{board-h board-height}])

    (inherit freeze*
             freeze/dc)

    (super-new)

    (define inner-w (* board-w TILE-SIZE))
    (define inner-h (* board-h TILE-SIZE))
    (define outer-w (+ inner-w GRID-BORDER-WIDTH))
    (define outer-h (+ inner-h GRID-BORDER-WIDTH))

    ;; -------------------------------------------------------------------------

    (define/private (tile-bg color)
      (~> (filled-rounded-rectangle (- TILE-SIZE GRID-LINE-WIDTH)
                                    (- TILE-SIZE GRID-LINE-WIDTH)
                                    GRID-TILE-RADIUS
                                    #:draw-border? #f
                                    #:color color)
          freeze*
          (inset (/ GRID-LINE-WIDTH 2))))

    (define/private (tile-symbol symbol)
      (cc-superimpose
       (ghost empty-bg-1)
       (freeze* symbol)))

    (define empty-bg-1 (tile-bg TILE-EMPTY-COLOR-1))
    (define draw-empty-bg-1 (make-pict-drawer empty-bg-1))
    (define draw-empty-bg-2 (make-pict-drawer (tile-bg TILE-EMPTY-COLOR-2)))
    (define draw-full-bg (make-pict-drawer (tile-bg TILE-FULL-COLOR)))

    (define draw-cross (make-pict-drawer (tile-symbol (cross-symbol))))
    (define draw-mark (make-pict-drawer (tile-symbol (mark-symbol))))

    (define/private (draw-background dc board)
      (define end-timing (timing-start 'draw-background))
      (define old-transform (send dc get-transformation))
      (send dc translate (/ GRID-BORDER-WIDTH 2) (/ GRID-BORDER-WIDTH 2))

      (send dc set-pen (make-pen #:style 'transparent))
      (send dc set-brush (make-brush #:color GRID-MINOR-COLOR))
      (send dc draw-rectangle 0 0 inner-w inner-h)

      (for* ([i (in-range board-w)]
             [j (in-range board-h)])
        (define x (* i TILE-SIZE))
        (define y (* j TILE-SIZE))
        (match (board-ref board i j)
          ['full
           (draw-full-bg dc x y)]
          [other
           (if (even? (+ i j))
               (draw-empty-bg-1 dc x y)
               (draw-empty-bg-2 dc x y))

           (match other
             ['empty (void)]
             ['cross (draw-cross dc x y)]
             ['mark  (draw-mark dc x y)])]))

      (send dc set-transformation old-transform)
      (end-timing))

    ;; -------------------------------------------------------------------------

    (define draw-major-grid-lines
      (~> (freeze/dc
           #:width outer-w
           #:height outer-h
           (λ (dc)
             (define path (new dc-path%))
             (send path rounded-rectangle
                   0
                   0
                   outer-w
                   outer-h
                   (+ GRID-TILE-RADIUS (/ GRID-BORDER-WIDTH 2)))

             (for* ([i (in-range 0 board-w GRID-MAJOR-INTERVAL)]
                    [j (in-range 0 board-h GRID-MAJOR-INTERVAL)])
               (define tile-w (min (- board-w i) GRID-MAJOR-INTERVAL))
               (define tile-h (min (- board-h j) GRID-MAJOR-INTERVAL))
               (send path rounded-rectangle
                     (+ (* i TILE-SIZE) (/ GRID-BORDER-WIDTH 2) (/ GRID-LINE-WIDTH 2))
                     (+ (* j TILE-SIZE) (/ GRID-BORDER-WIDTH 2) (/ GRID-LINE-WIDTH 2))
                     (- (* TILE-SIZE tile-w) GRID-LINE-WIDTH)
                     (- (* TILE-SIZE tile-h) GRID-LINE-WIDTH)
                     GRID-TILE-RADIUS))

             (send dc set-pen (make-pen #:style 'transparent))
             (send dc set-brush (make-brush #:color GRID-MAJOR-COLOR))
             (send dc draw-path path)))
          make-pict-drawer))

    ;; -------------------------------------------------------------------------

    (define/public (draw dc board [x 0] [y 0])
      (define old-pen (send dc get-pen))
      (define old-brush (send dc get-brush))
      (define old-smoothing (send dc get-smoothing))
      (define old-transform (send dc get-transformation))

      (send dc set-smoothing 'smoothed)
      (send dc translate x y)

      (draw-background dc board)
      (define end-timing (timing-start 'draw-major-grid-lines))
      (draw-major-grid-lines dc 0 0)
      (end-timing)

      (send dc set-transformation old-transform)
      (send dc set-smoothing old-smoothing)
      (send dc set-brush old-brush)
      (send dc set-pen old-pen))

    (define/public (get-pict board)
      (unsafe-dc (λ (dc x y) (draw dc board x y))
                 outer-w
                 outer-h))))

;; -----------------------------------------------------------------------------

(define puzzle-renderer%
  (class renderer%
    (init-field puzzle
                [output-scale 1.0]
                [board-analysis #f])
    (init [backing-scale 1.0])
    (define specified-backing-scale backing-scale)
    (define total-scale (* output-scale backing-scale))

    (inherit freeze-to*)
    (super-new [backing-scale total-scale])

    (define board-w (board-width (puzzle-board puzzle)))
    (define board-h (board-height (puzzle-board puzzle)))
    (define board-renderer
      (new board-renderer%
           [board-width board-w]
           [board-height board-h]
           [backing-scale total-scale]))

    (define board-pict #f)
    (define rendered-puzzle-bm-box (box #f))
    (define rendered-puzzle #f)

    (define/private (render!)
      (define timing-end (timing-start 'render!))
      (set! board-pict
            (send board-renderer get-pict (puzzle-board puzzle)))

      (define clues (puzzle-clues puzzle))
      (set! rendered-puzzle
            (~> (hb-append
                 (render-axis-clues 'row
                                    (board-clues-row-clues clues)
                                    (and~> board-analysis board-analysis-row-analysis))
                 (vr-append
                  (render-axis-clues 'column
                                     (board-clues-column-clues clues)
                                     (and~> board-analysis board-analysis-column-analysis))
                  board-pict))
                (freeze-to* rendered-puzzle-bm-box)
                (inset 5)
                (scale output-scale)))
      (timing-end))

    (define/public (get-output-scale)
      output-scale)
    (define/public (get-backing-scale)
      specified-backing-scale)

    (define/public (update! new-puzzle new-board-analysis)
      (unless (and (equal? puzzle new-puzzle)
                   (equal? board-analysis new-board-analysis))
        (set! puzzle new-puzzle)
        (set! board-analysis new-board-analysis)
        (set! rendered-puzzle #f)))

    (define/public (get-render)
      (unless rendered-puzzle
        (render!))
      rendered-puzzle)

    (define/public (get-size)
      (pict-size (get-render)))

    (define/public (get-tile-at mouse-location)
      (get-render) ;; render if needed

      (define w-to-c (tf:world-to-child rendered-puzzle board-pict))
      (match-define (and tile-location (point tile-x tile-y))
        (truncate-point
         (tf* (tf:scale (/ 1 TILE-SIZE))
              w-to-c
              mouse-location)))

      (define board (puzzle-board puzzle))
      (define in-bounds?
        (and (>= tile-x 0) (< tile-x (board-width board))
             (>= tile-y 0) (< tile-y (board-height board))))

      (and in-bounds? tile-location))))

(define (get-base-puzzle-size pz)
  (send (new puzzle-renderer% [puzzle pz]) get-size))

;; -----------------------------------------------------------------------------

;; render-clue : clue? (or/c clue-analysis? 'error) -> pict?
(define (render-clue clue [analysis 'pending])
  (define p (text (number->string clue) '() CLUE-SIZE))
  (match analysis
    ['pending p]
    ['done (colorize p CLUE-DONE-COLOR)]
    ['error (colorize p CLUE-ERROR-COLOR)]))

;; render-clue : axis? clue-line? (or/c clue-line-analysis? #f) -> pict?
(define (render-line-clues axis line-clues line-analysis)
  (define clue-picts
    (for/list ([clue (in-list (if (empty? line-clues)
                                  '(0)
                                  line-clues))]
               [analysis (match line-analysis
                           [#f (in-cycle '(pending))]
                           ['done (in-cycle '(done))]
                           ['error (in-cycle '(error))]
                           [_ (in-list line-analysis)])])
      (render-clue clue analysis)))
  (match axis
    ['row    (hc-append (apply hc-append CLUE-GAP clue-picts)
                        (blank CLUE-BOARD-GAP TILE-SIZE))]
    ['column (vc-append (apply vc-append clue-picts)
                        (blank TILE-SIZE 0))]))

;; render-clue-axis : axis? axis-clues? (or/c clue-axis-analysis? #f) -> pict?
(define (render-axis-clues axis axis-clues [axis-analysis #f])
  (define line-picts
    (for/list ([clue-line (in-array axis-clues)]
               [line-analysis (if axis-analysis
                                  (in-array axis-analysis)
                                  (in-cycle '(#f)))])
      (render-line-clues axis clue-line line-analysis)))
  (~> (match axis
        ['row    (apply vr-append line-picts)]
        ['column (apply hb-append line-picts)])
      (time-pict 'render-axis-clues)))
