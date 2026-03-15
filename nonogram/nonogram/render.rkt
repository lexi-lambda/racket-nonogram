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
         "core.rkt"
         "geometry.rkt"
         "lib/array.rkt"
         "logger.rkt"
         "solve.rkt")

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

            [get-puzzle (->m puzzle?)]
            [get-output-scale (->m real?)]
            [get-backing-scale (->m real?)]

            [update! (->m puzzle?
                          (or/c board-analysis? #f)
                          (hash/c natural? integer-point? #:immutable #t)
                          void?)]
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
(define GRID-MAJOR-COLOR (make-color #x48 #x48 #x48))
(define GRID-MINOR-COLOR (make-color #x80 #x80 #x80))
(define GRID-LINE-WIDTH 1.5)
(define GRID-BORDER-WIDTH 2)
(define GRID-TILE-RADIUS 2)

(define CURSOR-COLORS
  (array (make-color #x3d #x7b #xe0)
         (make-color #xe0 #x3d #x3d)
         (make-color #x3d #xe0 #x58)
         (make-color #xf0 #xe9 #x30)))

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

(define tile-cursor
  (let ()
    (define (color client-id)
      (array-ref CURSOR-COLORS (remainder client-id (array-length CURSOR-COLORS))))

    ;; Renders a series of diagonal stripes for use as a clipping region when
    ;; rendering overlapping cursors.
    (define (build-hatch-path divisions index)
      (define p (new dc-path%))
      (define stripes/2 3)
      (define stripes (* stripes/2 2))
      (define stripe-delta (/ 1.0 stripes/2))
      (define division-delta (/ stripe-delta divisions))
      (define offset1 (* division-delta index))
      (define offset2 (* division-delta (add1 index)))
      (for ([i (in-range stripes/2)])
        (define d1 (+ (* stripe-delta i) offset1))
        (define d2 (+ (* stripe-delta i) offset2))
        (send p move-to 0.0 d1)
        (send p line-to 0.0 d2)
        (send p line-to d2 0.0)
        (send p line-to d1 0.0)
        (send p close))
      (for ([i (in-range stripes/2)])
        (define d1 (+ (* stripe-delta i) offset1))
        (define d2 (+ (* stripe-delta i) offset2))
        (send p move-to d1 1.0)
        (send p line-to d2 1.0)
        (send p line-to 1.0 d2)
        (send p line-to 1.0 d1)
        (send p close))
      (send p scale
            (+ TILE-SIZE GRID-BORDER-WIDTH)
            (+ TILE-SIZE GRID-BORDER-WIDTH))
      (send p translate
            (- (/ GRID-BORDER-WIDTH 2))
            (- (/ GRID-BORDER-WIDTH 2)))
      p)

    ;; Memoize `hatch-path`, as the number of distinct paths should be small.
    (define hatch-paths (make-hash))
    (define (hatch-path divisions index)
      (hash-ref! hatch-paths
                 (array divisions index)
                 (λ () (build-hatch-path divisions index))))
    
    (λ (client-ids)
      (match client-ids
        ;; Simple case: just a rounded rectangle.
        [(list client-id)
         (rounded-rectangle TILE-SIZE TILE-SIZE GRID-TILE-RADIUS
                            #:border-color (color client-id)
                            #:border-width GRID-BORDER-WIDTH)]

        ;; Overlapping case: hatched rounded rectangle.
        [(cons client-id client-ids)
         (define divisions (add1 (length client-ids)))
         (define paths
           (for/list ([i (in-range 1 divisions)])
             (hatch-path divisions i)))

         (unsafe-dc
          (λ (dc x y)
            (define old-pen (send dc get-pen))
            (define old-brush (send dc get-brush))
            (define old-region (send dc get-clipping-region))
            (define old-transform (send dc get-transformation))

            (send dc set-brush (make-brush #:style 'transparent))
            (send dc translate x y)
            (define region (new region%))

            (define (stroke-rect client-id)
              (send dc set-pen (make-pen #:color (color client-id) #:width GRID-BORDER-WIDTH))
              (send dc draw-rounded-rectangle 0 0 TILE-SIZE TILE-SIZE GRID-TILE-RADIUS))

            (stroke-rect client-id)
            (for ([client-id (in-list client-ids)]
                  [path (in-list paths)])
              (send region set-path path)
              (send dc set-clipping-region region)
              (stroke-rect client-id)
              (send dc set-clipping-region #f))

            (send dc set-transformation old-transform)
            (send dc set-clipping-region old-region)
            (send dc set-brush old-brush)
            (send dc set-pen old-pen))
          TILE-SIZE
          TILE-SIZE)]))))

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

    (define cursor-locations (hasheqv))
    (define grouped-cursor-locations (hash))

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
    (define tf:tile-to-puzzle #f)
    (define tf:puzzle-to-tile #f)

    (define/private (render!)
      (define timing-end (timing-start 'render!))
      (set! board-pict
            (send board-renderer get-pict (puzzle-board puzzle)))

      (define clues (puzzle-clues puzzle))
      (set! rendered-puzzle
            (~> (hb-append
                 (~> (render-axis-clues 'row
                                        (board-clues-row-clues clues)
                                        (and~> board-analysis board-analysis-row-analysis))
                     (inset 0 (/ GRID-BORDER-WIDTH 2)))
                 (vr-append
                  (~> (render-axis-clues 'column
                                         (board-clues-column-clues clues)
                                         (and~> board-analysis board-analysis-column-analysis))
                      (inset (/ GRID-BORDER-WIDTH 2) 0))
                  board-pict))
                (freeze-to* rendered-puzzle-bm-box)
                (inset 5)
                (scale output-scale)))

      (unless tf:tile-to-puzzle
        (define t-to-p
          (tf* (tf:child-to-world rendered-puzzle board-pict)
               (tf:translate (/ GRID-BORDER-WIDTH 2)
                             (/ GRID-BORDER-WIDTH 2))
               (tf:scale TILE-SIZE)))

        (set! tf:tile-to-puzzle t-to-p)
        (set! tf:puzzle-to-tile (tf-invert t-to-p)))

      (timing-end))

    (define/private (overlay-cursors p)
      (for/fold ([p p])
                ([(tile-location client-ids) (in-immutable-hash grouped-cursor-locations)])
        (match-define (point x y) (tf* tf:tile-to-puzzle tile-location))
        (pin-over p x y (scale (tile-cursor client-ids) output-scale))))

    (define/public (get-puzzle)
      puzzle)
    (define/public (get-output-scale)
      output-scale)
    (define/public (get-backing-scale)
      specified-backing-scale)

    (define/public (update! new-puzzle new-board-analysis new-cursor-locations)
      (unless (and (equal? puzzle new-puzzle)
                   (equal? board-analysis new-board-analysis))
        (set! puzzle new-puzzle)
        (set! board-analysis new-board-analysis)
        (set! rendered-puzzle #f))
      (unless (equal? cursor-locations new-cursor-locations)
        (set! cursor-locations new-cursor-locations)
        ;; Invert mapping from `client-id => location` to `location => (listof client-id)`
        ;; to allow rendering overlapping cursors specially.
        (set! grouped-cursor-locations
              (for/foldr ([cursor-locations (hash)])
                         ([client-id+location (in-list (hash->list new-cursor-locations #t))])
                (match-define (cons client-id location) client-id+location)
                (hash-update cursor-locations
                             location
                             (λ~> (cons client-id _))
                             '())))))

    (define/public (get-render [mouse-location #f])
      (unless rendered-puzzle
        (render!))
      (overlay-cursors rendered-puzzle))

    (define/public (get-size)
      (pict-size (get-render)))

    (define/public (get-tile-at mouse-location)
      (get-render) ;; render if needed

      (match-define (and tile-location (point tile-x tile-y))
        (floor-point (tf* tf:puzzle-to-tile mouse-location)))

      (define board (puzzle-board puzzle))
      (define in-bounds?
        (and (>= tile-x 0) (< tile-x (board-width board))
             (>= tile-y 0) (< tile-y (board-height board))))

      (and in-bounds? tile-location))))

(define (get-base-puzzle-size pz)
  (send (new puzzle-renderer% [puzzle pz]) get-size))

;; -----------------------------------------------------------------------------

;; clue-analysis-color : (or/c clue-analysis? 'error #f) -> color?
(define (clue-analysis-color analysis)
  (match analysis
    [(or #f 'pending) CLUE-PENDING-COLOR]
    ['done CLUE-DONE-COLOR]
    ['error CLUE-ERROR-COLOR]))

(define CLUE-FONT
  (make-font #:size CLUE-SIZE
             #:size-in-pixels? #t
             #:font-list the-font-list
             #:hinting 'unaligned))

(define MEGA-CLUE-FONT
  (make-font #:size MEGA-CLUE-SIZE
             #:size-in-pixels? #t
             #:font-list the-font-list
             #:hinting 'unaligned))

;; render-clue/single : clue? (or/c clue-analysis? 'error #f) -> pict?
(define (render-clue/single clue [analysis #f])
  (~> (text (number->string clue) CLUE-FONT)
      (colorize (clue-analysis-color analysis))))

;; render-clue/mega : axis? clue? (or/c clue-analysis? 'error #f) -> pict?
(define (render-clue/mega axis clue [analysis #f])
  (define clue-str (number->string clue))

  (define font (if (or (eq? axis 'column) (= clue 2))
                   CLUE-FONT
                   MEGA-CLUE-FONT))
  (define text-p (text clue-str font))
  (define text-w (pict-width text-p))
  (define text-h (pict-height text-p))

  (define-values [total-w total-h text-x text-y]
    (match axis
      ['row
       (define padding (if (= clue 2) MEGA-CLUE-TWO-PADDING MEGA-CLUE-PADDING))
       (define total-h (* (- TILE-SIZE MEGA-CLUE-MARGIN) 2))
       (values (+ text-w (* padding 2))
               total-h
               padding
               (/ (- total-h text-h) 2))]
      ['column
       (define total-w (* (- TILE-SIZE MEGA-CLUE-MARGIN) 2))
       (values total-w
               text-h
               (/ (- total-w text-w) 2)
               0)]))

  (define path (new dc-path%))
  (send path rounded-rectangle 0 0 total-w total-h MEGA-CLUE-RADIUS)
  (send path text-outline font clue-str text-x text-y)

  (define pen (make-pen #:style 'transparent))
  (define brush (make-brush #:color (clue-analysis-color analysis)))
  (unsafe-dc
   (λ (dc x y)
     (define old-pen (send dc get-pen))
     (define old-brush (send dc get-brush))
     (send dc set-pen pen)
     (send dc set-brush brush)
     (send dc draw-path path x y)
     (send dc set-brush old-brush)
     (send dc set-pen old-pen))
   total-w
   total-h))

;; render-line-clues/single : axis? single-line-clues? (or/c single-line-analysis? #f) -> pict?
(define (render-line-clues/single axis line-clues [line-analysis #f])
  (define clue-picts
    (for/list ([clue (in-list line-clues)]
               [analysis (if (list? line-analysis)
                             (in-list line-analysis)
                             (in-cycle (list line-analysis)))])
      (render-clue/single clue analysis)))
  (match axis
    ['row    (hc-append (apply hc-append CLUE-GAP clue-picts)
                        (blank 0 TILE-SIZE))]
    ['column (vc-append (apply vc-append clue-picts)
                        (blank TILE-SIZE 0))]))

;; render-line-clues/mega : axis? mega-line-clues? (or/c mega-line-analysis? #f) -> pict?
(define (render-line-clues/mega axis line-clues-lst [line-analysis #f])
  (define line-clues (list->array line-clues-lst))
  (define num-chunks (array-length line-clues))
  (define chunk-picts
    (for/list ([(chunk i) (in-indexed (in-array line-clues))]
               [analysis (if (list? line-analysis)
                             (in-list line-analysis)
                             (in-cycle (list line-analysis)))])
      (match chunk
        [(array clues-0 clues-1)
         (define-values [analysis-0 analysis-1]
           (if (array? analysis)
               (values (array-ref analysis 0) (array-ref analysis 1))
               (values analysis analysis)))
         (define p0 (render-line-clues/single axis clues-0 analysis-0))
         (define p1 (render-line-clues/single axis clues-1 analysis-1))
         (match axis
           ['row    (vr-append p0 p1)]
           ['column (hb-append p0 p1)])]

        [clue
         (~> (render-clue/mega axis clue analysis)
             (when~> (and (eq? axis 'column)
                          (or (= (add1 i) num-chunks)
                              (clue? (array-ref line-clues (add1 i)))))
               (inset 0 0 0 MEGA-CLUE-MARGIN)))])))

  (match axis
    ['row    (hc-append (apply hc-append MEGA-CLUE-GAP chunk-picts)
                        (blank 0 (* TILE-SIZE 2)))]
    ['column (vc-append (apply vc-append chunk-picts)
                        (blank (* TILE-SIZE 2) 0))]))

;; render-line-clues : axis? line-clues? (or/c line-clue-analysis? #f) -> pict?
(define (render-line-clues axis clues [line-analysis #f])
  (match-define (line-clues type clues*) clues)
  (match type
    ['single (render-line-clues/single axis (if (empty? clues*) '(0) clues*) line-analysis)]
    ['mega (render-line-clues/mega axis clues* line-analysis)]))

;; render-clue-axis : axis? axis-clues? (or/c clue-axis-analysis? #f) -> pict?
(define (render-axis-clues axis axis-clues [axis-analysis #f])
  (define line-picts
    (for/list ([clue-line (in-array axis-clues)]
               [line-analysis (if axis-analysis
                                  (in-array axis-analysis)
                                  (in-cycle '(#f)))])
      (render-line-clues axis clue-line line-analysis)))
  (~> (match axis
        ['row    (~> (apply vr-append line-picts)
                     (inset 0 0 CLUE-BOARD-GAP 0))]
        ['column (apply hb-append line-picts)])
      (time-pict 'render-axis-clues)))
