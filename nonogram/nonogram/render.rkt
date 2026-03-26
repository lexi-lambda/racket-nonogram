#lang racket/base

(require opengl
         racket/class
         racket/contract
         racket/flonum
         racket/format
         racket/gui/base
         racket/list
         racket/match
         racket/math
         toolbox/format
         threading
         "core.rkt"
         "lib/array.rkt"
         "lib/atlas.rkt"
         (except-in "lib/geometry.rkt" pict-size)
         "lib/gl/core.rkt"
         "lib/gl/dc.rkt"
         "lib/gl/pict.rkt"
         "render/constants.rkt"
         "render/sprites.rkt"
         "solve.rkt")

(provide default-gl-config
         make-gl-config
         (contract-out
          [puzzle-renderer%
           (class/c
            (init-field [gl-context (is-a?/c gl-context<%>)]
                        [puzzle puzzle?]
                        [board-analysis (or/c board-analysis? #f)]
                        [show-fps? boolean?])
            [set-size! (->m exact-positive-integer? exact-positive-integer? void?)]
            [set-puzzle! (->m puzzle? void?)]
            [set-board-analysis! (->m (or/c board-analysis? #f) void?)]
            [set-cursor-locations! (->m (hash/c natural? integer-point? #:immutable #t) void?)]
            [set-show-fps?! (->m any/c void?)]
            [get-tile-at (->m point? (or/c integer-point? #f))]
            [render! (->m void?)])]))

;; -----------------------------------------------------------------------------

;; render-tiles : board? -> pict?
(define (render-tiles b)
  (launder
   (for/fold ([rows-p (blank)])
             ([row (in-array (board-rows b))])
     (vl-append
      rows-p
      (for/fold ([row-p (blank)])
                ([cell (in-array row)])
        (ht-append row-p (tile cell)))))))

;; -----------------------------------------------------------------------------

;; clue-analysis-color : (or/c clue-analysis? 'error #f) -> color?
(define (clue-analysis-color analysis)
  (match analysis
    [(or #f 'pending) CLUE-PENDING-COLOR]
    ['done CLUE-DONE-COLOR]
    ['error CLUE-ERROR-COLOR]))

;; render-clue/single : clue? (or/c clue-analysis? 'error #f) -> pict?
(define (render-clue/single n [analysis #f])
  (clue n #:color (clue-analysis-color analysis)))

;; render-clue/mega : axis? clue? (or/c clue-analysis? 'error #f) -> pict?
(define (render-clue/mega axis n [analysis #f])
  (mega-clue axis n #:color (clue-analysis-color analysis)))

;; render-line-clues/single : axis? single-line-clues? (or/c single-line-analysis? #f) -> pict?
(define (render-line-clues/single axis line-clues [line-analysis #f])
  (define clue-picts
    (for/list ([clue (in-list line-clues)]
               [analysis (if (list? line-analysis)
                             (in-list line-analysis)
                             (in-cycle (list line-analysis)))])
      (render-clue/single clue analysis)))
  (match axis
    ['row    (hc-append (apply hc-append clue-picts #:gap CLUE-GAP)
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
    ['row    (hc-append (apply hc-append chunk-picts #:gap MEGA-CLUE-GAP)
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
  (define border-margin (/ GRID-BORDER-WIDTH 2))
  (~> (match axis
        ['row    (~> (apply vr-append line-picts)
                     (inset 0 0 CLUE-BOARD-GAP 0)
                     (inset 0 border-margin))]
        ['column (~> (apply hb-append line-picts)
                     (inset border-margin 0))])
      launder))

;; -----------------------------------------------------------------------------

;; A transformation that places (0, 0) at the top left of the viewport and
;; (1, 1) at the bottom right.
(define tf:base-view
  (tf* (tf:translate -1.0 1.0)
       (tf:scale 2.0 -2.0)))

(define puzzle-renderer%
  (class object%
    (init-field gl-context
                [{pz puzzle}]
                [board-analysis #f]
                [show-fps? #f])
    (define cursor-locations (hasheqv))
    (define grouped-cursor-locations (hash))
    (super-new)

    (define/private (with-gl-context thunk)
      (send gl-context call-as-current thunk))
    (define/private (swap-gl-buffers)
      (send gl-context swap-buffers))

    (define-values [prog:dc atlas-texture]
      (with-gl-context
       (λ ()
         (glEnable GL_MULTISAMPLE)
         (glClearColor 1.0 1.0 1.0 1.0)
         (values (link-gl-dc-program)
                 (make-gl-texture)))))

    ;; -------------------------------------------------------------------------
    ;; viewport

    (define tf:view tf:base-view)
    (define viewport-p (blank 1.0 1.0))

    (define/public (set-size! width height)
      (define width.0 (->fl width))
      (define height.0 (->fl height))
      (set! tf:view (tf* tf:base-view
                         (tf:scale (/ width.0) (/ height.0))))
      (set! viewport-p (blank width.0 height.0))
      (with-gl-context
       (λ () (glViewport 0 0 width height)))
      (set-board-dirty!))

    ;; -------------------------------------------------------------------------
    ;; atlas

    (define atlas-scale #f)
    (define atlas #f)

    (define/private (pack-atlas! #:scale scale)
      (set! atlas-scale scale)
      (set! atlas (sprite-atlas #:scale scale))
      (gl-texture-load-bitmap! atlas-texture (atlas-bitmap atlas)))

    ;; -------------------------------------------------------------------------
    ;; layer contexts

    (define board-bg-p #f)
    (define board-grid-p #f)

    (define/private (build-static-picts!)
      (define board-w (board-width (puzzle-board pz)))
      (define board-h (board-height (puzzle-board pz)))
      (set! board-bg-p (board-background board-w board-h))
      (set! board-grid-p (board-grid board-w board-h)))

    (build-static-picts!)

    (define board-bg-dc #f)
    (define board-dc #f)
    (define board-grid-dc #f)
    (define overlay-dc #f)

    (define/private (create-layer-dcs!)
      (define (make-one #:usage [buffer-usage GL_STREAM_DRAW])
        (make-gl-dc #:atlas atlas
                    #:texture atlas-texture
                    #:usage buffer-usage))

      (define (make-static p)
        (define dc (make-one #:usage GL_STATIC_DRAW))
        ((pict-draw p) dc)
        dc)

      (set! board-bg-dc (make-static board-bg-p))
      (set! board-grid-dc (make-static board-grid-p))
      (set! board-dc (make-one #:usage GL_DYNAMIC_DRAW))
      (set! overlay-dc (make-one)))

    ;; -------------------------------------------------------------------------
    ;; state updates

    (define dirty? #t)
    (define board-p #f)

    (define/private (set-board-dirty!)
      (set! dirty? #t)
      (set! board-p #f))

    (define/public (set-puzzle! new-pz)
      (define old-pz pz)
      (set! pz new-pz)
      (cond
        [(not (equal? (puzzle-clues old-pz) (puzzle-clues new-pz)))
         (set! board-analysis #f)
         (build-static-picts!)
         (create-layer-dcs!)
         (set-board-dirty!)]
        [(not (equal? (puzzle-board old-pz) (puzzle-board new-pz)))
         (set-board-dirty!)]))

    (define/public (set-board-analysis! new-analysis)
      (unless (equal? board-analysis new-analysis)
        (set! board-analysis new-analysis)
        (set-board-dirty!)))

    (define/public (set-cursor-locations! new-locations)
      (unless (equal? cursor-locations new-locations)
        (set! cursor-locations new-locations)
        ;; Invert mapping from `client-id => location` to `location => (listof client-id)`
        ;; to allow rendering overlapping cursors specially.
        (set! grouped-cursor-locations
              (for/foldr ([cursor-locations (hash)])
                         ([client-id+location (in-list (hash->list new-locations #t))])
                (match-define (cons client-id location) client-id+location)
                (hash-update cursor-locations
                             location
                             (λ~> (cons client-id _))
                             '())))
        (set! dirty? #t)))

    (define/public (set-show-fps?! new-show-fps?)
      (set! show-fps? (and new-show-fps? #t)))

    ;; -------------------------------------------------------------------------
    ;; coordinate mapping

    (define tf:viewport-to-tile tf:identity)

    (define/public (get-tile-at loc)
      (define tile-loc (floor-point (tf* tf:viewport-to-tile loc)))
      (match-define (point tx ty) tile-loc)
      (define brd (puzzle-board pz))
      (and (>= tx 0) (< tx (board-width brd))
           (>= ty 0) (< ty (board-height brd))
           tile-loc))

    ;; -------------------------------------------------------------------------
    ;; render

    (define last-frame-ms #f)
    (define frame-ms #f)

    (define/private (fps-overlay)
      (define fps-p
        (text (~a (if (and frame-ms (not (zero? frame-ms)))
                      (~r* (/ 1000.0 frame-ms) #:precision 0)
                      "--")
                  " fps")))

      (define frame-ms-p
        (text (~a (if frame-ms (~r* frame-ms #:precision '(= 1)) "--") " ms")))

      (vr-append (scale fps-p 1.25) frame-ms-p))

    (define/public (render!)
      (when (or dirty? show-fps?)
        (set! dirty? #f)
        (with-gl-context
         (λ ()
           ;; assemble board and clues
           (define board-dirty? (not board-p))
           (when board-dirty?
             (define tiles-p (render-tiles (puzzle-board pz)))
             (define row-clues-p
               (render-axis-clues 'row
                                  (board-clues-row-clues (puzzle-clues pz))
                                  (and~> board-analysis board-analysis-row-analysis)))
             (define column-clues-p
               (render-axis-clues 'column
                                  (board-clues-column-clues (puzzle-clues pz))
                                  (and~> board-analysis board-analysis-column-analysis)))
             (set! board-p
                   (~> (cc-superimpose (ghost board-bg-p)
                                       tiles-p
                                       (ghost board-grid-p))
                       (hb-append row-clues-p _)
                       (vr-append column-clues-p _)
                       (inset 2))))

           ;; assemble scene
           (define scene-p board-p)
           (define scaled-scene-p (scale-to-fit scene-p viewport-p))
           (define centered-scene-p (cc-superimpose viewport-p scaled-scene-p))
           (define scene-scale (/ (pict-width scaled-scene-p) (pict-width scene-p)))
           (define tf:tile-to-viewport (tf:tile-to-scene centered-scene-p board-bg-p))
           (set! tf:viewport-to-tile (tf-invert tf:tile-to-viewport))

           ;; repack atlas if necessary
           (define target-atlas-scale (max 2 (inexact->exact (ceiling scene-scale))))
           (unless (and atlas-scale (= atlas-scale target-atlas-scale))
             (pack-atlas! #:scale target-atlas-scale)
             (create-layer-dcs!))

           ;; assemble overlays
           (define overlay-p
             (~> (for/fold ([p viewport-p])
                           ([(tile-location client-ids) (in-immutable-hash grouped-cursor-locations)])
                   ;; TODO: overlapping cursors
                   (pin-over p #:hole cc-find
                             (scale (cursor (first client-ids)) scene-scale)
                             (tf* tf:tile-to-viewport
                                  (tf:translate 0.5 0.5)
                                  tile-location)))
                 (when~> show-fps?
                   (pin-over (scale (fps-overlay) 2)
                             rt-find #:hole rt-find))))

           ;; upload vertex data
           (when board-dirty?
             (gl-dc-clear! board-dc)
             ((pict-draw centered-scene-p) board-dc))
           (gl-dc-clear! overlay-dc)
           ((pict-draw overlay-p) overlay-dc)

           ;; draw
           (glClear GL_COLOR_BUFFER_BIT)
           (glEnable GL_BLEND)
           (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)

           (use-gl-dc-program! prog:dc)
           (define (bind-transform! [p centered-scene-p])
             (gl-dc-program-bind!
              prog:dc
              #:transform (tf* tf:view (tf:child-to-parent centered-scene-p p))))

           (bind-transform! board-bg-p)
           (gl-dc-draw board-bg-dc)
           (bind-transform!)
           (gl-dc-draw board-dc)
           (bind-transform! board-grid-p)
           (gl-dc-draw board-grid-dc)

           (bind-transform!)
           (gl-dc-draw overlay-dc)

           (swap-gl-buffers))))

      (define now-ms (current-inexact-monotonic-milliseconds))
      (when last-frame-ms
        (define this-frame-ms (- now-ms last-frame-ms))
        (set! frame-ms (if frame-ms
                           (+ (* frame-ms 0.99) (* this-frame-ms 0.01))
                           this-frame-ms)))
      (set! last-frame-ms now-ms))))
