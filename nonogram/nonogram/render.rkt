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
(define (render-clue/mega axis n [analysis #f]
                          #:margin-before? [margin-before? #f]
                          #:margin-after? [margin-after? #f])
  (define row? (eq? axis 'row))
  (~> (mega-clue axis n #:color (clue-analysis-color analysis))
      (when~> margin-before?
        (cond~>
          [row? (inset MEGA-CLUE-MARGIN 0 0 0)]
          [else (inset 0 MEGA-CLUE-MARGIN 0 0)]))
      (when~> margin-after?
        (cond~>
          [row? (inset 0 0 MEGA-CLUE-MARGIN 0)]
          [else (inset 0 0 0 MEGA-CLUE-MARGIN)]))))

;; render-line-clues/single : axis? single-line-clues? (or/c single-line-analysis? #f) -> pict?
(define (render-line-clues/single axis line-clues [line-analysis #f]
                                  #:margin-before? [margin-before? #t]
                                  #:margin-after? [margin-after? #t])
  (define clue-picts
    (for/list ([clue (in-list line-clues)]
               [analysis (if (list? line-analysis)
                             (in-list line-analysis)
                             (in-cycle (list line-analysis)))])
      (render-clue/single clue analysis)))
  (match axis
    ['row    (~> (hc-append (apply hc-append clue-picts #:gap CLUE-GAP)
                            (blank 0 TILE-SIZE))
                 (inset (if margin-before? CLUE-BOARD-GAP 0) 0
                        (if margin-after? CLUE-BOARD-GAP 0) 0))]
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
      (define at-start? (= i 0))
      (define at-end? (= (add1 i) num-chunks))
      (match chunk
        [(array clues-0 clues-1)
         (define-values [analysis-0 analysis-1]
           (if (array? analysis)
               (values (array-ref analysis 0) (array-ref analysis 1))
               (values analysis analysis)))
         (define p0 (render-line-clues/single axis clues-0 analysis-0
                                              #:margin-before? at-start?
                                              #:margin-after? at-end?))
         (define p1 (render-line-clues/single axis clues-1 analysis-1
                                              #:margin-before? at-start?
                                              #:margin-after? at-end?))
         (match axis
           ['row    (vr-append p0 p1)]
           ['column (hb-append p0 p1)])]

        [clue
         (render-clue/mega
          axis clue analysis
          #:margin-before? at-start?
          #:margin-after? (or at-end?
                              (and (eq? axis 'column)
                                   (clue? (array-ref line-clues (add1 i))))))])))

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

;; render-axis-clues : axis? axis-clues? (or/c clue-axis-analysis? #f) -> pict?
(define (render-axis-clues axis axis-clues [axis-analysis #f])
  (define line-picts
    (for/list ([clue-line (in-array axis-clues)]
               [line-analysis (if axis-analysis
                                  (in-array axis-analysis)
                                  (in-cycle '(#f)))])
      (render-line-clues axis clue-line line-analysis)))
  (launder
   (match axis
     ['row    (apply vr-append line-picts)]
     ['column (apply hb-append line-picts)])))

;; render-axis-clue-underlays
;;   : axis? axis-clues? pict?
;;     #:analysis (or/c clue-axis-analysis? #f)
;;     #:cursor-lines (hash/c natural? (non-empty-listof natural?))
;;  -> pict? (hash/c natural? (non-empty-listof pict?))
(define (render-axis-clue-underlays axis axis-clues clues-p
                                    #:cursor-lines [cursor-lines (hasheqv)])
  (define main-size
    (match axis
      ['row    (pict-width clues-p)]
      ['column (pict-height clues-p)]))

  (define (line-underlay-ps i sibling-i)
    (define primary-ids (hash-ref cursor-lines i '()))
    (define secondary-ids (if sibling-i (hash-ref cursor-lines sibling-i '()) '()))

    (define colors
      (if (and (empty? primary-ids)
               (empty? secondary-ids))
          (list (tile-empty-color i))
          (append (map clue-underlay-highlight-color primary-ids)
                  (map clue-underlay-secondary-highlight-color secondary-ids))))

    (for/list ([color (in-list colors)])
      (clue-underlay axis main-size #:color color)))

  (define cross-append
    (match axis
      ['row    vl-append]
      ['column ht-append]))

  (for/fold ([i 0]
             [underlays-p (blank)]
             [hatched-underlays-ps (hash)]
             #:result (values underlays-p hatched-underlays-ps))
            ([clues (in-array axis-clues)])
    (match-define (cons solid-p hatched-ps)
      (match (line-clues-type clues)
        ['single
         (line-underlay-ps i #f)]
        ['mega
         (map cross-append
              (line-underlay-ps i (add1 i))
              (line-underlay-ps (add1 i) i))]))

    (define underlays-p* (cross-append underlays-p solid-p))
    (define divisions (add1 (length hatched-ps)))
    (define solid-p-loc (lt-find underlays-p* solid-p))

    (values (+ i (line-clues-span clues))
            underlays-p*
            (for/fold ([hatched-underlays-ps hatched-underlays-ps])
                      ([hatched-p (in-list hatched-ps)]
                       [i (in-naturals 1)])
              (hash-update hatched-underlays-ps
                           (cons divisions i)
                           (λ~> (pin hatched-p solid-p-loc) launder)
                           blank)))))

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
    (define cursor-rows (hasheqv))
    (define cursor-columns (hasheqv))

    (super-new)

    (define/private (with-gl-context thunk)
      ; work around racket/draw#60
      (if (get-current-gl-context)
          (thunk)
          (send gl-context call-as-current thunk)))
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
    (define stream-dc #f)

    (define/private (create-layer-dcs!)
      (with-gl-context
       (λ ()
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
         (set! stream-dc (make-one)))))

    ;; -------------------------------------------------------------------------
    ;; state updates

    (define dirty? #t)
    (define board-p #f)
    (define row-clues-p #f)
    (define column-clues-p #f)

    (define/private (set-board-dirty!)
      (set! board-p #f)
      (set! row-clues-p #f)
      (set! column-clues-p #f)
      (set! dirty? #t))

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
        ;; to allow rendering overlapping cursors specially. Also, accumulate mappings
        ;; from row/column indexes to cursors for highlighting selected clue lines.
        (set!-values
         [grouped-cursor-locations cursor-rows cursor-columns]
         (for/foldr ([grouped-locations (hash)]
                     [cursor-rows (hasheqv)]
                     [cursor-columns (hasheqv)])
                    ([client-id+location (in-list (hash->list new-locations #t))])
           (match-define (cons client-id location) client-id+location)
           (define add-id (λ~> (cons client-id _)))
           (values (hash-update grouped-locations location add-id '())
                   (hash-update cursor-rows (point-y location) add-id '())
                   (hash-update cursor-columns (point-x location) add-id '()))))
        (set! dirty? #t)))

    (define/public (set-show-fps?! new-show-fps?)
      (set! show-fps? (and new-show-fps? #t))
      (set! dirty? #t))

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
           (when (= (glCheckFramebufferStatus GL_DRAW_FRAMEBUFFER) GL_FRAMEBUFFER_COMPLETE)
             ;; assemble board and clues
             (define board-dirty? (not board-p))
             (define clues (puzzle-clues pz))
             (when board-dirty?
               (define tiles-p (render-tiles (puzzle-board pz)))
               (set! row-clues-p
                     (render-axis-clues 'row
                                        (board-clues-row-clues clues)
                                        (and~> board-analysis board-analysis-row-analysis)))
               (set! column-clues-p
                     (render-axis-clues 'column
                                        (board-clues-column-clues clues)
                                        (and~> board-analysis board-analysis-column-analysis)))
               (set! board-p
                     (~> (cc-superimpose (ghost board-bg-p)
                                         tiles-p
                                         (ghost board-grid-p))
                         (pin row-clues-p (λ~> (lt-find board-bg-p)) #:hole rt-find #:extend? #t)
                         (pin column-clues-p (λ~> (lt-find board-bg-p)) #:hole lb-find #:extend? #t))))

             (define-values [row-underlays-p row-hatched-underlays-ps]
               (render-axis-clue-underlays 'row
                                           (board-clues-row-clues clues)
                                           row-clues-p
                                           #:cursor-lines cursor-rows))
             (define-values [column-underlays-p column-hatched-underlays-ps]
               (render-axis-clue-underlays 'column
                                           (board-clues-column-clues clues)
                                           column-clues-p
                                           #:cursor-lines cursor-columns))

             ;; assemble scene
             (define scene-p
               (~> board-p
                   (pin (ghost row-underlays-p) (λ~> (rt-find row-clues-p))
                        #:hole rt-find #:extend? #t #:over? #f)
                   (pin (ghost column-underlays-p) (λ~> (lb-find column-clues-p))
                        #:hole lb-find #:extend? #t #:over? #f)
                   (inset 2)))
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

             ;; assemble underlays
             (define (build-underlays-p #:rows [rows-p #f] #:columns [columns-p #f])
               (~> (ghost scene-p)
                   (when~> rows-p
                     (pin rows-p (λ~> (lt-find row-underlays-p))))
                   (when~> columns-p
                     (pin columns-p (λ~> (lt-find column-underlays-p))))))

             (define solid-underlays-p
               (build-underlays-p #:rows row-underlays-p
                                  #:columns column-underlays-p))

             ;; assemble overlays
             (define-values [plain-cursors-p hatched-cursors]
               (for/fold ([plain-p viewport-p]
                          [hatched-cursors '()])
                         ([(tile-loc client-ids) (in-immutable-hash grouped-cursor-locations)])
                 ; render the first cursor normally
                 (define cursor-p (cursor (first client-ids)))
                 (define plain-p*
                   (pin (launder plain-p)
                        (scale cursor-p scene-scale)
                        (tf* tf:tile-to-viewport
                             (tf:translate 0.5 0.5)
                             tile-loc)
                        #:hole cc-find))
                 (values plain-p*
                         (if (empty? (rest client-ids))
                             hatched-cursors
                             ; accumulate any subsequent cursors for hatched rendering
                             (cons (cons (tf:child-to-parent plain-p* cursor-p)
                                         (map cursor (rest client-ids)))
                                   hatched-cursors)))))
             (define overlay-p
               (~> plain-cursors-p
                   (when~> show-fps?
                     (pin (scale (fps-overlay) 2)
                          rt-find #:hole rt-find))))

             ;; upload vertex data
             (when board-dirty?
               (gl-dc-clear! board-dc)
               ((pict-draw centered-scene-p) board-dc))
             (gl-dc-clear! stream-dc)
             ((pict-draw solid-underlays-p) stream-dc)

             ;; draw
             (glClear GL_COLOR_BUFFER_BIT)
             (glEnable GL_BLEND)
             (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)

             (use-gl-dc-program! prog:dc)
             (define (bind-transform! [p centered-scene-p])
               (gl-dc-program-bind!
                prog:dc
                #:transform (tf* tf:view (tf:child-to-parent centered-scene-p p))))

             ;; draw underlays
             (bind-transform! scene-p)
             (gl-dc-draw stream-dc)

             (define (draw-hatched-underlays hatched-lines-ps build-p)
               (for ([(divisions+index lines-p) (in-immutable-hash hatched-lines-ps)])
                 (match-define (cons divisions index) divisions+index)
                 (gl-dc-clear! stream-dc)
                 ((pict-draw (build-p lines-p)) stream-dc)
                 (gl-dc-program-bind!
                  prog:dc
                  #:hatch-divisions divisions
                  #:hatch-transform (tf:diag-hatch #:divisions divisions
                                                   #:index index
                                                   #:scale (/ CURSOR-STRIPES TILE-SIZE)))
                 (gl-dc-draw stream-dc)))

             (draw-hatched-underlays row-hatched-underlays-ps
                                     (λ~> (build-underlays-p #:rows _)))
             (draw-hatched-underlays column-hatched-underlays-ps
                                     (λ~> (build-underlays-p #:columns _)))
             (gl-dc-program-bind! prog:dc #:hatch-divisions 0)

             ;; draw board
             (bind-transform! board-bg-p)
             (gl-dc-draw board-bg-dc)
             (bind-transform!)
             (gl-dc-draw board-dc)
             (bind-transform! board-grid-p)
             (gl-dc-draw board-grid-dc)

             ;; draw overlays
             (gl-dc-clear! stream-dc)
             ((pict-draw overlay-p) stream-dc)
             (bind-transform!)
             (gl-dc-draw stream-dc)

             ;; draw hatched cursors
             (for ([hatched-cursor (in-list hatched-cursors)])
               (match-define (cons tf cursor-ps) hatched-cursor)
               (define divisions (add1 (length cursor-ps)))
               (for ([cursor-p (in-list cursor-ps)]
                     [i (in-naturals 1)])
                 (gl-dc-clear! stream-dc)
                 ((pict-draw cursor-p) stream-dc)
                 (gl-dc-program-bind!
                  prog:dc
                  #:transform (tf* tf:view tf)
                  #:hatch-divisions divisions
                  #:hatch-transform (tf:diag-hatch #:divisions divisions
                                                   #:index i
                                                   #:scale (/ CURSOR-STRIPES (pict-width cursor-p))))
                 (gl-dc-draw stream-dc)))

             (swap-gl-buffers)))))

      (define now-ms (current-inexact-monotonic-milliseconds))
      (when last-frame-ms
        (define this-frame-ms (- now-ms last-frame-ms))
        (set! frame-ms (if frame-ms
                           (+ (* frame-ms 0.99) (* this-frame-ms 0.01))
                           this-frame-ms)))
      (set! last-frame-ms now-ms))))
