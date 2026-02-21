#lang racket/base

(require pict
         racket/class
         racket/contract
         racket/gui/base
         racket/match
         threading
         "analyze.rkt"
         "core.rkt"
         "geometry.rkt"
         "render.rkt")

;; -----------------------------------------------------------------------------

(define drag-mode? (or/c #f tile? 'unmark))

(struct world
  (puzzle
   board-analysis
   drag-mode)
  #:transparent)

(define (make-world puzzle)
  (world puzzle
         (analyze-puzzle puzzle)
         #f))

;; drag-mode? board? natural? natural? -> board?
(define (on-mouse-drag mode old-board tile-x tile-y)
  (define old-tile (board-ref old-board tile-x tile-y))
  (define new-tile
    (match* {mode old-tile}
      [{'empty  _                } 'empty]
      [{'unmark 'mark            } 'empty]
      [{'full   (or 'empty 'mark)} 'full]
      [{'cross  (or 'empty 'mark)} 'cross]
      [{'mark   'empty           } 'mark]
      [{_       _                } old-tile]))
  (board-set old-board tile-x tile-y new-tile))

;; world? mouse-event-type? natural? natural? -> world?
(define (on-mouse-event wld1 event-type tile-x tile-y)
  ;; process drag mode changes
  (define wld2
    (match event-type
      [(or 'left-down 'middle-down 'right-down)
       (define pp (world-puzzle wld1))
       (define old-board (puzzle-board pp))
       (define old-tile (board-ref old-board tile-x tile-y))
       (define new-drag-mode
         (match* {old-tile event-type}
           [{(or 'empty 'mark) 'left-down}   'full]
           [{(or 'empty 'mark) 'right-down}  'cross]
           [{'empty            'middle-down} 'mark]
           [{'mark             'middle-down} 'unmark]
           [{(or 'full 'cross) _}            'empty]))
       (struct-copy world wld1 [drag-mode new-drag-mode])]
      [(or 'left-up 'middle-up 'right-up)
       (struct-copy world wld1 [drag-mode #f])]
      [_ wld1]))

  ;; handle mouse down event board changes
  (match event-type
    [(or 'left-down 'middle-down 'right-down 'motion)
     #:when (world-drag-mode wld2)
     (define drag-mode (world-drag-mode wld2))
     (define old-puzzle (world-puzzle wld2))
     (define old-board (puzzle-board old-puzzle))
     (define new-board (on-mouse-drag drag-mode old-board tile-x tile-y))
     (cond
       [(equal? old-board new-board)
        wld2]
       [else
        (define new-puzzle (struct-copy puzzle old-puzzle [board new-board]))
        (struct-copy world wld2
                     [puzzle new-puzzle]
                     [board-analysis (analyze-puzzle new-puzzle)])])]
    [_ wld2]))

;; -----------------------------------------------------------------------------

(define (run initial-puzzle)
  (define frame (new frame% [label "Pictcross"]))

  (new
   (class canvas%
     (inherit get-dc)

     (define mouse-location #f)
     (define last-cursor-update #f)

     (define wld (make-world initial-puzzle))
     (define puzzle-renderer #f)

     (define/override (on-event event)
       (when puzzle-renderer
         (define event-type (send event get-event-type))
         (define location (point (send event get-x)
                                 (send event get-y)))
         (match event-type
           ['motion
            (set! mouse-location location)
            (when (or (not last-cursor-update)
                      (>= (- (current-inexact-monotonic-milliseconds) last-cursor-update)
                          (/ 1000 60)))
              (set! last-cursor-update (current-inexact-monotonic-milliseconds))
              (refresh))]
           ['leave
            (set! mouse-location #f)
            (refresh)]
           [_ (void)])

         (define tile-point (send puzzle-renderer get-tile-at location))
         (when (or tile-point (memq event-type '(left-up middle-up right-up)))
           (define new-wld (on-mouse-event wld
                                           event-type
                                           (and~> tile-point point-x)
                                           (and~> tile-point point-y)))
           (unless (equal? wld new-wld)
             (set! wld new-wld)
             (refresh)))))

     (define/private (update-renderer!)
       (define backing-scale (send (get-dc) get-backing-scale))
       (cond
         [(or (not puzzle-renderer)
              (not (= backing-scale (send puzzle-renderer get-backing-scale))))
          (set! puzzle-renderer
                (new puzzle-renderer%
                     [puzzle (world-puzzle wld)]
                     [board-analysis (world-board-analysis wld)]
                     [output-scale 2.0]
                     [backing-scale backing-scale]))]
         [else
          (send puzzle-renderer update!
                (world-puzzle wld)
                (world-board-analysis wld))]))

     (define/private (paint dc)
       (update-renderer!)
       (send dc set-smoothing 'smoothed)
       (draw-pict (send puzzle-renderer get-render) dc 0 0)
       (match mouse-location
         [(point x y)
          (draw-pict (cellophane (colorize (disk 10) "red") 0.5) dc (- x 5) (- y 5))]
         [_ (void)]))

     (define/override (refresh)
       (collect-garbage 'incremental)
       (super refresh))

     (super-new
      [parent frame]
      [min-width 300]
      [min-height 300]
      [paint-callback
       (λ (canvas dc) (paint dc))])))

  (send frame show #t))

(module+ main
  (require (submod "core.rkt" example))
  (run puzzle-s5-061))
