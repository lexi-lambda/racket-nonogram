#lang racket/base

(require pict
         racket/class
         racket/contract
         racket/gui/base
         racket/match
         threading
         toolbox/logging
         "analyze.rkt"
         "core.rkt"
         "geometry.rkt"
         "logger.rkt"
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

(struct modifier-keys
  (shift-down?
   control-down? ; Control, not Command, on macOS
   alt-down?)    ; Option on macOS
  #:transparent)

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

;; world? mouse-event-type? modifier-keys? (or/c natural? #f) (or/c natural? #f) -> world?
(define (on-mouse-event wld1 event-type modifiers tile-x tile-y)
  (define timing-end (timing-start 'on-mouse-event))

  (match-define (modifier-keys shift? control? alt?) modifiers)
  (define action-type
    (match* {event-type shift? control? alt?}
      [{'left-down   #f #f #f} '(start full)]
      [{'left-down   #f #t #f} '(start cross)]
      [{'left-down   #f #f #t} '(start mark)]
      [{'right-down  _  _  #f} '(start cross)]
      [{'right-down  _  _  #t} '(start mark)]
      [{'middle-down _  _  _ } '(start mark)]
      [{(or 'left-up 'middle-up 'right-up) _ _ _} 'end]
      [{'motion _ _ _} 'drag]
      [{_ _ _ _} #f]))

  ;; process drag mode changes
  (define wld2
    (match action-type
      [(list 'start start-type)
       (define pp (world-puzzle wld1))
       (define old-board (puzzle-board pp))
       (define old-tile (board-ref old-board tile-x tile-y))
       (define new-drag-mode
         (match* {old-tile start-type}
           [{(or 'empty 'mark) 'full } 'full]
           [{(or 'empty 'mark) 'cross} 'cross]
           [{'empty            'mark } 'mark]
           [{'mark             'mark } 'unmark]
           [{(or 'full 'cross) _     } 'empty]))
       (struct-copy world wld1 [drag-mode new-drag-mode])]
      ['end
       (struct-copy world wld1 [drag-mode #f])]
      [_ wld1]))

  ;; handle mouse down event board changes
  (match action-type
    [(or (list 'start _) 'drag)
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
        (define new-board-analysis
          (reanalyze-lines-at new-puzzle (world-board-analysis wld2) (point tile-x tile-y)))
        (timing-end)
        (struct-copy world wld2
                     [puzzle new-puzzle]
                     [board-analysis new-board-analysis])])]
    [_ wld2]))

;; -----------------------------------------------------------------------------

(define (run initial-puzzle)
  (define frame
    (new
     (class frame%
       (super-new [label "Pictcross"]
                  [width 800]
                  [height 600])
       (define/augment (on-close)
         (send refresh-timer stop)))))

  (define base-size (get-base-puzzle-size initial-puzzle))
  (define canvas
    (new
     (class canvas%
       (inherit get-client-size
                get-dc
                refresh
                refresh-now)

       (define needs-refresh? #t)
       (define mouse-location #f)

       (define wld (make-world initial-puzzle))
       (define puzzle-renderer #f)
       (define tf:canvas-to-render tf:identity)

       (define/override (on-event event)
         (when puzzle-renderer
           (define event-type (send event get-event-type))
           (define location (point (send event get-x)
                                   (send event get-y)))
           (match event-type
             ['motion
              (set! mouse-location location)
              (set! needs-refresh? #t)]
             ['leave
              (set! mouse-location #f)
              (set! needs-refresh? #t)]
             [_ (void)])

           (define tile-point (send puzzle-renderer get-tile-at
                                    (tf* tf:canvas-to-render location)))
           (when (or tile-point (memq event-type '(left-up middle-up right-up)))
             (define modifiers (modifier-keys
                                (send event get-shift-down)
                                (send event get-control-down)
                                (match (system-type 'os)
                                  ['macosx (send event get-alt-down)]
                                  [_       (or (send event get-alt-down)
                                               (send event get-meta-down))])))
             (define new-wld (on-mouse-event wld
                                             event-type
                                             modifiers
                                             (and~> tile-point point-x)
                                             (and~> tile-point point-y)))
             (unless (equal? wld new-wld)
               (set! wld new-wld)
               (set! needs-refresh? #t)))))

       (define/private (update-renderer!)
         (define backing-scale (send (get-dc) get-backing-scale))
         (define output-scale (calculate-output-scale))
         (cond
           [(or (not puzzle-renderer)
                (not (= output-scale (send puzzle-renderer get-output-scale)))
                (not (= backing-scale (send puzzle-renderer get-backing-scale))))
            (set! puzzle-renderer
                  (new puzzle-renderer%
                       [puzzle (world-puzzle wld)]
                       [board-analysis (world-board-analysis wld)]
                       [output-scale output-scale]
                       [backing-scale backing-scale]))]
           [else
            (send puzzle-renderer update!
                  (world-puzzle wld)
                  (world-board-analysis wld))]))

       (define/private (calculate-output-scale)
         (define-values [w h] (get-client-size))
         (size-scaling-factor-to-fit base-size (size w h)))
       (define/private (get-bounds-pict)
         (define-values [w h] (get-client-size))
         (blank w h))

       (define/private (paint dc)
         (update-renderer!)
         (define rendered-p (send puzzle-renderer get-render))

         (define centered-p (cc-superimpose (get-bounds-pict) rendered-p))
         (define render-loc (truncate-point (pict-child-point centered-p rendered-p)))
         (set! tf:canvas-to-render (tf:translate (point- render-loc)))

         (send dc set-smoothing 'smoothed)
         (define timing-end (timing-start 'blit))
         (draw-pict rendered-p dc (point-x render-loc) (point-y render-loc))
         (timing-end)

         (match mouse-location
           [(point x y)
            (draw-pict (cellophane (colorize (disk 10) "red") 0.5) dc (- x 5) (- y 5))]
           [_ (void)]))

       (define/public (do-refresh)
         (collect-garbage 'incremental)
         (when needs-refresh?
           (set! needs-refresh? #f)
           (refresh-now (λ (dc) (paint dc)))))

       (define/override (on-paint)
         (super on-paint)
         (paint (get-dc)))

       (define/override (on-size w h)
         (set! needs-refresh? #t)
         (super on-size w h)
         (do-refresh))

       (super-new
        [parent frame]
        [min-width 100]
        [min-height 100]))))

  (define refresh-timer
    (new timer%
         [interval (floor (/ 1000 60))]
         [notify-callback (λ () (send canvas do-refresh))]))

  (send frame show #t)
  (yield 'wait)
  (void))

(module+ main
  (require racket/cmdline
           (submod "core.rkt" example))

  (define log-timings? #f)
  (define what-to-do #f)
  (command-line
   #:once-any
   ["--puzzle" name
    "Play puzzle named <name>"
    (set! what-to-do (list 'play name))]
   ["--list-puzzles"
    "List all available puzzles"
    (set! what-to-do 'list)]
   ["--load-puzzle" puzzle-string columns
    "Load a custom puzzle from a string encoded in the puzzle-nonograms.com format"
    (set! what-to-do (list 'load puzzle-string columns))]
   #:once-each
   ["--debug-log-timings"
    "Log timings during puzzle analyze and render"
    (set! log-timings? #t)])

  (define log-writer
    (spawn-pretty-log-writer
     (make-nonogram-log-receiver #:timing? log-timings?)))

  (define (print-puzzle-list)
    (for ([name+pz (in-list all-puzzles)])
      (match-define (cons name pz) name+pz)
      (define b (puzzle-board pz))
      (eprintf "  ~v (~a×~a)\n" name (board-width b) (board-height b))))

  (match what-to-do
    [#f
     (eprintf "No puzzle selected. Please select one of the following puzzles with `--puzzle <name>`:\n")
     (print-puzzle-list)
     (exit 1)]
    ['list
     (eprintf "Available puzzles:\n")
     (print-puzzle-list)]
    [(list 'play puzzle-name)
     (define pz (get-puzzle puzzle-name))
     (unless pz
       (eprintf "No puzzle named ~v." puzzle-name)
       (exit 1))
     (run pz)]
    [(list 'load pz-str cols-str)
     (define cols (string->number cols-str))
     (define clues (parse-puzzle-nonograms.com-clues pz-str cols))
     (run (clues->puzzle clues))])

  (close-log-writer log-writer))
