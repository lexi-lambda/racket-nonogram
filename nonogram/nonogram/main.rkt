#lang racket/base

(require file/gunzip
         file/gzip
         net/rfc6455
         net/url
         pict
         racket/class
         racket/contract
         racket/fasl
         racket/gui/base
         racket/match
         racket/path
         racket/port
         racket/runtime-path
         racket/serialize
         threading
         toolbox/list
         toolbox/logging
         "core.rkt"
         "geometry.rkt"
         "logger.rkt"
         "render.rkt"
         "solve.rkt")

;; -----------------------------------------------------------------------------

(define-runtime-path nonogram-dir ".")

(serializable-struct world
  (puzzle
   cursor-locations)
  #:transparent)

(define (make-world puzzle)
  (world puzzle (hasheqv)))

(define drag-mode? (or/c #f tile? 'unmark))

(struct client-state
  (client-id
   world
   board-analysis
   drag-mode)
  #:transparent)

(define (make-client-state client-id wld)
  (client-state client-id
                wld
                (analyze-puzzle (world-puzzle wld))
                #f))

(serializable-struct action () #:transparent)
(serializable-struct a:set-puzzle action (puzzle) #:transparent)
(serializable-struct a:set-tile action (location tile) #:transparent)
(serializable-struct a:move-cursor action (location) #:transparent)
(struct a:local action (proc) #:transparent)

(define (a:set-drag-mode new-mode)
  (a:local (λ (cs) (struct-copy client-state cs [drag-mode new-mode]))))

(define (echo-action? action)
  (match action
    [(or (? a:set-puzzle?) (? a:set-tile?)) #t]
    [(? a:move-cursor?) #f]))

;; -----------------------------------------------------------------------------

(struct modifier-keys
  (shift-down?
   control-down? ; Control, not Command, on macOS
   alt-down?)    ; Option on macOS
  #:transparent)

(define (event->modifier-keys event)
  (modifier-keys
   (send event get-shift-down)
   (send event get-control-down)
   (match (system-type 'os)
     ['macosx (send event get-alt-down)]
     [_       (or (send event get-alt-down)
                  (send event get-meta-down))])))

;; on-mouse-drag : drag-mode? board? integer-point? -> (listof action?)
(define (on-mouse-drag mode old-board tile-loc)
  (match-define (point tile-x tile-y) tile-loc)
  (define old-tile (board-ref old-board tile-x tile-y))
  (define new-tile
    (match* {mode old-tile}
      [{'empty  _                } 'empty]
      [{'unmark 'mark            } 'empty]
      [{'full   (or 'empty 'mark)} 'full]
      [{'cross  (or 'empty 'mark)} 'cross]
      [{'mark   'empty           } 'mark]
      [{_       _                } old-tile]))
  (unless/list (equal? old-tile new-tile)
    (a:set-tile tile-loc new-tile)))

;; on-mouse-event : client-state?
;;                  mouse-event-type?
;;                  modifier-keys?
;;                  (or/c integer-point? #f)
;;               -> (listof action?)
(define (on-mouse-event cs event-type modifiers tile-loc)
  (define client-id (client-state-client-id cs))
  (define wld (client-state-world cs))
  (define pz (world-puzzle wld))
  (define cursors (world-cursor-locations wld))

  ;; process cursor changes
  (define cursor-actions
    (unless/list (equal? (hash-ref cursors client-id #f) tile-loc)
      (a:move-cursor tile-loc)))

  ;; process drag mode changes
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

  (define-values [drag-update? drag-mode]
    (match action-type
      [(list 'start start-type)
       #:when tile-loc
       (match-define (point tile-x tile-y) tile-loc)
       (define old-board (puzzle-board pz))
       (define old-tile (board-ref old-board tile-x tile-y))
       (define new-drag-mode
         (match* {old-tile start-type}
           [{(or 'empty 'mark) 'full } 'full]
           [{(or 'empty 'mark) 'cross} 'cross]
           [{'empty            'mark } 'mark]
           [{'mark             'mark } 'unmark]
           [{(or 'full 'cross) _     } 'empty]))
       (values #t new-drag-mode)]
      ['end
       (values #t #f)]
      [_
       (values #f (client-state-drag-mode cs))]))

  ;; process board changes
  (define board-actions
    (match action-type
      [(or (list 'start _) 'drag)
       #:when (and drag-mode tile-loc)
       (define old-puzzle pz)
       (define old-board (puzzle-board old-puzzle))
       (on-mouse-drag drag-mode old-board tile-loc)]
      [_ '()]))

  (append
   cursor-actions
   (when/list drag-update?
     (a:set-drag-mode drag-mode))
   board-actions))

;; on-keyboard-event : client-state? (or/c 'press 'release) (or/c char? key-code-symbol?) modifier-keys? -> (listof action?)
(define (on-keyboard-event cs event-type key-code modifiers)
  (define timing-end (timing-start 'on-keyboard-event))
  (define wld (client-state-world cs))

  (define (process-solver)
    (match event-type
      ['press
       (define old-puzzle (world-puzzle wld))
       (define new-puzzle
         (match key-code
           ['f1 (solve-puzzle-axis old-puzzle 'row)]
           ['f2 (solve-puzzle-axis old-puzzle 'column)]
           ['f3 (solve-puzzle old-puzzle)]
           ['f12
            (struct-copy puzzle old-puzzle
                         [board (board-clear (puzzle-board old-puzzle))])]
           [_ old-puzzle]))
       (unless/list (equal? old-puzzle new-puzzle)
         (a:set-puzzle new-puzzle))]
      [_ '()]))

  (begin0
    (process-solver)
    (timing-end)))

;; perform-action : client-state? client-id? action? -> client-state?
(define (perform-action cs client-id action)
  (match action
    [(a:set-puzzle pz)
     (struct-copy client-state cs
       [world (struct-copy world (client-state-world cs)
                [puzzle pz])]
       [board-analysis (analyze-puzzle pz)])]

    [(a:set-tile (and loc (point x y)) tile)
     (define old-wld (client-state-world cs))
     (define old-pz (world-puzzle old-wld))
     (define new-pz (struct-copy puzzle old-pz
                      [board (board-set (puzzle-board old-pz) x y tile)]))
     (struct-copy client-state cs
       [world (struct-copy world old-wld
                [puzzle new-pz])]
       [board-analysis
        (reanalyze-lines-at new-pz
                            (client-state-board-analysis cs)
                            loc)])]

    [(a:move-cursor loc)
     (define wld (client-state-world cs))
     (struct-copy client-state cs
       [world (struct-copy world wld
                [cursor-locations
                 (~> (world-cursor-locations wld)
                     (cond~>
                      [loc  (hash-set client-id loc)]
                      [else (hash-remove client-id)]))])])]

    [(a:local proc)
     (proc cs)]))

;; -----------------------------------------------------------------------------

(define (gzip-bytes in-bs)
  (call-with-output-bytes
   (λ (out)
     (gzip-through-ports (open-input-bytes in-bs)
                         out
                         #f
                         (current-seconds)))))

(define (gunzip-bytes in-bs)
  (call-with-output-bytes
   (λ (out)
     (gunzip-through-ports (open-input-bytes in-bs) out))))

(define complete-nonogram-dir (simple-form-path nonogram-dir))

(define (serialize-message msg)
  (~> msg
      (serialize #:relative-directory complete-nonogram-dir)
      s-exp->fasl
      gzip-bytes))

(define (deserialize-message msg)
  (~> msg
      gunzip-bytes
      fasl->s-exp
      deserialize))

(define client-id=? eqv?)
(struct client-action (client-id action from-server?) #:transparent)

(define (run initial-world
             #:client [this-client-id 0]
             #:serve [listen-port #f]
             #:upstream [server-conn #f])
  (define frame
    (new
     (class frame%
       (super-new [label "Pictcross"]
                  [width 800]
                  [height 600])
       (define/augment (on-close)
         (send refresh-timer stop)))))

  (define base-size (get-base-puzzle-size (world-puzzle initial-world)))
  (define this-cs (make-client-state this-client-id
                                     initial-world))

  ;; ---------------------------------------------------------------------------

  (define connected-clients (make-hasheqv))

  (define (get-next-client-id)
    (for/first ([i (in-naturals 1)]
                #:unless (member i (hash-values connected-clients) client-id=?))
      i))

  (define (recv-loop conn #:client [client-id #f])
    (let loop ()
      (match (ws-recv conn)
        [(? eof-object?)
         (ws-close! conn)]
        [bs
         (define msg (deserialize-message bs))
         (enqueue-update!
          (if client-id
              (client-action client-id msg #f)
              (match msg
                [(cons client-id msg)
                 (client-action client-id msg #t)])))
         (loop)])))

  (define (do-send! conn bs)
    (ws-send! conn bs #:payload-type 'binary))

  (define (send-to-server! action)
    (when server-conn
      (do-send! server-conn (serialize-message action))))

  (define (broadcast! client-id action)
    (define echo? (echo-action? action))
    (define bs (serialize-message (cons client-id action)))
    (for ([(conn client-id*) (in-mutable-hash connected-clients)]
          #:when (or echo? (not (client-id=? client-id client-id*))))
      (thread
       (λ ()
         (with-handlers ([exn:fail? (λ (exn)
                                      (enqueue-client-disconnected! conn)
                                      (raise exn))])
           (do-send! conn bs))))))

  ;; ---------------------------------------------------------------------------

  (define (perform-action! client-id action from-server?)
    (set! this-cs (perform-action this-cs client-id action))
    (send canvas need-refresh!)
    (unless (a:local? action)
      (when (not from-server?)
        (send-to-server! action))
      (broadcast! client-id action)))

  (define update-thread
    (thread
     (λ ()
       (let loop ()
         (with-handlers ([exn:fail?
                          (λ (exn)
                            ((error-display-handler) (exn-message exn) exn))])
           (match (thread-receive)
             [(client-action client-id action from-server?)
              (perform-action! client-id action from-server?)]
             [(cons 'mouse-event args)
              (enqueue-local-actions! (apply on-mouse-event this-cs args))]
             [(cons 'keyboard-event args)
              (enqueue-local-actions! (apply on-keyboard-event this-cs args))]
             [(cons 'client-connected conn)
              (define client-id (get-next-client-id))
              (hash-set! connected-clients conn client-id)
              (do-send! conn (serialize-message (cons client-id (client-state-world this-cs))))
              (thread
               (λ ()
                 (dynamic-wind
                  void
                  (λ () (recv-loop conn #:client client-id))
                  (λ () (enqueue-client-disconnected! conn)))))]
             [(cons 'client-disconnected conn)
              (hash-remove! connected-clients conn)]))
         (loop)))))

  (define (enqueue-update! msg)
    (thread-send update-thread msg))

  (define (enqueue-local-actions! actions)
    (for ([action (in-list actions)])
      (enqueue-update! (client-action this-client-id action #f))))

  (define (enqueue-client-disconnected! conn)
    (enqueue-update! (cons 'client-disconnected conn)))

  (define shutdown-ws-server!
    (if listen-port
        (ws-serve
         #:port listen-port
         (λ (conn req)
           (enqueue-update! (cons 'client-connected conn))))
        void))

  ;; ---------------------------------------------------------------------------

  (define canvas
    (new
     (class canvas%
       (inherit get-client-size
                get-dc
                refresh
                refresh-now)

       (define needs-refresh? #t)
       (define mouse-location #f)

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
              (need-refresh!)]
             ['leave
              (set! mouse-location #f)
              (need-refresh!)]
             [_ (void)])

           (define tile-loc (send puzzle-renderer get-tile-at
                                    (tf* tf:canvas-to-render location)))
           (enqueue-update!
            (list 'mouse-event
                  event-type
                  (event->modifier-keys event)
                  tile-loc))))

       (define/override (on-char event)
         (define-values [event-type key-code]
           (match (send event get-key-code)
             ['release (values 'release (send event get-key-release-code))]
             [key-code (values 'press key-code)]))
         (enqueue-update!
          (list 'keyboard-event
                event-type
                key-code
                (event->modifier-keys event))))

       (define/public (need-refresh!)
         (set! needs-refresh? #t))

       (define/private (update-renderer!)
         (define wld (client-state-world this-cs))
         (define backing-scale (send (get-dc) get-backing-scale))
         (define output-scale (calculate-output-scale))
         (cond
           [(or (not puzzle-renderer)
                (not (= output-scale (send puzzle-renderer get-output-scale)))
                (not (= backing-scale (send puzzle-renderer get-backing-scale))))
            (set! puzzle-renderer
                  (new puzzle-renderer%
                       [puzzle (world-puzzle wld)]
                       [board-analysis (client-state-board-analysis this-cs)]
                       [output-scale output-scale]
                       [backing-scale backing-scale]))]
           [else
            (send puzzle-renderer update!
                  (world-puzzle wld)
                  (client-state-board-analysis this-cs)
                  (world-cursor-locations wld))]))

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
         (timing-end))

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

  (when server-conn
    (thread (λ () (recv-loop server-conn))))

  (send frame show #t)
  (send canvas focus)
  (yield 'wait)
  (shutdown-ws-server!)
  (void))

;; -----------------------------------------------------------------------------

(module+ main
  (require racket/cmdline
           racket/string
           "lib/array.rkt"
           (submod "core.rkt" example))

  (deserialize-module-guard
   (λ (mod-path sym)
     (define (bad-mod-path)
       (raise-arguments-error 'deserialize-module-guard "not a relative library path"
                              "module path" mod-path))
     (match mod-path
       [`(lib ,str)
        (unless (and (relative-path? str)
                     (andmap path-for-some-system?
                             (explode-path (simplify-path str #f))))
          (bad-mod-path))]
       [_
        (bad-mod-path)])))

  (define (connect-string->ws-url str)
    (match (string-split str ":")
      [(list host port)
       (url "ws" #f host (string->number port) #t (list (path/param "" '())) '() #f)]
      [_
       (eprintf "error: Argument to --connect must have the form <host:port>.\n")
       (exit 1)]))

  (define log-timings? #f)
  (define what-to-do #f)
  (define listen-port #f)

  (command-line
   #:once-any
   ["--puzzle" name
    "Play puzzle named <name>"
    (set! what-to-do (array 'play name))]
   ["--list-puzzles"
    "List all available puzzles"
    (set! what-to-do 'list-puzzles)]
   ["--load-puzzle-no" puzzle-string
    "Load puzzle encoded in the nonograms.org format"
    (set! what-to-do (array 'load/no puzzle-string))]
   ["--load-puzzle-pbn" puzzle-string
    "Load puzzle encoded in the pbnsolve XML format provided by webpbn.com"
    (set! what-to-do (array 'load/pbn puzzle-string))]
   ["--load-puzzle-pnc" puzzle-string columns
    "Load puzzle encoded in the puzzle-nonograms.com format"
    (set! what-to-do (array 'load/pnc puzzle-string columns))]
   ["--connect" host:port
    "Connect to a server listening with --serve on <host:port>"
    (set! what-to-do (array 'connect (connect-string->ws-url host:port)))]
   #:once-each
   ["--serve" port
    "Start a server on <port> for others to connect to"
    (set! listen-port (string->number port))]
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

  (define (do-run pz
                  #:client [this-client-id 0]
                  #:upstream [server-conn #f])
    (run pz
         #:client this-client-id
         #:serve listen-port
         #:upstream server-conn))

  (match what-to-do
    [#f
     (eprintf "error: No puzzle selected. Please select one of the following puzzles with `--puzzle <name>`:\n")
     (print-puzzle-list)
     (exit 1)]
    ['list-puzzles
     (eprintf "Available puzzles:\n")
     (print-puzzle-list)]
    [(array 'play puzzle-name)
     (define pz (get-puzzle puzzle-name))
     (unless pz
       (eprintf "error: No puzzle named ~v.\n" puzzle-name)
       (exit 1))
     (do-run (make-world pz))]
    [(array 'load/no pz-str)
     (define solution (parse-nonograms.org-solution pz-str))
     (do-run (make-world (solved-board->puzzle solution)))]
    [(array 'load/pbn pz-str)
     (define solution (parse-pbnsolve-solution pz-str))
     (do-run (make-world (solved-board->puzzle solution)))]
    [(array 'load/pnc pz-str cols-str)
     (define cols (string->number cols-str))
     (define clues (parse-puzzle-nonograms.com-clues pz-str cols))
     (do-run (make-world (clues->puzzle clues)))]
    [(array 'connect connect-url)
     (when listen-port
       (eprintf "error: Cannot combine --connect with --serve.\n")
       (exit 1))

     (define conn (ws-connect connect-url))
     (match-define (cons client-id wld) (deserialize-message (ws-recv conn)))
     (do-run wld
             #:client client-id
             #:upstream conn)])

  (close-log-writer log-writer))
