#lang racket/base

(require file/gunzip
         file/gzip
         net/rfc6455
         racket/class
         racket/contract
         racket/fasl
         racket/gui/base
         racket/list
         racket/match
         racket/math
         racket/path
         racket/port
         racket/random
         racket/runtime-path
         racket/serialize
         racket/set
         threading
         toolbox/list
         toolbox/who
         "core.rkt"
         "lib/array.rkt"
         "lib/geometry.rkt"
         "logger.rkt"
         "render.rkt"
         "solve.rkt")

;; -----------------------------------------------------------------------------

(define-runtime-path nonogram-dir ".")

(serializable-struct world
  (puzzle
   hint-mode ;; (or/c 'none 'errors 'all)
   hint-rows
   hint-columns
   cursor-locations
   show-errors?)
  #:transparent)

(define (make-world puzzle)
  (world puzzle
         'errors
         (seteqv)
         (seteqv)
         (hasheqv)
         #f))

(define drag-mode? (or/c #f tile? 'unmark))

(struct client-state
  (client-id
   world
   board-analysis
   solved-board
   drag-mode
   show-fps?)
  #:transparent)

(define/who (make-client-state client-id wld #:show-fps? [show-fps? #f])
  (define solution (solve-puzzle (world-puzzle wld)))
  (client-state client-id
                wld
                (analyze-puzzle (world-puzzle wld))
                (match solution
                  [(solver-result solved-board solved?)
                   (and solved? solved-board)]
                  ['error
                   (raise-arguments-error who "solving the given puzzle resulted in an error"
                                          "puzzle" (world-puzzle wld))])
                #f
                show-fps?))

(serializable-struct action () #:transparent)
(serializable-struct a:set-puzzle action (puzzle) #:transparent)
(serializable-struct a:set-tile action (location tile) #:transparent)
(serializable-struct a:set-hint-mode action (value) #:transparent)
(serializable-struct a:add-hint-line action (axis index) #:transparent)
(serializable-struct a:move-cursor action (location) #:transparent)
(serializable-struct a:set-show-errors? action (value) #:transparent)

(struct a:local action (proc) #:transparent)

(define (a:set-drag-mode new-mode)
  (a:local (λ (cs) (struct-copy client-state cs [drag-mode new-mode]))))
(define (a:set-show-fps? new-show-fps?)
  (a:local (λ (cs) (struct-copy client-state cs [show-fps? new-show-fps?]))))

(define (echo-action? action)
  (match action
    [(or (? a:set-puzzle?)
         (? a:set-tile?)
         (? a:set-hint-mode?)
         (? a:add-hint-line?)
         (? a:set-show-errors?))
     #t]
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

  (define (select-hint-line)
    (define (select-best lines excluded)
      (for/fold ([best-is '()]
                 [best-weight 1])
                ([(line i) (in-indexed (in-array lines))]
                 #:unless (set-member? excluded i))
        (cond
          [(line-clue-analysis? line)
           (define weight (line-clue-analysis-hint-weight line))
           (cond
             [(> weight best-weight)
              (values (list i) weight)]
             [(= weight best-weight)
              (values (cons i best-is) best-weight)]
             [else
              (values best-is best-weight)])])))

    (define analysis (client-state-board-analysis cs))
    (define-values [best-rows best-row-weight]
      (select-best (board-analysis-row-analysis analysis)
                   (world-hint-rows wld)))
    (define-values [best-columns best-column-weight]
      (select-best (board-analysis-column-analysis analysis)
                   (world-hint-columns wld)))

    (cond
      [(and (empty? best-rows) (empty? best-columns))
       #f]
      [(> best-row-weight best-column-weight)
       (a:add-hint-line 'row (random-ref best-rows))]
      [(> best-column-weight best-row-weight)
       (a:add-hint-line 'column (random-ref best-columns))]
      [else
       (random-ref (append (map (λ~> (a:add-hint-line 'row _)) best-rows)
                           (map (λ~> (a:add-hint-line 'column _)) best-columns)))]))

  (define (process-solver)
    (match event-type
      ['press
       (define old-puzzle (world-puzzle wld))

       (define (extract-result result)
         (match result
           ['error old-puzzle]
           [(solver-result new-board _)
            (struct-copy puzzle old-puzzle
              [board new-board])]))

       (define new-puzzle
         (match key-code
           ['f1 (extract-result (solve-puzzle-axis old-puzzle 'row))]
           ['f2 (extract-result (solve-puzzle-axis old-puzzle 'column))]
           ['f3 (extract-result (solve-puzzle old-puzzle))]
           ['f9 (demegaify-puzzle old-puzzle)]
           ['f10 (megaify-puzzle old-puzzle)]
           ['f12
            (struct-copy puzzle old-puzzle
              [board (board-clear (puzzle-board old-puzzle))])]
           [_ old-puzzle]))

       (append
        (when/list* (eq? key-code #\h)
          (maybe->list (select-hint-line)))
        (when/list (eq? key-code 'f4)
          (a:set-hint-mode (match (world-hint-mode wld)
                             [(or 'none 'all) 'errors]
                             ['errors         'all])))
        (when/list (eq? key-code 'f5)
          (a:set-show-errors? (not (world-show-errors? wld))))
        (when/list (eq? key-code 'f8)
          (a:set-show-fps? (not (client-state-show-fps? cs))))
        (unless/list (equal? old-puzzle new-puzzle)
          (a:set-puzzle new-puzzle)))]
      [_ '()]))

  (begin0
    (process-solver)
    (timing-end)))

;; perform-action : client-state? client-id? action? -> client-state?
(define (perform-action cs client-id action)
  (define (remove-completed-hint-lines wld analysis)
    (define (do-axis axis-analysis hint-lines)
      (for/fold ([hint-lines* hint-lines])
                ([clue-i (in-immutable-set hint-lines)])
        (match (array-ref axis-analysis clue-i)
          [(? line-clue-analysis? analysis)
           #:when (zero? (line-clue-analysis-hint-weight analysis))
           (set-remove hint-lines* clue-i)]
          [_
           hint-lines*])))

    (struct-copy world wld
      [hint-rows    (do-axis (board-analysis-row-analysis analysis)
                             (world-hint-rows wld))]
      [hint-columns (do-axis (board-analysis-column-analysis analysis)
                             (world-hint-columns wld))]))

  (match action
    [(a:set-puzzle pz)
     (define new-analysis (analyze-puzzle pz))
     (struct-copy client-state cs
       [world (struct-copy world (client-state-world cs)
                [puzzle pz]
                [hint-rows (seteqv)]
                [hint-columns (seteqv)])]
       [board-analysis new-analysis])]

    [(a:set-tile (and loc (point x y)) tile)
     (define old-wld (client-state-world cs))
     (define old-pz (world-puzzle old-wld))
     (define new-pz (struct-copy puzzle old-pz
                      [board (board-set (puzzle-board old-pz) x y tile)]))
     (define new-analysis (reanalyze-lines-at new-pz
                                              (client-state-board-analysis cs)
                                              loc))
     (struct-copy client-state cs
       [world (~> (struct-copy world old-wld
                    [puzzle new-pz])
                  (remove-completed-hint-lines new-analysis))]
       [board-analysis new-analysis])]

    [(a:set-hint-mode value)
     (struct-copy client-state cs
       [world (struct-copy world (client-state-world cs)
                [hint-mode value])])]

    [(a:add-hint-line axis index)
     (define wld (client-state-world cs))
     (struct-copy client-state cs
       [world
        (match axis
          ['row
           (struct-copy world wld
             [hint-rows (set-add (world-hint-rows wld) index)])]
          ['column
           (struct-copy world wld
             [hint-columns (set-add (world-hint-columns wld) index)])])])]

    [(a:move-cursor loc)
     (define wld (client-state-world cs))
     (struct-copy client-state cs
       [world (struct-copy world wld
                [cursor-locations
                 (~> (world-cursor-locations wld)
                     (cond~>
                      [loc  (hash-set client-id loc)]
                      [else (hash-remove client-id)]))])])]

    [(a:set-show-errors? value)
     (struct-copy client-state cs
       [world (struct-copy world (client-state-world cs)
                [show-errors? value])])]

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

(define PROTOCOL-VERSION "1.0")
(define (protocol-version-compatible? v)
  (string=? v PROTOCOL-VERSION))

(define client-id? natural?)
(define client-id=? eqv?)
(struct client-action (client-id action from-server?) #:transparent)

(serializable-struct client-hello (version) #:transparent)
(serializable-struct server-hello (version client-id world) #:transparent)
(serializable-struct protocol-error (message) #:transparent)

(define (make-version-mismatch-error client-version)
  (protocol-error
   (format "version mismatch\n  client version: ~e\n  server version: ~e"
           client-version
           PROTOCOL-VERSION)))

(struct connected-client (id conn send-thread) #:transparent)

;; -----------------------------------------------------------------------------

(define (run initial-world
             #:client [this-client-id 0]
             #:serve [listen-port #f]
             #:upstream [server-conn #f]
             #:show-fps? [show-fps? #f]
             #:fps-limit [fps-limit 60])
  (define frame
    (new
     (class frame%
       (super-new [label "Pictcross"]
                  [width 800]
                  [height 600])
       (define/augment (on-close)
         (break-thread refresh-thread)))))

  (define this-cs (make-client-state this-client-id
                                     initial-world
                                     #:show-fps? show-fps?))

  ;; ---------------------------------------------------------------------------

  ;; (hash/c ws-conn? connected-client?)
  (define connected-clients (make-hasheq))

  (define (get-next-client-id)
    (define connected-ids
      (map connected-client-id (hash-values connected-clients)))
    (for/first ([i (in-naturals 1)]
                #:unless (member i connected-ids client-id=?))
      i))

  (define (add-connected-client! conn)
    (define client-id (get-next-client-id))
    (define send-thread (thread (λ () (client-send-loop conn))))
    (define client (connected-client client-id conn send-thread))
    (hash-set! connected-clients conn client)
    (thread
     (λ ()
       (dynamic-wind
        void
        (λ () (recv-loop conn #:client client-id))
        (λ () (enqueue-client-disconnected! conn)))))
    (define hello (server-hello PROTOCOL-VERSION client-id (client-state-world this-cs)))
    (connected-client-send! client (serialize-message hello)))

  (define (remove-connected-client! conn)
    (define client (hash-ref connected-clients conn #f))
    (when client
      (hash-remove! connected-clients conn)
      (enqueue-update! (client-action (connected-client-id client) (a:move-cursor #f) #f))
      (thread (λ () (ws-close! (connected-client-conn client)))))
    (void))

  (define (connected-client-send! client msg)
    (thread-send (connected-client-send-thread client)
                 msg
                 (λ () (enqueue-client-disconnected!
                        (connected-client-conn client)))))

  (define (recv-loop conn #:client [client-id #f])
    (let loop ()
      (match (ws-recv conn)
        [(? eof-object?)
         (ws-close! conn)]
        [bs
         (match (deserialize-message bs)
           [(protocol-error message)
            (error message)]
           [msg
            (enqueue-update!
             (if client-id
                 (client-action client-id msg #f)
                 (match msg
                   [(cons client-id msg)
                    (client-action client-id msg #t)])))])
         (loop)])))

  (define (client-send-loop conn)
    (with-handlers ([exn:fail? (λ (exn)
                                 (enqueue-client-disconnected! conn)
                                 (raise exn))])
      (let loop ()
        (do-send! conn (thread-receive))
        (loop))))

  ;; ---------------------------------------------------------------------------

  (define (do-send! conn bs)
    (ws-send! conn bs #:payload-type 'binary))

  (define (send-to-server! action)
    (when server-conn
      (do-send! server-conn (serialize-message action))))

  (define (broadcast! client-id action)
    (define echo? (echo-action? action))
    (define bs (serialize-message (cons client-id action)))
    (for ([client (in-mutable-hash-values connected-clients)]
          #:when (or echo? (not (client-id=? client-id (connected-client-id client)))))
      (connected-client-send! client bs)))

  ;; ---------------------------------------------------------------------------

  (define (perform-action! client-id action from-server?)
    (set! this-cs (perform-action this-cs client-id action))
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
              (add-connected-client! conn)]
             [(cons 'client-disconnected conn)
              (remove-connected-client! conn)]))
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
           (match (ws-recv conn)
             [(? bytes? (app deserialize-message (client-hello client-version)))
              (cond
                [(protocol-version-compatible? client-version)
                 (enqueue-update! (cons 'client-connected conn))]
                [else
                 (do-send! conn (serialize-message (make-version-mismatch-error client-version)))
                 (ws-close! conn)])]
             [_
              (ws-close! conn)])))
        void))

  ;; ---------------------------------------------------------------------------

  (define canvas
    (new
     (class canvas%
       (inherit get-client-size
                get-gl-client-size
                get-dc
                refresh-now)

       (super-new
        [parent frame]
        [min-width 100]
        [min-height 100]
        [style '(gl no-autoclear)]
        [gl-config default-gl-config])

       (define/private (tf:canvas-to-gl)
         (define-values [width height] (get-client-size))
         (define-values [gl-width gl-height] (get-gl-client-size))
         (tf:scale (/ (real->double-flonum gl-width) width)
                   (/ (real->double-flonum gl-height) height)))

       (define puzzle-renderer
         (new puzzle-renderer%
              [gl-context (send (get-dc) get-gl-context)]
              [puzzle (world-puzzle (client-state-world this-cs))]))

       (define/override (on-event event)
         (when puzzle-renderer
           (define event-type (send event get-event-type))
           (define location (point (send event get-x)
                                   (send event get-y)))
           (define tile-loc (send puzzle-renderer get-tile-at
                                    (tf* (tf:canvas-to-gl) location)))
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

       (define/public (do-refresh)
         (collect-garbage 'incremental)
         (define cs this-cs)
         (define wld (client-state-world cs))
         (send puzzle-renderer set-state!
               #:puzzle (world-puzzle wld)
               #:board-analysis (client-state-board-analysis cs)
               #:hint-mode (world-hint-mode wld)
               #:hint-rows (world-hint-rows wld)
               #:hint-columns (world-hint-columns wld)
               #:solved-board (client-state-solved-board cs)
               #:show-errors? (world-show-errors? wld)
               #:cursor-locations (world-cursor-locations wld)
               #:show-fps? (client-state-show-fps? cs))
         (send puzzle-renderer render!))

       (define/override (on-size w h)
         (super on-size w h)
         (define-values [gl-w gl-h] (get-gl-client-size))
         (send puzzle-renderer set-size! gl-w gl-h)))))

  (define refresh-thread
    (thread
     (λ ()
       (with-handlers* ([exn:break? void])
         (cond
           [fps-limit
            (define ms-per-frame (/ 1000.0 fps-limit))
            (let loop ([start-ms (current-inexact-monotonic-milliseconds)])
              (send canvas do-refresh)
              (define end-ms (current-inexact-monotonic-milliseconds))
              (define next-start-ms (+ start-ms ms-per-frame))
              (define sleep-ms (- next-start-ms end-ms))
              (cond
                [(<= sleep-ms 0)
                 (loop end-ms)]
                [else
                 (sleep (/ sleep-ms 1000))
                 (loop next-start-ms)]))]
           [else
            (let loop ()
              (send canvas do-refresh)
              (sleep)
              (loop))])))))

  (when server-conn
    (thread (λ () (recv-loop server-conn))))

  (send frame show #t)
  (send canvas focus)
  (yield 'wait)
  (shutdown-ws-server!)
  (void))

;; -----------------------------------------------------------------------------

(module+ main
  (require net/url
           racket/cmdline
           racket/string
           toolbox/logging
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
  (define show-fps? #f)
  (define fps-limit 60)

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
    (set! log-timings? #t)]
   ["--debug-no-fps-limit"
    "Disable the FPS limit"
    (set! fps-limit #f)]
   ["--debug-show-fps"
    "Enable rendering the current FPS by default"
    (set! show-fps? #t)])

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
         #:upstream server-conn
         #:show-fps? show-fps?
         #:fps-limit fps-limit))

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
     (ws-send! conn (serialize-message (client-hello PROTOCOL-VERSION)) #:payload-type 'binary)
     (match (ws-recv conn)
       [(? bytes? bs)
        (match (deserialize-message bs)
          [(protocol-error message)
           (error message)]
          [(server-hello _ client-id wld)
           (do-run wld
                   #:client client-id
                   #:upstream conn)])])])

  (close-log-writer log-writer))
