#lang racket/base

(require racket/contract
         racket/list
         racket/match
         threading
         toolbox/who
         "../array.rkt"
         "../core.rkt"
         "core.rkt")

(module+ test
  (require rackunit))

(provide (contract-out
          [analyze-line/mega
           (-> mega-line-clues? mega-tile-line? line-clue-analysis?)]
          [solve-line/mega (-> mega-line-clues? mega-tile-line? (or/c mega-tile-line? 'error))]))

;; -----------------------------------------------------------------------------

(struct mega-line-solution
  (board-tiles
   clue-tiless
   line-clue-indexes)
  #:transparent)

(define (clues-ref cvec clue-i)
  (match clue-i
    [(single-line-index chunk-i line-i i)
     (vector-ref (array-ref (vector-ref cvec chunk-i) line-i) i)]
    [chunk-i
     (vector-ref cvec chunk-i)]))

(define (clues-set! cvec clue-i val)
  (match clue-i
    [(single-line-index chunk-i line-i i)
     (vector-set! (array-ref (vector-ref cvec chunk-i) line-i) i val)]
    [chunk-i
     (vector-set! cvec chunk-i val)]))

(define (clues-update! cvec clue-i proc)
  (match clue-i
    [(single-line-index chunk-i line-i i)
     (define vec (array-ref (array-ref cvec chunk-i) line-i))
     (define val (proc (vector-ref vec i)))
     (vector-set! vec i val)
     val]
    [chunk-i
     (define val (proc (vector-ref cvec chunk-i)))
     (vector-set! cvec chunk-i val)
     val]))

;; make-clue-tiles-ref/mega : mega-tiles/c -> (clue-index? mega-index? -> tile?)
(define ((make-clue-tiles-ref/mega clue-tiless) clue-i mi)
  (match clue-i
    [(single-line-index _ line-i _)
     (if (= line-i (mega-index-line mi))
         (vector-ref (clues-ref clue-tiless clue-i) (mega-index-tile mi))
         'cross)]
    [_
     (tiles-ref/mega (clues-ref clue-tiless clue-i) mi)]))

#| Note [Mirror mega 2 clue tiles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Mega 2 clues are particularly restrictive, so much so that it can be useful to
think of them as “in between” a single-line clue and a proper mega clue. In many
ways, they behave like a hybrid of both:

  * Like mega clues, mega 2 clues span both lines.

  * However, like single-line clues, mega 2 clues only need a single tiles
    vector to track their placement restrictions, as they are always symmetrical
    across the mega line axis. That is, crosses and filled tiles in one line of
    a mega 2 can always be mirrored to the other line.

  * Like single-line clues, mega 2 clues have exactly one fixed shape.

Another way of thinking of mega 2 clues is that they are analogous to a single-
line 1 clue *in a non-mega line*. To illustrate this point, consider a line
consisting entirely of mega 2 clues: solving this line is precisely identical to
solving a non-mega line of the same number of 1 clues. The only difference
between a line of mega 2 clues and a single line of 1 clues is the constraints
the mega line imposes on the cross axis, which are irrelevant when line solving.

For the most part, we still treat mega 2 clues like other mega clues in our line
solver, as they do behave more like mega clues than single-line clues do in mega
lines. However, we actually use *sharing* to make the mirroring of mega 2 clue
tiles mostly automatic: both lines are represented by the same mutable vector.
This is a cute simplification, but it unfortunately doesn’t really spare us from
treating it as a special case:

  * When writing a 'full tile to a mega 2 clue, we must take care to transfer
    the tile to *both* lines of the board.

  * Similarly, we must place crosses on both lines of other clue tiles.

In principle, it could be considerably more efficient to treat mega 2 clues
specially in other ways, too, but for now, we just use the normal mega clue
solver to keep the logic simpler. |#

;; do-solve-line : mega-line-clues? mega-tile-line? -> (or/c line-solution? 'error)
(define (do-solve-line clues-lst user-tiles)
  (parameterize ([current-gained-information? #f])
    (define num-tiles (array-length (array-ref user-tiles 0)))
    (define num-tiles/mega (* num-tiles 2))
    (define num-chunks (length clues-lst))

    (define (make-empty-tiles)
      (make-vector num-tiles 'empty))
    (define (make-empty-tiles/mega)
      (array (make-empty-tiles)
             (make-empty-tiles)))

    ;; see Note [Mirror mega 2 clue tiles]
    (define (make-empty-tiles/mega-2)
      (define tiles (make-empty-tiles))
      (array tiles tiles))

    ;; `unassigned-user-tiles` tracks all tiles filled by the user that have not
    ;; yet been claimed by any clues.
    (define unassigned-user-tiles (make-empty-tiles/mega))
    (for ([mi (in-range num-tiles/mega)]
          #:when (tile-full? (tiles-ref/mega user-tiles mi)))
      (tiles-set!/mega unassigned-user-tiles mi 'full #:track? #f))

    (define board-tiles (make-empty-tiles/mega))

    (define clues
      (for/array #:length num-chunks
                 ([chunk (in-list clues-lst)])
        (match chunk
          [(array line-0 line-1)
           (array (list->array line-0)
                  (list->array line-1))]
          [_ chunk])))

    (define clue-indexes
      (for/array #:length num-chunks
                 ([(chunk i) (in-indexed (in-array clues))])
        (match chunk
          [(array line-0 line-1)
           (array (for/array #:length (array-length line-0)
                             ([j (in-range (array-length line-0))])
                    (single-line-index i 0 j))
                  (for/array #:length (array-length line-1)
                             ([j (in-range (array-length line-1))])
                    (single-line-index i 1 j)))]
          [_ i])))

    (define mega-clue-indexes
      (for/array ([(chunk i) (in-indexed (in-array clues))]
                  #:when (clue? chunk))
        i))

    (define single-line-clue-indexes
      (for*/array ([chunk (in-array clue-indexes)]
                   #:when (array? chunk)
                   [line-i (in-list '(0 1))]
                   [clue-i (in-array (array-ref chunk line-i))])
        clue-i))

    ;; line-clue-indexes : (or/c 0 1) -> (arrayof (or/c natural? single-line-index?))
    ;; Returns an array like `clue-indexes`, but filtered to the clues that
    ;; appear on each line (with mega clues included in both).
    (define line-clue-indexes
      (let ()
        (define (make line-i)
          (for*/array ([is (in-array clue-indexes)]
                       [i (in-array (if (array? is)
                                        (array-ref is line-i)
                                        (array is)))])
            i))

        (define line-clue-indexes-0 (make 0))
        (define line-clue-indexes-1 (make 1))
        (match-lambda
          [0 line-clue-indexes-0]
          [1 line-clue-indexes-1])))

    (define clue-tiless
      (for/array #:length num-chunks
                 ([chunk (in-array clues)])
        (match chunk
          [(array line-0 line-1)
           (array (for/array #:length (array-length line-0)
                             ([i (in-range (array-length line-0))])
                    (make-empty-tiles))
                  (for/array #:length (array-length line-1)
                             ([i (in-range (array-length line-1))])
                    (make-empty-tiles)))]
          [2
           (make-empty-tiles/mega-2)]
          [_
           (make-empty-tiles/mega)])))

    ;; Tracks the number of tiles that still need to be placed (or assigned) for
    ;; each clue.
    (define clue-tiles-left
      (for/vector #:length num-chunks
                  ([chunk (in-array clues)])
        (match chunk
          [(array line-0 line-1)
           (array (for/vector #:length (array-length line-0)
                              ([clue (in-array line-0)])
                    clue)
                  (for/vector #:length (array-length line-1)
                              ([clue (in-array line-1)])
                    clue))]
          [clue clue])))

    ;; -------------------------------------------------------------------------

    ;; previous-clues : clue-index? -> (listof clue-index?)
    (define (previous-clues clue-i)
      (match clue-i
        [(single-line-index chunk-i line-i i)
         (if (zero? i)
             (if (zero? chunk-i)
                 '()
                 (list (sub1 chunk-i)))
             (list (single-line-index chunk-i line-i (sub1 i))))]
        [_
         (if (zero? clue-i)
             '()
             (match (array-ref clue-indexes (sub1 clue-i))
               [(? array? lines)
                (for/list ([line (in-array lines)]
                           #:unless (zero? (array-length line)))
                  (array-ref line (sub1 (array-length line))))]
               [prev-i
                (list prev-i)]))]))

    ;; next-clues : clue-index? -> (listof clue-index?)
    (define (next-clues clue-i)
      (match clue-i
        [(single-line-index chunk-i line-i i)
         (define line (array-ref (array-ref clue-indexes chunk-i) line-i))
         (if (= (add1 i) (array-length line))
             (if (= (add1 chunk-i) num-chunks)
                 '()
                 (list (add1 chunk-i)))
             (list (array-ref line (add1 i))))]
        [_
         (if (= (add1 clue-i) num-chunks)
             '()
             (match (array-ref clue-indexes (add1 clue-i))
               [(? array? lines)
                (for/list ([line (in-array lines)]
                           #:unless (zero? (array-length line)))
                  (array-ref line 0))]
               [next-i
                (list next-i)]))]))

    ;; clue-tiles-left : clue-index? -> boolean?
    (define (clue-solved? clue-i)
      (zero? (clues-ref clue-tiles-left clue-i)))

    ;; board-set!/mega : mega-index? tile? -> void?
    (define (board-set!/mega mi val
                             #:contradiction-reason [contradiction-reason (current-contradiction-reason)])
      (unless (eq? (tiles-ref/mega board-tiles mi) val)
        (tiles-set!/mega board-tiles mi val #:contradiction-reason contradiction-reason)

        ; propagate crosses to clues
        (when (tile-cross? val)
          (define line-i (mega-index-line mi))
          (for ([clue-i (in-array (line-clue-indexes line-i))])
            (clue-tiles-set!/mega clue-i mi 'cross #:contradiction-reason contradiction-reason)))))

    (define (board-fill!/mega val [start-mi 0] [end-mi num-tiles/mega]
                              #:contradiction-reason [contradiction-reason (current-contradiction-reason)])
      (for ([mi (in-range start-mi end-mi)])
        (board-set!/mega mi val #:contradiction-reason contradiction-reason)))

    (define (board-fill-line! line-i val [start-i 0] [end-i num-tiles]
                              #:contradiction-reason [contradiction-reason (current-contradiction-reason)])
      (for ([i (in-range start-i end-i)])
        (board-set!/mega (mega-index line-i i) val
                         #:contradiction-reason contradiction-reason)))

    ;; clue-tiles-ref/mega : clue-index? mega-index? -> tile?
    (define clue-tiles-ref/mega (make-clue-tiles-ref/mega clue-tiless))

    ;; clue-tiles-set! : clue-index? natural? tile? -> void?
    (define/who (clue-tiles-set! clue-i i val
                                 #:contradiction-reason [contradiction-reason (current-contradiction-reason)])
      (unless (single-line-index? clue-i)
        (raise-arguments-error who "clue index refers to a mega clue"
                               "clue index" clue-i))
      (clue-tiles-set!/mega clue-i (mega-index (single-line-index-line clue-i) i) val
                            #:contradiction-reason contradiction-reason))

    (define (clue-tiles-fill! clue-i val [start-i 0] [end-i num-tiles]
                              #:contradiction-reason [contradiction-reason (current-contradiction-reason)])
      (for ([i (in-range start-i end-i)])
        (clue-tiles-set! clue-i i val #:contradiction-reason contradiction-reason)))

    ;; clue-tiles-set!/mega : clue-index? mega-index? tile? -> void?
    (define/who (clue-tiles-set!/mega clue-i mi val
                                      #:contradiction-reason [contradiction-reason (current-contradiction-reason)])
      (unless (eq? (clue-tiles-ref/mega clue-i mi) val)
        (define clue-tiles (clues-ref clue-tiless clue-i))
        (match clue-i
          [(? single-line-index?)
           (define other-line? (not (= (single-line-index-line clue-i) (mega-index-line mi))))
           (cond
             [(not (= (single-line-index-line clue-i) (mega-index-line mi)))
              (when (tile-full? val)
                (raise-contradiction "filled tile on wrong line for single-line clue"))]
             [else
              (define tile-i (mega-index-tile mi))
              (tiles-set!/track clue-tiles tile-i val #:contradiction-reason contradiction-reason)])]
          [_
           (tiles-set!/mega clue-tiles mi val #:contradiction-reason contradiction-reason)])

        ; propagate filled tiles to board and cross other clues
        (when (tile-full? val)
          (define clue (clues-ref clues clue-i))

          (define (cross-empty-cells! line-tiles)
            (for ([i (in-range num-tiles)]
                  #:when (tile-empty? (vector-ref line-tiles i)))
              (tiles-set!/track line-tiles i 'cross)))

          (define (finish-solved-clue!)
            (cond
              [(single-line-index? clue-i)
               (cross-empty-cells! clue-tiles)
               (propagate-information-to-neighbors/single clue-i)]
              [else
               (cross-empty-cells! (array-ref clue-tiles 0))
               ;; see Note [Mirror mega 2 clue tiles]
               (unless (= clue 2)
                 (cross-empty-cells! (array-ref clue-tiles 1)))
               (propagate-information-to-neighbors/mega clue-i)]))

          (define (transfer! mi)
            (define tiles-left (clues-update! clue-tiles-left clue-i sub1))
            (cond
              [(= tiles-left 0)
               (finish-solved-clue!)]
              [(< tiles-left 0)
               (raise-contradiction "too many tiles filled for clue")])

            (tiles-set!/mega unassigned-user-tiles mi 'empty #:track? #f)
            (board-set!/mega mi 'full)
            (for ([other-i (in-array (line-clue-indexes (mega-index-line mi)))]
                  #:unless (equal? clue-i other-i))
              (clue-tiles-set!/mega other-i mi 'cross
                                    #:contradiction-reason "tile claimed by multiple clues")))
          (transfer! mi)
          (cond
            [(single-line-index? clue-i)
             (define opposite-mi (opposite mi))
             (board-set!/mega opposite-mi 'cross
                              #:contradiction-reason "single-line clue opposes full tile")]

            ;; see Note [Mirror mega 2 clue tiles]
            [(= clue 2)
             (transfer! (opposite mi))]))))

    (define (clue-tiles-fill!/mega clue-i val [start-mi 0] [end-mi num-tiles/mega]
                                   #:contradiction-reason [contradiction-reason (current-contradiction-reason)])
      (for ([mi (in-range start-mi end-mi)])
        (clue-tiles-set!/mega clue-i mi val #:contradiction-reason contradiction-reason)))

    (define (clue-tiles-fill-line! clue-i line-i val [start-i 0] [end-i num-tiles]
                                    #:contradiction-reason [contradiction-reason (current-contradiction-reason)])
      (unless (and (single-line-index? clue-i)
                   (eq? val 'cross)
                   (not (= line-i (single-line-index-line clue-i))))
        (for ([i (in-range start-i end-i)])
          (clue-tiles-set!/mega clue-i (mega-index line-i i) val
                                #:contradiction-reason contradiction-reason))))

    ;; initialize board with user tiles
    (for ([mi (in-range num-tiles/mega)])
      (define tile (tiles-ref/mega user-tiles mi))
      (when (or (tile-full? tile)
                (tile-cross? tile))
        (board-set!/mega mi tile)))

    ;; -------------------------------------------------------------------------

    (define (clue-neighbor-full? clue-i mi)
      (match clue-i
        [(single-line-index _ line-i _)
         (and (= line-i (mega-index-line mi))
              (neighbor-matching? (clues-ref clue-tiless clue-i)
                                  (mega-index-tile mi)
                                  tile-full?))]
        [_
         (neighbor-matching?/mega (clues-ref clue-tiless clue-i) mi tile-full?)]))

    ;; Returns the minimum number of tiles that may be filled to connect
    ;; `first-mi` to `last-mi`, including both endpoints.
    (define/who (shortest-path-size tiles first-mi last-mi)
      (define last-i (mega-index-tile last-mi))
      (let loop ([size 1]
                 [mi first-mi])
        (cond
          [(= mi last-mi) size]
          [else
           (define i (mega-index-tile mi))
           (cond
             [(= i last-i) (add1 size)]
             [(tile-hole? (tiles-ref/mega tiles (afront mi)))
              (loop (add1 size) (afront mi))]
             [(and (tile-hole? (tiles-ref/mega tiles (opposite mi)))
                   (tile-hole? (tiles-ref/mega tiles (afront (opposite mi)))))
              (loop (+ size 2) (afront (opposite mi)))]
             [else
              (raise-arguments-error who "no path"
                                     "first index" first-mi
                                     "last index" last-mi
                                     "tiles" tiles)])])))

    ;; -------------------------------------------------------------------------

    (define (gain-information-from-self/single clue-i)
      (define clue (clues-ref clues clue-i))
      (define clue-tiles (clues-ref clue-tiless clue-i))

      ;; Cross out holes where the clue doesn’t fit.
      (let loop ([i 0])
        (match (find-next-hole clue-tiles i)
          [#f (void)]
          [i
           (define len (hole-length clue-tiles i))
           (when (< len clue)
             (clue-tiles-fill! clue-i 'cross i (+ i len)))
           (loop (+ i len))]))

      ;; If any boxes are filled...
      (match (find-first clue-tiles tile-full?)
        [#f (void)]
        [first-full-i
         (define last-full-i (find-last clue-tiles tile-full?))

         ;; ...fill in boxes between filled boxes.
         (clue-tiles-fill! clue-i 'full first-full-i (add1 last-full-i)
                           #:contradiction-reason "clue is discontiguous")

         ;; ...cross off tiles unreachable due to clue length.
         (clue-tiles-fill! clue-i 'cross 0 (- last-full-i (sub1 clue))
                           #:contradiction-reason "clue is too long")
         (clue-tiles-fill! clue-i 'cross (+ first-full-i clue)
                           #:contradiction-reason "clue is too long")

         ;; ...cross off all holes unreachable due to a separating cross.
         (define prev-cross-i (find-prev clue-tiles first-full-i tile-cross?))
         (when prev-cross-i
           (clue-tiles-fill! clue-i 'cross 0 prev-cross-i))
         (define next-cross-i (find-next clue-tiles first-full-i tile-cross?))
         (when next-cross-i
           (clue-tiles-fill! clue-i 'cross next-cross-i))])

      ;; ensure there is a hole for the clue to actually go
      (unless (find-next-hole clue-tiles 0)
        (raise-contradiction "no space for clue"))

      ;; if there is only one hole, fill boxes if possible
      (match (find-singular-hole clue-tiles)
        [#f (void)]
        [i
         (define len (hole-length clue-tiles i))
         (cond
           [(odd? len)
            (define mid-tile (/ (sub1 len) 2))
            (when (< mid-tile clue)
              (define wing-len (- clue mid-tile 1))
              (clue-tiles-fill!
               #:contradiction-reason "not enough space for clue"
               clue-i
               'full
               (- (+ mid-tile i) wing-len)
               (+ (+ mid-tile i) wing-len 1)))]
           [else
            (define half-len (/ len 2))
            (when (< half-len clue)
              (define wing-len (- clue half-len))
              (clue-tiles-fill!
               #:contradiction-reason "not enough space for clue"
               clue-i
               'full
               (- (+ half-len i) wing-len)
               (+ (+ half-len i) wing-len)))])]))

    ;; -------------------------------------------------------------------------

    (define (gain-information-from-self/mega clue-i)
      (define clue (clues-ref clues clue-i))
      (define clue-tiles (clues-ref clue-tiless clue-i))

      ;; Cross out holes where the clue doesn’t fit.
      (let loop ([mi 0])
        (match (find-next-hole/mega clue-tiles mi)
          [#f (void)]
          [start-mi
           (define-values [size end-mi] (hole-size+end/mega clue-tiles start-mi))
           (when (or (< size clue)
                     (not (connected-region-spans-both-lines? start-mi end-mi size)))
             (clue-tiles-fill!/mega clue-i 'cross start-mi end-mi
                                    #:contradiction-reason "not enough space for clue"))
           (loop end-mi)]))

      ;; Since we need to have at least one tile in each line, check if there is
      ;; only one valid location for a tile in each line and fill it if so.
      (for ([line-i (in-range 2)])
        (define line-tiles (array-ref clue-tiles line-i))
        (define line-first-hole-i (find-first line-tiles tile-hole?))
        (unless line-first-hole-i
          (raise-contradiction "mega clue cannot cover both lines"))
        (define line-last-hole-i (find-last line-tiles tile-hole?))
        (when (= line-first-hole-i line-last-hole-i)
          (clue-tiles-set!/mega clue-i (mega-index line-i line-first-hole-i) 'full)))

      ;; If any boxes are filled...
      (match (find-first/mega clue-tiles tile-full?)
        [#f (void)]
        [first-full-mi
         (define last-full-mi (find-last/mega clue-tiles tile-full?))
         (define first-full-line-i (mega-index-line first-full-mi))
         (define first-full-i (mega-index-tile first-full-mi))
         (define last-full-line-i (mega-index-line last-full-mi))
         (define last-full-i (mega-index-tile last-full-mi))

         (when (>= (- (add1 last-full-i) first-full-i) clue)
           (raise-contradiction "clue is too long"))

         ;; ...fill in boxes that must be filled to bridge filled boxes. Also,
         ;; keep track of the minimum number of tiles filled between the start
         ;; and end points, as well as whether it must have tiles in each line.
         (define-values [min-filled backwards-line-aff forwards-line-aff]
           (cond
             [(= first-full-i last-full-i)
              (if (= first-full-line-i last-full-line-i)
                  (values 1 first-full-line-i first-full-line-i)
                  (values 2 #f #f))]
             [else
              (fill-forced-tiles-in-mega-span!
               clue-tiles
               (mega-index->placement-bound first-full-mi)
               (mega-index->placement-bound last-full-mi)
               #:set-full!
               (λ (mi)
                 (clue-tiles-set!/mega clue-i mi 'full
                                       #:contradiction-reason "clue is discontiguous")))]))

         ;; ...cross out boxes unreachable due to clue length. Also, while we’re
         ;; at it, cross out all holes unreachable due to separating crosses.
         (when (> min-filled clue)
           (raise-contradiction "clue is too long"))
         (define max-left (- clue min-filled))

         ;; Cross out boxes behind us.
         (let ()
           (match-define (array min-filled-i-0 min-filled-i-1)
             (earliest-reachable/mega clue-tiles backwards-line-aff first-full-i max-left))
           (clue-tiles-fill-line! clue-i 0 'cross 0 min-filled-i-0)
           (clue-tiles-fill-line! clue-i 1 'cross 0 min-filled-i-1))

         ;; Cross out boxes in front of us.
         (let ()
           (match-define (array max-filled-i-0 max-filled-i-1)
             (latest-reachable/mega clue-tiles forwards-line-aff last-full-i max-left))
           (clue-tiles-fill-line! clue-i 0 'cross (add1 max-filled-i-0))
           (clue-tiles-fill-line! clue-i 1 'cross (add1 max-filled-i-1)))])

      ;; Ensure there is a hole for the clue to actually go.
      (unless (find-next-hole/mega clue-tiles 0)
        (raise-contradiction "no space for clue"))

      ;; If there is only one hole, fill boxes if possible.
      (match (find-singular-hole/mega clue-tiles)
        [#f (void)]
        [(array start-mi end-mi size)
         (cond
           [(= size clue)
            (for ([mi (in-range start-mi end-mi)]
                  #:when (tile-empty? (tiles-ref/mega clue-tiles mi)))
              (clue-tiles-set!/mega clue-i mi 'full))]
           [(< size clue)
            (raise-contradiction "no space for clue")]
           [else
            (match (find-earliest-tightest-placement-start+end/mega clue-tiles start-mi clue)
              [#f (raise-contradiction "no space for clue")]
              [(cons start-mi* end-lower-bound)
               (match-define (cons last-mi start-upper-bound)
                 (find-latest-tightest-placement-end+start/mega clue-tiles (sub1 end-mi) clue))
               (define end-mi* (add1 last-mi))

               ;; Cross out any ends of the hole where the placement cannot go.
               (clue-tiles-fill!/mega clue-i 'cross start-mi start-mi*
                                      #:contradiction-reason "filled tile cannot belong to earliest tightest placement")
               (clue-tiles-fill!/mega clue-i 'cross end-mi* end-mi
                                      #:contradiction-reason "filled tile cannot belong to latest tightest placement")

               (define end-lower-bounds (mega-placement-end-bound->line-lower-bounds end-lower-bound))
               (define start-upper-bounds (mega-placement-start-bound->line-upper-bounds start-upper-bound))

               ;; It is time to try to fill in boxes using the usual left/right
               ;; solve strategy. However, for mega clues, this is substantially
               ;; more complicated. We have to consider two cases separately:
               ;;
               ;;   1. If the earliest/latest placement overlaps with an existing
               ;;      filled-in tile, we want to “extend” the filled region to the
               ;;      placement’s bound. For example, suppose we have a mega 5
               ;;      clue in the following row:
               ;;        ☒☒☒☐☐
               ;;        ☐■☐☐☐
               ;;      Since the earliest placement’s end bound is larger than the
               ;;      filled tile, we want to fill any tiles forced by crosses
               ;;      from the tile to the bound:
               ;;        ☒☒☒☐☐
               ;;        ☐■■■☐
               ;;
               ;;   2. If the earliest/latest bounds overlap, we want to fill any
               ;;      tiles forced by crosses between them. For example, suppose we
               ;;      have a mega 5 clue in the following row:
               ;;        ☒☐☐☐☐ ⇒ ☒■■■☐
               ;;        ☐☐☒☒☐ ⇒ ☐☐☒☒☐
               ;;
               ;; It might seem as though tiles filled by the second rule would
               ;; always include those filled by the first, as long as we’ve placed
               ;; crosses appropriately to limit the size of the hole. Unfortunately,
               ;; this is not always true, at least with our current algorithm for
               ;; selecting an earliest/latest placement. For example, suppose we
               ;; modify the first example slightly:
               ;;   ☒☒☐☐☐
               ;;   ☐■☐☐☐
               ;; Here, there is no overlap guaranteed by the bounds alone, so if
               ;; we didn’t have any filled-in tiles, we wouldn’t be able to gain
               ;; any information.

               (define (fill-span! first-bound last-bound)
                 (fill-forced-tiles-in-mega-span!
                  clue-tiles first-bound last-bound
                  #:set-full!
                  (λ (mi)
                    (clue-tiles-set!/mega clue-i mi 'full
                                          #:contradiction-reason "not enough space for clue"))))

               ;; First, fill spans based on overlap with filled tiles (case 1 above).
               (define first-full-mi (find-next/mega clue-tiles start-mi* tile-full? #:end end-mi*))
               (when first-full-mi
                 (define last-full-mi (find-prev/mega clue-tiles end-mi* tile-full? #:start start-mi*))
                 (when (< first-full-mi (mega-placement-bound->least-upper-bound end-lower-bound))
                   (fill-span! (mega-index->placement-bound first-full-mi) end-lower-bound))
                 (when (< (mega-placement-bound->greatest-lower-bound start-upper-bound) last-full-mi)
                   (fill-span! start-upper-bound (mega-index->placement-bound last-full-mi))))

               ;; Next, fill span if earliest/latest bounds overlap.
               (when (<= (mega-placement-bound-tile start-upper-bound)
                         (mega-placement-bound-tile end-lower-bound))
                 (fill-span! start-upper-bound end-lower-bound))])])]))

    ;; -------------------------------------------------------------------------

    (define (propagate-information-to-neighbors/single clue-i)
      (define clue (clues-ref clues clue-i))
      (define clue-tiles (clues-ref clue-tiless clue-i))
      (define line-i (single-line-index-line clue-i))
      (define opposite-i (opposite line-i))

      (define contradiction-reason "not enough space between neighboring clues’ filled tiles")

      ;; cross off tiles in previous clue’s range that would necessarily overlap with us
      (define prev-clue-is (previous-clues clue-i))
      (unless (empty? prev-clue-is)
        (define first-full-i (find-first clue-tiles tile-full?))
        (define last-hole-i (find-last clue-tiles tile-hole?))
        (unless last-hole-i
          (raise-contradiction "no space for clue"))
        (define start-i-upper-bound
          (min (or first-full-i num-tiles)
               ;; the latest our first filled tile can appear is bounded by the last hole and our clue size
               (- (add1 last-hole-i) clue)))
        ;; on our line, there must be a gap before the previous clue
        (define line-cross-start-i (max 0 (sub1 start-i-upper-bound)))
        (define opposite-cross-start-i (max 0 start-i-upper-bound))
        (for ([prev-clue-i (in-list prev-clue-is)])
          (clue-tiles-fill-line! prev-clue-i line-i 'cross line-cross-start-i
                                 #:contradiction-reason contradiction-reason)
          (clue-tiles-fill-line! prev-clue-i opposite-i 'cross opposite-cross-start-i
                                 #:contradiction-reason contradiction-reason)))

      ;; cross off tiles in next clue’s range that would necessarily overlap with us
      (define next-clue-is (next-clues clue-i))
      (unless (empty? next-clue-is)
        (define last-full-i (find-last clue-tiles tile-full?))
        (define first-hole-i (find-first clue-tiles tile-hole?))
        (unless first-hole-i
          (raise-contradiction "no space for clue"))
        (define end-i-lower-bound
          (max (if last-full-i (add1 last-full-i) 0)
               ;; the earliest our last filled tile can appear is bounded by the first hole and our clue size
               (if first-hole-i (+ first-hole-i clue) 0)))
        ;; on our line, there must be a gap before the next clue
        (define line-cross-end-i (min num-tiles (add1 end-i-lower-bound)))
        (define opposite-cross-end-i (min num-tiles end-i-lower-bound))
        (for ([next-clue-i (in-list next-clue-is)])
          (clue-tiles-fill-line! next-clue-i line-i 'cross 0 line-cross-end-i
                                 #:contradiction-reason contradiction-reason)
          (clue-tiles-fill-line! next-clue-i opposite-i 'cross 0 opposite-cross-end-i
                                 #:contradiction-reason contradiction-reason))))

    (define (propagate-information-to-neighbors/mega clue-i)
      (define clue (clues-ref clues clue-i))
      (define clue-tiles (clues-ref clue-tiless clue-i))

      (define contradiction-reason "not enough space between neighboring clues’ filled tiles")

      ;; cross off tiles in previous clue’s range that would necessarily overlap with us
      (define prev-clue-is (previous-clues clue-i))
      (unless (empty? prev-clue-is)
        (define first-full-mi (find-first/mega clue-tiles tile-full?))
        (define last-hole-mi (find-last/mega clue-tiles tile-hole?))
        (unless last-hole-mi
          (raise-contradiction "no space for clue"))

        (define first-full-i (and~> first-full-mi mega-index-tile))
        (match (find-latest-tightest-placement-end+start/mega clue-tiles last-hole-mi clue)
          [#f (raise-contradiction "no space for clue")]
          [(cons _ start-bound)
           (define start-upper-bounds (mega-placement-start-bound->line-upper-bounds start-bound))

           (define (get-cross-start line-i)
             (define first-full-bound
               (if first-full-i
                   (if (tile-full? (tiles-ref/mega clue-tiles (mega-index line-i first-full-i)))
                       (sub1 first-full-i)
                       first-full-i)
                   num-tiles))
             (define start-upper-bound (sub1 (array-ref start-upper-bounds line-i)))
             (max 0 (min first-full-bound start-upper-bound)))

           (define cross-start-i-0 (get-cross-start 0))
           (define cross-start-i-1 (get-cross-start 1))
           (for ([prev-clue-i (in-list prev-clue-is)])
             (clue-tiles-fill-line! prev-clue-i 0 'cross cross-start-i-0
                                    #:contradiction-reason contradiction-reason)
             (clue-tiles-fill-line! prev-clue-i 1 'cross cross-start-i-1
                                    #:contradiction-reason contradiction-reason))]))

      ;; cross off tiles in next clue’s range that would necessarily overlap with us
      (define next-clue-is (next-clues clue-i))
      (unless (empty? next-clue-is)
        (define last-full-mi (find-last/mega clue-tiles tile-full?))
        (define first-hole-mi (find-first/mega clue-tiles tile-hole?))
        (unless first-hole-mi
          (raise-contradiction "no space for clue"))

        (define last-full-i (and~> last-full-mi mega-index-tile))
        (match (find-earliest-tightest-placement-start+end/mega clue-tiles first-hole-mi clue)
          [#f
           (raise-contradiction "no space for clue")]
          [(cons _ end-bound)
           (define end-lower-bounds (mega-placement-end-bound->line-lower-bounds end-bound))

           (define (get-cross-end line-i)
             (define last-full-bound
               (if last-full-i
                   (if (tile-full? (tiles-ref/mega clue-tiles (mega-index line-i last-full-i)))
                       (add1 last-full-i)
                       last-full-i)
                   0))
             (define end-lower-bound (+ (array-ref end-lower-bounds line-i) 2))
             (min num-tiles (max last-full-bound end-lower-bound)))

           (define cross-end-i-0 (get-cross-end 0))
           (define cross-end-i-1 (get-cross-end 1))
           (for ([prev-clue-i (in-list next-clue-is)])
             (clue-tiles-fill-line! prev-clue-i 0 'cross 0 cross-end-i-0
                                    #:contradiction-reason contradiction-reason)
             (clue-tiles-fill-line! prev-clue-i 1 'cross 0 cross-end-i-1
                                    #:contradiction-reason contradiction-reason))])))

    ;; -------------------------------------------------------------------------

    (define (propagate-information-from-user)
      ;; Fill in tiles filled by the user that can only belong to one clue.
      ;; Also, check whether the tiles must belong to a mega clue or to a single
      ;; clue and cross out tiles as appropriate.
      (let loop ([start-mi 0])
        (match (find-next/mega unassigned-user-tiles start-mi tile-full?)
          [#f (void)]
          [full-start-mi
           (define-values [full-size full-end-mi]
             (connected-region-size+end/mega unassigned-user-tiles full-start-mi tile-full?))
           (define mega-only?
             (connected-region-spans-both-lines? full-start-mi full-end-mi full-size))
           (define relevant-clue-indexes
             (cond
               [mega-only?
                (for ([clue-i (in-array single-line-clue-indexes)])
                  (clue-tiles-fill!/mega clue-i 'cross full-start-mi full-end-mi
                                         #:contradiction-reason "single line clue would span both lines"))
                mega-clue-indexes]
               [else
                (line-clue-indexes (mega-index-line full-start-mi))]))

           (define candidate-clue-is
             (for/fold ([candidate-clue-is '()])
                       ([clue-i (in-array relevant-clue-indexes)])
               (define compatibility
                 (for/or ([full-mi (in-range full-start-mi full-end-mi)]
                          #:when (tile-full? (tiles-ref/mega unassigned-user-tiles full-mi)))
                   (cond
                     [(tile-cross? (clue-tiles-ref/mega clue-i full-mi))
                      'cannot-claim]
                     [(clue-neighbor-full? clue-i full-mi)
                      'must-claim]
                     [else #f])))
               #:final (eq? compatibility 'must-claim)
               (match compatibility
                 ['cannot-claim
                  (clue-tiles-fill!/mega clue-i 'cross full-start-mi full-end-mi
                                         #:contradiction-reason "user tile borders clue that cannot claim it")
                  candidate-clue-is]
                 ['must-claim (list clue-i)]
                 [#f (cons clue-i candidate-clue-is)])))

           (match candidate-clue-is
             ['()
              (raise-contradiction "tile filled by user cannot belong to any clues")]
             [(list candidate-clue-i)
              (for ([full-mi (in-range full-start-mi full-end-mi)]
                    #:when (tile-full? (tiles-ref/mega unassigned-user-tiles full-mi)))
                (clue-tiles-set!/mega candidate-clue-i full-mi 'full
                                      #:contradiction-reason "user tile borders clue that cannot claim it"))]
             [_
              ;; TODO: Intersect possible placements a la the single-line solver.
              (define single-only? (for/and ([clue-i (in-list candidate-clue-is)])
                                     (single-line-index? clue-i)))
              (when single-only?
                (board-fill-line! (opposite (mega-index-line full-start-mi))
                                  'cross
                                  (mega-index-tile full-start-mi)
                                  (mega-index-tile (add1 full-end-mi))
                                  #:contradiction-reason "single-line clue opposes full tile"))])

           (loop (add1 full-end-mi))])))

    ;; -------------------------------------------------------------------------

    (define (gain-information-to-fixed-point)
      (let go-again ()
        (for ([chunk (in-array clue-indexes)])
          (match chunk
            [(? array?)
             (for* ([line (in-array chunk)]
                    [clue-i (in-array line)])
               (unless (clue-solved? clue-i)
                 (gain-information-from-self/single clue-i))
               (unless (clue-solved? clue-i)
                 (propagate-information-to-neighbors/single clue-i)))]
            [clue-i
             (unless (clue-solved? clue-i)
               (gain-information-from-self/mega clue-i))
             (unless (clue-solved? clue-i)
               (propagate-information-to-neighbors/mega clue-i))]))
        (propagate-information-from-user)
        (when (current-gained-information?)
          (current-gained-information? #f)
          (go-again))))

    ;; ---------------------------------------------------------------------------

    (with-handlers* ([exn:contradiction? (λ (exn) 'error)])
      (gain-information-to-fixed-point)
      (mega-line-solution board-tiles clue-tiless line-clue-indexes))))

;; -------------------------------------------------------------------------

;; solve-line/mega : mega-line-clues? mega-tile-line? -> (or/c tile-line? 'error)
(define (solve-line/mega clues tiles)
  (match (do-solve-line clues tiles)
    ['error 'error]
    [(mega-line-solution board-tiles clue-tiless line-clue-indexes)
     (define num-tiles (mega-tiles-length board-tiles))
     (define clue-tiles-ref/mega (make-clue-tiles-ref/mega clue-tiless))
     (for/array #:length 2 ([line-i (in-range 2)])
       (for/array #:length num-tiles ([i (in-range num-tiles)])
         (define mi (mega-index line-i i))
         (match (tiles-ref/mega board-tiles mi)
           ['empty
            (if (for/and ([clue-i (in-array (line-clue-indexes line-i))])
                  (eq? (clue-tiles-ref/mega clue-i mi) 'cross))
                'cross
                'empty)]
           [tile tile])))]))

(module+ test
  (check-equal? (solve-line/mega '(2) #(#(cross full empty empty)
                                        #(empty full cross empty)))
                #(#(cross full cross cross)
                  #(cross full cross cross)))
  (check-equal? (solve-line/mega '(4) #(#(empty empty)
                                        #(empty empty)))
                #(#(full full)
                  #(full full)))
  (check-equal? (solve-line/mega '(2 2 2) #(#(empty empty empty empty empty)
                                            #(empty empty empty empty empty)))
                #(#(full cross full cross full)
                  #(full cross full cross full)))
  (check-equal? (solve-line/mega '(5) #(#(cross empty empty)
                                        #(empty empty empty)))
                #(#(cross full full)
                  #(full  full full)))
  (check-equal? (solve-line/mega '(3) #(#(cross empty empty empty)
                                        #(full  empty empty empty)))
                #(#(cross full cross cross)
                  #(full  full cross cross)))
  (check-equal? (solve-line/mega '(3) #(#(cross cross empty empty)
                                        #(cross full  empty empty)))
                #(#(cross cross full cross)
                  #(cross full  full cross)))
  (check-equal? (solve-line/mega '(2 3) #(#(full cross cross empty empty)
                                          #(full cross full  empty empty)))
                #(#(full cross cross full cross)
                  #(full cross full  full cross)))
  (check-equal? (solve-line/mega '(4) #(#(cross empty empty)
                                        #(full  empty empty)))
                #(#(cross empty empty)
                  #(full  full  empty)))

  (check-equal? (solve-line/mega '(3) #(#(full full)
                                        #(full empty)))
                #(#(full full)
                  #(full cross)))
  (check-equal? (solve-line/mega '(2 3) #(#(empty empty empty full full)
                                          #(empty empty empty full empty)))
                #(#(empty empty cross full full)
                  #(empty empty cross full cross)))
  (check-equal? (solve-line/mega '(3 2) #(#(full empty empty empty empty)
                                          #(full full  empty empty empty)))
                #(#(full cross cross empty empty)
                  #(full full  cross empty empty)))
  (check-equal? (solve-line/mega '(3 2) #(#(full full  empty empty empty)
                                          #(full empty empty empty empty)))
                #(#(full full  cross empty empty)
                  #(full cross cross empty empty)))

  (check-equal? (solve-line/mega '(3) #(#(full  empty cross)
                                        #(empty empty empty)))
                #(#(full  empty cross)
                  #(empty empty cross)))

  (check-equal? (solve-line/mega '(3 4 #[(1 1) ()])
                                 #(#(empty empty empty empty empty empty empty empty)
                                   #(full  cross full  full  full  cross cross cross)))
                #(#(full full  cross full cross full  cross full)
                  #(full cross full  full full  cross cross cross)))

  (check-equal? (solve-line/mega '(#[(2 1) (2)] 4)
                                 #(#(empty empty empty full  empty empty empty empty empty empty)
                                   #(empty empty empty empty empty full  empty empty empty cross)))
                #(#(empty empty empty full  empty empty empty empty empty empty)
                  #(empty empty empty cross empty full  empty empty empty cross))))

;; -----------------------------------------------------------------------------

;; analyze-line/mega/simple : mega-line-clues? mega-tile-line? -> (or/c 'done 'error #f)
(define (analyze-line/mega/simple clues tiles)
  (define num-tiles (array-length (array-ref tiles 0)))
  (define num-tiles/mega (* num-tiles 2))

  (let loop ([clues clues]
             [mi 0])
    (cond
      [(< mi num-tiles/mega)
       (match (tiles-ref/mega tiles mi)
         ['full
          (match clues
            ['() 'error]
            [(cons chunk clues)
             (match chunk
               ;; single line clues
               [(and line-clues (array clues-0 clues-1))
                (define line-i (mega-index-line mi))
                (match (array-ref line-clues line-i)
                  ['() #f]
                  [(cons clue line-clues)
                   (define start-i (mega-index-tile mi))
                   (define end-i (+ start-i clue))
                   (define line-tiles (array-ref tiles line-i))
                   (define opposite-tiles (array-ref tiles (opposite line-i)))
                   (and (<= end-i num-tiles)
                        (for/and ([i (in-range start-i end-i)])
                          (and (tile-full? (array-ref line-tiles i))
                               (not (tile-full? (array-ref opposite-tiles i)))))
                        (or (= end-i num-tiles)
                            (not (tile-full? (array-ref line-tiles end-i))))
                        (let ()
                          (define chunk* (match line-i
                                           [0 (array line-clues clues-1)]
                                           [1 (array clues-0 line-clues)]))
                          (loop (if (and (empty? (array-ref chunk* 0))
                                         (empty? (array-ref chunk* 1)))
                                    clues
                                    (cons chunk* clues))
                                (opposite (mega-index line-i end-i)))))])]
               ;; mega clue
               [clue
                (define-values [size end-mi]
                  (connected-region-size+end/mega tiles mi tile-full?))
                (and (= size clue)
                     (connected-region-spans-both-lines? mi end-mi size)
                     (loop clues end-mi))])])]
         [_ (loop clues (add1 mi))])]
      [(empty? clues) 'done]
      [else #f])))

(module+ test
  (check-equal? (analyze-line/mega/simple '(2) #(#(empty empty) #(empty empty))) #f)
  (check-equal? (analyze-line/mega/simple '(2) #(#(full full) #(empty empty))) #f)
  (check-equal? (analyze-line/mega/simple '(2) #(#(full empty) #(full empty))) 'done)
  (check-equal? (analyze-line/mega/simple '(2) #(#(full empty full) #(full empty empty))) 'error)

  (check-equal? (analyze-line/mega/simple '(#[(1) (2)]) #(#(full  empty empty)
                                                          #(empty full  full)))
                'done)
  (check-equal? (analyze-line/mega/simple '(#[(1) (2)]) #(#(empty empty full)
                                                          #(full  full  empty)))
                'done)
  (check-equal? (analyze-line/mega/simple '(4 #[(1) ()])
                                          #(#(cross full cross full)
                                            #(full  full full  cross)))
                'done)
  (check-equal? (analyze-line/mega/simple '(#[(1) ()] 3)
                                          #(#(empty cross full)
                                            #(empty full  full)))
                #f))

;; analyze-line/mega/solve : mega-line-clues? mega-tile-line? -> line-clue-analysis?
(define (analyze-line/mega/solve clues user-tiles)
  (match (do-solve-line clues user-tiles)
    ['error 'error]
    [(mega-line-solution _ clue-tiless _)
     ;; Like `extract-analysis` for single line clues, but also checks that all of
     ;; the opposite tiles are crossed out.
     (define (extract-analysis/single clue-i)
       (define line-i (single-line-index-line clue-i))
       (define clue-tiles (clues-ref clue-tiless clue-i))
       (define first-full-i (find-first clue-tiles tile-full?))
       (cond
         [first-full-i
          (define last-full-i (find-last clue-tiles tile-full?))
          (define this-user-tiles (array-ref user-tiles line-i))
          (define opposite-user-tiles (array-ref user-tiles (opposite line-i)))
          (if (and (bounded-before? this-user-tiles first-full-i)
                   (bounded-after? this-user-tiles last-full-i)
                   (for/and ([i (in-inclusive-range first-full-i last-full-i)])
                     (and (tile-full? (array-ref this-user-tiles i))
                          (tile-cross? (array-ref opposite-user-tiles i)))))
              'done
              'pending)]
         [else 'pending]))

     (define (extract-analysis/mega clue-i)
       (define clue-tiles (clues-ref clue-tiless clue-i))
       (define first-full-mi (find-first/mega clue-tiles tile-full?))
       (cond
         [first-full-mi
          (define last-full-mi (find-last/mega clue-tiles tile-full?))
          (if (and (bounded-before?/mega user-tiles first-full-mi)
                   (bounded-after?/mega user-tiles last-full-mi)
                   (for/and ([mi (in-inclusive-range first-full-mi last-full-mi)])
                     (define tile (tiles-ref/mega user-tiles mi))
                     (or (tile-full? tile)
                         (tile-cross? tile))))
              'done
              'pending)]
         [else 'pending]))

     (for/list ([(chunk i) (in-indexed (in-list clues))])
       (match chunk
         [(array line-0 line-1)
          (array (for/list ([j (in-range (length line-0))])
                   (extract-analysis/single (single-line-index i 0 j)))
                 (for/list ([j (in-range (length line-1))])
                   (extract-analysis/single (single-line-index i 1 j))))]
         [_
          (extract-analysis/mega i)]))]))

(module+ test
  (check-equal? (analyze-line/mega/solve '(2) #(#(empty empty empty empty)
                                                #(empty empty empty empty)))
                '(pending))
  (check-equal? (analyze-line/mega/solve '(2) #(#(empty full empty empty)
                                                #(empty full empty empty)))
                '(pending))
  (check-equal? (analyze-line/mega/solve '(2) #(#(cross full cross empty)
                                                #(cross full empty empty)))
                '(pending))
  (check-equal? (analyze-line/mega/solve '(2) #(#(cross full empty empty)
                                                #(empty full cross empty)))
                '(pending))
  (check-equal? (analyze-line/mega/solve '(2) #(#(cross full cross empty)
                                                #(empty full cross empty)))
                '(pending))
  (check-equal? (analyze-line/mega/solve '(2) #(#(empty full cross empty)
                                                #(cross full cross empty)))
                '(pending))
  (check-equal? (analyze-line/mega/solve '(2) #(#(cross full  cross empty)
                                                #(cross empty cross empty)))
                '(pending))
  (check-equal? (analyze-line/mega/solve '(2) #(#(cross empty cross empty)
                                                #(cross full  cross empty)))
                '(pending))
  (check-equal? (analyze-line/mega/solve '(2) #(#(cross full cross empty)
                                                #(cross full cross empty)))
                '(done))
  (check-equal? (analyze-line/mega/solve '(2) #(#(cross cross cross cross)
                                                #(empty empty empty empty)))
                'error)
  (check-equal? (analyze-line/mega/solve '(2) #(#(full  full  empty empty)
                                                #(empty empty empty empty)))
                'error))

;; analyze-line/mega : mega-line-clues? mega-tile-line? -> line-clue-analysis?
(define (analyze-line/mega clues tiles)
  (or (analyze-line/mega/simple clues tiles)
      (analyze-line/mega/solve clues tiles)))

(module+ test
  (check-equal? (analyze-line/mega '(#[() (1)] 3)
                                   #(#(empty empty empty empty)
                                     #(full  empty empty empty)))
                '(#[() (pending)] pending))
  (check-equal? (analyze-line/mega '(#[() (1)] 3)
                                   #(#(cross empty empty empty)
                                     #(full  cross empty empty)))
                '(#[() (done)] pending))
  (check-equal? (analyze-line/mega '(2 #[(1 1) ()])
                                   #(#(empty empty cross full  cross empty)
                                     #(empty empty empty cross empty empty)))
                '(pending #[(done pending) ()]))
  (check-equal? (analyze-line/mega '(2 3 #[(1) ()])
                                   #(#(empty empty empty empty empty)
                                     #(empty empty empty empty empty)))
                '(pending pending #[(pending) ()]))
  (check-equal? (analyze-line/mega '(3 5)
                                   #(#(empty full empty cross empty full)
                                     #(empty full empty full  full  full)))
                '(pending pending))
  (check-equal? (analyze-line/mega '(3 4)
                                   #(#(empty full  cross full empty)
                                     #(full  cross empty full empty)))
                '(pending pending))
  (check-equal? (analyze-line/mega '(3 4)
                                   #(#(empty full  cross full empty)
                                     #(full  empty empty full empty)))
                '(pending pending))
  (check-equal? (analyze-line/mega '(#[(1) ()] 3)
                                   #(#(empty cross full)
                                     #(empty full  full)))
                '(#[(pending) ()] pending))
  (check-equal? (analyze-line/mega '(3 4 #[(1 1) ()])
                                   #(#(empty empty empty empty empty empty empty empty)
                                     #(full  cross full  full  full  cross cross cross)))
                '(pending pending #[(pending pending) ()]))
  (check-equal? (analyze-line/mega '(3 3 #[(1 1) ()])
                                   #(#(empty empty empty empty empty empty empty)
                                     #(full  cross full  full  cross cross cross)))
                'error)
  (check-equal? (analyze-line/mega '(#[(1) (1)] 2 2)
                                   #(#(empty empty empty empty empty empty)
                                     #(full  full  empty empty empty empty)))
                'error)
  (check-equal? (analyze-line/mega '(#[(1) (1)] 2 2)
                                   #(#(empty full empty empty empty empty)
                                     #(full  full empty empty empty empty)))
                'error)

  (check-equal? (analyze-line/mega '(2 2 4)
                                   #(#(empty empty cross full cross empty cross empty empty)
                                     #(empty empty cross full cross empty full  empty empty)))
                '(pending done pending)))
