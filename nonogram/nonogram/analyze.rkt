#lang racket/base

(require racket/contract
         racket/list
         racket/match
         racket/math
         threading
         toolbox/who
         "array.rkt"
         "core.rkt"
         "geometry.rkt"
         "vector.rkt")

(module+ test
  (require rackunit
           (submod "core.rkt" example)))

(provide (contract-out
          [clue-analysis? flat-contract?]
          [single-line-analysis? flat-contract?]
          [mega-line-analysis? flat-contract?]
          [line-clue-analysis? flat-contract?]
          [axis-clue-analysis? flat-contract?]
          (struct board-analysis ([row-analysis axis-clue-analysis?]
                                  [column-analysis axis-clue-analysis?]))

          [analyze-line (-> line-clues? tile-line? line-clue-analysis?)]
          [analyze-puzzle (-> puzzle? board-analysis?)]
          [reanalyze-lines-at (-> puzzle? board-analysis? integer-point? board-analysis?)]))

;; -----------------------------------------------------------------------------

(define current-gained-information? (make-parameter #f))

(define/who (tiles-set!/track vec i val #:contradiction-reason [contradiction-reason #f])
  (when (eq? val 'empty)
    (raise-arguments-error who "internal error: setting clue range tile to 'empty"))
  (match (vector-ref vec i)
    ['empty
     (vector-set! vec i val)
     (current-gained-information? #t)]
    [(== val eq?)
     (void)]
    [other-val
     (if contradiction-reason
         (raise-contradiction contradiction-reason)
         (raise-arguments-error who "internal error: overwriting non-empty clue range tile"
                                "old tile" other-val
                                "new tile" val))]))

(define (tiles-fill!/track vec value [start-i 0] [end-i (vector-length vec)]
                           #:contradiction-reason [contradiction-reason #f])
  (for ([i (in-range start-i end-i)])
    (tiles-set!/track vec i value #:contradiction-reason contradiction-reason)))

(define (tile-cross? tile)
  (eq? tile 'cross))
(define (tile-full? tile)
  (eq? tile 'full))
(define (tile-hole? tile)
  (not (eq? tile 'cross)))

(define (not-full-before? tiles i)
  (or (= i 0)
      (not (tile-full? (vector-ref tiles (sub1 i))))))
(define (not-full-after? tiles i)
  (or (= (add1 i) (array-length tiles))
      (not (tile-full? (vector-ref tiles (add1 i))))))

(define (bounded-before? tiles i)
  (or (= i 0)
      (tile-cross? (vector-ref tiles (sub1 i)))))
(define (bounded-after? tiles i)
  (or (= (add1 i) (array-length tiles))
      (tile-cross? (vector-ref tiles (add1 i)))))

(define (find-next vec start-i pred?)
  (let loop ([i start-i])
    (if (< i (vector-length vec))
        (if (pred? (vector-ref vec i))
            i
            (loop (add1 i)))
        #f)))

(define (find-first vec pred?)
  (find-next vec 0 pred?))

(define (find-prev vec end-i pred?)
  (let loop ([i (sub1 end-i)])
    (if (>= i 0)
        (if (pred? (vector-ref vec i))
            i
            (loop (sub1 i)))
        #f)))

(define (find-last vec pred?)
  (find-prev vec (vector-length vec) pred?))

;; Returns the length of the contiguous span starting at `start-i` values satisfying `pred`.
(define (span-length vec start-i pred?)
  (let loop ([i start-i])
    (if (pred? (vector-ref vec i))
        (if (< (add1 i) (vector-length vec))
            (loop (add1 i))
            (- (add1 i) start-i))
        (- i start-i))))

(define (find-next-hole vec start-i)
  (find-next vec start-i tile-hole?))

(define (hole-length vec start-i)
  (span-length vec start-i tile-hole?))

;; Like `find-next-hole`, but returns `#f` if there is more than one hole.
(define (find-singular-hole vec)
  (match (find-next-hole vec 0)
    [#f #f]
    [first-i
     (define first-len (hole-length vec first-i))
     (match (find-next-hole vec (+ first-i first-len))
       [#f first-i]
       [_  #f])]))

(define (neighbor-matching? tiles i pred?)
  (or (and (> i 0)
           (pred? (vector-ref tiles (sub1 i))))
      (and (< (add1 i) (vector-length tiles))
           (pred? (vector-ref tiles (add1 i))))))

(define (cross-out-holes-too-small-to-fit clue clue-range)
  (let loop ([i 0])
    (match (find-next-hole clue-range i)
      [#f (void)]
      [i
       (define len (hole-length clue-range i))
       (when (< len clue)
         (tiles-fill!/track clue-range 'cross i (+ i len)))
       (loop (+ i len))])))

;; -----------------------------------------------------------------------------

(define clue-analysis? (or/c 'done 'pending))

(define single-line-analysis? (listof clue-analysis?))
(define mega-line-analysis?
  (listof (or/c clue-analysis?
                (array/c single-line-analysis?
                         single-line-analysis?))))

(define line-clue-analysis?
  (or/c 'done
        'error
        single-line-analysis?
        mega-line-analysis?))

(define axis-clue-analysis? (listof line-clue-analysis?))
(struct board-analysis
  (row-analysis     ;; axis-clue-analysis?
   column-analysis) ;; axis-clue-analysis?
  #:transparent)

(struct exn:contradiction exn:fail () #:transparent)
(define (raise-contradiction why)
  (raise (exn:contradiction
          (format "analyze-puzzle: contradiction in puzzle\n  reason: ~a" why)
          (current-continuation-marks))))

;; -----------------------------------------------------------------------------

;; analyze-line/simple : line-clues? tile-line? -> (or/c 'done 'error #f)
(define (analyze-line/simple clues tiles)
  (define num-tiles (array-length tiles))
  (let loop ([clues clues]
             [i 0])
    (cond
      [(< i num-tiles)
       (match (array-ref tiles i)
         ['full
          (match clues
            ['() 'error]
            [(cons clue clues)
             (and (<= (+ i clue) num-tiles)
                  (for/and ([j (in-range 1 clue)])
                    (eq? (array-ref tiles (+ i j)) 'full))
                  (or (= (+ i clue) num-tiles)
                      (not (eq? (array-ref tiles (+ i clue)) 'full)))
                  (loop clues (+ i clue 1)))])]
         [_ (loop clues (add1 i))])]
      [(empty? clues) 'done]
      [else #f])))

(module+ test
  (check-equal? (analyze-line/simple '(1) #(empty empty empty)) #f)
  (check-equal? (analyze-line/simple '(1) #(full empty empty)) 'done)
  (check-equal? (analyze-line/simple '(1) #(empty full empty)) 'done)
  (check-equal? (analyze-line/simple '(1) #(empty empty full)) 'done)
  (check-equal? (analyze-line/simple '(1 1) #(full full empty)) #f)
  (check-equal? (analyze-line/simple '(1 1) #(full empty full)) 'done)
  (check-equal? (analyze-line/simple '(1) #(full empty full)) 'error))

;; -----------------------------------------------------------------------------

;; analyze-line/fancy : single-line-clues? tile-line? -> single-line-analysis?
(define (analyze-line/fancy clues-lst user-tiles)
  (parameterize ([current-gained-information? #f])
    (define num-tiles (array-length user-tiles))

    (define clues (list->array clues-lst))
    (define num-clues (array-length clues))
    (define clue-ranges
      (for/array #:length num-clues
                 ([i (in-range num-clues)])
        (define clue-range (make-vector num-tiles 'empty))

        ;; add crosses based on space needed by clues before/after
        (define occupied-before
          (for/sum ([clue (in-array clues 0 i)])
            (add1 clue)))
        (define occupied-after
          (for/sum ([clue (in-array clues (add1 i))])
            (add1 clue)))
        (vector-fill! clue-range 'cross 0 occupied-before)
        (vector-fill! clue-range 'cross (- num-tiles occupied-after))

        ;; propagate crosses from user tiles
        (for ([(tile j) (in-indexed (in-array user-tiles))]
              #:when (eq? tile 'cross))
          (vector-set! clue-range j 'cross))

        clue-range))

    ;; ---------------------------------------------------------------------------

    ;; Returns the index of the next clue of length `at-least-len`, starting with
    ;; index `clue-i`, for which the tile at index `tile-i` is 'empty in its clue
    ;; range.
    (define (find-next-clue-with-hole-at clue-i tile-i #:at-least-length at-least-len)
      (for/first ([clue (in-array clues clue-i)]
                  [clue-range (in-array clue-ranges clue-i)]
                  [i (in-naturals)]
                  #:when (and (>= clue at-least-len)
                              (tile-hole? (vector-ref clue-range tile-i))))
        (+ clue-i i)))

    ;; Returns whether a clue could be legally placed starting at the given
    ;; `start-i`, checking consistency with both user-tiles and its clue-range.
    (define (valid-clue-placement? clue-i start-i)
      (define clue (array-ref clues clue-i))
      (define clue-range (array-ref clue-ranges clue-i))
      (define end-i (+ start-i clue))
      (and (<= end-i num-tiles)
           (not-full-before? user-tiles start-i)
           (not-full-before? clue-range start-i)
           (not-full-after? user-tiles (sub1 end-i))
           (not-full-after? clue-range (sub1 end-i))
           (for/and ([i (in-range start-i end-i)])
             (and (tile-hole? (array-ref user-tiles i))
                  (tile-hole? (vector-ref clue-range i))))))

    ;; Returns the smallest or largest (depending on `which`) index at which the
    ;; placement of clue `clue-i` could begin that would cause it to overlap
    ;; `tile-i`. If no such placement exists, returns #f.
    ;;
    ;; Note: This is a kind of string matching, which could theoretically be
    ;; accelerated using a string matching algorithm like KMP or BM.
    (define (find-placement-covering-tile which clue-i tile-i)
      (define clue (array-ref clues clue-i))
      (define start-i (max 0 (add1 (- tile-i clue))))
      (define end-i (min tile-i (- num-tiles clue)))
      (match which
        ['earliest (for/first ([i (in-inclusive-range start-i end-i)]
                               #:when (valid-clue-placement? clue-i i))
                     i)]
        ['latest   (for/first ([i (in-inclusive-range end-i start-i -1)]
                               #:when (valid-clue-placement? clue-i i))
                     i)]))

    ;; Searches for the first clue after `start-clue-i` with a valid placement
    ;; covering `tile-i`. If found, returns a pair of two values: the clue’s index
    ;; and the index of the first tile in the found placement. If not found,
    ;; returns #f.
    (define (find-earliest-clue-with-placement-covering-tile start-clue-i tile-i #:placement which-placement)
      (for/or ([clue-i (in-range start-clue-i num-clues)])
        (define placement-i (find-placement-covering-tile which-placement clue-i tile-i))
        (and placement-i (cons clue-i placement-i))))

    ;; Like `find-earliest-clue-with-placement-covering-tile`, but for the latest
    ;; clue instead of the earliest.
    (define (find-latest-clue-with-placement-covering-tile end-clue-i tile-i #:placement which-placement)
      (for/or ([clue-i (in-inclusive-range (sub1 end-clue-i) 0 -1)])
        (define placement-i (find-placement-covering-tile which-placement clue-i tile-i))
        (and placement-i (cons clue-i placement-i))))

    ;; ---------------------------------------------------------------------------

    (define (gain-information-from-self* clue-i)
      (define clue (array-ref clues clue-i))
      (define clue-range (array-ref clue-ranges clue-i))

      ;; Cross out holes where the clue doesn’t fit.
      (let loop ([i 0])
        (match (find-next-hole clue-range i)
          [#f (void)]
          [i
           (define len (hole-length clue-range i))
           (when (< len clue)
             (tiles-fill!/track clue-range 'cross i (+ i len)))
           (loop (+ i len))]))

      ;; if any boxes are filled...
      (match (find-first clue-range tile-full?)
        [#f (void)]
        [first-full-i
         (define last-full-i (find-last clue-range tile-full?))

         ;; ...fill in boxes between filled boxes
         (tiles-fill!/track clue-range 'full first-full-i (add1 last-full-i)
                            #:contradiction-reason "clue is discontiguous")

         ;; ...cross off tiles unreachable due to clue length
         (tiles-fill!/track clue-range 'cross 0 (- last-full-i (sub1 clue))
                            #:contradiction-reason "clue is too long")
         (tiles-fill!/track clue-range 'cross (+ first-full-i clue)
                            #:contradiction-reason "clue is too long")

         ;; ...cross off all holes unreachable due to a separating cross
         (define prev-cross-i (find-prev clue-range first-full-i tile-cross?))
         (when prev-cross-i
           (tiles-fill!/track clue-range 'cross 0 prev-cross-i))
         (define next-cross-i (find-next clue-range first-full-i tile-cross?))
         (when next-cross-i
           (tiles-fill!/track clue-range 'cross next-cross-i))])

      ;; ensure there is a hole for the clue to actually go
      (unless (find-next-hole clue-range 0)
        (raise-contradiction "not enough space for clue"))

      ;; if there is only one hole, fill boxes if possible
      (match (find-singular-hole clue-range)
        [#f (void)]
        [i
         (define len (hole-length clue-range i))
         (cond
           [(odd? len)
            (define mid-tile (/ (sub1 len) 2))
            (when (< mid-tile clue)
              (define wing-len (- clue mid-tile 1))
              (tiles-fill!/track
               #:contradiction-reason "not enough space for clue"
               clue-range
               'full
               (- (+ mid-tile i) wing-len)
               (+ (+ mid-tile i) wing-len 1)))]
           [else
            (define half-len (/ len 2))
            (when (< half-len clue)
              (define wing-len (- clue half-len))
              (tiles-fill!/track
               #:contradiction-reason "not enough space for clue"
               clue-range
               'full
               (- (+ half-len i) wing-len)
               (+ (+ half-len i) wing-len)))])]))

    (define (propagate-information-to-neighbors clue-i)
      (define clue (array-ref clues clue-i))
      (define clue-range (array-ref clue-ranges clue-i))

      ;; cross off tiles in previous clue’s range that would necessarily overlap with us
      (unless (zero? clue-i)
        (define first-full-i (find-first clue-range tile-full?))
        (define last-hole-i (find-last clue-range tile-hole?))
        (define last-tile-previous-clue-can-occupy
          (min
           ;; if we have a filled tile, then the previous clue must leave a gap before it
           (if first-full-i (sub1 first-full-i) num-tiles)
           ;; the latest our first filled tile can appear is determined by the last hole and our clue size
           (if last-hole-i (- last-hole-i clue) num-tiles)))
        (when (< last-tile-previous-clue-can-occupy num-tiles)
          (define other-clue-range (array-ref clue-ranges (sub1 clue-i)))
          (tiles-fill!/track other-clue-range 'cross (max 0 last-tile-previous-clue-can-occupy)
                             #:contradiction-reason "not enough space between neighboring clues’ full tiles")))

      ;; cross off tiles in next clue’s range that would necessarily overlap with us
      (when (< (add1 clue-i) num-clues)
        (define last-full-i (find-last clue-range tile-full?))
        (define first-hole-i (find-first clue-range tile-hole?))
        (define first-tile-next-clue-can-occupy
          (max
           ;; if we have a filled tile, then the next clue must leave a gap after it
           (if last-full-i (+ last-full-i 2) 0)
           ;; the earliest our last filled tile can appear is determined by the first hole and our clue size
           (if first-hole-i (+ first-hole-i clue 1) 0)))
        (when (> first-tile-next-clue-can-occupy 0)
          (define other-clue-range (array-ref clue-ranges (add1 clue-i)))
          (tiles-fill!/track other-clue-range 'cross 0 (min num-tiles first-tile-next-clue-can-occupy)
                             #:contradiction-reason "not enough space between neighboring clues’ full tiles"))))

    (define (propagate-information-from-user)
      ;; Scan forwards through the user’s filled-in tiles.
      (let loop ([start-tile-i 0]
                 [start-clue-i 0])
        (match (find-next user-tiles start-tile-i tile-full?)
          [#f (void)]
          [full-i
           ;; Find the earliest clue that could potentially cover this tile.
           (match (find-earliest-clue-with-placement-covering-tile start-clue-i full-i #:placement 'latest)
             [#f
              (raise-contradiction "tile filled by user cannot belong to any clues")]
             [(cons placed-clue-i placement-i)
              (define placed-clue (array-ref clues placed-clue-i))
              ;; Cross out tiles in the previous clue at/after the placement, as
              ;; it must be placed before this one.
              (unless (zero? placed-clue-i)
                (define prev-clue-range (array-ref clue-ranges (sub1 placed-clue-i)))
                (tiles-fill!/track prev-clue-range 'cross (max 0 (sub1 placement-i))
                                   #:contradiction-reason "users’ filled tiles are inconsistent with clue order"))
              ;; Advance to the end of the placement and continue. The next filled
              ;; tiles, if any, must belong to later clues.
              (define start-tile-i* (+ placement-i placed-clue 1))
              (when (< start-tile-i* num-tiles)
                (loop start-tile-i* (add1 placed-clue-i)))])]))

      ;; Now repeat the above process but scanning backwards, instead.
      (let loop ([end-tile-i num-tiles]
                 [end-clue-i num-clues])
        (match (find-prev user-tiles end-tile-i tile-full?)
          [#f (void)]
          [full-i
           ;; Find the latest clue that could potentially cover this tile.
           (match (find-latest-clue-with-placement-covering-tile end-clue-i full-i #:placement 'earliest)
             [#f
              (raise-contradiction "tile filled by user cannot belong to any clues")]
             [(cons placed-clue-i placement-i)
              (define placed-clue (array-ref clues placed-clue-i))
              ;; Cross out tiles in the next clue at/before the placement, as it
              ;; must be placed after this one.
              (when (< (add1 placed-clue-i) num-clues)
                (define placement-end-i (+ placement-i placed-clue))
                (define prev-clue-range (array-ref clue-ranges (add1 placed-clue-i)))
                (tiles-fill!/track prev-clue-range 'cross 0 (min num-tiles (add1 placement-i))
                                   #:contradiction-reason "users’ filled tiles are inconsistent with clue order"))
              ;; Advance to the start of the placement and continue. The next filled
              ;; tiles, if any, must belong to earlier clues.
              (define end-tile-i* (sub1 placement-i))
              (when (> end-tile-i* 0)
                (loop end-tile-i* placed-clue-i))])]))

      ;; fill in tiles filled by the user that can only belong to one clue
      (for ([(tile tile-i) (in-indexed (in-array user-tiles))]
            #:when (eq? tile 'full))
        (define len (span-length user-tiles tile-i tile-full?))
        (define first-empty-clue-i (find-next-clue-with-hole-at 0 tile-i #:at-least-length len))
        (if first-empty-clue-i
            (unless (find-next-clue-with-hole-at (add1 first-empty-clue-i) tile-i #:at-least-length len)
              ;; only one clue range with a hole at this location
              (tiles-set!/track (array-ref clue-ranges first-empty-clue-i) tile-i 'full))
            (raise-contradiction "tile filled by user cannot belong to any clues"))))

    (define (gain-information-to-fixed-point)
      (let go-again ()
        (for ([i (in-range num-clues)])
          (gain-information-from-self* i)
          (propagate-information-to-neighbors i))
        (propagate-information-from-user)
        (when (current-gained-information?)
          (current-gained-information? #f)
          (go-again))))

    ;; ---------------------------------------------------------------------------

    (define (extract-analysis clue-range)
      (define first-full-i (find-first clue-range tile-full?))
      (cond
        [first-full-i
         (define last-full-i (find-last clue-range tile-full?))
         (if (and (bounded-before? user-tiles first-full-i)
                  (bounded-after? user-tiles last-full-i)
                  (for/and ([i (in-inclusive-range first-full-i last-full-i)])
                    (tile-full? (array-ref user-tiles i))))
             'done
             'pending)]
        [else 'pending]))

    (with-handlers* ([exn:contradiction? (λ (exn) 'error)])
      (gain-information-to-fixed-point)
      (for/list ([clue-range (in-array clue-ranges)])
        (extract-analysis clue-range)))))

;; analyze-line : single-line-clues? tile-line? -> line-clue-analysis?
(define (analyze-line line-clues tiles)
  (or (analyze-line/simple line-clues tiles)
      (analyze-line/fancy line-clues tiles)))

(module+ test
  (check-equal? (analyze-line '(1 1) #(full cross empty)) '(done pending))
  (check-equal? (analyze-line '(1 1) #(full full  empty)) 'error)
  (check-equal? (analyze-line '(3) #(cross empty full)) 'error)
  (check-equal? (analyze-line '(3) #(cross empty empty)) 'error)
  (check-equal? (analyze-line '(3 1) #(empty empty empty cross full cross empty)) '(pending done))
  (check-equal? (analyze-line '(1 1 1) #(cross cross full cross full cross empty cross cross)) '(done done pending))
  (check-equal? (analyze-line '(1 1) #(cross cross full cross empty empty empty)) '(done pending))
  (check-equal? (analyze-line '(1 3 2) #(empty cross full full full cross empty empty empty cross full empty)) '(pending done pending))
  (check-equal? (analyze-line '(2 2 1 1) #(empty empty cross full full cross empty full empty empty full empty cross full cross empty)) '(done pending pending done)))

;; -----------------------------------------------------------------------------

;; A *mega index* is a tile index for mega lines.
(define mega-index? natural?)
(define (mega-index line tile)
  (bitwise-ior (arithmetic-shift tile 1) line))
(define (mega-index-line mi)
  (bitwise-bit-field mi 0 1))
(define (mega-index-tile mi)
  (arithmetic-shift mi -1))

;; Note [Mega tile directions]
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; A tile in a mega line has a maximum of three neighbors, not two. We
;; generally describe relative positions along the main axis as “afront” or
;; “behind” and relative positions along the cross axis as “opposite”. A tile
;; always has a neighbor opposite it, it has a neighbor afront it unless it is
;; at the end of the line, and it has a neighbor behind it unless it is at the
;; start of the line.

(define (opposite mi)
  (bitwise-xor mi 1))

(define afront
  (case-lambda
    [(mi)   (+ mi 2)]
    [(mi n) (+ mi (* n 2))]))

(define behind
  (case-lambda
    [(mi)   (- mi 2)]
    [(mi n) (- mi (* n 2))]))

(define (tiles-ref/mega tiles mi)
  (vector-ref (array-ref tiles (mega-index-line mi))
              (mega-index-tile mi)))

(define (tiles-count/mega tiles pred? [start-mi 0] [end-mi (vector-length tiles)])
  (for/sum ([mi (in-range start-mi end-mi)]
            #:when (pred? (tiles-ref/mega tiles mi)))
    1))

;; Given a mega index in the middle of a connected region matching `pred?`,
;; returns the index of the first tile in the region.
(define/who (connected-region-start/mega tiles middle-mi pred?)
  (unless (pred? (tiles-ref/mega tiles middle-mi))
    (raise-arguments-error who "given tile does not match predicate"
                           "tiles" tiles
                           "index" middle-mi))
  (let loop ([mi middle-mi])
    (define at-start? (zero? (mega-index-tile mi)))
    (define connected-behind?
      (and (not at-start?)
           (pred? (tiles-ref/mega tiles (behind mi)))))
    (if connected-behind?
        (loop (behind mi))
        (match (mega-index-line mi)
          [0 (if (and (not at-start?)
                      (pred? (tiles-ref/mega tiles (opposite mi)))
                      (pred? (tiles-ref/mega tiles (sub1 mi))))
                 (loop (sub1 mi))
                 mi)]
          [1 (if (pred? (tiles-ref/mega tiles (opposite mi)))
                 (loop (opposite mi))
                 mi)]))))

;; Given a mega index at the start of a connected region matching `pred?`,
;; returns two values. The first is the size of the region (i.e. the number
;; of connected tiles), and the second is the index one past the last tile
;; in the region.
(define/who (connected-region-size+end/mega tiles start-mi pred?)
  (unless (pred? (tiles-ref/mega tiles start-mi))
    (raise-arguments-error who "starting tile does not match predicate"
                           "tiles" tiles
                           "index" start-mi))
  (define num-tiles (array-length (array-ref tiles 0)))
  (let loop ([size 1]
             [mi start-mi])
    (define at-end?
      (= (add1 (mega-index-tile mi)) num-tiles))
    (define connected-afront?
      (and (not at-end?)
           (pred? (tiles-ref/mega tiles (afront mi)))))
    (match (mega-index-line mi)
      [0
       (define connected-opposite?
         (pred? (tiles-ref/mega tiles (opposite mi))))
       (if connected-afront?
           (if connected-opposite?
               (loop (+ size 2) (afront mi))
               (loop (+ size 1) (afront mi)))
           (if connected-opposite?
               (loop (+ size 1) (opposite mi))
               (values size (opposite mi))))]
      [1
       (if connected-afront?
           (if (pred? (tiles-ref/mega tiles (add1 mi)))
               (loop (+ size 1) (add1 mi))
               (loop (+ size 1) (afront mi)))
           (values size (add1 mi)))])))

(define (connected-region-end/mega tiles start-mi pred?)
  (define-values [size end-mi]
    (connected-region-size+end/mega tiles start-mi pred?))
  end-mi)

(define (hole-size+end/mega tiles start-mi)
  (connected-region-size+end/mega tiles start-mi tile-hole?))

(define (connected-region-spans-both-lines? start-mi end-mi size)
  (define len (- (mega-index-tile (afront (sub1 end-mi)))
                 (mega-index-tile start-mi)))
  (< len size))

;; Note [Line affinity]
;; ~~~~~~~~~~~~~~~~~~~~
;; In some situations, using a mega index is too precise: it always
;; identifies a specific line. For example, consider the following row:
;;   █▔X▔▔
;;   █▁▁▁█
;; Suppose we want to calculate the minimum number of tiles needed to
;; connect the two contiguous filled regions. We cannot specify the location
;; of the starting point using a mega index, as it would require picking
;; a specific tile in one of the two lines. Instead, in these situations, we
;; use a combination of a tile index and a “line affinity”, which is either
;; 0, 1, or #f, where #f represents “no affinity”.

;; earliest-reachable/mega : mega-tile-line? line-affinity? natural? natural?
;;                        -> (array/c natural? natural?)
;;
;; Given a line affinity and starting tile index, returns an array of two
;; tile indexes corresponding to the earliest tile in each line that could
;; be included in a contiguous region of size at most `size` bordering the
;; starting point.
(define (earliest-reachable/mega tiles line-aff start-tile-i size)
  (let loop ([line-aff line-aff]
             [i start-tile-i]
             [size size])
    (cond
      [(zero? size)
       (array (if (eqv? line-aff 1) (add1 i) i)
              (if (eqv? line-aff 0) (add1 i) i))]
      [(zero? i)
       (array 0 0)]
      [else
       (define mi0 (mega-index 0 (sub1 i)))
       (define mi1 (mega-index 1 (sub1 i)))
       (define tile-0 (tiles-ref/mega tiles mi0))
       (define tile-1 (tiles-ref/mega tiles mi1))
       (match* {tile-0 tile-1}
         [{'cross 'cross}
          (array i i)]
         [{_ 'cross}
          (cond
            [(tile-cross? (tiles-ref/mega tiles (afront mi0)))
             (array (add1 i) i)]
            [(eqv? line-aff 1)
             (loop #f i (sub1 size))]
            [else
             (loop 0 (sub1 i) (sub1 size))])]
         [{'cross _}
          (cond
            [(tile-cross? (tiles-ref/mega tiles (afront mi1)))
             (array i (add1 i))]
            [(eqv? line-aff 0)
             (loop #f i (sub1 size))]
            [else
             (loop 1 (sub1 i) (sub1 size))])]
         [{_ _}
          (loop line-aff (sub1 i) (sub1 size))])])))

(module+ test
  (let ([tiles #(#(empty empty empty empty cross empty)
                 #(empty cross empty empty empty empty))])
    (check-equal? (earliest-reachable/mega tiles #f 5 0) #(5 5))
    (check-equal? (earliest-reachable/mega tiles 0  5 0) #(5 6))
    (check-equal? (earliest-reachable/mega tiles 1  5 0) #(6 5))
    (check-equal? (earliest-reachable/mega tiles #f 5 1) #(5 4))
    (check-equal? (earliest-reachable/mega tiles 0  5 1) #(5 5))
    (check-equal? (earliest-reachable/mega tiles 1  5 1) #(5 4))
    (check-equal? (earliest-reachable/mega tiles #f 5 2) #(4 3))
    (check-equal? (earliest-reachable/mega tiles #f 5 3) #(3 2))
    (check-equal? (earliest-reachable/mega tiles #f 5 4) #(2 2))
    (check-equal? (earliest-reachable/mega tiles #f 5 5) #(1 2))
    (check-equal? (earliest-reachable/mega tiles #f 5 6) #(0 1))
    (check-equal? (earliest-reachable/mega tiles #f 5 7) #(0 0))
    (check-equal? (earliest-reachable/mega tiles #f 5 8) #(0 0)))

  (check-equal? (earliest-reachable/mega #(#(empty cross)
                                           #(cross empty))
                                         1 1 1)
                #(2 1)))

;; Like `earliest-reachable/mega`, but searching forwards instead of
;; backwards.
(define (latest-reachable/mega tiles line-aff start-tile-i size)
  (define num-tiles (vector-length (array-ref tiles 0)))
  (let loop ([line-aff line-aff]
             [i start-tile-i]
             [size size])
    (cond
      [(zero? size)
       (array (if (eqv? line-aff 1) (sub1 i) i)
              (if (eqv? line-aff 0) (sub1 i) i))]
      [(= (add1 i) num-tiles)
       (array (sub1 num-tiles)
              (sub1 num-tiles))]
      [else
       (define mi0 (mega-index 0 (add1 i)))
       (define mi1 (mega-index 1 (add1 i)))
       (define tile-0 (tiles-ref/mega tiles mi0))
       (define tile-1 (tiles-ref/mega tiles mi1))
       (match* {tile-0 tile-1}
         [{'cross 'cross}
          (array i i)]
         [{_ 'cross}
          (cond
            [(tile-cross? (tiles-ref/mega tiles (behind mi0)))
             (array (sub1 i) i)]
            [(eqv? line-aff 1)
             (loop #f i (sub1 size))]
            [else
             (loop 0 (add1 i) (sub1 size))])]
         [{'cross _}
          (cond
            [(tile-cross? (tiles-ref/mega tiles (behind mi1)))
             (array i (sub1 i))]
            [(eqv? line-aff 0)
             (loop #f i (sub1 size))]
            [else
             (loop 1 (add1 i) (sub1 size))])]
         [{_ _}
          (loop line-aff (add1 i) (sub1 size))])])))

(module+ test
  (let ([tiles #(#(empty empty empty empty cross empty)
                 #(empty cross empty empty empty empty))])
    (check-equal? (latest-reachable/mega tiles #f 0 0) #(0 0))
    (check-equal? (latest-reachable/mega tiles 0  0 0) #(0 -1))
    (check-equal? (latest-reachable/mega tiles 1  0 0) #(-1 0))
    (check-equal? (latest-reachable/mega tiles #f 0 1) #(1 0))
    (check-equal? (latest-reachable/mega tiles 0  0 1) #(1 0))
    (check-equal? (latest-reachable/mega tiles 1  0 1) #(0 0))
    (check-equal? (latest-reachable/mega tiles #f 0 2) #(2 1))
    (check-equal? (latest-reachable/mega tiles #f 0 3) #(3 2))
    (check-equal? (latest-reachable/mega tiles #f 0 4) #(3 3))
    (check-equal? (latest-reachable/mega tiles #f 0 5) #(3 4))
    (check-equal? (latest-reachable/mega tiles #f 0 6) #(4 5))
    (check-equal? (latest-reachable/mega tiles #f 0 7) #(5 5))
    (check-equal? (latest-reachable/mega tiles #f 0 8) #(5 5)))

  (check-equal? (latest-reachable/mega #(#(empty cross)
                                         #(cross empty))
                                       0 0 1)
                #(0 -1)))

(define mega-placement-bound-line? (or/c 0 1 'either 'both))
(struct mega-placement-bound (line tile) #:transparent)

;; mega-placement-end-bound->line-lower-bounds
;;   : mega-placement-bound? -> (array/c natural? natural?)
;;
;; Given a mega-placement-bound that represents an ending bound for a mega clue,
;; returns the lower bound of the possible tile indexes in each line.
(define (mega-placement-end-bound->line-lower-bounds bound)
  (match-define (mega-placement-bound line-i i) bound)
  (match line-i
    [0       (array i        (sub1 i))]
    [1       (array (sub1 i) i       )]
    ['both   (array i        i       )]
    ['either (array (sub1 i) (sub1 i))]))

;; mega-placement-start-bound->line-upper-bounds
;;   : mega-placement-bound? -> (array/c natural? natural?)
;;
;; Given a mega-placement-bound that represents a starting bound for a mega clue,
;; returns the upper bound of the possible tile indexes in each line.
(define (mega-placement-start-bound->line-upper-bounds bound)
  (match-define (mega-placement-bound line-i i) bound)
  (match line-i
    [0       (array i        (add1 i))]
    [1       (array (add1 i) i       )]
    ['both   (array i        i       )]
    ['either (array (add1 i) (add1 i))]))

;; Given a mega index at the start of a hole, finds the earliest or latest
;; (depending on `which`), smallest placement in the hole for a clue of size
;; `size`. Returns a pair of two values:
;;   1. A starting (ending) mega index that identifies the start (end) of the placement.
;;   2. A mega-placement-bound structure that identifies the end (start) of the placement.
;; If no valid placement exists within the hole, returns #f.
;;
;; The returned starting index may be larger than the given start of the hole if
;; the region must be placed later to ensure it spans both lines. For example,
;; given the row
;;   ▔▔▔▔▔
;;   XXX▁▁
;; then the earliest possible placement for a clue of size 4 is
;;   ▔███▔
;;   XXX█▁
;; which necessitates a larger starting index.
;;
;; Currently, this function does not consider placement restrictions imposed by
;; existing filled-in tiles.
(define (do-find-tightest-placement+bounds/mega tiles start-mi size
                                                #:who who
                                                #:which which)
  (unless (tile-hole? (tiles-ref/mega tiles start-mi))
    (raise-arguments-error who "starting tile is not a hole"
                           "tiles" tiles
                           "index" start-mi))

  (define-values [next prev next/mega special-line-i before-end?]
    (match which
      ['earliest (values add1 sub1 afront 1 (λ (i) (< i num-tiles)))]
      ['latest   (values sub1 add1 behind 0 (λ (i) (>= i 0)))]))

  (define num-tiles (array-length (array-ref tiles 0)))
  (define start-line-i (mega-index-line start-mi))
  (define start-tile-i (mega-index-tile start-mi))
  (let loop ([start-mi start-mi]
             [line-aff (if (= start-line-i special-line-i) special-line-i #f)]
             [i (if (= start-line-i special-line-i) (next start-tile-i) start-tile-i)]
             [size (if (= start-line-i special-line-i) (sub1 size) size)]
             [covered-both? #f])
    (cond
      [(zero? size)
       (if covered-both?
           (cons start-mi (mega-placement-bound (or line-aff 'both) (prev i)))
           (loop (next/mega start-mi) line-aff i (add1 size) #f))]
      [(before-end? i)
       (define mi0 (mega-index 0 i))
       (define mi1 (mega-index 1 i))
       (define tile-0 (tiles-ref/mega tiles mi0))
       (define tile-1 (tiles-ref/mega tiles mi1))
       (match* {tile-0 tile-1}
         [{'cross 'cross} #f]
         [{_ 'cross}
          (if (eqv? line-aff 1)
              #f
              (loop start-mi 0 (next i) (sub1 size) covered-both?))]
         [{'cross _}
          (if (eqv? line-aff 0)
              #f
              (loop start-mi 1 (next i) (sub1 size) covered-both?))]
         [{_ _}
          (if (= size 1)
              (if covered-both?
                  (cons start-mi (mega-placement-bound (or line-aff 'either) i))
                  (cons (next/mega start-mi) (mega-placement-bound 'both i)))
              (loop start-mi #f (next i) (- size 2) #t))])]
      [else #f])))

;; find-earliest-tightest-placement-start+end/mega
;;   : mega-tile-line? mega-index? mega-clue?
;;  -> (or/c (cons/c mega-index? mega-placement-bound?) #f)
;;
;; See `do-find-tightest-placement+bounds/mega`.
(define/who (find-earliest-tightest-placement-start+end/mega tiles start-mi size)
  (do-find-tightest-placement+bounds/mega tiles start-mi size
                                          #:who who
                                          #:which 'earliest))

;; find-latest-tightest-placement-end+start/mega
;;   : mega-tile-line? mega-index? mega-clue?
;;  -> (or/c (cons/c mega-index? mega-placement-bound?) #f)
;;
;; Like find-earliest-tightest-placement-start+end/mega, but starting from the
;; end of a hole instead of the beginning. The given `end-mi` should be the
;; final tile in the hole (i.e. it is inclusive).
(define/who (find-latest-tightest-placement-end+start/mega tiles end-mi size)
  (do-find-tightest-placement+bounds/mega tiles end-mi size
                                          #:who who
                                          #:which 'latest))

(module+ test
  (check-equal? (find-latest-tightest-placement-end+start/mega
                 #(#(cross empty cross cross)
                   #(cross empty empty cross))
                 (mega-index 1 2)
                 3)
                (cons (mega-index 1 2)
                      (mega-placement-bound 'both 1)))
  (check-equal? (find-latest-tightest-placement-end+start/mega
                 #(#(empty empty empty empty empty)
                   #(empty empty cross cross cross))
                 (mega-index 0 4)
                 4)
                (cons (mega-index 0 3)
                      (mega-placement-bound 'both 1))))

;; clue-index? = (or/c natural? single-line-index?)
;; clue index for single line clues in mega lines
(struct single-line-index (chunk line index) #:transparent)

;; analyze-line/mega/simple : mega-line-clues? (array/c tile-line? tile-line?) -> (or/c 'done 'error #f)
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
                               (tile-hole? (array-ref opposite-tiles i))))
                        (or (= end-i num-tiles)
                            (tile-hole? (array-ref line-tiles end-i)))
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
                     (loop clues (add1 end-mi)))])])]
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
  (check-equal? (analyze-line/mega/simple '(#[(1) ()] 3)
                                          #(#(empty cross full)
                                            #(empty full  full)))
                #f))

;; analyze-line/mega/fancy : mega-line-clues? (array/c tile-line? tile-line?) -> mega-line-analysis?
(define (analyze-line/mega/fancy clues-lst user-tiles)

  ;; -------------------------------------------------------------------------

  (parameterize ([current-gained-information? #f])
    (define num-tiles (array-length (array-ref user-tiles 0)))
    (define num-tiles/mega (* num-tiles 2))
    (define num-chunks (length clues-lst))

    (define (make-empty-tiles)
      (make-vector num-tiles 'empty))
    (define (make-empty-tiles/mega)
      (array (make-empty-tiles)
             (make-empty-tiles)))

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

    (define clue-ranges
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
          [_
           (make-empty-tiles/mega)])))

    ;; -------------------------------------------------------------------------

    (define (clues-ref carr clue-i)
      (match clue-i
        [(single-line-index chunk-i line-i i)
         (array-ref (array-ref (array-ref carr chunk-i) line-i) i)]
        [chunk-i
         (array-ref carr chunk-i)]))

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

    (define/who (tiles-set!/mega tiles mi val #:contradiction-reason [contradiction-reason #f])
      (tiles-set!/track (array-ref tiles (mega-index-line mi))
                        (mega-index-tile mi)
                        val
                        #:contradiction-reason contradiction-reason))

    (define (tiles-fill!/mega tiles val [start-mi 0] [end-mi num-tiles/mega]
                              #:contradiction-reason [contradiction-reason #f])
      (for ([mi (in-range start-mi end-mi)])
        (tiles-set!/mega tiles mi val #:contradiction-reason contradiction-reason)))

    ;; board-set!/mega : mega-index? tile? -> void?
    (define (board-set!/mega mi val #:contradiction-reason [contradiction-reason #f])
      (tiles-set!/mega board-tiles mi val)

      ; propagate crosses to clues
      (when (tile-cross? val)
        (define line-i (mega-index-line mi))
        (for ([clue-i (in-array (line-clue-indexes line-i))])
          (define clue-range (clues-ref clue-ranges clue-i))
          (clue-tiles-set!/mega clue-i mi 'cross #:contradiction-reason contradiction-reason))))

    (define (board-fill!/mega val [start-mi 0] [end-mi num-tiles/mega]
                              #:contradiction-reason contradiction-reason)
      (for ([mi (in-range start-mi end-mi)])
        (board-fill!/mega mi val #:contradiction-reason contradiction-reason)))

    ;; clue-tiles-ref/mega : clue-index? mega-index? -> tile?
    (define (clue-tiles-ref/mega clue-i mi)
      (match clue-i
        [(single-line-index _ line-i _)
         (if (= line-i (mega-index-line mi))
             (vector-ref (clues-ref clue-ranges clue-i) (mega-index-tile mi))
             'cross)]
        [_
         (tiles-ref/mega (clues-ref clue-ranges clue-i) mi)]))

    ;; clue-tiles-set! : clue-index? natural? tile? -> void?
    (define/who (clue-tiles-set! clue-i i val #:contradiction-reason [contradiction-reason #f])
      (unless (single-line-index? clue-i)
        (raise-arguments-error who "clue index refers to a mega clue"
                               "clue index" clue-i))
      (clue-tiles-set!/mega clue-i (mega-index (single-line-index-line clue-i) i) val))

    (define (clue-tiles-fill! clue-i val [start-i 0] [end-i num-tiles]
                              #:contradiction-reason [contradiction-reason #f])
      (for ([i (in-range start-i end-i)])
        (clue-tiles-set! clue-i i val #:contradiction-reason contradiction-reason)))

    ;; clue-tiles-set!/mega : clue-index? mega-index? tile? -> void?
    (define/who (clue-tiles-set!/mega clue-i mi val #:contradiction-reason [contradiction-reason #f])
      (define clue-range (clues-ref clue-ranges clue-i))
      (match clue-i
        [(? single-line-index?)
         (define other-line? (not (= (single-line-index-line clue-i) (mega-index-line mi))))
         (cond
           [(not (= (single-line-index-line clue-i) (mega-index-line mi)))
            (when (tile-full? val)
              (raise-contradiction "filled tile on wrong line for single-line clue"))]
           [else
            (define tile-i (mega-index-tile mi))
            (tiles-set!/track clue-range tile-i val #:contradiction-reason contradiction-reason)])]
        [_
         (tiles-set!/mega clue-range mi val #:contradiction-reason contradiction-reason)])

      ; propagate filled tiles to board and cross other clues
      (when (tile-full? val)
        (board-set!/mega mi 'full)
        (for ([other-i (in-array (line-clue-indexes (mega-index-line mi)))]
              #:unless (equal? clue-i other-i))
          (clue-tiles-set!/mega other-i mi 'cross
                                #:contradiction-reason "tile claimed by multiple clues"))

        ; if this is a single-line clue, cross opposite tiles
        (when (single-line-index? clue-i)
          (define opposite-mi (opposite mi))
          (board-set!/mega opposite-mi 'cross
                           #:contradiction-reason "single-line clue opposes full tile"))))

    (define (clue-tiles-fill!/mega clue-i val [start-mi 0] [end-mi num-tiles/mega]
                                   #:contradiction-reason [contradiction-reason #f])
      (for ([mi (in-range start-mi end-mi)])
        (clue-tiles-set!/mega clue-i mi val #:contradiction-reason contradiction-reason)))

    (define (clue-tiles-fill-line! clue-i line-i val [start-i 0] [end-i num-tiles]
                                    #:contradiction-reason [contradiction-reason #f])
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

    (define (bounded-before?/mega tiles mi)
      (define at-start? (zero? (mega-index-tile mi)))
      (and (or at-start?
               (tile-cross? (tiles-ref/mega tiles (behind mi))))
           (or (tile-cross? (tiles-ref/mega tiles (opposite mi)))
               (match (mega-index-line mi)
                 [0 (or at-start?
                        (tile-cross? (tiles-ref/mega tiles (sub1 mi))))]
                 [1 #f]))))

    (define (bounded-after?/mega tiles mi)
      (define at-end? (= (add1 (mega-index-tile mi)) num-tiles))
      (and (or at-end?
               (tile-cross? (tiles-ref/mega tiles (afront mi))))
           (or (tile-cross? (tiles-ref/mega tiles (opposite mi)))
               (match (mega-index-line mi)
                 [0 #f]
                 [1 (or at-end?
                        (tile-cross? (tiles-ref/mega tiles (add1 mi))))]))))

    (define (find-next/mega tiles start-mi pred?)
      (let loop ([mi start-mi])
        (if (< mi num-tiles/mega)
            (if (pred? (tiles-ref/mega tiles mi))
                mi
                (loop (add1 mi)))
            #f)))

    (define (find-first/mega tiles pred?)
      (find-next/mega tiles 0 pred?))

    (define (find-prev/mega tiles end-mi pred?)
      (let loop ([mi (sub1 end-mi)])
        (if (>= mi 0)
            (if (pred? (tiles-ref/mega tiles mi))
                mi
                (loop (sub1 mi)))
            #f)))

    (define (find-last/mega tiles pred?)
      (find-prev/mega tiles num-tiles/mega pred?))

    (define (find-next-hole/mega tiles start-mi)
      (find-next/mega tiles start-mi tile-hole?))

    (define (neighbor-matching?/mega tiles mi pred?)
      (or (pred? (tiles-ref/mega tiles (opposite mi)))
          (and (> (mega-index-tile mi) 0)
               (pred? (tiles-ref/mega tiles (behind mi))))
          (and (< (add1 (mega-index-tile mi)) num-tiles)
               (pred? (tiles-ref/mega tiles (afront mi))))))

    (define (clue-neighbor-full? clue-i mi)
      (match clue-i
        [(single-line-index _ line-i _)
         (and (= line-i (mega-index-line mi))
              (neighbor-matching? (clues-ref clue-ranges clue-i)
                                  (mega-index-tile mi)
                                  tile-full?))]
        [_
         (neighbor-matching?/mega (clues-ref clue-ranges clue-i) mi tile-full?)]))

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
      (define clue-range (clues-ref clue-ranges clue-i))

      ;; Cross out holes where the clue doesn’t fit.
      (let loop ([i 0])
        (match (find-next-hole clue-range i)
          [#f (void)]
          [i
           (define len (hole-length clue-range i))
           (when (< len clue)
             (clue-tiles-fill! clue-i 'cross i (+ i len)))
           (loop (+ i len))]))

      ;; if any boxes are filled...
      (match (find-first clue-range tile-full?)
        [#f (void)]
        [first-full-i
         (define last-full-i (find-last clue-range tile-full?))

         ;; ...fill in boxes between filled boxes
         (clue-tiles-fill! clue-i 'full first-full-i (add1 last-full-i)
                           #:contradiction-reason "clue is discontiguous")

         ;; ...cross off tiles unreachable due to clue length
         (clue-tiles-fill! clue-i 'cross 0 (- last-full-i (sub1 clue))
                           #:contradiction-reason "clue is too long")
         (clue-tiles-fill! clue-i 'cross (+ first-full-i clue)
                           #:contradiction-reason "clue is too long")

         ;; ...cross off all holes unreachable due to a separating cross
         (define prev-cross-i (find-prev clue-range first-full-i tile-cross?))
         (when prev-cross-i
           (clue-tiles-fill! clue-i 'cross 0 prev-cross-i))
         (define next-cross-i (find-next clue-range first-full-i tile-cross?))
         (when next-cross-i
           (clue-tiles-fill! clue-i 'cross next-cross-i))])

      ;; ensure there is a hole for the clue to actually go
      (unless (find-next-hole clue-range 0)
        (raise-contradiction "no space for clue"))

      ;; if there is only one hole, fill boxes if possible
      (match (find-singular-hole clue-range)
        [#f (void)]
        [i
         (define len (hole-length clue-range i))
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
      (define clue-range (clues-ref clue-ranges clue-i))

      ;; Cross out holes where the clue doesn’t fit.
      (let loop ([mi 0])
        (match (find-next-hole/mega clue-range mi)
          [#f (void)]
          [start-mi
           (define-values [size end-mi] (hole-size+end/mega clue-range start-mi))
           (when (or (< size clue)
                     (not (connected-region-spans-both-lines? start-mi end-mi size)))
             (clue-tiles-fill!/mega clue-i 'cross start-mi end-mi
                                    #:contradiction-reason "not enough space for clue"))
           (loop end-mi)]))

      ;; Since we need to have at least one tile in each line, check if there is
      ;; only one valid location for a tile in each line and fill it if so.
      (for ([line-i (in-range 2)])
        (define line-tiles (array-ref clue-range line-i))
        (define line-first-hole-i (find-first line-tiles tile-hole?))
        (unless line-first-hole-i
          (raise-contradiction "mega clue cannot cover both lines"))
        (define line-last-hole-i (find-last line-tiles tile-hole?))
        (when (= line-first-hole-i line-last-hole-i)
          (clue-tiles-set!/mega clue-i (mega-index line-i line-first-hole-i) 'full)))

      ;; If any boxes are filled...
      (match (find-first/mega clue-range tile-full?)
        [#f (void)]
        [first-full-mi
         (define last-full-mi (find-last/mega clue-range tile-full?))
         (define first-full-line-i (mega-index-line first-full-mi))
         (define first-full-i (mega-index-tile first-full-mi))
         (define last-full-line-i (mega-index-line last-full-mi))
         (define last-full-i (mega-index-tile last-full-mi))

         (when (>= (- (add1 last-full-i) first-full-i) clue)
           (raise-contradiction "clue is too long"))

         ;; ...fill in boxes that must be filled to bridge filled boxes. Also,
         ;; keep track of the minimum number of tiles filled between the start
         ;; and end points, as well as whether it must have tiles in each line.
         (define-values [min-filled filled-0? filled-1?]
           (let ()
             (define (fill! mi)
               (clue-tiles-set!/mega clue-i mi 'full #:contradiction-reason "clue is discontiguous"))

             (let loop ([min-filled 0]
                        [filled-0? (= first-full-line-i 0)]
                        [filled-1? (= first-full-line-i 1)]
                        [line-aff #f]
                        [i first-full-i])

               (define (line-aff? line-i)
                 (or (not line-aff) (= line-aff line-i)))
               (define (line-cost line-i)
                 (if (line-aff? line-i) 0 1))

               (cond
                 [(> i last-full-i)
                  (values min-filled filled-0? filled-1?)]
                 [else
                  (define mi0 (mega-index 0 i))
                  (define mi1 (mega-index 1 i))
                  (define tile-0 (tiles-ref/mega clue-range mi0))
                  (define tile-1 (tiles-ref/mega clue-range mi1))
                  (match* {tile-0 tile-1}
                    [{_ 'cross}
                     (unless (= i first-full-i)
                       (fill! (behind mi0)))
                     (fill! mi0)
                     (unless (= i last-full-i)
                       (fill! (afront mi0)))
                     (loop (+ min-filled 1 (line-cost 0))
                           #t
                           filled-1?
                           0
                           (add1 i))]
                    [{'cross _}
                     (unless (= i first-full-i)
                       (fill! (behind mi1)))
                     (fill! mi1)
                     (unless (= i last-full-i)
                       (fill! (afront mi1)))
                     (loop (+ min-filled 1 (line-cost 1))
                           filled-0?
                           #t
                           1
                           (add1 i))]
                    [{'full 'full}
                     (loop (+ min-filled 2) #t #t #f (add1 i))]
                    [{'full 'empty}
                     (if (line-aff? 0)
                         (loop (+ min-filled 1) #t filled-1? 0 (add1 i))
                         (loop (+ min-filled 2) #t #t #f (add1 i)))]
                    [{'empty 'full}
                     (if (line-aff? 1)
                         (loop (+ min-filled 1) filled-0? #t 1 (add1 i))
                         (loop (+ min-filled 2) #t #t #f (add1 i)))]
                    [{'empty 'empty}
                     (loop (+ min-filled 1) filled-0? filled-1? line-aff (add1 i))])]))))

         ;; ...cross out boxes unreachable due to clue length. Also, while we’re
         ;; at it, cross out all holes unreachable due to separating crosses.
         (when (> min-filled clue)
           (raise-contradiction "clue is too long"))
         (define max-left (- clue min-filled))

         ;; Cross out boxes behind us.
         (let ()
           (define line-aff (if (tile-cross? (tiles-ref/mega clue-range (opposite first-full-mi)))
                                first-full-line-i
                                #f))
           (match-define (array min-filled-i-0 min-filled-i-1)
             (earliest-reachable/mega clue-range line-aff first-full-i max-left))
           #;((current-print) (list 'behind
                                    (array 'clue-i clue-i)
                                    (array 'clue clue)
                                    (array 'clue-range clue-range)
                                    (array 'line-aff line-aff)
                                    (array 'first-full-i first-full-i)
                                    (array 'max-left max-left)))
           (clue-tiles-fill-line! clue-i 0 'cross 0 min-filled-i-0)
           (clue-tiles-fill-line! clue-i 1 'cross 0 min-filled-i-1))

         ;; Cross out boxes in front of us.
         (let ()
           (define line-aff (if (tile-cross? (tiles-ref/mega clue-range (opposite last-full-mi)))
                                last-full-line-i
                                #f))
           (match-define (array max-filled-i-0 max-filled-i-1)
             (latest-reachable/mega clue-range line-aff last-full-i max-left))
           (clue-tiles-fill-line! clue-i 0 'cross (add1 max-filled-i-0))
           (clue-tiles-fill-line! clue-i 1 'cross (add1 max-filled-i-1)))])

      ;; Ensure there is a hole for the clue to actually go.
      (unless (find-next-hole/mega clue-range 0)
        (raise-contradiction "no space for clue")))

    ;; -------------------------------------------------------------------------

    (define (propagate-information-to-neighbors/single clue-i)
      (define clue (clues-ref clues clue-i))
      (define clue-range (clues-ref clue-ranges clue-i))
      (define line-i (single-line-index-line clue-i))
      (define opposite-i (opposite line-i))

      (define contradiction-reason "not enough space between neighboring clues’ filled tiles")

      ;; cross off tiles in previous clue’s range that would necessarily overlap with us
      (define prev-clue-is (previous-clues clue-i))
      (unless (empty? prev-clue-is)
        (define first-full-i (find-first clue-range tile-full?))
        (define last-hole-i (find-last clue-range tile-hole?))
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
        (define last-full-i (find-last clue-range tile-full?))
        (define first-hole-i (find-first clue-range tile-hole?))
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
      (define clue-range (clues-ref clue-ranges clue-i))

      (define contradiction-reason "not enough space between neighboring clues’ filled tiles")

      ;; cross off tiles in previous clue’s range that would necessarily overlap with us
      (define prev-clue-is (previous-clues clue-i))
      (unless (empty? prev-clue-is)
        (define first-full-mi (find-first/mega clue-range tile-full?))
        (define last-hole-mi (find-last/mega clue-range tile-hole?))
        (unless last-hole-mi
          (raise-contradiction "no space for clue"))

        (define first-full-i (and~> first-full-mi mega-index-tile))
        #;((current-print) (list (array 'clue-i clue-i)
                               (array 'clue clue)
                               (array 'clue-range clue-range)
                               (array 'first-full-mi first-full-mi)
                               (array 'last-hole-mi last-hole-mi)))
        (match-define (cons _ start-bound)
          (find-latest-tightest-placement-end+start/mega clue-range last-hole-mi clue))
        (define start-upper-bounds (mega-placement-start-bound->line-upper-bounds start-bound))

        (define (get-cross-start line-i)
          (define first-full-bound
            (if first-full-i
                (if (tile-full? (tiles-ref/mega clue-range (mega-index line-i first-full-i)))
                    (sub1 first-full-i)
                    first-full-i)
                num-tiles))
          (define start-upper-bound (sub1 (array-ref start-upper-bounds line-i)))
          (max 0 (min first-full-bound start-upper-bound)))

        (define cross-start-i-0 (get-cross-start 0))
        (define cross-start-i-1 (get-cross-start 1))
        #;((current-print) (list (array 'clue-i clue-i)
                               (array 'clue clue)
                               (array 'clue-range clue-range)
                               (array 'tightest-placement (find-latest-tightest-placement-end+start/mega clue-range last-hole-mi clue))
                               (array 'start-upper-bounds start-upper-bounds)
                               (array 'first-full-i first-full-i)
                               (array 'cross-start-i-0 cross-start-i-0)
                               (array 'cross-start-i-1 cross-start-i-1)))
        (for ([prev-clue-i (in-list prev-clue-is)])
          (clue-tiles-fill-line! prev-clue-i 0 'cross cross-start-i-0
                                 #:contradiction-reason contradiction-reason)
          (clue-tiles-fill-line! prev-clue-i 1 'cross cross-start-i-1
                                 #:contradiction-reason contradiction-reason)))

      ;; cross off tiles in next clue’s range that would necessarily overlap with us
      (define next-clue-is (next-clues clue-i))
      (unless (empty? next-clue-is)
        (define last-full-mi (find-last/mega clue-range tile-full?))
        (define first-hole-mi (find-first/mega clue-range tile-hole?))
        (unless first-hole-mi
          (raise-contradiction "no space for clue"))

        (define last-full-i (and~> last-full-mi mega-index-tile))
        (match-define (cons _ end-bound)
          (find-earliest-tightest-placement-start+end/mega clue-range first-hole-mi clue))
        (define end-lower-bounds (mega-placement-end-bound->line-lower-bounds end-bound))

        (define (get-cross-end line-i)
          (define last-full-bound
            (if last-full-i
                (if (tile-full? (tiles-ref/mega clue-range (mega-index line-i last-full-i)))
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
                                 #:contradiction-reason contradiction-reason))))

    ;; -------------------------------------------------------------------------

    (define (propagate-information-from-user)
      ;; Fill in tiles filled by the user that can only belong to one clue.
      (for ([mi (in-range num-tiles/mega)]
            #:when (tile-full? (tiles-ref/mega user-tiles mi)))
        (define clue-is (line-clue-indexes (mega-index-line mi)))
        (define first-clue-ii
          (for/or ([(clue-i clue-ii) (in-indexed (in-array clue-is))])
            (cond
              ;; Fill the tile if it touches any of this clue’s filled tiles.
              [(clue-neighbor-full? clue-i mi)
               (clue-tiles-set!/mega clue-i mi 'full
                                     #:contradiction-reason "user tile borders clue that cannot claim it")
               #t]
              [else
               (and (tile-hole? (clue-tiles-ref/mega clue-i mi))
                    clue-ii)])))

        (match first-clue-ii
          [#t (void)]
          [#f (raise-contradiction "tile filled by user cannot belong to any clues")]
          [_  (unless (for/first ([clue-i (in-array clue-is (add1 first-clue-ii))]
                                  #:when (tile-hole? (clue-tiles-ref/mega clue-i mi)))
                        #t)
                (define first-clue-i (array-ref clue-is first-clue-ii))
                (clue-tiles-set!/mega first-clue-i mi 'full))])))

    ;; -------------------------------------------------------------------------

    (define (gain-information-to-fixed-point)
      (let go-again ()
        (for ([(chunk i) (in-indexed (in-array clues))])
          (match chunk
            [(array line-0 line-1)
             (for ([j (in-range (array-length line-0))])
               (define clue-i (single-line-index i 0 j))
               #;((current-print) (list clue-i clue-ranges))
               (gain-information-from-self/single clue-i)
               (propagate-information-to-neighbors/single clue-i))
             (for ([j (in-range (array-length line-1))])
               (define clue-i (single-line-index i 1 j))
               #;((current-print) (list clue-i clue-ranges))
               (gain-information-from-self/single clue-i)
               (propagate-information-to-neighbors/single clue-i))]
            [_
             #;((current-print) (list i clue-ranges))
             (gain-information-from-self/mega i)
             (propagate-information-to-neighbors/mega i)]))
        (propagate-information-from-user)
        (when (current-gained-information?)
          (current-gained-information? #f)
          (go-again))))

    ;; ---------------------------------------------------------------------------

    ;; Like `extract-analysis` for single line clues, but also checks that all of
    ;; the opposite tiles are crossed out.
    (define (extract-analysis/single clue-i)
      (define line-i (single-line-index-line clue-i))
      (define clue-range (clues-ref clue-ranges clue-i))
      (define first-full-i (find-first clue-range tile-full?))
      (cond
        [first-full-i
         (define last-full-i (find-last clue-range tile-full?))
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
      (define clue-range (clues-ref clue-ranges clue-i))
      (define first-full-mi (find-first/mega clue-range tile-full?))
      (cond
        [first-full-mi
         (define last-full-mi (find-last/mega clue-range tile-full?))
         (if (and (bounded-before?/mega user-tiles first-full-mi)
                  (bounded-after?/mega user-tiles last-full-mi)
                  (for/and ([mi (in-inclusive-range first-full-mi last-full-mi)])
                    (define tile (tiles-ref/mega user-tiles mi))
                    (or (tile-full? tile)
                        (tile-cross? tile))))
             'done
             'pending)]
        [else 'pending]))

    (with-handlers* ([exn:contradiction? (λ (exn) 'error)])
      (gain-information-to-fixed-point)
      (for/list ([(chunk i) (in-indexed (in-array clues))])
        (match chunk
          [(array line-0 line-1)
           (array (for/list ([j (in-range (array-length line-0))])
                    (extract-analysis/single (single-line-index i 0 j)))
                  (for/list ([j (in-range (array-length line-1))])
                    (extract-analysis/single (single-line-index i 1 j))))]
          [_
           (extract-analysis/mega i)])))))

(module+ test
  (check-equal? (analyze-line/mega/fancy '(2) #(#(empty empty empty empty)
                                                #(empty empty empty empty)))
                '(pending))
  (check-equal? (analyze-line/mega/fancy '(2) #(#(empty full empty empty)
                                                #(empty full empty empty)))
                '(pending))
  (check-equal? (analyze-line/mega/fancy '(2) #(#(cross full cross empty)
                                                #(cross full empty empty)))
                '(pending))
  (check-equal? (analyze-line/mega/fancy '(2) #(#(cross full empty empty)
                                                #(empty full cross empty)))
                '(pending))
  (check-equal? (analyze-line/mega/fancy '(2) #(#(cross full cross empty)
                                                #(empty full cross empty)))
                '(pending))
  (check-equal? (analyze-line/mega/fancy '(2) #(#(empty full cross empty)
                                                #(cross full cross empty)))
                '(pending))
  (check-equal? (analyze-line/mega/fancy '(2) #(#(cross full  cross empty)
                                                #(cross empty cross empty)))
                '(pending))
  (check-equal? (analyze-line/mega/fancy '(2) #(#(cross empty cross empty)
                                                #(cross full  cross empty)))
                '(pending))
  (check-equal? (analyze-line/mega/fancy '(2) #(#(cross full cross empty)
                                                #(cross full cross empty)))
                '(done))
  (check-equal? (analyze-line/mega/fancy '(2) #(#(cross cross cross cross)
                                                #(empty empty empty empty)))
                'error)
  (check-equal? (analyze-line/mega/fancy '(2) #(#(full  full  empty empty)
                                                #(empty empty empty empty)))
                'error))

;; analyze-line/mega : mega-line-clues? (array/c tile-line? tile-line?) -> mega-line-analysis?
(define (analyze-line/mega clues tiles)
  (or (analyze-line/mega/simple clues tiles)
      (analyze-line/mega/fancy clues tiles)))

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
                'error))

;; -----------------------------------------------------------------------------

;; analyze-line-at : puzzle? axis? natural? -> line-analysis?
#;(define (analyze-line-at pz axis i)
  (analyze-line (board-clues-line (puzzle-clues pz) axis i)
                (board-line (puzzle-board pz) axis i)))

;; analyze-lines-at : puzzle? integer-point? -> (values line-analysis? line-analysis?)
#;(define (analyze-lines-at pz location)
  (match-define (point x y) location)
  (values (analyze-line-at pz 'row y)
          (analyze-line-at pz 'column x)))

;; analyze-puzzle : puzzle? -> board-analysis?
(define (analyze-puzzle pp)
  (define (do-axis axis)
    (for/list ([tiles+clues (in-list (puzzle-axis-tiles+clues pp axis))])
      (match tiles+clues
        [(array tile-line clue-line)
         (analyze-line clue-line tile-line)]
        [(array tile-line-1 tile-line-2 clue-line)
         (analyze-line/mega clue-line (array tile-line-1 tile-line-2))])))

  (board-analysis
   (do-axis 'row)
   (do-axis 'column)))

(module+ test
  (check-equal? (analyze-puzzle
                 (puzzle (board #(#(empty empty cross)
                                  #(full  cross empty)
                                  #(empty empty full)))
                         clues-1))
                (board-analysis
                 '((pending) (done pending) (pending))
                 '(done (pending) error))))

;; reanalyze-lines-at : puzzle? board-analysis? integer-point? -> board-analysis?
#;(define (reanalyze-lines-at new-pz old-board-analysis location)
  (match-define (point x y) location)
  (define old-row-analyses (board-analysis-row-analysis old-board-analysis))
  (define old-column-analyses (board-analysis-column-analysis old-board-analysis))
  (define-values [new-row-analysis new-column-analysis] (analyze-lines-at new-pz location))
  (board-analysis
   (array-set old-row-analyses (point-y location) new-row-analysis)
   (array-set old-column-analyses (point-x location) new-column-analysis)))

(define (reanalyze-lines-at new-pz old-board-analysis location)
  ;; FIXME: Adapt to support mega lines.
  (analyze-puzzle new-pz))
