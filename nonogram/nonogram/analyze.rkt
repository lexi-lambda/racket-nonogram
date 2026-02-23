#lang racket/base

(require racket/contract
         racket/list
         racket/match
         "array.rkt"
         "core.rkt"
         "geometry.rkt"
         "vector.rkt")

(module+ test
  (require rackunit
           (submod "core.rkt" example)))

(provide (contract-out
          [clue-analysis? flat-contract?]
          [line-clue-analysis? flat-contract?]
          [axis-clue-analysis? flat-contract?]
          (struct board-analysis ([row-analysis axis-clue-analysis?]
                                  [column-analysis axis-clue-analysis?]))

          [analyze-line (-> line-clues? tile-line? line-clue-analysis?)]
          [analyze-puzzle (-> puzzle? board-analysis?)]
          [reanalyze-lines-at (-> puzzle? board-analysis? integer-point? board-analysis?)]))

;; -----------------------------------------------------------------------------

(define clue-analysis? (or/c 'done 'pending))
(define line-clue-analysis? (or/c 'done 'error (listof clue-analysis?)))
(define axis-clue-analysis? (arrayof line-clue-analysis?))
(struct board-analysis
  (row-analysis     ;; axis-clue-analysis?
   column-analysis) ;; axis-clue-analysis?
  #:transparent)

(struct exn:contradiction exn:fail () #:transparent)
(define (raise-contradiction why)
  (raise (exn:contradiction
          (format "analyze-puzzle: contradiction in puzzle\n  reason: ~a" why)
          (current-continuation-marks))))

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

;; analyze-line/fancy : line-clues? tile-line? -> line-clue-analysis?
(define (analyze-line/fancy clues-lst user-tiles)
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

  (define gained-information? #f)

  (define (vector-set!/track vec i val #:contradiction-reason [contradiction-reason #f])
    (when (eq? val 'empty)
      (raise-arguments-error 'vector-set!/track "internal error: setting clue range tile to 'empty"))
    (match (vector-ref vec i)
      ['empty
       (vector-set! vec i val)
       (set! gained-information? #t)]
      [(== val eq?)
       (void)]
      [other-val
       (if contradiction-reason
           (raise-contradiction contradiction-reason)
           (raise-arguments-error 'vector-set!/track "internal error: overwriting non-empty clue range tile"
                                  "old tile" other-val
                                  "new tile" val))]))

  (define (vector-fill!/track vec value [start-i 0] [end-i (vector-length vec)]
                              #:contradiction-reason [contradiction-reason #f])
    (for ([i (in-range start-i end-i)])
      (vector-set!/track vec i value #:contradiction-reason contradiction-reason)))

  ;; ---------------------------------------------------------------------------

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
    (or (= (add1 i) num-tiles)
        (not (tile-full? (vector-ref tiles (add1 i))))))

  (define (bounded-before? tiles i)
    (or (= i 0)
        (tile-cross? (vector-ref tiles (sub1 i)))))
  (define (bounded-after? tiles i)
    (or (= (add1 i) num-tiles)
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

  (define (gain-information-from-self clue-i)
    (define clue (array-ref clues clue-i))
    (define clue-range (array-ref clue-ranges clue-i))

    ;; cross off boxes where it doesn’t fit
    (let loop ([i 0])
      (match (find-next-hole clue-range i)
        [#f (void)]
        [i
         (define len (hole-length clue-range i))
         (when (< len clue)
           (vector-fill!/track clue-range 'cross i (+ i len)))
         (loop (+ i len))]))

    ;; if any boxes are filled...
    (match (find-first clue-range tile-full?)
      [#f (void)]
      [first-full-i
       (define last-full-i (find-last clue-range tile-full?))

       ;; ...fill in boxes between filled boxes
       (vector-fill!/track clue-range 'full first-full-i (add1 last-full-i)
                           #:contradiction-reason "clue is discontiguous")

       ;; ...cross off tiles unreachable due to clue length
       (vector-fill!/track clue-range 'cross 0 (- last-full-i (sub1 clue))
                           #:contradiction-reason "clue is too long")
       (vector-fill!/track clue-range 'cross (+ first-full-i clue)
                           #:contradiction-reason "clue is too long")

       ;; ...cross off all holes unreachable due to a separating cross
       (define prev-cross-i (find-prev clue-range first-full-i tile-cross?))
       (when prev-cross-i
         (vector-fill!/track clue-range 'cross 0 prev-cross-i))
       (define next-cross-i (find-next clue-range first-full-i tile-cross?))
       (when next-cross-i
         (vector-fill!/track clue-range 'cross next-cross-i))])

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
            (vector-fill!/track
             #:contradiction-reason "not enough space for clue"
             clue-range
             'full
             (- (+ mid-tile i) wing-len)
             (+ (+ mid-tile i) wing-len 1)))]
         [else
          (define half-len (/ len 2))
          (when (< half-len clue)
            (define wing-len (- clue half-len))
            (vector-fill!/track
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
        (vector-fill!/track other-clue-range 'cross (max 0 last-tile-previous-clue-can-occupy)
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
        (vector-fill!/track other-clue-range 'cross 0 (min num-tiles first-tile-next-clue-can-occupy)
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
              (vector-fill!/track prev-clue-range 'cross (max 0 (sub1 placement-i))
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
              (vector-fill!/track prev-clue-range 'cross 0 (min num-tiles (add1 placement-i))
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
            (vector-set!/track (array-ref clue-ranges first-empty-clue-i) tile-i 'full))
          (raise-contradiction "tile filled by user cannot belong to any clues"))))

  (define (gain-information-to-fixed-point)
    (let go-again ()
      (for ([i (in-range num-clues)])
        (gain-information-from-self i)
        (propagate-information-to-neighbors i))
      (propagate-information-from-user)
      (when gained-information?
        (set! gained-information? #f)
        (go-again))))

  ;; ---------------------------------------------------------------------------

  (with-handlers* ([exn:contradiction? (λ (exn) 'error)])
    (gain-information-to-fixed-point)
    (for/list ([clue-range (in-array clue-ranges)])
      (define first-full-i (find-first clue-range tile-full?))
      (cond
        [first-full-i
         (define last-full-i (find-last clue-range tile-full?))
         (if (and (bounded-before? clue-range first-full-i)
                  (bounded-after? clue-range last-full-i)
                  (for/and ([i (in-inclusive-range first-full-i last-full-i)])
                    (tile-full? (array-ref user-tiles i)))
                  (bounded-before? user-tiles first-full-i)
                  (bounded-after? user-tiles last-full-i))
             'done
             'pending)]
        [else 'pending]))))

;; analyze-line : line-clues? tile-line? -> line-clue-analysis?
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

;; analyze-line-at : puzzle? axis? natural? -> line-analysis?
(define (analyze-line-at pz axis i)
  (analyze-line (board-clues-line (puzzle-clues pz) axis i)
                (board-line (puzzle-board pz) axis i)))

;; analyze-lines-at : puzzle? integer-point? -> (values line-analysis? line-analysis?)
(define (analyze-lines-at pz location)
  (match-define (point x y) location)
  (values (analyze-line-at pz 'row y)
          (analyze-line-at pz 'column x)))

;; analyze-puzzle : puzzle? -> board-analysis?
(define (analyze-puzzle pp)
  (define board (puzzle-board pp))
  (board-analysis
   (for/array #:length (board-height board)
              ([i (in-range (board-height board))])
     (analyze-line-at pp 'row i))
   (for/array #:length (board-width board)
             ([i (in-range (board-width board))])
     (analyze-line-at pp 'column i))))

(module+ test
  (check-equal? (analyze-puzzle
                 (puzzle (board #(#(empty empty cross)
                                  #(full  cross empty)
                                  #(empty empty full)))
                         clues-1))
                (board-analysis
                 #((pending) (done pending) (pending))
                 #(done (pending) error))))

;; reanalyze-lines-at : puzzle? board-analysis? integer-point? -> board-analysis?
(define (reanalyze-lines-at new-pz old-board-analysis location)
  (define old-row-analyses (board-analysis-row-analysis old-board-analysis))
  (define old-column-analyses (board-analysis-column-analysis old-board-analysis))
  (define-values [new-row-analysis new-column-analysis] (analyze-lines-at new-pz location))
  (board-analysis
   (array-set old-row-analyses (point-y location) new-row-analysis)
   (array-set old-column-analyses (point-x location) new-column-analysis)))
