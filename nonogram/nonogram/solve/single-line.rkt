#lang racket/base

(require racket/contract
         racket/list
         racket/match
         threading
         "../array.rkt"
         "../core.rkt"
         "core.rkt")

(module+ test
  (require rackunit))

(provide (contract-out
          [analyze-line (-> single-line-clues? tile-line? line-clue-analysis?)]
          [solve-line (-> single-line-clues? tile-line? (or/c tile-line? 'error))]))

;; -----------------------------------------------------------------------------

(struct line-solution (board-tiles clue-tiless) #:transparent)

;; do-solve-line : single-line-clues? tile-line? -> (or/c line-solution? 'error)
(define (do-solve-line clues-lst user-tiles)
  (parameterize ([current-gained-information? #f])
    (define num-tiles (array-length user-tiles))

    (define (make-empty-tiles)
      (make-vector num-tiles 'empty))

    (define clues (list->array clues-lst))
    (define num-clues (array-length clues))

    ;; `unassigned-user-tiles` tracks all tiles filled by the user that have not
    ;; yet been claimed by any clues.
    (define unassigned-user-tiles (make-empty-tiles))
    (for ([i (in-range num-tiles)]
          #:when (tile-full? (array-ref user-tiles i)))
      (vector-set! unassigned-user-tiles i 'full))

    (define board-tiles (make-empty-tiles))
    (define clue-tiless
      (for/array #:length num-clues
                 ([i (in-range num-clues)])
        (make-empty-tiles)))

    ;; -------------------------------------------------------------------------

    (define (board-set! i val
                        #:contradiction-reason [reason (current-contradiction-reason)])
      (tiles-set!/track board-tiles i val #:contradiction-reason reason)

      ; propagage crosses to clues
      (when (tile-cross? val)
        (for ([clue-i (in-range num-clues)])
          (clue-tiles-set! clue-i i 'cross #:contradiction-reason reason))))

    (define (board-fill! val [start-i 0] [end-i num-tiles]
                         #:contradiction-reason [reason (current-contradiction-reason)])
      (for ([i (in-range start-i end-i)])
        (board-set! i val #:contradiction-reason reason)))

    (define (clue-tiles-set! clue-i i val
                             #:contradiction-reason [reason (current-contradiction-reason)])
      (define clue-tiles (array-ref clue-tiless clue-i))
      (tiles-set!/track clue-tiles i val #:contradiction-reason reason)

      ; propagate filled tiles to board and cross other clues
      (when (tile-full? val)
        (vector-set! unassigned-user-tiles i 'empty)
        (board-set! i 'full)
        (for ([other-i (in-range num-clues)]
              #:unless (equal? clue-i other-i))
          (clue-tiles-set! other-i i 'cross
                           #:contradiction-reason "tile claimed by multiple clues"))))

    (define (clue-tiles-fill! clue-i val [start-i 0] [end-i num-tiles]
                              #:contradiction-reason [contradiction-reason (current-contradiction-reason)])
      (for ([i (in-range start-i end-i)])
        (clue-tiles-set! clue-i i val #:contradiction-reason contradiction-reason)))

    ;; initialize board with user tiles
    (for ([i (in-range num-tiles)])
      (define tile (array-ref user-tiles i))
      (when (or (tile-full? tile)
                (tile-cross? tile))
        (board-set! i tile)))

    ;; -------------------------------------------------------------------------

    ;; Returns the index of the next clue of length `at-least-len`, starting with
    ;; index `clue-i`, for which the tile at index `tile-i` is 'empty in its clue
    ;; tiles.
    (define (find-next-clue-with-hole-at clue-i tile-i #:at-least-length at-least-len)
      (for/first ([clue (in-array clues clue-i)]
                  [clue-tiles (in-array clue-tiless clue-i)]
                  [i (in-naturals)]
                  #:when (and (>= clue at-least-len)
                              (tile-hole? (vector-ref clue-tiles tile-i))))
        (+ clue-i i)))

    ;; Returns whether a clue could be legally placed starting at the given
    ;; `start-i`, checking consistency with both board-tiles and its clue-tiles.
    (define (valid-clue-placement? clue-i start-i
                                   #:already-checked-span? [already-checked-span? #f])
      (define clue (array-ref clues clue-i))
      (define clue-tiles (array-ref clue-tiless clue-i))
      (define end-i (+ start-i clue))
      (and (<= end-i num-tiles)
           (not-full-before? board-tiles start-i)
           (not-full-before? clue-tiles start-i)
           (not-full-after? board-tiles (sub1 end-i))
           (not-full-after? clue-tiles (sub1 end-i))
           (or already-checked-span?
               (span-all? clue-tiles start-i end-i tile-hole?))))

    ;; Returns the smallest or largest (depending on `which`) index at which the
    ;; placement of clue `clue-i` could begin that would cause it to completely
    ;; cover the span [start-i, end-i). If `which` is 'both, the result is a
    ;; pair of the smallest and largest indexes. If no such placement exists,
    ;; returns #f.
    (define (find-placement-covering-span which clue-i start-i end-i)
      (define clue (array-ref clues clue-i))
      (define clue-tiles (array-ref clue-tiless clue-i))
      (define len (- end-i start-i))
      (cond
        [(and (>= clue len)
              (span-all? clue-tiles start-i end-i tile-hole?))
         (define min-placement-start-i
           (span-start clue-tiles start-i tile-hole? #:start (max 0 (- start-i (- clue len)))))
         (define max-placement-end-i
           (span-end clue-tiles (sub1 end-i) tile-hole? #:end (min num-tiles (+ end-i (- clue len)))))

         (define (get-earliest)
           (for/first ([i (in-range min-placement-start-i max-placement-end-i)]
                       #:when (valid-clue-placement? clue-i i #:already-checked-span? #t))
             i))

         (define (get-latest [start-i min-placement-start-i])
           (for/first ([i (in-inclusive-range (sub1 max-placement-end-i) start-i -1)]
                       #:when (valid-clue-placement? clue-i i #:already-checked-span? #t))
             i))

         (and (<= clue (- max-placement-end-i min-placement-start-i))
              (match which
                ['earliest (get-earliest)]
                ['latest (get-latest)]
                ['both
                 (define earliest-i (get-earliest))
                 (and earliest-i
                      (cons earliest-i (or (get-latest (add1 earliest-i))
                                           earliest-i)))]))]
        [else #f]))

    ;; Searches for the first clue after `start-clue-i` with a valid placement
    ;; covering the span [start-i, clue-i). If found, returns a pair of two
    ;; values: the clue’s index and the index of the first tile in the found
    ;; placement. If not found, returns #f.
    (define (find-earliest-clue-with-placement-covering-span start-clue-i start-i end-i #:placement which-placement)
      (for/or ([clue-i (in-range start-clue-i num-clues)])
        (define placement-i (find-placement-covering-span which-placement clue-i start-i end-i))
        (and placement-i (cons clue-i placement-i))))

    ;; Like `find-earliest-clue-with-placement-covering-tile`, but for the latest
    ;; clue instead of the earliest.
    (define (find-latest-clue-with-placement-covering-span end-clue-i start-i end-i #:placement which-placement)
      (for/or ([clue-i (in-inclusive-range (sub1 end-clue-i) 0 -1)])
        (define placement-i (find-placement-covering-span which-placement clue-i start-i end-i))
        (and placement-i (cons clue-i placement-i))))

    ;; -------------------------------------------------------------------------

    (define (gain-information-from-self* clue-i)
      (define clue (array-ref clues clue-i))
      (define clue-tiles (array-ref clue-tiless clue-i))

      ;; Cross out holes where the clue doesn’t fit.
      (let loop ([i 0])
        (match (find-next-hole clue-tiles i)
          [#f (void)]
          [i
           (define len (hole-length clue-tiles i))
           (when (< len clue)
             (clue-tiles-fill! clue-i 'cross i (+ i len)))
           (loop (+ i len))]))

      ;; if any boxes are filled...
      (match (find-first clue-tiles tile-full?)
        [#f (void)]
        [first-full-i
         (define last-full-i (find-last clue-tiles tile-full?))

         ;; ...fill in boxes between filled boxes
         (clue-tiles-fill! clue-i 'full first-full-i (add1 last-full-i)
                           #:contradiction-reason "clue is discontiguous")

         ;; ...cross off tiles unreachable due to clue length
         (clue-tiles-fill! clue-i 'cross 0 (- last-full-i (sub1 clue))
                           #:contradiction-reason "clue is too long")
         (clue-tiles-fill! clue-i 'cross (+ first-full-i clue)
                           #:contradiction-reason "clue is too long")

         ;; ...cross off all holes unreachable due to a separating cross
         (define prev-cross-i (find-prev clue-tiles first-full-i tile-cross?))
         (when prev-cross-i
           (clue-tiles-fill! clue-i 'cross 0 prev-cross-i))
         (define next-cross-i (find-next clue-tiles first-full-i tile-cross?))
         (when next-cross-i
           (clue-tiles-fill! clue-i 'cross next-cross-i))])

      ;; ensure there is a hole for the clue to actually go
      (unless (find-next-hole clue-tiles 0)
        (raise-contradiction "not enough space for clue"))

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

    (define (propagate-information-to-neighbors clue-i)
      (define clue (array-ref clues clue-i))
      (define clue-tiles (array-ref clue-tiless clue-i))

      ;; cross off tiles in previous clue’s tiles that would necessarily overlap with us
      (unless (zero? clue-i)
        (define first-full-i (find-first clue-tiles tile-full?))
        (define last-hole-i (find-last clue-tiles tile-hole?))
        (define last-tile-previous-clue-can-occupy
          (min
           ;; if we have a filled tile, then the previous clue must leave a gap before it
           (if first-full-i (sub1 first-full-i) num-tiles)
           ;; the latest our first filled tile can appear is determined by the last hole and our clue size
           (if last-hole-i (- last-hole-i clue) num-tiles)))
        (when (< last-tile-previous-clue-can-occupy num-tiles)
          (clue-tiles-fill! (sub1 clue-i) 'cross (max 0 last-tile-previous-clue-can-occupy)
                            #:contradiction-reason "not enough space between neighboring clues’ full tiles")))

      ;; cross off tiles in next clue’s tiles that would necessarily overlap with us
      (when (< (add1 clue-i) num-clues)
        (define last-full-i (find-last clue-tiles tile-full?))
        (define first-hole-i (find-first clue-tiles tile-hole?))
        (define first-tile-next-clue-can-occupy
          (max
           ;; if we have a filled tile, then the next clue must leave a gap after it
           (if last-full-i (+ last-full-i 2) 0)
           ;; the earliest our last filled tile can appear is determined by the first hole and our clue size
           (if first-hole-i (+ first-hole-i clue 1) 0)))
        (when (> first-tile-next-clue-can-occupy 0)
          (clue-tiles-fill! (add1 clue-i) 'cross 0 (min num-tiles first-tile-next-clue-can-occupy)
                            #:contradiction-reason "not enough space between neighboring clues’ full tiles"))))

    (define (propagate-information-from-user)
      ;; Scan through the user’s filled-in tiles.
      (define (scan-unassigned-tiles forwards?)
        (define first-clue? (if forwards? zero? (λ~> add1 (= num-clues))))
        (define prev (if forwards? sub1 add1))
        (define next (if forwards? add1 sub1))
        (define after (if forwards? add1 values))
        (define lower-bound (if forwards? min max))
        (define earliest (if forwards? 'earliest 'latest))

        (define find-next*
          (if forwards?
              find-next
              (λ (tiles start-i pred? #:end [end-i 0])
                (find-prev tiles start-i pred? #:start end-i))))

        (let loop ([start-tile-i (if forwards? 0 num-tiles)]
                   [start-clue-i (if forwards? 0 num-clues)])
          (match (find-next* unassigned-user-tiles start-tile-i tile-full?)
            [#f (void)]
            [full-i
             (define-values [full-start-i full-end-i] (span-start+end board-tiles full-i tile-full?))
             (define (get-next-placement start-clue-i #:which which-placement)
               (if forwards?
                   (find-earliest-clue-with-placement-covering-span start-clue-i full-start-i full-end-i
                                                                    #:placement which-placement)
                   (find-latest-clue-with-placement-covering-span start-clue-i full-start-i full-end-i
                                                                  #:placement which-placement)))
             ;; Find the earliest clue that could potentially cover this tile.
             (match (get-next-placement start-clue-i #:which 'both)
               [#f
                (raise-contradiction "tile filled by user cannot belong to any clues")]
               [(cons placed-clue-i (cons earliest-placement-i latest-placement-i))
                (define placed-clue (array-ref clues placed-clue-i))
                (define earliest-placement-end-i (+ earliest-placement-i placed-clue))
                (define latest-placement-end-i (+ latest-placement-i placed-clue))

                ;; Cross out tiles in the previous clue at/after the placement, as
                ;; it must be placed before this one.
                (unless (first-clue? placed-clue-i)
                  (parameterize ([current-contradiction-reason "users’ filled tiles are inconsistent with clue order"])
                    (if forwards?
                        (clue-tiles-fill! (prev placed-clue-i) 'cross (max 0 (prev latest-placement-i)))
                        (clue-tiles-fill! (prev placed-clue-i) 'cross 0 (min num-tiles (prev earliest-placement-end-i))))))

                ;; Check if there are any other clues that could cover this tile,
                ;; or if this is the only one.
                (match (get-next-placement (after placed-clue-i) #:which earliest)
                  [#f
                   ;; This is the only clue, so we can assign this tile and any
                   ;; other tiles that must be covered by the placement to it.
                   (let assign-loop ([full-i full-i])
                     (clue-tiles-set! placed-clue-i full-i 'full)
                     (match (find-next* unassigned-user-tiles
                                        (after full-i)
                                        tile-full?
                                        #:end (if forwards? earliest-placement-end-i latest-placement-i))
                       [#f (void)]
                       [full-i (assign-loop full-i)]))]

                  [other-clue-placement
                   ;; There are other clues that could cover this tile. However, we can still
                   ;; potentially gain information in one of two ways:
                   ;;
                   ;;   1. We can calculate the lower bound of all possible placements’ start
                   ;;      positions. If they all must start at `full-i`, we can place a cross
                   ;;      on the board before it.
                   ;;
                   ;;      For example, suppose we have the following puzzle row:
                   ;;          1 3 ☐☐■☐■☐☐
                   ;;      This will result in the following solver state:
                   ;;        board ☐☐■☐■☐☐
                   ;;            1 ☐☐☐☒☒☒☒
                   ;;            3 ☒☒☐☐■☐☐
                   ;;      We can’t be certain whether the first filled-in tile belongs to the
                   ;;      1 or the 3. However, we do know that the lower bound of both
                   ;;      placements’ start positions is column 2. Therefore, we can place a
                   ;;      cross on the board just before it:
                   ;;        board ☐☒■☐■☐☐
                   ;;
                   ;;   2. We can calculate the lower bound of all possible placements’ end
                   ;;      positions. If they are greater than `full-i`, then all tiles
                   ;;      between `full-i` and the bound must be filled.
                   ;;
                   ;;      For example, suppose we have the following puzzle row:
                   ;;          2 3 ☐☐☒■☐☐☐☐☐
                   ;;      We can’t know whether the filled tile belongs to the 2 or the 3.
                   ;;      However, we do know that all possible placements must be at least
                   ;;      2 tiles long. Therefore, we can fill the board tile after it:
                   ;;        board ☐☐☒■■☐☐☐☐
                   (let bounds-loop ([start-bound (if forwards? earliest-placement-i latest-placement-i)]
                                     [end-bound (if forwards? earliest-placement-end-i latest-placement-end-i)]
                                     [other-placement other-clue-placement])
                     (match other-placement
                       [(cons clue-i earliest-placement-i)
                        (define clue (array-ref clues clue-i))
                        (bounds-loop (lower-bound start-bound earliest-placement-i)
                                     (lower-bound end-bound (+ earliest-placement-i clue))
                                     (get-next-placement (after clue-i) #:which earliest))]
                       [#f
                        (cond
                          [forwards?
                           (when (and (= start-bound full-i) (> full-i 0))
                             (board-set! (sub1 full-i) 'cross))
                           (board-fill! 'full (add1 full-i) end-bound)]
                          [else
                           (define after-full-i (add1 full-i))
                           (when (and (= after-full-i end-bound) (< after-full-i num-tiles))
                             (board-set! after-full-i 'cross))
                           (board-fill! 'full start-bound full-i)])]))])

                ;; Advance to the end of the placement and continue (leaving a gap for the
                ;; following cross). The next filled tiles, if any, must belong to later clues.
                (cond
                  [forwards?
                   (define start-tile-i* (next latest-placement-end-i))
                   (when (< start-tile-i* num-tiles)
                     (loop start-tile-i* (after placed-clue-i)))]
                  [else
                   (define start-tile-i* (next earliest-placement-i))
                   (when (>= start-tile-i* 0)
                     (loop start-tile-i* (after placed-clue-i)))])])])))

      (scan-unassigned-tiles #t)
      (scan-unassigned-tiles #f))

    (define (gain-information-to-fixed-point)
      (let go-again ()
        (for ([i (in-range num-clues)])
          (gain-information-from-self* i)
          (propagate-information-to-neighbors i))
        (propagate-information-from-user)
        (when (current-gained-information?)
          (current-gained-information? #f)
          (go-again))))

    ;; -------------------------------------------------------------------------

    (with-handlers* ([exn:contradiction? (λ (exn) 'error)])
      (gain-information-to-fixed-point)
      (line-solution board-tiles clue-tiless))))

;; -------------------------------------------------------------------------

;; solve-line : single-line-clues? tile-line? -> (or/c tile-line? 'error)
(define (solve-line clues tiles)
  (match (do-solve-line clues tiles)
    ['error 'error]
    [(line-solution board-tiles clue-tiless)
     (define num-tiles (vector-length board-tiles))
     (for/array #:length num-tiles
                ([i (in-range num-tiles)])
       (match (vector-ref board-tiles i)
         ['empty
          (if (for/and ([clue-tiles (in-array clue-tiless)])
                (eq? (vector-ref clue-tiles i) 'cross))
              'cross
              'empty)]
         [tile tile]))]))

(module+ test
  (check-equal? (solve-line '(1 2 1) #(empty empty empty empty empty empty))
                #(full cross full full cross full))
  (check-equal? (solve-line '(4 1) #(empty empty empty full full empty full))
                #(cross full full full full cross full)))

;; -------------------------------------------------------------------------

;; analyze-line/simple : single-line-clues? tile-line? -> (or/c 'done 'error #f)
;;
;; The simple analyzer performs a quick, cheap check to see whether a given line
;; is fully solved. A result of #f requires the use of the fancy analyzer.
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

;; analyze-line/solve : single-line-clues? tile-line? -> line-clue-analysis?
(define (analyze-line/solve clues user-tiles)
  (define (extract-analysis clue-tiles)
    (define first-full-i (find-first clue-tiles tile-full?))
    (cond
      [first-full-i
       (define last-full-i (find-last clue-tiles tile-full?))
       (if (and (bounded-before? user-tiles first-full-i)
                (bounded-after? user-tiles last-full-i)
                (for/and ([i (in-inclusive-range first-full-i last-full-i)])
                  (tile-full? (array-ref user-tiles i))))
           'done
           'pending)]
      [else 'pending]))

  (match (do-solve-line clues user-tiles)
    ['error 'error]
    [(line-solution _ clue-tiless)
     (for/list ([clue-tiles (in-array clue-tiless)])
       (extract-analysis clue-tiles))]))

;; analyze-line : single-line-clues? tile-line? -> line-clue-analysis?
(define (analyze-line clues tiles)
  (or (analyze-line/simple clues tiles)
      (analyze-line/solve clues tiles)))

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
