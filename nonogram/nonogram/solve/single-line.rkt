#lang racket/base

(require racket/contract
         racket/list
         racket/match
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

    (define (clue-tiles-set! clue-i i val
                             #:contradiction-reason [reason (current-contradiction-reason)])
      (define clue-tiles (array-ref clue-tiless clue-i))
      (tiles-set!/track clue-tiles i val #:contradiction-reason reason)

      ; propagate filled tiles to board and cross other clues
      (when (tile-full? val)
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
    ;; `start-i`, checking consistency with both user-tiles and its clue-tiles.
    (define (valid-clue-placement? clue-i start-i)
      (define clue (array-ref clues clue-i))
      (define clue-tiles (array-ref clue-tiless clue-i))
      (define end-i (+ start-i clue))
      (and (<= end-i num-tiles)
           (not-full-before? user-tiles start-i)
           (not-full-before? clue-tiles start-i)
           (not-full-after? user-tiles (sub1 end-i))
           (not-full-after? clue-tiles (sub1 end-i))
           (for/and ([i (in-range start-i end-i)])
             (and (tile-hole? (array-ref user-tiles i))
                  (tile-hole? (vector-ref clue-tiles i))))))

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
                (clue-tiles-fill! (sub1 placed-clue-i) 'cross (max 0 (sub1 placement-i))
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
                (clue-tiles-fill! (add1 placed-clue-i) 'cross 0 (min num-tiles (add1 placement-i))
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
              ;; only one clue with a hole at this location
              (clue-tiles-set! first-empty-clue-i tile-i 'full))
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
                #(full cross full full cross full)))

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
