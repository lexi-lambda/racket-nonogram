#lang racket/base

(require racket/contract
         racket/list
         racket/match
         "array.rkt"
         "core.rkt"
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

          [analyze-line (-> line-clues? (arrayof tile?) line-clue-analysis?)]
          [analyze-puzzle (-> puzzle? board-analysis?)]))

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

;; analyze-line/simple : line-clues? (arrayof tile?) -> (or/c 'done 'error #f)
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

;; analyze-line/fancy : line-clues? (arrayof tile?) -> line-clue-analysis?
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

  (define (find-next vec start-i pred?)
    (let loop ([i start-i])
      (if (< i (vector-length vec))
          (if (pred? (vector-ref vec i))
              i
              (loop (add1 i)))
          #f)))

  (define (find-first vec pred?)
    (find-next vec 0 pred?))

  (define (find-prev vec start-i pred?)
    (let loop ([i (sub1 start-i)])
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

  ;; Returns the index of the next clue, starting with index `clue-i`, for which
  ;; the tile at index `tile-i` is 'empty in its clue range.
  (define (find-next-clue-with-hole-at clue-i tile-i)
    (for/first ([(clue-range i) (in-indexed (in-vector clue-ranges clue-i))]
                #:when (tile-hole? (vector-ref clue-range tile-i)))
      (+ clue-i i)))

  (define (bounded-before? tiles i)
    (or (= i 0)
        (eq? (vector-ref tiles (sub1 i)) 'cross)))
  (define (bounded-after? tiles i)
    (or (= (add1 i) (vector-length tiles))
        (eq? (vector-ref tiles (add1 i)) 'cross)))

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

  (define (propagate-information-to-others clue-i)
    (define clue-range (array-ref clue-ranges clue-i))

    ;; cross off tiles in other ranges that would overlap based on our filled tiles
    (define first-full-i (find-first clue-range tile-full?))
    (when first-full-i
      (for/fold ([cross-after-i (sub1 first-full-i)])
                ([clue-j (in-inclusive-range (sub1 clue-i) 0 -1)])
        (define other-clue (array-ref clues clue-j))
        (define other-clue-range (array-ref clue-ranges clue-j))
        (vector-fill!/track other-clue-range 'cross (max 0 cross-after-i)
                            #:contradiction-reason "not enough space between filled clue tiles")
        (- cross-after-i other-clue 1))

      (define last-full-i (find-last clue-range tile-full?))
      (for/fold ([cross-before-i (+ first-full-i 2)])
                ([clue-j (in-range (add1 clue-i) num-clues)])
        (define other-clue (array-ref clues clue-j))
        (define other-clue-range (array-ref clue-ranges clue-j))
        (vector-fill!/track other-clue-range 'cross 0 (min num-tiles cross-before-i)
                            #:contradiction-reason "not enough space between filled clue tiles")
        (+ cross-before-i other-clue 1))))

  (define (propagate-information-from-user)
    ;; fill in tiles filled by the user that can only belong to one clue
    (for ([(tile tile-i) (in-indexed (in-array user-tiles))]
          #:when (eq? tile 'full))
      (define first-empty-clue-i (find-next-clue-with-hole-at 0 tile-i))
      (if first-empty-clue-i
          (unless (find-next-clue-with-hole-at (add1 first-empty-clue-i) tile-i)
            ;; only one clue range with a hole at this location
            (vector-set!/track (array-ref clue-ranges first-empty-clue-i) tile-i 'full))
          (raise-contradiction "tile filled by user cannot belong to any clues"))))

  (define (gain-information-to-fixed-point)
    (let go-again ()
      (for ([i (in-range num-clues)])
        (gain-information-from-self i)
        (propagate-information-to-others i))
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

;; analyze-line : line-clues? (arrayof tile?) -> line-clue-analysis?
(define (analyze-line line-clues tiles)
  (or (analyze-line/simple line-clues tiles)
      (analyze-line/fancy line-clues tiles)))

(module+ test
  (check-equal? (analyze-line '(1 1) #(full cross empty)) '(done pending))
  (check-equal? (analyze-line '(1 1) #(full full  empty)) 'error)
  (check-equal? (analyze-line '(3) #(cross empty full)) 'error)
  (check-equal? (analyze-line '(3) #(cross empty empty)) 'error)
  (check-equal? (analyze-line '(3 1) #(empty empty empty cross full cross empty)) '(pending done)))

;; analyze-puzzle : puzzle? -> board-analysis?
(define (analyze-puzzle pp)
  (define board (puzzle-board pp))
  (define clues (puzzle-clues pp))
  (board-analysis
   (for/array #:length (board-height board)
              ([(line-clues i) (in-indexed (in-array (board-clues-row-clues clues)))])
     (analyze-line line-clues (board-row board i)))
   (for/array #:length (board-width board)
             ([(line-clues i) (in-indexed (in-array (board-clues-column-clues clues)))])
     (analyze-line line-clues (board-column board i)))))

(module+ test
  (check-equal? (analyze-puzzle
                 (puzzle (board #(#(empty empty cross)
                                  #(full  cross empty)
                                  #(empty empty full)))
                         clues-1))
                (board-analysis
                 #((pending) (done pending) (pending))
                 #(done (pending) error))))
