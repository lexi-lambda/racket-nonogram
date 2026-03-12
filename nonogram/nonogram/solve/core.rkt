#lang racket/base

(require racket/contract
         racket/match
         racket/math
         racket/mutability
         threading
         toolbox/who
         "../array.rkt"
         "../core.rkt")

(module+ test
  (require rackunit))

(provide (contract-out
          [clue-analysis? flat-contract?]
          [single-line-analysis? flat-contract?]
          [mega-line-analysis? flat-contract?]
          [line-clue-analysis? flat-contract?]
          [axis-clue-analysis? flat-contract?]
          (struct board-analysis ([row-analysis axis-clue-analysis?]
                                  [column-analysis axis-clue-analysis?]))
          [line-analysis-solved? (-> line-clue-analysis? boolean?)]
          [axis-analysis-solved? (-> axis-clue-analysis? boolean?)]
          [board-analysis-solved? (-> board-analysis? boolean?)]

          [tile-predicate/c contract?]
          [tile-empty? tile-predicate/c]
          [tile-cross? tile-predicate/c]
          [tile-full? tile-predicate/c]
          [tile-hole? tile-predicate/c])

         (struct-out exn:contradiction)
         (contract-out
          [raise-contradiction (-> string? none/c)]
          [current-contradiction-reason (parameter/c (or/c string? #f))]
          [current-gained-information? (parameter/c boolean?)]
          [tiles-set!/track (->* [mutable-tiles/c natural? tile?]
                                 [#:contradiction-reason (or/c string? #f)]
                                 void?)]
          [tiles-fill!/track (->* [mutable-tiles/c tile?]
                                  [natural? natural?
                                            #:contradiction-reason (or/c string? #f)]
                                  void?)]

          [not-full-before? (-> tiles/c natural? boolean?)]
          [not-full-after? (-> tiles/c natural? boolean?)]
          [bounded-before? (-> tiles/c natural? boolean?)]
          [bounded-after? (-> tiles/c natural? boolean?)]
          [neighbor-matching? (-> tiles/c natural? tile-predicate/c boolean?)]
          [span-all? (-> tiles/c natural? natural? tile-predicate/c boolean?)]

          [find-next (->* [tiles/c natural? tile-predicate/c] [#:end natural?] (or/c natural? #f))]
          [find-prev (->* [tiles/c natural? tile-predicate/c] [#:start natural?] (or/c natural? #f))]
          [find-first (-> tiles/c tile-predicate/c (or/c natural? #f))]
          [find-last (-> tiles/c tile-predicate/c (or/c natural? #f))]
          [span-start (->* [tiles/c natural? tile-predicate/c] [#:start natural?] natural?)]
          [span-end (->* [tiles/c natural? tile-predicate/c] [#:end natural?] natural?)]
          [span-start+end (->* [tiles/c natural? tile-predicate/c]
                               [#:start natural?
                                #:end natural?]
                               (values natural? natural?))]
          [span-length (-> tiles/c natural? tile-predicate/c natural?)]

          [find-next-hole (-> tiles/c natural? (or/c natural? #f))]
          [hole-length (-> tiles/c natural? natural?)]
          [find-singular-hole (-> tiles/c (or/c natural? #f))]

          (struct single-line-index ([chunk natural?]
                                     [line mega-line-offset?]
                                     [index natural?]))
          [mega-clue-index? flat-contract?])

         mega-index?
         mega-index
         mega-index-line
         mega-index-tile
         opposite
         afront
         behind

         (contract-out
          [mega-tiles/c contract?]
          [mutable-mega-tiles/c contract?]
          [mega-tiles-length (-> mega-tiles/c natural?)]
          [mega-tiles-length/mega (-> mega-tiles/c natural?)]
          [tiles-ref/mega (-> mega-tiles/c mega-index? tile?)]
          [tiles-count/mega (->* [mega-tiles/c tile-predicate/c]
                                 [natural? natural?]
                                 natural?)]
          [tiles-set!/mega (->* [mutable-mega-tiles/c mega-index? tile?]
                                [#:track? any/c
                                 #:contradiction-reason (or/c string? #f)]
                                void?)]
          [tiles-fill!/mega (->* [mutable-mega-tiles/c tile?]
                                 [mega-index? mega-index?
                                  #:contradiction-reason (or/c string? #f)]
                                 void?)]

          [bounded-behind?/mega (-> mega-tiles/c mega-index? boolean?)]
          [bounded-afront?/mega (-> mega-tiles/c mega-index? boolean?)]
          [bounded-before?/mega (-> mega-tiles/c mega-index? boolean?)]
          [bounded-after?/mega (-> mega-tiles/c mega-index? boolean?)]
          [neighbor-matching?/mega (-> mega-tiles/c mega-index? tile-predicate/c boolean?)]
          [neighbors-count/mega (-> mega-tiles/c mega-index? tile-predicate/c (integer-in 0 3))]

          [find-next/mega
           (->* [mega-tiles/c mega-index? tile-predicate/c]
                [#:end mega-index?]
                (or/c mega-index? #f))]
          [find-prev/mega
           (->* [mega-tiles/c mega-index? tile-predicate/c]
                [#:start mega-index?]
                (or/c mega-index? #f))]
          [find-first/mega (-> mega-tiles/c tile-predicate/c (or/c mega-index? #f))]
          [find-last/mega (-> mega-tiles/c tile-predicate/c (or/c mega-index? #f))]
          [find-next-hole/mega (-> mega-tiles/c mega-index? (or/c mega-index? #f))]
          [find-singular-hole/mega (-> mega-tiles/c (or/c (array/c mega-index? mega-index? natural?) #f))]

          [connected-region-spans-both-lines?
           (-> mega-index? mega-index? clue? boolean?)]
          [connected-region-start/mega
           (-> mega-tiles/c mega-index? tile-predicate/c natural?)]
          [connected-region-size+end/mega
           (-> mega-tiles/c mega-index? tile-predicate/c (values natural? natural?))]
          [connected-region-end/mega
           (-> mega-tiles/c mega-index? tile-predicate/c natural?)]
          [hole-size+end/mega (-> mega-tiles/c mega-index? (values natural? natural?))]
          [hole-end/mega (-> mega-tiles/c mega-index? natural?)]

          [line-affinity? flat-contract?]
          [earliest-reachable/mega
           (-> mega-tiles/c line-affinity? natural? natural? (array/c natural? natural?))]
          [latest-reachable/mega
           (-> mega-tiles/c line-affinity? natural? natural? (array/c natural? natural?))]

          [mega-placement-bound-line? flat-contract?]
          (struct mega-placement-bound ([line mega-placement-bound-line?]
                                        [tile natural?]))
          [mega-index->placement-bound (-> mega-index? mega-placement-bound?)]
          [mega-placement-bound->least-upper-bound (-> mega-placement-bound? mega-index?)]
          [mega-placement-bound->greatest-lower-bound (-> mega-placement-bound? mega-index?)]
          [mega-placement-end-bound->line-lower-bounds
           (-> mega-placement-bound? (array/c natural? natural?))]
          [mega-placement-start-bound->line-upper-bounds
           (-> mega-placement-bound? (array/c natural? natural?))]

          [find-earliest-tightest-placement-start+end/mega
           (-> mega-tiles/c mega-index? clue?
               (or/c (cons/c mega-index? mega-placement-bound?) #f))]
          [find-latest-tightest-placement-end+start/mega
           (-> mega-tiles/c mega-index? clue?
               (or/c (cons/c mega-index? mega-placement-bound?) #f))]

          [fill-forced-tiles-in-mega-span! (-> mega-tiles/c
                                               mega-placement-bound?
                                               mega-placement-bound?
                                               #:set-full! (-> mega-index? any)
                                               (values natural? line-affinity? line-affinity?))]))

;; -----------------------------------------------------------------------------

(define strict-contracts? #t)

(define tiles/c
  (if strict-contracts? (vectorof tile? #:eager #f) vector?))
(define mutable-tiles/c
  (if strict-contracts? (vectorof tile? #:immutable #f) mutable-vector?))

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

(define axis-clue-analysis? (arrayof line-clue-analysis?))
(struct board-analysis
  (row-analysis     ;; axis-clue-analysis?
   column-analysis) ;; axis-clue-analysis?
  #:transparent)

(define (line-analysis-solved? analysis)
  (eq? analysis 'done))

(define (axis-analysis-solved? analysis)
  (for/and ([analysis (in-array analysis)])
    (line-analysis-solved? analysis)))

(define (board-analysis-solved? analysis)
  (match-define (board-analysis row-analysis column-analysis) analysis)
  ; only need to check one axis, so pick the smaller one
  (axis-analysis-solved?
   (if (< (array-length row-analysis) (array-length column-analysis))
       row-analysis
       column-analysis)))

;; -----------------------------------------------------------------------------

(define tile-predicate/c (-> tile? boolean?))

(define (tile-empty? tile)
  (eq? tile 'empty))

(define (tile-cross? tile)
  (eq? tile 'cross))

(define (tile-full? tile)
  (eq? tile 'full))

(define (tile-hole? tile)
  (not (eq? tile 'cross)))

;; -----------------------------------------------------------------------------

(struct exn:contradiction exn:fail () #:transparent)
(define (raise-contradiction why)
  (raise (exn:contradiction
          (format "analyze-puzzle: contradiction in puzzle\n  reason: ~a" why)
          (current-continuation-marks))))

(define current-contradiction-reason (make-parameter #f))
(define current-gained-information? (make-parameter #f))

(define/who (tiles-set!/track vec i val
                              #:contradiction-reason [contradiction-reason (current-contradiction-reason)])
  (when (eq? val 'empty)
    (raise-arguments-error who "internal error: setting tile to 'empty"))
  (match (vector-ref vec i)
    ['empty
     (vector-set! vec i val)
     (current-gained-information? #t)]
    [(== val eq?)
     (void)]
    [other-val
     (if contradiction-reason
         (raise-contradiction contradiction-reason)
         (raise-arguments-error who "internal error: overwriting non-empty tile"
                                "old tile" other-val
                                "new tile" val
                                "index" i
                                "tiles..." vec))]))

(define (tiles-fill!/track vec value [start-i 0] [end-i (vector-length vec)]
                           #:contradiction-reason [contradiction-reason (current-contradiction-reason)])
  (for ([i (in-range start-i end-i)])
    (tiles-set!/track vec i value #:contradiction-reason contradiction-reason)))

;; -----------------------------------------------------------------------------

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

(define (neighbor-matching? tiles i pred?)
  (or (and (> i 0)
           (pred? (vector-ref tiles (sub1 i))))
      (and (< (add1 i) (vector-length tiles))
           (pred? (vector-ref tiles (add1 i))))))

(define (span-all? tiles start-i end-i pred?)
  (for/and ([i (in-range start-i end-i)])
    (pred? (vector-ref tiles i))))

;; -----------------------------------------------------------------------------

(define (find-next vec start-i pred? #:end [end-i (vector-length vec)])
  (let loop ([i start-i])
    (if (< i end-i)
        (if (pred? (vector-ref vec i))
            i
            (loop (add1 i)))
        #f)))

(define (find-prev vec end-i pred? #:start [start-i 0])
  (let loop ([i (sub1 end-i)])
    (if (>= i start-i)
        (if (pred? (vector-ref vec i))
            i
            (loop (sub1 i)))
        #f)))

(define (find-first vec pred?)
  (find-next vec 0 pred?))

(define (find-last vec pred?)
  (find-prev vec (vector-length vec) pred?))

;; Returns the index of the first tile in the contiguous span covering
;; `middle-i` satisfying `pred?`.
(define/who (span-start vec middle-i pred? #:start [start-i 0])
  (unless (pred? (vector-ref vec middle-i))
    (raise-arguments-error who "tile at the given index does not match predicate"
                           "index" middle-i
                           "tile" (vector-ref vec middle-i)
                           "predicate" pred?
                           "vector..." vec))
  (match (find-prev vec middle-i (λ~> pred? not) #:start start-i)
    [#f start-i]
    [prev-i (add1 prev-i)]))

;; Returns one more than the index of the last tile in the contiguous span
;; covering `middle-i` satisfying `pred?`.
(define/who (span-end vec middle-i pred? #:end [end-i (vector-length vec)])
  (unless (pred? (vector-ref vec middle-i))
    (raise-arguments-error who "tile at the given index does not match predicate"
                           "index" middle-i
                           "tile" (vector-ref vec middle-i)
                           "predicate" pred?
                           "vector..." vec))
  (match (find-next vec middle-i (λ~> pred? not) #:end end-i)
    [#f end-i]
    [next-i next-i]))

(define (span-start+end vec middle-i pred?
                        #:start [start-i 0]
                        #:end [end-i (vector-length vec)])
  (values (span-start vec middle-i pred? #:start start-i)
          (span-end vec middle-i pred? #:end end-i)))

;; Returns the length of the contiguous span starting at `start-i` values satisfying `pred?`.
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

;; -----------------------------------------------------------------------------

;; An index for a single line clue in a mega line.
(struct single-line-index (chunk line index) #:transparent)
(define mega-clue-index? (or/c natural? single-line-index?))

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

;; -----------------------------------------------------------------------------

(define mega-tiles/c (array/c tiles/c tiles/c))
(define mutable-mega-tiles/c (array/c mutable-tiles/c mutable-tiles/c))

(define (mega-tiles-length tiles)
  (vector-length (array-ref tiles 0)))

(define (mega-tiles-length/mega tiles)
  (* (mega-tiles-length tiles) 2))

(define (tiles-ref/mega tiles mi)
  (vector-ref (array-ref tiles (mega-index-line mi))
              (mega-index-tile mi)))

(define (tiles-count/mega tiles pred? [start-mi 0] [end-mi (mega-tiles-length/mega tiles)])
  (for/sum ([mi (in-range start-mi end-mi)]
            #:when (pred? (tiles-ref/mega tiles mi)))
    1))

(define/who (tiles-set!/mega tiles mi val
                             #:track? [track? #t]
                             #:contradiction-reason [contradiction-reason (current-contradiction-reason)])
  (define tile-line (array-ref tiles (mega-index-line mi)))
  (if track?
      (tiles-set!/track tile-line (mega-index-tile mi) val
                        #:contradiction-reason contradiction-reason)
      (vector-set! tile-line (mega-index-tile mi) val)))

(define (tiles-fill!/mega tiles val [start-mi 0] [end-mi (mega-tiles-length/mega tiles)]
                          #:contradiction-reason [contradiction-reason (current-contradiction-reason)])
  (for ([mi (in-range start-mi end-mi)])
    (tiles-set!/mega tiles mi val #:contradiction-reason contradiction-reason)))

;; -----------------------------------------------------------------------------

(define (bounded-behind?/mega tiles mi)
  (or (zero? (mega-index-tile mi))
      (tile-cross? (tiles-ref/mega tiles (behind mi)))))

(define (bounded-afront?/mega tiles mi)
  (or (= (add1 (mega-index-tile mi)) (mega-tiles-length tiles))
      (tile-cross? (tiles-ref/mega tiles (afront mi)))))

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
  (define at-end? (= (add1 (mega-index-tile mi))
                     (mega-tiles-length tiles)))
  (and (or at-end?
           (tile-cross? (tiles-ref/mega tiles (afront mi))))
       (or (tile-cross? (tiles-ref/mega tiles (opposite mi)))
           (match (mega-index-line mi)
             [0 #f]
             [1 (or at-end?
                    (tile-cross? (tiles-ref/mega tiles (add1 mi))))]))))

(define (neighbor-matching?/mega tiles mi pred?)
  (or (pred? (tiles-ref/mega tiles (opposite mi)))
      (and (> (mega-index-tile mi) 0)
           (pred? (tiles-ref/mega tiles (behind mi))))
      (and (< (add1 (mega-index-tile mi)) (mega-tiles-length tiles))
           (pred? (tiles-ref/mega tiles (afront mi))))))

(define (neighbors-count/mega tiles mi pred?)
  (define i (mega-index-tile mi))
  (define (->1 v) (if v 1 0))
  (+ (->1 (pred? (tiles-ref/mega tiles (opposite mi))))
     (->1 (and (> i 0)
               (pred? (tiles-ref/mega tiles (behind mi)))))
     (->1 (and (< (add1 i) (mega-tiles-length tiles))
               (pred? (tiles-ref/mega tiles (afront mi)))))))

;; -----------------------------------------------------------------------------

(define (find-next/mega tiles start-mi pred? #:end [end-mi (mega-tiles-length/mega tiles)])
  (let loop ([mi start-mi])
    (if (< mi end-mi)
        (if (pred? (tiles-ref/mega tiles mi))
            mi
            (loop (add1 mi)))
        #f)))

(define (find-prev/mega tiles end-mi pred? #:start [start-mi 0])
  (let loop ([mi (sub1 end-mi)])
    (if (>= mi start-mi)
        (if (pred? (tiles-ref/mega tiles mi))
            mi
            (loop (sub1 mi)))
        #f)))

(define (find-first/mega tiles pred?)
  (find-next/mega tiles 0 pred?))

(define (find-last/mega tiles pred?)
  (find-prev/mega tiles (mega-tiles-length/mega tiles) pred?))

(define (find-next-hole/mega tiles start-mi)
  (find-next/mega tiles start-mi tile-hole?))

(define (find-singular-hole/mega tiles)
  (match (find-next-hole/mega tiles 0)
    [#f #f]
    [start-mi
     (define-values [size end-mi] (hole-size+end/mega tiles start-mi))
     (match (find-next-hole/mega tiles end-mi)
       [#f (array start-mi end-mi size)]
       [_  #f])]))

;; -----------------------------------------------------------------------------

(define (connected-region-spans-both-lines? start-mi end-mi size)
  (define len (- (mega-index-tile (afront (sub1 end-mi)))
                 (mega-index-tile start-mi)))
  (< len size))

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

(define (hole-end/mega tiles start-mi)
  (connected-region-end/mega tiles start-mi tile-hole?))

;; -----------------------------------------------------------------------------

;; Note [Line affinity]
;; ~~~~~~~~~~~~~~~~~~~~
;; In some situations, using a mega index is too precise: it always
;; identifies a specific line. For example, consider the following row:
;;   ■☐☒☐☐
;;   ■☐☐☐■
;; Suppose we want to calculate the minimum number of tiles needed to
;; connect the two contiguous filled regions. We cannot specify the location
;; of the starting point using a mega index, as it would require picking
;; a specific tile in one of the two lines. Instead, in these situations, we
;; use a combination of a tile index and a “line affinity”, which is either
;; 0, 1, or #f, where #f represents “no affinity”.

(define line-affinity? (or/c 0 1 #f))

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

;; -----------------------------------------------------------------------------

(define mega-placement-bound-line? (or/c 0 1 'either 'both))
(struct mega-placement-bound (line tile) #:transparent)

(define (mega-index->placement-bound mi)
  (mega-placement-bound (mega-index-line mi)
                        (mega-index-tile mi)))

;; mega-placement-bound->least-upper-bound : mega-placement-bound? -> mega-index?
(define (mega-placement-bound->least-upper-bound bound)
  (match-define (mega-placement-bound line-i i) bound)
  (match line-i
    [(or 0 1) (mega-index line-i i)]
    ['both    (mega-index 1 i)]
    ['either  (mega-index 0 i)]))

;; mega-placement-bound->greatest-lower-bound : mega-placement-bound? -> mega-index?
(define (mega-placement-bound->greatest-lower-bound bound)
  (match-define (mega-placement-bound line-i i) bound)
  (match line-i
    [(or 0 1) (mega-index line-i i)]
    ['both    (mega-index 0 i)]
    ['either  (mega-index 1 i)]))

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
;;   ☐☐☐☐☐
;;   ☒☒☒☐☐
;; then the earliest possible placement for a clue of size 4 is
;;   ☐■■■☐
;;   ☒☒☒■☐
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

  ;; A 2 clue cannot start on line 1 or end on line 0.
  (define start-mi*
    (if (and (= size 2)
             (= (mega-index-line start-mi) special-line-i))
        (next start-mi)
        start-mi))

  (define num-tiles (array-length (array-ref tiles 0)))
  (define start-line-i (mega-index-line start-mi*))
  (define start-tile-i (mega-index-tile start-mi*))
  (let loop ([start-mi start-mi*]
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
  (check-equal? (find-earliest-tightest-placement-start+end/mega
                 #(#(cross empty empty)
                   #(empty empty cross))
                 (mega-index 1 0)
                 2)
                (cons (mega-index 0 1)
                      (mega-placement-bound 'both 1)))
  (check-equal? (find-latest-tightest-placement-end+start/mega
                 #(#(cross empty empty)
                   #(empty empty cross))
                 (mega-index 0 2)
                 2)
                (cons (mega-index 1 1)
                      (mega-placement-bound 'both 1)))
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

;; fill-forced-tiles-in-mega-span!
;;   : mega-tiles/c mega-placement-bound? mega-placement-bound?
;;     #:set-full! (-> mega-index? any)
;;  -> (values natural? line-affinity? line-affinity?)
;;
;; Given two mega placement bounds, fills any tiles forced by crosses along the
;; path bridging the two bounds. For example, suppose we have the following row:
;;   ☒☐☐☐☐☐☐☒☐■
;;   ■☐☐☒☐☐☐☐☐☐
;; This function will fill any tiles that must be filled to connect the two
;; filled boxes:
;;   ☒☐■■■☐☐☒☐■
;;   ■■☐☒☐☐■■■☐
;;
;; Along the way, it also calculates and returns the minimum number of tiles
;; that must be filled within the span, taking existing filled tiles into
;; account. It also returns two line affinities that can be used to calculate
;; the minimum cost of extending a placement with the given bounds backwards or
;; forwards, respectively. For example, given the row
;;   ☐☐■☐☒☐☐☐
;;   ☐☐☐☐☐■☐☐
;; then the results of this function applied with bounds (mega-placement-bound 0 2)
;; and (mega-placement-bound 1 5) will be as follows:
;;
;;   1. The returned minimum size will be 5, as at least 5 tiles must be filled
;;      to bridge the two bounds.
;;
;;   2. The first line affinity will be #f, as one possible way to bridge the
;;      two bounds is
;;        ☐☐■☐☒☐☐☐
;;        ☐☐■■■■☐☐
;;      which fills both tiles in the first column. Therefore, it is possible to
;;      extend the filled region backwards to either line at equal cost.
;;
;;   3. The second line affinity will be 1, as filling the tile in the first
;;      line above the end bound would require filling 6 tiles, not five.
;;      Therefore, extending the filled region forwards is biased towards the
;;      second line.
;;
;; Note that the way this function interprets bounds with line 'both or 'either
;; can be somewhat counterintuitive; see Note [Filling forced tiles between
;; placement bounds] for an explanation.
(define/who (fill-forced-tiles-in-mega-span! tiles first-bound last-bound #:set-full! set-full!)
  (match-define (mega-placement-bound first-line first-i) first-bound)
  (match-define (mega-placement-bound last-line last-i) last-bound)

  (define (bound-line->aff bl)
    (match bl
      [(or 0 1) bl]
      [(or 'either 'both) #f]))

  (define num-tiles (mega-tiles-length tiles))

  ;; Note [Filling forced tiles between placement bounds]
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; This function is used in two different ways:
  ;;
  ;;   1. It is used to bridge the first and last filled tiles in a mega clue.
  ;;      When used in this way, the bounds always refer to a specific line,
  ;;      never 'both or 'either. This is the mode in which the return values
  ;;      are utilized.
  ;;
  ;;   2. It is used to fill tiles forced by the intersection of the earliest
  ;;      and latest tightest placements for a clue (or the intersection of one
  ;;      such placement and a filled-in tile). This mode is called purely for
  ;;      its side effects (though we still try to return reasonable values).
  ;;
  ;; The interpretation of bounds supplied in the second case can be somewhat
  ;; counterintuitive, as they represent the greatest lower bound and least
  ;; upper bound for a clue’s placement, not tiles that must necessarily be
  ;; filled by the placement. For example, suppose we have the following empty
  ;; row for a mega 6 clue:
  ;;   ☐☐☐☐
  ;;   ☐☐☐☐
  ;; When we calculate the earliest and latest placements for this clue, we will
  ;; get the following two results:
  ;;   ▧▧▧☐  ☐▧▧▧
  ;;   ▧▧▧☐  ☐▧▧▧
  ;; The earliest placement has an end bound of (mega-placement-bound 'both 2)
  ;; and the latest placement has a start bound of (mega-placement-bound 'both 1).
  ;; The intersection of these bounds yields the following span:
  ;;   ☐▧▧☐
  ;;   ☐▧▧☐
  ;; This function will be called to fill any tiles forced within that span.
  ;; Given the fact that the bounds specify 'both lines, it would be easy to
  ;; mistakenly believe that all four tiles should be filled. However, that
  ;; would be completely wrong in this case, as the bounds do not specify which
  ;; tiles are (or should be) filled, they simply define the region inside which
  ;; forced tiles *may* be filled. In this example, there are no forced tiles,
  ;; so the proper behavior is to do nothing at all.
  ;;
  ;; A natural followup question is to ask what the difference is between a
  ;; bound using 'both and a bound using 'either, or indeed what the difference
  ;; is between such a bound and one using a specific line. The answer lies in
  ;; what we consider to be “forced” at the span endpoints; see Note [Forced
  ;; mega span endpoints] for details.
  ;;
  ;; Note [Forced mega span endpoints]
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;; Endpoints of the span must be handled specially. Normally, we fill tiles
  ;; in one of the following shapes:
  ;;   ■■■  ☐☒☐
  ;;   ☐☒☐  ■■■
  ;; This corresponds to the span having to snake around a cross. However, at
  ;; consider the following row:
  ;;   ☒☐☒☐☐☐☒
  ;;   ☒☐☐☐☒☐☒
  ;; Suppose the span starts at the top left empty tile and ends at the bottom
  ;; right empty tile. If we only fill tiles in the above patterns, we’ll end up
  ;; with the following:
  ;;   ☒☐☒■■■☒
  ;;   ☒■■■☒☐☒
  ;; But this is missing the tiles at the ends! However, we don’t want to fill
  ;; the bounds unconditionally. Suppose instead we have the following row:
  ;;   ☐☐☒☐☐☐☐
  ;;   ☐☐☐☐☒☐☐
  ;; If the start and end locations are still the same, the tiles at the ends
  ;; are no longer forced. On the other hand, if we have the row
  ;;   ☐☐☒☐☐☐☒
  ;;   ☒☐☐☐☒☐☐
  ;; they *are* forced. Programmatically, the necessary conditions are:
  ;;
  ;;   1. The bound must cover 'both lines.
  ;;   2. The opposite tile must be bounded behind or afront.
  ;;
  ;; Somewhat counterintuitively, the second condition is all that matters, and
  ;; it holds regardless of whether the bound is the start bound or the end
  ;; bound. To illustrate, consider the following examples:
  ;;   ☐☐▧☐☐  ☐☐▧☐☐
  ;;   ☐☒☐☐☐  ☐☐☐☒☐
  ;; In both examples, ▧ represents a tile in one of the bound columns.
  ;; Regardless of whether we are filling from the left or the right, if the
  ;; bound’s line is 'both, the ▧ tile must be filled.

  ;; see Note [Forced mega span endpoints]
  (define (maybe-fill-end-tile! mi)
    (cond
      [(or (bounded-behind?/mega tiles (opposite mi))
           (bounded-afront?/mega tiles (opposite mi)))
       (set-full! mi)
       1]
      [(tile-full? (tiles-ref/mega tiles mi)) 1]
      [else 0]))

  ;; see Note [Forced mega span endpoints]
  (define (maybe-fill-end-tiles! bound)
    (match-define (mega-placement-bound line i) bound)
    (match line
      ['both
       (+ (maybe-fill-end-tile! (mega-index 0 i))
          (maybe-fill-end-tile! (mega-index 1 i)))]
      [_ 0]))

  (cond
    [(> first-i last-i)
     (raise-arguments-error who "first bound is larger than last bound"
                            "first bound" first-bound
                            "last bound" last-bound)]
    [(= first-i last-i)
     (match* {first-line last-line}
       [{'both 'both}
        (values (+ (maybe-fill-end-tile! (mega-index 0 first-i))
                   (maybe-fill-end-tile! (mega-index 1 first-i)))
                #f
                #f)]
       [{(or 0 'both) (or 0 'both)}
        (set-full! (mega-index 0 first-i))
        (values 1 0 0)]
       [{(or 1 'both) (or 1 'both)}
        (set-full! (mega-index 1 first-i))
        (values 1 1 1)]
       [{_ _}
        (values 0 #f #f)])]
    [else
     (maybe-fill-end-tiles! first-bound)
     (maybe-fill-end-tiles! last-bound)

     (define first-aff (bound-line->aff first-line))
     (define last-aff (bound-line->aff last-line))
     (define (last-aff? line-i)
       (or (not last-aff) (= last-aff line-i)))
     (define-values [min-filled backwards-aff forwards-aff]
       (let loop ([min-filled 0]
                  [backwards-aff #f]  ;; (or/c #f 0 1 'start 'both)
                  [forwards-aff 'end] ;; (or/c 0 1 'end 'both)
                  [line-aff first-aff]
                  [i first-i])

         (define (line-aff? line-i)
           (or (not line-aff) (= line-aff line-i)))
         (define (line-cost line-i)
           (if (line-aff? line-i) 0 1))

         (cond
           [(> i last-i)
            (values min-filled (or backwards-aff 'start) forwards-aff)]
           [else
            (define mi0 (mega-index 0 i))
            (define mi1 (mega-index 1 i))
            (define tile-0 (tiles-ref/mega tiles mi0))
            (define tile-1 (tiles-ref/mega tiles mi1))
            (match* {tile-0 tile-1}
              [{_ 'cross}
               (unless (= i first-i)
                 (set-full! (behind mi0)))
               (set-full! mi0)
               (unless (= i last-i)
                 (set-full! (afront mi0)))
               (loop (+ min-filled 1 (line-cost 0))
                     (or backwards-aff 0)
                     0
                     0
                     (add1 i))]
              [{'cross _}
               (unless (= i first-i)
                 (set-full! (behind mi1)))
               (set-full! mi1)
               (unless (= i last-i)
                 (set-full! (afront mi1)))
               (loop (+ min-filled 1 (line-cost 1))
                     (or backwards-aff 1)
                     1
                     1
                     (add1 i))]
              [{'full 'full}
               (loop (+ min-filled 2)
                     (or backwards-aff (if (= i first-i) 'both 'start))
                     (if (= i last-i) 'both 'end)
                     #f
                     (add1 i))]
              [{'full 'empty}
               (define forwards-aff* (if (last-aff? 0) forwards-aff 'both))
               (if (line-aff? 0)
                   (loop (+ min-filled 1) backwards-aff forwards-aff* 0 (add1 i))
                   (loop (+ min-filled 2) (or backwards-aff 'both) forwards-aff* #f (add1 i)))]
              [{'empty 'full}
               (define forwards-aff* (if (last-aff? 1) forwards-aff 'both))
               (if (line-aff? 1)
                   (loop (+ min-filled 1) backwards-aff forwards-aff* 1 (add1 i))
                   (loop (+ min-filled 2) (or backwards-aff 'both) forwards-aff* #f (add1 i)))]
              [{'empty 'empty}
               (loop (+ min-filled 1) backwards-aff forwards-aff line-aff (add1 i))])])))

     (values min-filled
             (match* {backwards-aff first-aff}
               [{'both          _        } #f]
               [{(or 'start #f) (or 0 1) } first-aff]
               [{0              (or 0 #f)} 0]
               [{1              (or 1 #f)} 1]
               [{_              _        } #f])
             (match* {forwards-aff last-aff}
               [{'both          _        } #f]
               [{(or 'end #f)   (or 0 1) } last-aff]
               [{0              (or 0 #f)} 0]
               [{1              (or 1 #f)} 1]
               [{_              _        } #f]))]))

(module+ test
  (define (fill-forced-tiles-in-mega-span!* tiles first-bound last-bound)
    (define tiles* (array-map array->vector tiles))
    (define-values [min-filled backwards-aff forwards-aff]
      (fill-forced-tiles-in-mega-span!
       tiles* first-bound last-bound
       #:set-full! (λ (mi) (tiles-set!/mega tiles* mi 'full))))
    (array min-filled backwards-aff forwards-aff tiles*))

  (check-equal? (fill-forced-tiles-in-mega-span!*
                 #(#(empty empty full empty)
                   #(empty full empty empty))
                 (mega-placement-bound 1 1)
                 (mega-placement-bound 0 2))
                (array 3 #f #f
                       #(#(empty empty full empty)
                         #(empty full empty empty))))

  (check-equal? (fill-forced-tiles-in-mega-span!*
                 #(#(empty full  empty cross empty empty)
                   #(empty empty empty empty full  empty))
                 (mega-placement-bound 0 1)
                 (mega-placement-bound 1 4))
                (array 5 #f 1
                       #(#(empty full  empty cross empty empty)
                         #(empty empty full  full  full  empty))))

  (check-equal? (fill-forced-tiles-in-mega-span!*
                 #(#(empty cross empty)
                   #(empty empty empty))
                 (mega-placement-bound 1 0)
                 (mega-placement-bound 'both 2))
                (array 4 1 #f
                       #(#(empty cross full)
                         #(full  full  full))))

  (check-equal? (fill-forced-tiles-in-mega-span!*
                 #(#(cross cross cross empty empty)
                   #(empty empty empty empty empty))
                 (mega-placement-bound 1 1)
                 (mega-placement-bound 'either 4))
                (array 4 1 1
                       #(#(cross cross cross empty empty)
                         #(empty full  full  full  empty))))

  (check-equal? (fill-forced-tiles-in-mega-span!*
                 #(#(cross empty empty)
                   #(full  full  empty))
                 (mega-placement-bound 'both 1)
                 (mega-placement-bound 'either 2))
                (array 2 #f #f
                       #(#(cross empty empty)
                         #(full  full  empty)))))
