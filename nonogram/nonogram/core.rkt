#lang racket/base

(require json
         racket/contract
         racket/list
         racket/match
         racket/math
         racket/string
         threading
         toolbox/who
         "array.rkt")

(module+ example
  (provide board-1
           board-2
           clues-1
           clues-2
           puzzle-1
           puzzle-2

           all-puzzles
           all-puzzle-names
           get-puzzle))

(module+ test
  (require rackunit
           (submod ".." example)))

(provide (contract-out
          [axis? flat-contract?]
          [tile? flat-contract?]
          [mega-line-offset? flat-contract?]
          [tile-line? flat-contract?]

          (struct board ([rows (arrayof tile-line?)]))
          [make-board (-> natural? natural? board?)]
          [board-width (-> board? natural?)]
          [board-height (-> board? natural?)]
          [board-axis-length (-> board? axis? natural?)]
          [board-ref (-> board? natural? natural? tile?)]
          [board-set (-> board? natural? natural? tile? board?)]
          [board-row (-> board? natural? tile-line?)]
          [board-column (-> board? natural? tile-line?)]
          [board-line (-> board? axis? natural? tile-line?)]
          [board-clear (-> board? board?)]

          [clue? flat-contract?]
          [single-line-clues? flat-contract?]
          [mega-line-clues-chunk? flat-contract?]
          [mega-line-clues? flat-contract?]
          [line-clues-type? flat-contract?]
          (struct line-clues ([type line-clues-type?]
                              [clues (or/c single-line-clues?
                                           mega-line-clues?)]))
          [axis-clues? flat-contract?]
          (struct board-clues ([row-clues axis-clues?]
                               [column-clues axis-clues?]))
          [axis-clues-line-count (-> axis-clues? natural?)]
          [line-index->axis-clue-index (-> axis-clues?
                                           natural?
                                           (or/c natural?
                                                 (array/c natural? mega-line-offset?)))]
          [board-clues-axis (-> board-clues? axis? axis-clues?)]
          [board-clues-line (-> board-clues?
                                axis?
                                natural?
                                (or/c single-line-clues?
                                      (array/c mega-line-clues? mega-line-offset?)))]
          [solved-line->clues (-> tile-line? line-clues?)]
          [solved-board->clues (-> board? board-clues?)]

          (struct puzzle ([board board?]
                          [clues board-clues?]))
          [clues->puzzle (-> board-clues? puzzle?)]
          [solved-board->puzzle (-> board? puzzle?)]
          [puzzle-axis-tiles+clues
           (-> puzzle?
               axis?
               (arrayof (or/c (array/c tile-line? single-line-clues?)
                              (array/c tile-line? tile-line? mega-line-clues?))))]

          [parse-puzzle-nonograms.com-clues (-> string? natural? board-clues?)]
          [parse-nonograms.org-solution (-> string? board?)]))

;; -----------------------------------------------------------------------------

(define axis? (or/c 'row 'column))
(define tile? (or/c 'empty 'full 'cross 'mark))

(define mega-line-offset? (or/c 0 1))

;; A *tile line* is a row or a column of tiles.
(define tile-line? (arrayof tile?))

;; -----------------------------------------------------------------------------
;; board

(struct board (rows) #:transparent)

;; make-board : natural? natural? -> board?
(define (make-board width height)
  (define row (make-array width 'empty))
  (board (make-array height row)))

(module+ example
  (define board-1
    (board #(#(empty empty full)
             #(full  empty full)
             #(empty full  full))))

  (define board-2
    (board #(#(empty empty full empty)
             #(empty full  full full)))))

;; board-width : board? -> natural?
(define (board-width b)
  (define rows (board-rows b))
  (if (zero? (array-length rows))
      0
      (array-length (array-ref rows 0))))

(module+ test
  (check-equal? (board-width board-1) 3)
  (check-equal? (board-width board-2) 4))

;; board-height : board? -> natural?
(define (board-height b)
  (array-length (board-rows b)))

(module+ test
  (check-equal? (board-height board-1) 3)
  (check-equal? (board-height board-2) 2))

;; board-axis-length : board? axis? -> natural?
;;   (Note that this returns the length of the given axis itself, not the length
;;   of the lines within the axis.)
(define (board-axis-length b axis)
  (match axis
    ['row    (board-height b)]
    ['column (board-width b)]))

;; board-ref : board? natural? natural? -> tile?
(define (board-ref b x y)
  (array-ref (array-ref (board-rows b) y) x))

(module+ test
  (check-equal? (board-ref board-1 0 0) 'empty)
  (check-equal? (board-ref board-1 2 0) 'full)
  (check-equal? (board-ref board-1 0 2) 'empty)
  (check-equal? (board-ref board-1 2 2) 'full))

;; board-set : board? natural? natural? tile? -> board?
(define (board-set b x y tile)
  (define rows (board-rows b))
  (define old-row (array-ref rows y))
  (define new-row (array-set old-row x tile))
  (board (array-set rows y new-row)))

(module+ test
  (check-equal? (board-set board-1 1 1 'full)
                (board #(#(empty empty full)
                         #(full  full  full)
                         #(empty full  full)))))

;; board-row : board? natural? -> tile-line?
(define (board-row b y)
  (array-ref (board-rows b) y))

;; board-column : board? natural? -> tile-line?
(define (board-column b x)
  (define height (board-height b))
  (for/array #:length height
             ([y (in-range height)])
    (board-ref b x y)))

;; board-line : board? axis? natural? -> tile-line?
(define (board-line b axis i)
  (match axis
    ['row (board-row b i)]
    ['column (board-column b i)]))

;; board-columns : board? -> (arrayof tile-line?)
(define (board-columns b)
  (define width (board-width b))
  (for/array #:length width
             ([x (in-range width)])
    (board-column b x)))

;; board-axis : board? axis? -> (arrayof tile-line?)
(define (board-axis b axis)
  (match axis
    ['row (board-rows b)]
    ['column (board-columns b)]))

;; board-clear : board? -> board?
(define (board-clear b)
  (make-board (board-width b) (board-height b)))

;; -----------------------------------------------------------------------------
;; clues

(define clue? exact-positive-integer?)
(define single-line-clues? (listof clue?))

(define mega-line-clues-chunk?
  (or/c clue? ;; mega clue
        (array/c single-line-clues?
                 single-line-clues?)))
(define mega-line-clues? (listof mega-line-clues-chunk?))

(define line-clues-type? (or/c 'single 'mega))
(struct line-clues (type clues) #:transparent)

;; single-line-clues : single-line-clues? -> line-clues?
(define (single-line-clues clues)
  (line-clues 'single clues))

(define axis-clues? (arrayof line-clues?))
(struct board-clues
  (row-clues     ;; axis-clues?
   column-clues) ;; axis-clues?
  #:transparent)

(module+ example
  (define clues-1
    (board-clues
     (array-map single-line-clues #((1) (1 1) (2)))
     (array-map single-line-clues #((1) (1) (3)))))

  (define clues-2
    (board-clues
     (array-map single-line-clues #((1) (3)))
     (array-map single-line-clues #(() (1) (2) (1))))))

;; lines-clues-span : line-clues? -> (or/c 1 2)
(define (lines-clues-span lc)
  (match (line-clues-type lc)
    ['single 1]
    ['mega   2]))

;; axis-clues? -> natural?
(define (axis-clues-line-count lcs)
  (for/sum ([lc (in-array lcs)])
    (lines-clues-span lc)))

;; line-index->axis-clue-index
;;   : axis-clues? natural?
;;  -> (or/c natural?
;;           (array/c natural? mega-line-offset?))
(define/who (line-index->axis-clue-index clues i #:who [who who])
  (define num-clues (array-length clues))
  (let loop ([ci 0]
             [j 0])
    (cond
      [(>= ci num-clues)
       (raise-range-error who "axis clues" "" i clues 0 (sub1 j))]
      [else
       (define lc (array-ref clues ci))
       (match (line-clues-type lc)
         ['single
          (if (= i j)
              ci
              (loop (add1 ci) (add1 j)))]
         ['mega
          (if (<= i (add1 j))
              (array ci (- i j))
              (loop (add1 ci) (+ j 2)))])])))

(module+ test
  (let ()
    (define axis-clues (array (line-clues 'single '(1))
                              (line-clues 'mega '(1))
                              (line-clues 'mega '(1))))
    (check-equal? (line-index->axis-clue-index axis-clues 0) 0)
    (check-equal? (line-index->axis-clue-index axis-clues 1) (array 1 0))
    (check-equal? (line-index->axis-clue-index axis-clues 2) (array 1 1))
    (check-equal? (line-index->axis-clue-index axis-clues 3) (array 2 0))
    (check-equal? (line-index->axis-clue-index axis-clues 4) (array 2 1))))

;; board-clues-clues : board-clues? axis? -> axis-clues?
(define (board-clues-axis clues axis)
  (match axis
    ['row    (board-clues-row-clues clues)]
    ['column (board-clues-column-clues clues)]))

;; board-clues-line : board-clues? axis? natural?
;;                 -> (or/c single-line-clues?
;;                          (array/c mega-line-clues? mega-line-offset?))
(define/who (board-clues-line clues axis i)
  (define axis-clues (board-clues-axis clues axis))
  (match (line-index->axis-clue-index axis-clues i #:who who)
    [(array ci which)
     (array (line-clues-clues (array-ref axis-clues ci)) which)]
    [ci
     (line-clues-clues (array-ref axis-clues ci))]))

(module+ test
  (check-equal? (board-clues-line clues-1 'row 0) '(1))
  (check-equal? (board-clues-line clues-1 'row 1) '(1 1))
  (check-equal? (board-clues-line clues-1 'row 2) '(2))
  (check-equal? (board-clues-line clues-1 'column 0) '(1))
  (check-equal? (board-clues-line clues-1 'column 1) '(1))
  (check-equal? (board-clues-line clues-1 'column 2) '(3)))

;; solved-line->clues : tile-line? -> line-clues?
(define (solved-line->clues tiles)
  (define num-tiles (array-length tiles))
  (single-line-clues
   (let outer-loop ([i 0])
     (if (< i num-tiles)
         (match (array-ref tiles i)
           ['full
            (let inner-loop ([j (add1 i)])
              (if (< j num-tiles)
                  (match (array-ref tiles j)
                    ['full (inner-loop (add1 j))]
                    [_ (cons (- j i) (outer-loop j))])
                  (list (- j i))))]
           [_ (outer-loop (add1 i))])
         '()))))

(module+ test
  (check-equal? (solved-line->clues
                 #(full cross full full cross cross full full full cross full))
                (line-clues 'single '(1 2 3 1))))

(define (solved-board->clues b)
  (define w (board-width b))
  (define h (board-height b))
  (board-clues
   (for/array #:length h ([i (in-range h)])
     (solved-line->clues (board-row b i)))
   (for/array #:length w ([i (in-range w)])
     (solved-line->clues (board-column b i)))))

(module+ test
  (check-equal? (solved-board->clues
                 (board #(#(full  cross cross full)
                          #(cross full  full  cross))))
                (board-clues
                 (array-map single-line-clues #((1 1) (2)))
                 (array-map single-line-clues #((1) (1) (1) (1))))))

;; -----------------------------------------------------------------------------
;; puzzle

(struct puzzle
  (board  ;; board?
   clues) ;; board-clues?
  #:transparent)

;; clues->puzzle : board-clues? -> puzzle?
(define (clues->puzzle clues)
  (puzzle (make-board (axis-clues-line-count (board-clues-column-clues clues))
                      (axis-clues-line-count (board-clues-row-clues clues)))
          clues))

;; solved-board->puzzle : board? -> puzzle?
(define (solved-board->puzzle b)
  (puzzle (board-clear b) (solved-board->clues b)))

;; puzzle-axis-tiles+clues
;;   : puzzle? axis?
;;  -> (arrayof (or/c (array/c tile-line? single-line-clues?)
;;                    (array/c tile-line? tile-line? multi-line-clues?)))
;; 
(define (puzzle-axis-tiles+clues pz axis)
  (define board (puzzle-board pz))
  (define clues (puzzle-clues pz))
  (define lcs (board-clues-axis clues axis))

  (define tiles+clues (make-vector (array-length lcs) #f))
  (for/fold ([j 0])
            ([(lc i) (in-indexed (in-array lcs))])
    (match-define (line-clues type clues) lc)
    (match type
      ['single
       (define t+c (array (board-line board axis j) clues))
       (vector-set! tiles+clues i t+c)
       (add1 j)]
      ['mega
       (define t+c (array (board-line board axis j)
                          (board-line board axis (add1 j))
                          clues))
       (vector-set! tiles+clues i t+c)
       (+ j 2)]))

  (unsafe-vector*->array! tiles+clues))

(module+ example
  (define puzzle-1 (puzzle board-1 clues-1))
  (define puzzle-2 (puzzle board-2 clues-2))

  (define puzzle-s5-001
    (clues->puzzle
     (board-clues
      (array-map single-line-clues #((1 1) (1 1 1) (5) (1) (3)))
      (array-map single-line-clues #((3) (1 1) (5) (1 1) (2))))))

  (define puzzle-s5-016
    (clues->puzzle
     (board-clues
      (array-map single-line-clues #((1 2) (1 2) (2) (3) (2 2 1) (2 2 3) (1 2 2 1) (2 6) (1 2 1) (2 1)))
      (array-map single-line-clues #((4) (1 1) (1 1) (4) (1 1 1) (1 1 1 1) (1 1 3) (1 1 5) (6 1) (3 2 1))))))

  (define puzzle-s5-031
    (clues->puzzle
     (board-clues
      (array-map single-line-clues #((2) (1 2) (2 1 3) (2 4) (1 7) (6) (1 4 1) (3 2 2) (5 3) (7)))
      (array-map single-line-clues #((2 3) (2 2) (2 2 3) (1 4 2) (1 5 2) (1 5 1) (1 5 1) (1 2 2) (1 1 3) (1 3))))))

  (define puzzle-s5-061
    (clues->puzzle
     (board-clues
      (array-map single-line-clues #((6 4) (5 4) (4 4) (7) (6) (1 1 1) (1 1 1 1) (1 1 1 1) (1 3) (1 1 1) (1 1) (1 1 1) (1 1) (3) (1 3)))
      (array-map single-line-clues #((1) (1) (2) (3) (4 2) (6 5 1) (4 2 1) (3 2 1 2) (3 3 2) (5 5 1) (4 2) (3) (2) (1) (1))))))

  (define puzzle-s5-135
    (clues->puzzle
     (board-clues
      (array-map single-line-clues #((3 4) (1 3 2) (2 3 1 1 1) (4 3 3) (1 1 1 1 1) (1 3 1) (1 3 1 1 1) (1 1 1 1) (4 1 1 1) (6 3) (3 7) (12) (9 1) (1 2 2) (8 3)))
      (array-map single-line-clues #((3 4) (1 2 3 1) (1 1 1 3 1) (3 2 2 1) (3 4 2 1) (2 2 5 1) (1 1 4 1) (3 7 1) (1 2 4 1) (1 2 2 1) (1 2 3 1) (1 3 6) (1 1 1 1 1) (2 2 1 2) (2 2 2))))))

  (define puzzle-s5-137
    (clues->puzzle
     (board-clues
      (array-map single-line-clues #((8 9) (3 8) (1 3 6) (6 1 4) (5 1 2) (5 3 1) (5 4) (5 5) (5 6) (6 6) (6 6) (5 7) (1 3 6 1) (4 2) (8 6)))
      (array-map single-line-clues #((1 9 3) (1 8 2) (1 9 2) (1 10 2) (12 1) (4 4 1) (2 2 1) (1 1 1) () (1) (1) (1 2) (2 3) (2 1 5) (3 9 1) (3 7 1) (4 7 1) (4 6 1) (5 3 2) (6 3))))))

  (define puzzle-s5-146
    (clues->puzzle
     (board-clues
      (array-map single-line-clues #((2 2) (1 1 1 11) (1 3 12) (1 3 3 7) (5 2 1 3 2) (2 1 3 1) (2 1 1 3 1 2) (5 5 2 1) (1 4 2 1 1 1) (5 1 1 1 2) (1 2 2 5) (1 5 4) (6 1) (1 1 5) (3 1 2)))
      (array-map single-line-clues #((1 1 1 2) (1 2 1 1 1) (1 1 2 1 1 1) (6 1 1) (3 3) (1 2 4) (1 3 1) (2 2 2 1) (4 1 1) (3 2 3) (5 2 2) (2 4 2) (2 4 4) (4 2 3 1) (4 1 1) (3 3 2 1) (3 2 2 1) (3 1 3 1) (3 1 4 2) (3 1 2 2))))))

  (define puzzle-s5-149
    (clues->puzzle
     (board-clues
      (array-map single-line-clues #((1 3) (5) (2 2) (1 2 2 1) (2 2 3 2) (2 3 4 1) (6 1 1 2 2 1) (3 1 5 3) (1 6 4 2) (1 2 1 2) (1 1 3) (3 10) (2 1 2 2 1) (3 1 3) (1 2 2)))
      (array-map single-line-clues #((3 2) (2 2) (2 1 1) (2 2) (3 2) (2 1 3 1) (2 1 1) (2 1 3) (2 3 2) (9) (1 2 1) (1 2 3) (6 1 4) (1 4 1 2 1) (2 3 1 4) (4 1 1 2) (3 1 1) (3 1) (1 3) (1 3))))))

  (define puzzle-s5-150
    (clues->puzzle
     (board-clues
      (array-map single-line-clues #((2 3 2 1) (3 1 2 2 1) (1 1 1 4 1 2) (1 3 1 2 1 1 2 1) (2 2 1 1 1 1 1) (3 1 1 2 2 1) (2 3 4 1 1) (4 6 1) (1 3 4 1) (3 1 3 2) (2 2 1 1) (3 1 2) (2 3 1) (5 1 1) (6 2)))
      (array-map single-line-clues #((1 1) (3 1) (2 2) (1 1) (4 1) (1 6 2) (1 1 1 1 2 2) (1 1 1 5 2) (1 4 1) (2 2 1 1 2 1) (1 1 1 4 1) (1 1 2 2) (3 5 1) (3 7 1) (1 2 4 2) (1 2) (1 2 3 5) (1 2 2 1 1) (1 3 2) (2))))))

  (define puzzle-s5-m001
    (clues->puzzle
     (board-clues
      (array (line-clues 'single '(2 2))
             (line-clues 'single '(1 1))
             (line-clues 'mega '(#[() (1)] 3))
             (line-clues 'single '(2 2)))
      (array-map single-line-clues #((2 2) (1 1) () (1 3) (2 2))))))

  (define puzzle-s5-m016
    (clues->puzzle
     (board-clues
      (array (line-clues 'single '(1))
             (line-clues 'mega '(3 4))
             (line-clues 'single '(7))
             (line-clues 'single '(1))
             (line-clues 'mega '(4 5 #[(1 1) ()]))
             (line-clues 'single '(1 7))
             (line-clues 'single '(2 4))
             (line-clues 'single '(3)))
      (array (line-clues 'single '(2 1))
             (line-clues 'single '(1 1 1))
             (line-clues 'single '(1 2))
             (line-clues 'single '(2 3))
             (line-clues 'single '(3 2))
             (line-clues 'mega '(#[(1) ()] 3 6))
             (line-clues 'single '(4 1 2))
             (line-clues 'mega '(3 #[() (1)] 4))))))

  (define puzzle-s5-m016/solved
    (puzzle
     (board #(#(cross cross cross cross cross cross cross full  cross cross)
              #(cross cross cross cross full  full  cross full  cross cross)
              #(cross cross cross cross full  cross full  full  full  cross)
              #(cross cross cross full  full  full  full  full  full  full )
              #(cross cross cross full  cross cross cross cross cross cross)
              #(full  full  full  cross cross full  cross full  cross full )
              #(full  cross cross full  full  full  full  cross cross cross)
              #(cross full  cross full  full  full  full  full  full  full )
              #(cross cross full  full  cross cross full  full  full  full )
              #(full  full  full  cross cross cross cross cross cross cross)))
     (puzzle-clues puzzle-s5-m016)))

  (define puzzle-s5-m076
    (clues->puzzle
     (board-clues
      (array (line-clues 'single '(4))
             (line-clues 'single '(2 2))
             (line-clues 'mega '(5 3))
             (line-clues 'mega '(7 #[() (1)] 5))
             (line-clues 'single '(8 5))
             (line-clues 'mega '(#[(1) (1)] 6 #[(1) (3)]))
             (line-clues 'single '(4 2 2))
             (line-clues 'single '(3 5 1))
             (line-clues 'single '(1 3 2))
             (line-clues 'single '(2 2))
             (line-clues 'single '(3 2))
             (line-clues 'single '(6)))
      (array (line-clues 'single '(5))
             (line-clues 'single '(4 1 2))
             (line-clues 'single '(8 2))
             (line-clues 'single '(1 7))
             (line-clues 'single '(1 7))
             (line-clues 'mega '(4 #[() (1)] 4 4))
             (line-clues 'single '(5 2 2))
             (line-clues 'mega '(#[(2) (1)] 7 2))
             (line-clues 'single '(1 4 1))
             (line-clues 'single '(1 2 2))
             (line-clues 'mega '(2 4))
             (line-clues 'single '(1 2))))))

  (define puzzle-s5-m106
    (clues->puzzle
     (board-clues
      (array (line-clues 'single '(1 3))
             (line-clues 'single '(2 1))
             (line-clues 'mega '(#[(4) (2)] 4))
             (line-clues 'single '(2 1 2))
             (line-clues 'single '(1 2 2 1))
             (line-clues 'single '(1 9))
             (line-clues 'single '(1 5 4))
             (line-clues 'single '(1 9 1))
             (line-clues 'mega '(#[() (1)] 6 #[(5) (2)] 3))
             (line-clues 'single '(1 2 6 1))
             (line-clues 'single '(1 1 3 2))
             (line-clues 'single '(3 2 2))
             (line-clues 'single '(2 3)))
      (array (line-clues 'single '(2))
             (line-clues 'single '(2 1 1))
             (line-clues 'single '(2 2 1 1))
             (line-clues 'single '(1 3 1))
             (line-clues 'single '(1 2 1 3))
             (line-clues 'single '(1 3 1 1))
             (line-clues 'mega '(2 9 2))
             (line-clues 'single '(1 5 3))
             (line-clues 'mega '(#[(1) (1)] 10 5))
             (line-clues 'single '(3 4 2))
             (line-clues 'single '(2 1 3 1 1))
             (line-clues 'single '(1 1 2 1 4))
             (line-clues 'single '(2 2 3 3))))))

  (define all-puzzles
    (list (cons "Test 1" puzzle-1)
          (cons "Test 2" puzzle-2)

          (cons "S5 001" puzzle-s5-001)
          (cons "S5 016" puzzle-s5-016)
          (cons "S5 031" puzzle-s5-031)
          (cons "S5 061" puzzle-s5-061)
          (cons "S5 135" puzzle-s5-135)
          (cons "S5 137" puzzle-s5-137)
          (cons "S5 146" puzzle-s5-146)
          (cons "S5 149" puzzle-s5-149)
          (cons "S5 150" puzzle-s5-150)

          (cons "S5 M001" puzzle-s5-m001)
          (cons "S5 M016" puzzle-s5-m016)
          (cons "S5 M016 (solved)" puzzle-s5-m016/solved)
          (cons "S5 M076" puzzle-s5-m076)
          (cons "S5 M106" puzzle-s5-m106)))

  (define all-puzzle-names (map car all-puzzles))
  (define all-puzzles-hash (make-immutable-hash all-puzzles))
  (define (get-puzzle name)
    (hash-ref all-puzzles-hash name #f)))

;; -----------------------------------------------------------------------------

;; The puzzle string can be obtained from `window.task` on a puzzle page.
(define (parse-puzzle-nonograms.com-clues str width)
  (define lines
    (for/list ([line (in-list (string-split str "/"))])
      (for/list ([clue (in-list (string-split line "."))])
        (string->number clue))))
  (define-values [columns rows] (split-at lines width))
  (board-clues
   (list->array rows)
   (list->array columns)))

(module+ test
  (check-equal? (parse-puzzle-nonograms.com-clues "2/3/3/3/1.1/1.1/4/3/1.1.1/1" 5)
                (board-clues
                 #((1 1) (4) (3) (1 1 1) (1))
                 #((2) (3) (3) (3) (1 1)))))

;; The puzzle string can be obtained from `JSON.stringify(window.d)` on a
;; puzzle page. It seems to be ciphered somehow. This function replicates the
;; logic used by the JS on the page to decode the solution.
(define (parse-nonograms.org-solution str)
  (define d (~> (string->jsexpr str)
                  (map list->array _)
                  list->array))
  (define (D i)
    (define d* (array-ref d i))
    (λ (j) (array-ref d* j)))

  (define (decipher/1 i)
    (define A (D i))
    (define n (A 3))
    (- (+ (modulo (A 0) n)
          (modulo (A 1) n))
       (modulo (A 2) n)))

  (define (decipher/2 i)
    (define A (D i))
    (define n (A 3))
    (+ (expt (modulo (A 0) n) 2)
       (* (modulo (A 1) n) 2)
       (* (modulo (A 2) n))))

  (define width (decipher/1 1))
  (define height (decipher/1 2))
  (define board-start (+ (decipher/1 3) 6))
  (define board-length (decipher/2 (sub1 board-start)))
  (define B (D board-start))

  (define rows (for/array ([i (in-range height)])
                 (make-vector width 0)))
  (for ([i (in-inclusive-range board-start (+ board-start board-length))])
    (define R (D i))
    (define row-i (- (R 3) (B 3) 1))
    (when (>= row-i 0)
      (define row (array-ref rows row-i))
      (for ([j (in-range (- (R 0) (B 0) 1)
                         (- (+ (R 0) (R 1)) (B 0) (B 1) 1))])
        (vector-set! row j (- (R 2) (B 2))))))

  (board (for/array #:length height ([row (in-array rows)])
           (for/array #:length width ([tile (in-vector row)])
             (match tile [0 'cross] [1 'full])))))

(module+ test
  (check-equal? (parse-nonograms.org-solution "[[1209,984,703,991],[20,38,54,61],[24,24,44,46],[47,11,57,58],[25,12,59,16],[12,25,16,60],[29,39,28,3],[23,26,21,25],[26,28,22,27],[26,27,22,29],[24,27,22,29],[24,29,22,28],[26,27,22,26],[15,18,42,4],[51,28,1,15],[52,29,10,19],[55,29,6,16],[52,29,11,18],[53,29,3,19],[53,30,2,18],[52,29,6,16],[53,29,3,17],[54,29,5,19],[53,29,3,16],[52,29,8,17],[55,29,10,19],[55,29,13,18],[54,29,5,16],[55,29,8,17],[54,29,5,17]]")
                (board #(#(cross cross full cross)
                         #(cross cross full full)
                         #(full  full  full cross)
                         #(full  cross full cross)))))
