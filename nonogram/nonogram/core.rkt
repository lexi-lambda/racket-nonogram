#lang racket/base

(require racket/contract
         racket/list
         racket/match
         racket/math
         racket/string
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
          [tile-line? flat-contract?]

          (struct board ([rows (arrayof tile-line?)]))
          [make-board (-> natural? natural? board?)]
          [board-width (-> board? natural?)]
          [board-height (-> board? natural?)]
          [board-ref (-> board? natural? natural? tile?)]
          [board-set (-> board? natural? natural? tile? board?)]
          [board-row (-> board? natural? (arrayof tile?))]
          [board-column (-> board? natural? tile-line?)]
          [board-line (-> board? axis? natural? tile-line?)]

          [clue? flat-contract?]
          [line-clues? flat-contract?]
          [axis-clues? flat-contract?]
          (struct board-clues ([row-clues axis-clues?]
                               [column-clues axis-clues?]))
          [board-clues-line (-> board-clues? axis? natural? line-clues?)]

          (struct puzzle ([board board?]
                          [clues board-clues?]))
          [clues->puzzle (-> board-clues? puzzle?)]

          [parse-puzzle-nonograms.com-clues (-> string? natural? board-clues?)]))

;; -----------------------------------------------------------------------------

(define axis? (or/c 'row 'column))
(define tile? (or/c 'empty 'full 'cross 'mark))

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

;; -----------------------------------------------------------------------------
;; clues

(define clue? exact-positive-integer?)
(define line-clues? (listof clue?))
(define axis-clues? (arrayof line-clues?))
(struct board-clues
  (row-clues     ;; axis-clues?
   column-clues) ;; axis-clues?
  #:transparent)

(module+ example
  (define clues-1
    (board-clues
     #((1) (1 1) (2))
     #((1) (1) (3))))

  (define clues-2
    (board-clues
     #((1) (3))
     #(() (1) (2) (1)))))

;; board-clues-line : clues? axis? natural? -> line-clues?
(define (board-clues-line clues axis i)
  (match axis
    ['row    (array-ref (board-clues-row-clues clues) i)]
    ['column (array-ref (board-clues-column-clues clues) i)]))

(module+ test
  (check-equal? (board-clues-line clues-1 'row 0) '(1))
  (check-equal? (board-clues-line clues-1 'row 1) '(1 1))
  (check-equal? (board-clues-line clues-1 'row 2) '(2))
  (check-equal? (board-clues-line clues-1 'column 0) '(1))
  (check-equal? (board-clues-line clues-1 'column 1) '(1))
  (check-equal? (board-clues-line clues-1 'column 2) '(3)))

;; -----------------------------------------------------------------------------
;; puzzle

(struct puzzle
  (board  ;; board?
   clues) ;; board-clues?
  #:transparent)

;; clues->puzzle : board-clues? -> puzzle?
(define (clues->puzzle clues)
  (puzzle (make-board (array-length (board-clues-column-clues clues))
                      (array-length (board-clues-row-clues clues)))
          clues))

(module+ example
  (define puzzle-1 (puzzle board-1 clues-1))
  (define puzzle-2 (puzzle board-2 clues-2))

  (define puzzle-s5-001
    (clues->puzzle
     (board-clues
      #((1 1) (1 1 1) (5) (1) (3))
      #((3) (1 1) (5) (1 1) (2)))))

  (define puzzle-s5-016
    (clues->puzzle
     (board-clues
      #((1 2) (1 2) (2) (3) (2 2 1) (2 2 3) (1 2 2 1) (2 6) (1 2 1) (2 1))
      #((4) (1 1) (1 1) (4) (1 1 1) (1 1 1 1) (1 1 3) (1 1 5) (6 1) (3 2 1)))))

  (define puzzle-s5-031
    (clues->puzzle
     (board-clues
      #((2) (1 2) (2 1 3) (2 4) (1 7) (6) (1 4 1) (3 2 2) (5 3) (7))
      #((2 3) (2 2) (2 2 3) (1 4 2) (1 5 2) (1 5 1) (1 5 1) (1 2 2) (1 1 3) (1 3)))))

  (define puzzle-s5-061
    (clues->puzzle
     (board-clues
      #((6 4) (5 4) (4 4) (7) (6) (1 1 1) (1 1 1 1) (1 1 1 1) (1 3) (1 1 1) (1 1) (1 1 1) (1 1) (3) (1 3))
      #((1) (1) (2) (3) (4 2) (6 5 1) (4 2 1) (3 2 1 2) (3 3 2) (5 5 1) (4 2) (3) (2) (1) (1)))))

  (define puzzle-s5-135
    (clues->puzzle
     (board-clues
      #((3 4) (1 3 2) (2 3 1 1 1) (4 3 3) (1 1 1 1 1) (1 3 1) (1 3 1 1 1) (1 1 1 1) (4 1 1 1) (6 3) (3 7) (12) (9 1) (1 2 2) (8 3))
      #((3 4) (1 2 3 1) (1 1 1 3 1) (3 2 2 1) (3 4 2 1) (2 2 5 1) (1 1 4 1) (3 7 1) (1 2 4 1) (1 2 2 1) (1 2 3 1) (1 3 6) (1 1 1 1 1) (2 2 1 2) (2 2 2)))))

  (define puzzle-s5-137
    (clues->puzzle
     (board-clues
      #((8 9) (3 8) (1 3 6) (6 1 4) (5 1 2) (5 3 1) (5 4) (5 5) (5 6) (6 6) (6 6) (5 7) (1 3 6 1) (4 2) (8 6))
      #((1 9 3) (1 8 2) (1 9 2) (1 10 2) (12 1) (4 4 1) (2 2 1) (1 1 1) () (1) (1) (1 2) (2 3) (2 1 5) (3 9 1) (3 7 1) (4 7 1) (4 6 1) (5 3 2) (6 3)))))

  (define puzzle-s5-146
    (clues->puzzle
     (board-clues
      #((2 2) (1 1 1 11) (1 3 12) (1 3 3 7) (5 2 1 3 2) (2 1 3 1) (2 1 1 3 1 2) (5 5 2 1) (1 4 2 1 1 1) (5 1 1 1 2) (1 2 2 5) (1 5 4) (6 1) (1 1 5) (3 1 2))
      #((1 1 1 2) (1 2 1 1 1) (1 1 2 1 1 1) (6 1 1) (3 3) (1 2 4) (1 3 1) (2 2 2 1) (4 1 1) (3 2 3) (5 2 2) (2 4 2) (2 4 4) (4 2 3 1) (4 1 1) (3 3 2 1) (3 2 2 1) (3 1 3 1) (3 1 4 2) (3 1 2 2)))))

  (define puzzle-s5-149
    (clues->puzzle
     (board-clues
      #((1 3) (5) (2 2) (1 2 2 1) (2 2 3 2) (2 3 4 1) (6 1 1 2 2 1) (3 1 5 3) (1 6 4 2) (1 2 1 2) (1 1 3) (3 10) (2 1 2 2 1) (3 1 3) (1 2 2))
      #((3 2) (2 2) (2 1 1) (2 2) (3 2) (2 1 3 1) (2 1 1) (2 1 3) (2 3 2) (9) (1 2 1) (1 2 3) (6 1 4) (1 4 1 2 1) (2 3 1 4) (4 1 1 2) (3 1 1) (3 1) (1 3) (1 3)))))

  (define puzzle-s5-150
    (clues->puzzle
     (board-clues
      #((2 3 2 1) (3 1 2 2 1) (1 1 1 4 1 2) (1 3 1 2 1 1 2 1) (2 2 1 1 1 1 1) (3 1 1 2 2 1) (2 3 4 1 1) (4 6 1) (1 3 4 1) (3 1 3 2) (2 2 1 1) (3 1 2) (2 3 1) (5 1 1) (6 2))
      #((1 1) (3 1) (2 2) (1 1) (4 1) (1 6 2) (1 1 1 1 2 2) (1 1 1 5 2) (1 4 1) (2 2 1 1 2 1) (1 1 1 4 1) (1 1 2 2) (3 5 1) (3 7 1) (1 2 4 2) (1 2) (1 2 3 5) (1 2 2 1 1) (1 3 2) (2)))))

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
          (cons "S5 150" puzzle-s5-150)))

  (define all-puzzle-names (map car all-puzzles))
  (define all-puzzles-hash (make-immutable-hash all-puzzles))
  (define (get-puzzle name)
    (hash-ref all-puzzles-hash name #f)))

;; -----------------------------------------------------------------------------

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
