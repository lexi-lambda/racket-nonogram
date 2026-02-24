#lang racket/base

(require json
         racket/contract
         racket/list
         racket/match
         racket/math
         racket/string
         threading
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
          [board-row (-> board? natural? tile-line?)]
          [board-column (-> board? natural? tile-line?)]
          [board-line (-> board? axis? natural? tile-line?)]
          [board-clear (-> board? board?)]

          [clue? flat-contract?]
          [line-clues? flat-contract?]
          [axis-clues? flat-contract?]
          (struct board-clues ([row-clues axis-clues?]
                               [column-clues axis-clues?]))
          [board-clues-line (-> board-clues? axis? natural? line-clues?)]
          [solved-line->clues (-> tile-line? line-clues?)]
          [solved-board->clues (-> board? board-clues?)]

          (struct puzzle ([board board?]
                          [clues board-clues?]))
          [clues->puzzle (-> board-clues? puzzle?)]
          [solved-board->puzzle (-> board? puzzle?)]

          [parse-puzzle-nonograms.com-clues (-> string? natural? board-clues?)]
          [parse-nonograms.org-solution (-> string? board?)]))

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

;; board-clear : board? -> board?
(define (board-clear b)
  (make-board (board-width b) (board-height b)))

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

;; solved-line->clues : tile-line? -> line-clues?
(define (solved-line->clues tiles)
  (define num-tiles (array-length tiles))
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
        '())))

(module+ test
  (check-equal? (solved-line->clues
                 #(full cross full full cross cross full full full cross full))
                '(1 2 3 1)))

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
                (board-clues #((1 1) (2))
                             #((1) (1) (1) (1)))))

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

;; solved-board->puzzle : board? -> puzzle?
(define (solved-board->puzzle b)
  (puzzle (board-clear b) (solved-board->clues b)))

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
