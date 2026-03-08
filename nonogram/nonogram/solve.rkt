#lang racket/base

(require racket/contract
         racket/match
         "array.rkt"
         "core.rkt"
         "geometry.rkt"
         "solve/core.rkt"
         "solve/mega-line.rkt"
         "solve/single-line.rkt")

(module+ test
  (require rackunit
           (submod "core.rkt" example)))

(provide clue-analysis?
         single-line-analysis?
         mega-line-analysis?
         line-clue-analysis?
         axis-clue-analysis?
         (struct-out board-analysis)

         (contract-out
          [analyze-puzzle (-> puzzle? board-analysis?)]
          [reanalyze-lines-at (-> puzzle? board-analysis? integer-point? board-analysis?)]))

;; -----------------------------------------------------------------------------

;; analyze-puzzle : puzzle? -> board-analysis?
(define (analyze-puzzle pp)
  (define (do-axis axis)
    (define tss+css (puzzle-axis-tiles+clues pp axis))
    (for/array #:length (array-length tss+css)
               ([tiles+clues (in-array tss+css)])
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
                 #((pending) (done pending) (pending))
                 #(done (pending) error))))

;; reanalyze-line-at : puzzle? axis-clue-analysis? axis? natural? -> axis-clue-analysis?
(define (reanalyze-line-at pz old-axis-analysis axis i)
  (define board (puzzle-board pz))
  (define axis-clues (board-clues-axis (puzzle-clues pz) axis))
  (match (line-index->axis-clue-index axis-clues i)
    [(array clue-i line-i)
     (define i* (- i line-i))
     (define new-analysis
       (analyze-line/mega (line-clues-clues (array-ref axis-clues clue-i))
                          (array (board-line board axis i*)
                                 (board-line board axis (add1 i*)))))
     (array-set old-axis-analysis clue-i new-analysis)]
    [clue-i
     (define new-analysis
       (analyze-line (line-clues-clues (array-ref axis-clues clue-i))
                     (board-line board axis i)))
     (array-set old-axis-analysis clue-i new-analysis)]))

;; reanalyze-lines-at : puzzle? board-analysis? integer-point? -> board-analysis?
(define (reanalyze-lines-at new-pz old-board-analysis location)
  (match-define (point x y) location)
  (define old-row-analyses (board-analysis-row-analysis old-board-analysis))
  (define old-column-analyses (board-analysis-column-analysis old-board-analysis))
  (board-analysis
   (reanalyze-line-at new-pz old-row-analyses 'row y)
   (reanalyze-line-at new-pz old-column-analyses 'column x)))
