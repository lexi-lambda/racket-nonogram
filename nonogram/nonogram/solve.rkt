#lang racket/base

(require racket/contract
         racket/list
         racket/match
         threading
         "array.rkt"
         "core.rkt"
         "geometry.rkt"
         "solve/core.rkt"
         "solve/mega-line.rkt"
         "solve/single-line.rkt")

(module+ test
  (require racket/exn
           racket/string
           rackunit
           raco/testing
           toolbox/print
           (submod "core.rkt" example))

  (port-count-lines! (current-output-port))
  (port-count-lines! (current-error-port)))

(provide clue-analysis?
         single-line-analysis?
         mega-line-analysis?
         line-clue-analysis?
         axis-clue-analysis?
         (struct-out board-analysis)

         (contract-out
          [analyze-puzzle (-> puzzle? board-analysis?)]
          [reanalyze-lines-at (-> puzzle? board-analysis? integer-point? board-analysis?)]

          [solve-puzzle-axis (-> puzzle? axis? (or/c puzzle? 'error))]
          [solve-puzzle (-> puzzle? (or/c puzzle? 'error))]))

;; -----------------------------------------------------------------------------

;; analyze-puzzle : puzzle? -> board-analysis?
(define (analyze-puzzle pp)
  (define (do-axis axis)
    (define lcs+ls (puzzle-axis-clues+lines pp axis))
    (for/array #:length (array-length lcs+ls)
               ([lc+l (in-array lcs+ls)])
      (match lc+l
        [(cons (line-clues 'single clue-line) tile-line)
         (analyze-line clue-line tile-line)]
        [(cons (line-clues 'mega clue-line) mega-tile-line)
         (analyze-line/mega clue-line mega-tile-line)])))

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

;; -----------------------------------------------------------------------------

;; solve-puzzle-axis : puzzle? axis? -> (or/c puzzle? 'error)
(define (solve-puzzle-axis pz axis)
  (let/ec bail
    (define (bail-on-error v)
      (match v
        ['error (bail 'error)]
        [_ v]))

    (define lcs+ls (puzzle-axis-clues+lines pz axis))
    (define new-board
      (~> (for/list ([lc+l (in-array lcs+ls)])
            (match lc+l
              [(cons (line-clues 'single clue-line) tile-line)
               (list (bail-on-error (solve-line clue-line tile-line)))]
              [(cons (line-clues 'mega clue-line) mega-tile-line)
               (array->list (bail-on-error (solve-line/mega clue-line mega-tile-line)))]))
          append*
          list->array
          (lines->board axis)))
    (struct-copy puzzle pz [board new-board])))

;; solve-puzzle : puzzle? -> (or/c puzzle? 'error)
(define (solve-puzzle pz)
  (let loop ([old-pz pz])
    (match (solve-puzzle-axis old-pz 'row)
      ['error 'error]
      [new-pz
       (match (solve-puzzle-axis new-pz 'column)
         ['error 'error]
         [new-pz
          (if (equal? old-pz new-pz)
              old-pz
              (loop new-pz))])])))

(module+ test
  (define-values [solved unsolved contradictions exns]
    (for/fold ([solved 0] [unsolved 0] [contradictions '()] [exns '()]
               #:result (values solved unsolved (reverse contradictions) (reverse exns)))
              ([puzzle-entry (in-list all-puzzles)])
      (match-define (cons name pz) puzzle-entry)
      (match (with-handlers ([exn:fail? values])
               (match (solve-puzzle pz)
                 ['error 'error]
                 [solved-pz (analyze-puzzle solved-pz)]))
        [(? board-analysis? analysis)
         (test-log! #t)
         (cond
           [(board-analysis-solved? analysis)
            (values (add1 solved) unsolved contradictions exns)]
           [else
            (printf "~v => 'pending\n" name)
            (values solved (add1 unsolved) contradictions exns)])]
        ['error
         (test-log! #f)
         (printf "~v => 'error\n" name)
         (values solved unsolved (cons name contradictions) exns)]
        [(? exn? exn)
         (test-log! #f)
         (printf "~v =>\n  ~a\n" name
                 (multiline-printing-string (string-trim (exn-message exn))))
         (values solved unsolved contradictions (cons (cons name exn) exns))])))
  (newline)
  (unless (empty? contradictions)
    (eprintf "contradictions: ~v\n" contradictions))
  (unless (empty? exns)
    (eprintf "exceptions raised while solving:\n")
    (for ([name+exn (in-list exns)])
      (match-define (cons name exn) name+exn)
      (eprintf "  ~v =>\n    ~a\n" name
               (multiline-printing-string (string-trim (exn->string exn))))))
  (printf "summary: ~a solved, ~a unsolved, ~a contradictions, and ~a exceptions\n"
          solved
          unsolved
          (length contradictions)
          (length exns)))
