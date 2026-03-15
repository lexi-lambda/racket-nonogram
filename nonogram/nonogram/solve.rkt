#lang racket/base

(require racket/contract
         racket/list
         racket/match
         racket/random
         racket/set
         threading
         toolbox/who
         "core.rkt"
         "geometry.rkt"
         "lib/array.rkt"
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
         board-analysis-solved?

         (struct-out solver-result)
         solver-result/c

         (contract-out
          [analyze-puzzle (-> puzzle? board-analysis?)]
          [reanalyze-lines-at (-> puzzle? board-analysis? integer-point? board-analysis?)]

          [solve-puzzle-axis (-> puzzle? axis? (solver-result/c puzzle?))]
          [solve-puzzle (-> puzzle? (solver-result/c puzzle?))]

          [demegaify-puzzle (-> puzzle? puzzle?)]
          [megaify-puzzle (-> puzzle? puzzle?)]))

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

;; solve-puzzle-axis : puzzle? axis? -> (solver-result/c puzzle?)
(define (solve-puzzle-axis pz axis)
  (let/ec bail
    (define lcs+ls (puzzle-axis-clues+lines pz axis))
    (define-values [new-lines solved?]
      (for/fold ([lines '()]
                 [solved? #t])
                ([lc+l (in-array lcs+ls)])
        (match lc+l
          [(cons (line-clues 'single clue-line) tile-line)
           (match (solve-line clue-line tile-line)
             ['error (bail 'error)]
             [(solver-result tile-line line-solved?)
              (values (cons tile-line lines)
                      (and solved? line-solved?))])]
          [(cons (line-clues 'mega clue-line) mega-tile-line)
           (match (solve-line/mega clue-line mega-tile-line)
             ['error (bail 'error)]
             [(solver-result (array tile-line-0 tile-line-1) line-solved?)
              (values (list* tile-line-1 tile-line-0 lines)
                      (and solved? line-solved?))])])))
    (solver-result
     (struct-copy puzzle pz
       [board (lines->board (list->array (reverse new-lines)) axis)])
     solved?)))

;; solve-puzzle : puzzle? -> (solver-result/c puzzle?)
(define (solve-puzzle pz)
  (let loop ([old-pz pz])
    (match (solve-puzzle-axis old-pz 'row)
      ['error 'error]
      [(and result (solver-result new-pz solved?))
       (if solved?
           result
           (match (solve-puzzle-axis new-pz 'column)
             ['error 'error]
             [(and result (solver-result new-pz solved?))
              (if (or solved? (equal? old-pz new-pz))
                  result
                  (loop new-pz))]))])))

(module+ test
  (define-values [solved unsolved contradictions exns]
    (for/fold ([solved 0] [unsolved 0] [contradictions '()] [exns '()]
               #:result (values solved unsolved (reverse contradictions) (reverse exns)))
              ([puzzle-entry (in-list all-puzzles)])
      (match-define (cons name pz) puzzle-entry)
      (match (with-handlers ([exn:fail? values])
               (solve-puzzle pz))
        [(solver-result _ solved?)
         (test-log! #t)
         (cond
           [solved?
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

;; -----------------------------------------------------------------------------

;; demegaify-puzzle : puzzle? -> puzzle?
(define/who (demegaify-puzzle pz)
  (match (solve-puzzle pz)
    ['error
     (raise-arguments-error who "solving puzzle resulted in contradiction"
                            "puzzle" pz)]
    [(solver-result solved-pz solved?)
     (unless solved?
       (raise-arguments-error who "could not solve puzzle"
                              "puzzle" solved-pz))
     (struct-copy puzzle pz
       [clues (solved-board->clues (puzzle-board solved-pz))])]))

;; megaify-puzzle : puzzle? -> puzzle?
(define/who (megaify-puzzle pz)
  (define (fail-contradiction pz)
    (raise-arguments-error who "solving puzzle resulted in contradiction"
                           "puzzle" pz))

  (match (solve-puzzle pz)
    ['error (fail-contradiction pz)]
    [(solver-result solved-pz solved?)
     (unless solved?
       (raise-arguments-error who "could not solve puzzle"
                              "puzzle" solved-pz))

     (define solved-board (puzzle-board solved-pz))
     (define (build-candidate-lines mega-rows mega-columns)
       (define (overlapping-index? mega-lines i)
         (or (set-member? mega-lines i)
             (set-member? mega-lines (sub1 i))
             (set-member? mega-lines (add1 i))))
       (set-union
        (for/set ([i (in-range (sub1 (board-height solved-board)))]
                  #:unless (overlapping-index? mega-rows i))
          (cons 'row i))
        (for/set ([i (in-range (sub1 (board-width solved-board)))]
                  #:unless (overlapping-index? mega-columns i))
          (cons 'column i))))

     (let loop ([candidate-lines (build-candidate-lines (seteqv) (seteqv))]
                [mega-rows (seteqv)]
                [mega-columns (seteqv)]
                [clues (puzzle-clues pz)]
                [any-replaced? #f])
       (cond
         [(set-empty? candidate-lines)
          (if any-replaced?
              ;; If we did any clue replacements, do another pass before bailing
              ;; out in case adding mega lines somehow made the puzzle easier.
              (loop (build-candidate-lines mega-rows mega-columns)
                    mega-rows
                    mega-columns
                    clues
                    #f)
              (struct-copy puzzle pz [clues clues]))]
         [else
          (define candidate-line (random-ref candidate-lines))
          (match-define (cons candidate-axis candidate-i) candidate-line)

          (define candidate-lines* (set-remove candidate-lines candidate-line))
          (define-values [mega-rows* mega-columns*]
            (match candidate-axis
              ['row    (values (set-add mega-rows candidate-i) mega-columns)]
              ['column (values mega-rows (set-add mega-columns candidate-i))]))

          (match (solved-board->puzzle solved-board
                                       #:mega-rows mega-rows*
                                       #:mega-columns mega-columns*
                                       #:fail #f)
            [#f
             (loop candidate-lines* mega-rows mega-columns clues any-replaced?)]
            [pz*
             (match (solve-puzzle pz*)
               ['error (fail-contradiction pz*)]
               [(solver-result _ solved?)
                (if solved?
                    (loop (~> candidate-lines*
                              (set-remove (cons candidate-axis (sub1 candidate-i)))
                              (set-remove (cons candidate-axis (add1 candidate-i))))
                          mega-rows*
                          mega-columns*
                          (puzzle-clues pz*)
                          #t)
                    (loop candidate-lines* mega-rows mega-columns clues any-replaced?))])])]))]))
