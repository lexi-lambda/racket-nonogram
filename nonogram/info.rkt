#lang info

(define version "1.0")
(define license 'ISC)

(define collection 'multi)

(define deps
  '("base"
    "data-lib"
    "draw-lib"
    "gui-lib"
    "pict-lib"
    "rfc6455"
    "threading-lib"
    "toolbox-lib"
    "toolbox-draw-lib"))
(define build-deps
  '("rackunit-lib"))
