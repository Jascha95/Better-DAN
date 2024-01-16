#lang racket
(require redex)
(require redex/tut-subst)

(define-metafunction let
  subst : x V E -> E
  [(subst x V E)
   ,(subst/proc x? (list (term x)) (list (term V)) (term E))])

(define x? (redex-match let x))