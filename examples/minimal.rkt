#lang racket/gui

(require search-list-box)

(define fr (new frame% [label ""]))
(define slb (new search-list-box%
                 [parent fr]
                 [contents '(1 2 3 a1 a2 aa2 bb2 bb3)]
                 [min-height 200]
                 [callback (λ (idx label content)
                             (writeln content))]))

(send fr show #t)
(send slb focus)
