#lang racket
(require search-list-box)

(new search-list-box-frame%
     [label "Searching..."]
     [contents '(1 2 3 a1 a2 aa2 bb2 bb3)]
     [callback (λ (idx label content)
                 (if idx
                   (writeln content)
                   (displayln "No content selected")))])
