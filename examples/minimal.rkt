#lang racket
(require search-list-box)

(define slb
  (new search-list-box-frame%
       [label "Searching..."]
       [contents '(1 2 3 a1 a2 aa2 bb2 bb3)]
       [callback (λ (idx label content)
                   (send slb set-status 
                         (if idx
                           (format "Selected: ~a" content)
                           "No content selected")))]))
