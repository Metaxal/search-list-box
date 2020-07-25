#lang scribble/manual
@require[@for-label[search-list-box
                    racket/gui]]

@title{Search-list-box}
@author{Laurent Orseau}

@defmodule[search-list-box]

A @racket[list-box%] with a search @racket[text-field%].

A @racket[search-list-box%] derives from a @racket[vertical-panel%], and thus accepts all the same
arguments.

Moreover, it has the following additional contracts:
@defform[(search-list-box%)
         #:contracts
         ([search-list-box%
           (class/c
            (init       [contents          list?])
            (init-field [parent            (or/c (is-a?/c frame%) (is-a?/c dialog%)
                                                 (is-a?/c panel%) (is-a?/c pane%))]
                        [label             (or/c label-string? #f)]
                        [text-field-mixin  (-> (is-a?/c text-field%)
                                               (is-a?/c text-field%))]
                        [list-box-mixin    (-> (is-a?/c list-box%)
                                               (is-a?/c list-box%))]
                        [filter            (-> string? label-string? any/c)]
                        [key               (-> any/c string?)]
                        [callback          (-> (or/c number? #f)
                                               (or/c label-string? #f)
                                               any/c any)])
            [focus (-> any)]
            [get-list-box (->m (is-a?/c list-box%))]
            [get-text-field (->m (is-a?/c text-field%))]
            [set-contents (->m list? any)]
            [set-text (->m label-string? any)])])]

The @racketid[key] argument builds a label string from a element of @racketid[contents].
By default is just displays the elements of @racketid[contents] as a string.

The @racketid[callback] takes the currently selected index in the list-box (or the first one if
none is selected), the corresponding displayed label-string and the corresponding content element in
@racketid[contents].

The @racketid[filter] argument allows the user to replace the default filter.


