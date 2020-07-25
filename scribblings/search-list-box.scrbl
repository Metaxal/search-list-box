#lang scribble/manual
@require[@for-label[search-list-box
                    racket/gui]
         racket/runtime-path
         pict]

@(define-runtime-path search-list-box-png "../img/search-list-box.png")

@title{Search-list-box}
@author{Laurent Orseau}

@defmodule[search-list-box]

A @racket[list-box%] with a search @racket[text-field%]. See some examples
@hyperlink["https://github.com/Metaxal/search-list-box/tree/master/examples"]{here}.

@(bitmap search-list-box-png)

Features:
@itemlist[
 @item{The search function can be customized.}
 @item{Can navigate between the text-field and the list-box with arrow keys.}
 @item{In the text field, press enter to validate; in the list-box press space.}
 @item{Pressing Escape in the list-box returns to the text-field.}
 @item{The filter function can be customized.}]



A @racket[search-list-box%] derives from a @racket[vertical-panel%], and thus accepts all the same
arguments.

Moreover, it has the following additional contracts:
@defform[(search-list-box%)
         #:contracts
         ([search-list-box%
           (class/c
            (init       [contents          list?])
            (init-field [parent            (or/c (is-a?/c frame%)
                                                 (is-a?/c dialog%)
                                                 (is-a?/c panel%)
                                                 (is-a?/c pane%))]
                        [label             (or/c label-string? #f)]
                        [text-field-mixin  (-> (subclass?/c text-field%)
                                               (subclass?/c text-field%))]
                        [list-box-mixin    (-> (subclass?/c list-box%)
                                               (subclass?/c list-box%))]
                        [filter            (-> string? label-string? any/c)]
                        [key               (-> any/c string?)]
                        [callback          (-> (or/c number? #f)
                                               (or/c label-string? #f)
                                               any/c
                                               any)])
            [focus          (->m any)]
            [get-list-box   (->m (is-a?/c list-box%))]
            [get-text-field (->m (is-a?/c text-field%))]
            [set-contents   (->m list? any)]
            [set-text       (->m label-string? any)])])]

The @racketid[key] argument builds a label string from a element of @racketid[contents].
By default is just displays the elements of @racketid[contents] as a string.

The @racketid[callback] takes the currently selected index in the list-box (or the first one if
none is selected), the corresponding displayed label-string and the corresponding content element in
@racketid[contents].

The @racketid[filter] argument allows the user to replace the default filter.


Minimal example:
@codeblock|{
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
(send slb focus)}|

