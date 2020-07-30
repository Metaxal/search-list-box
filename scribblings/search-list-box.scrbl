#lang scribble/manual
@(require (for-label search-list-box
                     racket/gui)
          scribble/extract
          racket/runtime-path
          pict)

@(define-runtime-path search-list-box-png "../img/search-list-box.png")

@title{Search-list-box}
@author{Laurent Orseau}

@defmodule[search-list-box]


A @racket[list-box%] with a search @racket[text-field%]. See some examples
@hyperlink["https://github.com/Metaxal/search-list-box/tree/master/examples"]{here}.

@(centered (bitmap search-list-box-png))

Features:
@itemlist[
 @item{The search function can be customized.}
 @item{Can navigate between the text-field and the list-box with arrow keys.}
 @item{In the text field, press enter to validate; in the list-box press space.}
 @item{Pressing Escape in the list-box returns to the text-field.}
 @item{The filter function can be customized.}]

@defclass[search-list-box-frame% frame% ()]{
 @defconstructor[([width (or/c dimension-integer? #f) 400]
                  [height (or/c dimension-integer? #f) 400]
                  [contents list? '()]
                  [filter (-> string? label-string? any/c) default-filter]
                  [key (-> any/c string?) ~a]
                  [callback
                   (-> (or/c number? #f)
                       (or/c label-string? #f)
                       any/c
                       any)
                   (λ (idx label content) (void))]
                  [close-on-escape? boolean? #t]
                  [show? boolean? #t])]{
Creates a simple frame that contains a single @racket[search-list-box%].
See @racket[search-list-box%] and @racket[frame%] for a description of the arguments;
All the initialization arguments of @racket[frame%] are also available.

Minimal example:
@codeblock|{
#lang racket
(require search-list-box)

(new search-list-box-frame%
     [label "Searching..."]
     [contents '(1 2 3 a1 a2 aa2 bb2 bb3)]
     [callback (λ (idx label content)
                 (if idx
                   (writeln content)
                   (displayln "No content selected")))])
}|}}

@defclass[search-list-box% vertical-panel% ()]{

 @defconstructor[([label (or/c label-string? #f) #f]
                  [text-field-mixin
                   (-> (subclass?/c text-field%)
                       (subclass?/c text-field%))
                   (λ (x) x)]
                  [list-box-mixin
                   (-> (subclass?/c list-box%)
                       (subclass?/c list-box%))
                   (λ (x) x)]
                  [filter
                   (-> string? label-string? any/c)
                   default-filter]
                  [key
                   (-> any/c string?)
                   ~a]
                  [callback
                   (-> (or/c number? #f)
                       (or/c label-string? #f)
                       any/c
                       any)
                   (λ (idx label content) (void))]
                  [close-on-escape
                   (or/c #f
                         (is-a?/c frame%)
                         (is-a?/c dialog%))
                   #f])]{
The @racket[search-list-box%] constructor also accepts all optional arguments
  of @racket[vertical-panel%].

The @racketid[key] argument builds a label string from a element of @racketid[contents].
By default is just displays the elements of @racketid[contents] as a string.

The @racketid[callback] takes the currently selected index in the list-box (or the first one if
none is selected), the corresponding displayed label-string and the corresponding content element in
@racketid[contents].

The @racketid[filter] argument allows the user to replace the default filter.

If a @racket[frame%] or @racket[dialog%] is passed to @racketid[close-on-escape],
then upon pressing @racketid[escape] in the text-field the frame or dialog is closed.
}

 @defmethod[(get-list-box) (is-a?/c list-box%)]{
 Returns the internal @racket[list-box%].}
 @defmethod[(get-text-field) (is-a?/c text-field%)]{
 Returns the internal @racket[text-field%].}
 @defmethod[(set-text [str string?]) void?]{
 Sets the text in the @racket[text-field%] to @racketid[str].}
 @defmethod[(set-contents [conts list?]) void?]{
 Replaces the current contents of the list box with @racketid[conts].
 The @racketid[key] and @racketid[filter] are not changed.}
}

@defproc[(default-filter [str string?] [lbl label-string?]) any/c]{
 Equivalent to
 @racketblock[
 (string-contains?
  (string-downcase str)
  (string-downcase search))]}



