#lang racket/gui
(require syntax/parse/define)

(provide
 (contract-out
  
  [default-filter (-> string? label-string? any/c)]
  [word-filter    (-> string? label-string? any/c)]
  [regexp-filter  (-> string? label-string? any/c)]
  
  [search-list-box-frame%
   (class/c
    (init [width             (or/c dimension-integer? #f)]
          [height            (or/c dimension-integer? #f)]
          [contents          list?]
          [filter            (-> string? label-string? any/c)]
          [key               (-> any/c string?)]
          [callback          (-> (or/c number? #f)
                                 (or/c label-string? #f)
                                 any/c
                                 any)]
          [close-on-escape?  boolean?]
          [show?             boolean?])
    [set-status (->m label-string? void?)]
    [get-search-list-box (->m (is-a?/c search-list-box%))])]
  
  [search-list-box%
   (class/c
    (init       [contents          list?]
                [message           (or/c label-string? #f)])
    (init-field [label             (or/c label-string? #f)]
                [text-field-mixin  (-> (subclass?/c text-field%)
                                       (subclass?/c text-field%))]
                [list-box-mixin    (-> (subclass?/c list-box%)
                                       (subclass?/c list-box%))]
                [filter            (-> string? label-string? any/c)]
                [key               (-> any/c string?)]
                [callback          (-> (or/c number? #f)
                                       (or/c label-string? #f)
                                       any/c
                                       any)]
                [close-on-escape   (or/c #f
                                         (is-a?/c frame%)
                                         (is-a?/c dialog%))])
    [focus          (->m any)]
    [get-list-box   (->m (is-a?/c list-box%))]
    [get-text-field (->m (is-a?/c text-field%))]
    [set-contents   (->m list? any)]
    [set-text       (->m label-string? any)])]))

;; Memoize the transformation applied to `search` so as to avoid
;; duplicate work, since the filter is called with the same search
;; on all yet-unfiltered values of `str`.
;; the expr... can depend on `search` but should not depend on `str`.
(define-simple-macro (define-filter (name search str)
                       ([id expr] ...)
                       body ...)
  (define name
    (let ([search-mem #f] [id #f] ...)
      (λ (search str)
        (unless (eq? search-mem search)
          (set! search-mem search)
          (set! id expr) ...)
        body ...))))

(define-filter (default-filter search str)
  ([search-down (string-downcase search)]) ; memoized
  (string-contains? (string-downcase str) search))

(define-filter (word-filter search str)
  ([words (map string-downcase (string-split search))]) ; memoized
  (let ([str (string-downcase str)])
    (andmap (λ (wd) (string-contains? str wd)) words)))

(define-filter (regexp-filter search str)
  ([search-rx (pregexp search)]) ; memoized
  (regexp-match search-rx (string-downcase str)))

(define mtext-field%
  (class text-field%
    (init-field [the-list-box #f]
                [close-on-escape #f])
    (define/override (on-subwindow-char tf ev)
      (case (send ev get-key-code)
        [(#\return) (send the-list-box on-return)]
        [(down)     (send the-list-box focus)] ; go into the list box
        [(escape)   (if close-on-escape
                      (send close-on-escape show #f)
                      (super on-subwindow-char tf ev))]
        [else       (super on-subwindow-char tf ev)]))
    (super-new)))

(define mlist-box%
  (class list-box%
    (init-field
     [the-text-field #f]
     [(return-callback on-return) (λ () (void))])

    ;; Returns the selected index, the corresponding label string,
    ;; and the corresponding data, or #f for all three.
    (define/public (get-selected)
      (define sel (or (send this get-selection)
                      (if (= (send this get-number) 0)
                        #false
                        0)))
      (define str (and sel (send this get-string sel)))
      (define data (and sel (send this get-data sel)))
      (values sel str data))

    (define/public (on-return)
      (define-values (sel str data) (get-selected))
      (return-callback sel str data))

    (define/override (on-subwindow-char lb ev)
      #;(println (send ev get-key-code))
      (case (send ev get-key-code)
        [(#\return #\space numpad-enter)
         (on-return)
         #true] ; #\return work on linux, but there's no defaut key on Windows
        [(up)
         (define sel (send lb get-selection))
         (if (or (not sel) (= 0 sel))
           (send the-text-field focus)
           (super on-subwindow-char lb ev))]
        [(escape)
         (send the-text-field focus)
         #true] ; go back to the search box
        [(down)
         (super on-subwindow-char lb ev)]
        [(scroll wheel-up wheel-down wheel-left wheel-right)
         ; Forward mouse events.
         (super on-subwindow-char lb ev)]
        [else
         #true
         ; Don't forward other key press to list-box%,
         ; to avoid triggering the list-box% simple search mechanism .
         #;(super on-subwindow-char lb ev)]))
    
    (super-new
     [callback (λ (lb ev)
                 (case (send ev get-event-type)
                   [(list-box-dclick) (on-return)]))])))

(define search-list-box%
  (class vertical-panel%
    (init-field [label #f]
                [text-field-mixin (λ (x) x)]
                [list-box-mixin (λ (x) x)]
                [(filt? filter) default-filter]
                [key ~a] ; Take a content and turn it into a string
                #;[style '()] ; the text-field style 
                [callback (λ (idx label content) (void))]
                [close-on-escape #f])
    (init [(init-contents contents) '()])

    (define contents #())
    
    ; Set the focus to the text-field
    (define/override (focus)
      (send tf focus))

    (define/public (get-list-box) lb)
    (define/public (get-text-field) tf)

    (define/public (set-contents conts)
      (set! contents
            (for/list ([cont (in-list conts)])
               ; label-strings are limited to 200 chars
              (define str (~a (key cont) #:max-width 200))
              (cons str cont)))
      (update-list-box))

    (define/public (set-text str)
      (send tf set-value str)
      (update-list-box))

    (define (update-list-box)
      (define search (send tf get-value))
      (send lb clear)
      (for ([key.cont (in-list contents)]
            [i (in-naturals)])
        (define label (car key.cont))
        (define cont (cdr key.cont))
        (when (filt? search label)
          (send lb append label cont))))

    (define/override (on-subwindow-char receiver ev)
      (case (send ev get-key-code)
        [(#\return)
         (if (eq? receiver lb)
           (send receiver on-return)
           (super on-subwindow-char receiver ev))]
        [else (super on-subwindow-char receiver ev)]))

    ;;; Initialization
    (super-new)
    (when label
      (new message%
           [parent this]
           [label label]))
    (define tf
      (new (text-field-mixin mtext-field%)
           [parent this]
           [label #f]
           [callback (λ (tf ev)
                       (case (send ev get-event-type)
                         [(text-field)
                          (update-list-box)]))]
           [close-on-escape close-on-escape]))
    (define lb
      (new mlist-box%
           [parent this]
           [label #f]
           [choices '()]
           [on-return callback]))
    
    (set-field! the-list-box tf lb)
    (set-field! the-text-field lb tf)
    
    (set-contents init-contents)
    (send tf focus)
    ))

(define search-list-box-frame%
  (class frame%
    (init [message #f]
          [width 400] [height 400]
          [contents '()]
          [key ~a]
          [[afilter filter] default-filter]
          [callback (λ (idx label content) (void))]
          [close-on-escape? #t]
          [show? #t])

    (define/public (get-search-list-box) slb)

    (define/public (set-status str)
      (send status set-label str))

    (define/override (on-subwindow-focus receiver on?)
      (when on?
        (cond
          [(is-a? receiver text-field%)
           (set-status "Press Enter to confirm, ↑↓ to navigate, Escape to exit")]
          [(is-a? receiver list-box%)
           (set-status "Press Enter to confirm, ↑↓ to navigate, Escape to edit text")])))

    (define/override (on-subwindow-char fr ev)
      (if (equal? #\return (send ev get-key-code))
        #false ; do not gobble #\return
        (super on-subwindow-char fr ev)))

    (super-new [width width] [height height])
    (define slb (new search-list-box% [parent this]
                     [label message]
                     [alignment '(left bottom)]
                     [contents contents]
                     [key key]
                     [filter afilter]
                     [callback callback]
                     [close-on-escape (and close-on-escape? this)]))
    (define status (new message% [parent this] [label ""] [stretchable-width #t]))

    (send this show show?)
    (send slb focus)))
