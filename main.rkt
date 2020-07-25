#lang racket/gui

(provide search-list-box%)

(define mtext-field%
  (class text-field%
    (init-field [the-list-box #f])
    (define/override (on-subwindow-char tf ev)
      (case (send ev get-key-code)
        [(#\return) (send the-list-box on-return)]
        [(down)     (send the-list-box focus)] ; go into the list box
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
                        #f
                        0)))
      (define str (and sel (send this get-string sel)))
      (define data (and sel (send this get-data sel)))
      (values sel str data))

    (define/public (on-return)
      (define-values (sel str data) (get-selected))
      (return-callback sel str data))

    (define/override (on-subwindow-char lb ev)
      (case (send ev get-key-code)
        #;[(#\return #;#\space) (on-return)] ; doesn't work
        [(up)
         (define sel (send lb get-selection))
         (if (or (not sel) (= 0 sel))
           (send the-text-field focus)
           (super on-subwindow-char lb ev))]
        [(escape) (send the-text-field focus)] ; go back to the search box
        [else (super on-subwindow-char lb ev)]))
    (super-new
     [callback (λ (lb ev)
                 (case (send ev get-event-type)
                   [(list-box-dclick) (on-return)]))])))

(define/contract search-list-box%
  (class/c
   (init       [contents          list?])
   (init-field [parent            (or/c (is-a?/c frame%)
                                        (is-a?/c dialog%)
                                        (is-a?/c panel%)
                                        (is-a?/c pane%))]
               [label             (or/c label-string? #f)]
               [text-field-mixin  (-> (is-a?/c text-field%)
                                      (is-a?/c text-field%))]
               [list-box-mixin    (-> (is-a?/c list-box%)
                                      (is-a?/c list-box%))]
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
   [set-text       (->m label-string? any)])
  
  (class vertical-panel%
    (init-field parent
                [label #f]
                [text-field-mixin (λ (x) x)]
                [list-box-mixin (λ (x) x)]
                [(filt? filter)
                 (λ (search str)
                   (string-contains?
                    (string-downcase str)
                    (string-downcase search)))]
                [key ~a] ; Take a content and turn it into a string
                #;[style '()] ; the text-field style 
                [callback (λ (idx label content) (void))])
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

    (super-new [parent parent])
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
                          (update-list-box)]))]))
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
