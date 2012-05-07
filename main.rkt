#lang racket
(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define
         (prefix-in gui: racket/gui)
         racket/draw
         racket/draw/unsafe/cairo
         racket/draw/private/local ;; HACK HACK HACK; needed for 'in-cairo-context'
         racket/draw/unsafe/pango ;; g_object_unref
         (only-in slideshow/pict dc))

(provide (struct-out pdf-file)
         page-count
         page-size
         render-doc-to-dc!
         pdf-page->pict)

;; See: http://comments.gmane.org/gmane.comp.lang.racket.user/11169
(define-ffi-definer define-poppler (ffi-lib "libpoppler-glib"))

(define _PopplerDocumentPointer (_cpointer 'PopplerDocument))
(define _PopplerPagePointer (_cpointer 'PopplerPage))

;; Holds a PDF file and a password.
(struct pdf-file (uri pw))

(define-poppler poppler_document_new_from_file
  (_fun [uri : _string]
        [password : _string]
        [err : _pointer = #f]
        -> [return : (_or-null _PopplerDocumentPointer)]
        -> (if return return (error "Could not open file " uri))))
(define (poppler_open pdf)
  ;; Nicer function for opening a PDF by filename
  (define p (if (pdf-file? pdf)
                pdf
                (pdf-file (string-append "file:" (path->string (path->complete-path pdf))) #f)))
  (poppler_document_new_from_file (pdf-file-uri p) (pdf-file-pw p)))

(define-poppler poppler_document_get_page
  (_fun [doc : _PopplerDocumentPointer]
        [index : _int]
        -> _PopplerPagePointer))

(define-poppler poppler_document_get_n_pages
  (_fun [doc : _PopplerDocumentPointer]
        -> _int))

(define-poppler poppler_page_get_size
  (_fun [page : _PopplerPagePointer]
        [width : (_ptr o _double)]
        [height : (_ptr o _double)]
        -> _void
        -> (list width height)))

(define-poppler poppler_page_render
  (_fun [page : _PopplerPagePointer]
        [cairo-context : _cairo_t]
        -> _void))
;;; void poppler_page_render (PopplerPage *page, cairo_t *cairo);
; Render the page to the given cairo context.
; This function is for rendering a page that will be displayed.
; If you want to render a page that will be printed use poppler_page_render_for_printing() instead
;    page:  the page to render from
;    cairo: cairo context to render to

(define (page-count doc)
  (define d (poppler_open doc))
  (begin0 (poppler_document_get_n_pages d)
    (g_object_unref d)))

(define (page-size doc idx)
  (define d (poppler_open doc))
  (begin0 (poppler_page_get_size (poppler_document_get_page d idx))
    (g_object_unref d)))

(define (render-doc-to-dc! doc page_idx dc)
  (define d (poppler_open doc))
  ;; Render the given page of the PDF file to the given dc.
  (define page (poppler_document_get_page d page_idx))
  (define tr (send dc get-transformation))
  (send dc in-cairo-context
        (λ(cairo_ctx)
          ;; HACK HACK HACK HACK HACK ...
          (poppler_page_render page cairo_ctx)))
  (send dc set-transformation tr)
  (g_object_unref d))

(define (pdf-page->pict doc page_idx)
  (match-define (list width height) (page-size doc page_idx))
  (dc (λ(ctx x y)
        (define tr (send ctx get-transformation))
        (send ctx translate x y)
        (render-doc-to-dc! doc page_idx ctx)
        (send ctx set-transformation tr))
      width
      height))

