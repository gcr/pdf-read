#lang racket
(require "ffi.rkt"
         racket/draw
         slideshow/pict)

(provide/contract
 [pdf-document? (any/c . -> . bool?)]
 [pdf-page? (any/c . -> . bool?)]
 [rectangle? (any/c . -> . bool?)]
 [open-pdf-uri (string? (or/c string? false?) . -> .
                (or/c PopplerDocumentPointer? false?))]
 [to-doc (pdf-document? . -> . PopplerDocumentPointer?)]
 [pdf-page (pdf-document? exact-nonnegative-integer? . -> .
            PopplerPagePointer?)]
 [to-page (pdf-page? . -> . PopplerPagePointer?)]
 [pdf-count-pages (pdf-document? . -> . exact-nonnegative-integer?)]
 [page-size (pdf-page? . -> . (list/c (and/c real? (not/c negative?))
                                      (and/c real? (not/c negative?))))]
 [page-crop-box (pdf-page? . -> . rectangle?)]
 [page-text-in-rect (pdf-page? (one-of/c 'glyph 'word 'line)
                               (and/c inexact? (not/c negative?))
                               (and/c inexact? (not/c negative?))
                               (and/c inexact? (not/c negative?))
                               (and/c inexact? (not/c negative?))
                               . -> . string?)]
 [page-text (pdf-page? . -> . string?)]
 [page-find-text (pdf-page? string? . -> . (listof rectangle?))]
 [page-text-layout (pdf-page? . -> . (listof rectangle?))]
 [page-text-with-layout (pdf-page? . -> . (listof (list/c string?
                                                          rectangle?)))]
 [page-render-to-cairo! (pdf-page? any/c . -> . any/c)]
 [page-render-to-dc! (pdf-page? (is-a?/c dc<%>) . -> . any/c)]
 [page->pict (pdf-page? . -> . pict?)]
 [page->bitmap (pdf-page? . -> . (is-a?/c bitmap%))]
 [pdf-title (pdf-document? . -> . (or/c false? string?))]
 [pdf-author (pdf-document? . -> . (or/c false? string?))]
 [pdf-subject (pdf-document? . -> . (or/c false? string?))]
 [pdf-keywords (pdf-document? . -> . (or/c false? string?))]
 [pdf-creator (pdf-document? . -> . (or/c false? string?))]
 [pdf-producer (pdf-document? . -> . (or/c false? string?))]
 [page-label (pdf-page? . -> . (or/c false? string?))]
 )

