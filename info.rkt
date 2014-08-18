#lang info
(define name "pdf-read")
(define blurb
  '("This library lets you turn PDF documents into picts and bitmap%s. You can also gather text and layout information. Small bindings for libpoppler."))
(define primary-file "main.rkt")
(define categories '(media io))
;; (define repositories '("4.x"))
(define deps '("base" "gui-lib" "draw-lib" "slideshow-lib"))
(define build-deps '("scribble-lib" "racket-doc" "pict-doc" "draw-doc"))
(define scribblings '(("pdf-read.scrbl" ())))
