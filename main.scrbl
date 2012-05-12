#lang scribble/doc
@(require scribble/manual
          planet/scribble
          (for-label racket)
          (for-label racket/gui)
          (for-label slideshow/pict)
          (for-label (this-package-in main)))

@title{@bold{pdf-render}: Render and parse PDF files}
@author{gcr, based on code by Jens Axel Søgaard}

@defmodule/this-package[main]{

This library lets Racket render PDF files, or gather information about
the text or layout.

This requires @tt{libpoppler}, and likely only works on Linux or Mac
OSX. Windows might work if you insctall Poppler.

}

@table-of-contents[]

@section{Examples}

Showing a PDF as a pict is as easy as you'd expect:
@codeblock{
#lang racket
(require slideshow/pict
         (planet gcr/pdf-render))

(show-pict (page->pict "oopsla04-gff.pdf"))
}
@image{imgs/pageone.png}

You can also search for text. This snippet searches for every occurance of the
word "the" and overlays a yellow rectangle on each:
@codeblock{
;; The first page of a PDF file. (Pages are zero-indexed)
(define page (pdf-page "oopsla04-gff.pdf" 0))

;; Overlay each box over the PDF.
(for/fold ([pageview (page->pict page)])
   ([box (in-list (page-find-text page "the"))])
 (match-define (list x1 y1 x2 y2) box)
 ;; ^ The coordinates of each of the matches.
 (pin-over pageview x1 y1
           (cellophane
            (colorize (filled-rectangle (- x2 x1) (- y2 y1)) "yellow")
            0.5)))
}
@image{imgs/search.png}

Finally, for low-level layout control, you can gather the bounding box geometry
for each letter on the page. This snippet outlines each letter's bounding box.
@codeblock{
(define page (pdf-page "oopsla04-gff.pdf" 0))
(for/fold ([pageview (apply blank (page-size page))])
     ([box (in-list (page-text-layout page))])
   (match-define (list x1 y1 x2 y2) box)
   (pin-over pageview x1 y1
             (colorize (rectangle (- x2 x1) (- y2 y1))
                       "gray")))
}
@image{imgs/text.png}


Note that @racket[page->pict] does not convert the file to a bitmap. You can
rotate, scale, or transform the resulting pict however you like without losing
quality.
@codeblock{
(rotate
 (frame (scale (inset/clip (page->pict page) -400 -300 -100 -400) 5))
 (* 0.125 pi))
}
@image{imgs/rotated.png}

If you want to coerce the result to a bitmap for speed, you can do that as
well:
@codeblock{
 (rotate
  (frame (scale (inset/clip (bitmap (page->bitmap page))
                            -400 -300 -100 -400)
                5))
  (* 0.125 pi))
}
@image{imgs/rotated-bitmap.png}

@section{PDF files}

ALl functions that accept pages or documents also accept filenames. This is
more convenient for you, but it is also less efficient because the document
must be opened every time. You can make this faster by keeping the result of
@racket[pdf-page] or @racket[open-pdf-uri] to ensure that this library only
opens the document once.

@defproc[(pdf-document? [maybe-doc any/c]) bool?]{
Returns @racket[#t] if @racket[maybe-doc] is a PDF document.
In general, any function that expects a @racket[pdf-document?] will accept a
filename string or the result of @racket[open-pdf-uri].
}
@defproc[(open-pdf-uri [uri string?] [password (or/c string? false?)])
         (or/c pdf-document? false?)]{
Opens the given PDF file with the given uri and the given password. The
@racket[password] may be @racket[#f] if the document has none. The @racket[uri]
is usually something like @racket["file:/tmp/test.pdf"]; anything libpoppler
accepts.

This function will throw an error if the PDF file does not exist.
}

@defproc[(pdf-page? [maybe-page any/c]) bool?]{
Returns true if @racket[maybe-page] is a page. Pages can be filenames (in which
case the first page of the document will be used), @racket[pdf-document?]s, or
the result of @racket[pdf-page].
}

@defproc[(pdf-page [maybe-doc pdf-document?] [page-index
exact-nonnegative-integer?]) pdf-page?]{
Opens the @racket[page-index]th page in @racket[maybe-doc]. If you want the
third page of @racket["/tmp/oopsla04-gff.pdf"], for example, use:
@racketblock[(pdf-page "/tmp/oopsla04-gff.pdf" 2)]
}


@section{Rendering}
  - all coordinates are in points

@section{Layout}
@defproc[(page-size [page pdf-page?])
(list/c (and/c real? (not/c negative?))
        (and/c real? (not/c negative?)))]{
Returns the width and height of @racket[page], in points (1/72 of an inch).
}

@defproc[(page-crop-box [page pdf-page?])
(list/c (and/c real? (not/c negative?))
        (and/c real? (not/c negative?))
        (and/c real? (not/c negative?))
        (and/c real? (not/c negative?)))]{
Returns the rectangle of @racket[page]'s crop box.

Each rectangle is a @racket[(list x1 y1 x2 y2)], where @tt{x1,y1} is the top
left corner and @tt{x2,y2} is the bottom right. Coordinates are in points (1/72
of an inch). }

@racket[(page-text-in-rect
[page pdf-page?]
[mode (one-of/c 'glyph 'word 'line)]
[x1 (and/c inexact? (not/c negative?))]
[y1 (and/c inexact? (not/c negative?))]
[x2 (and/c inexact? (not/c negative?))]
[y2 (and/c inexact? (not/c negative?))])
string?]{
Returns the string inside

mode

rectangle
}

@section{Searching}
@section{Metadata}
@defproc[(pdf-count-pages [doc pdf-document?]) exact-nonnegative-integer?]{
Returns the number of pages in @racket[doc]
}


@section{Bugs and Issues}
Note that @racket[pdf->pict] draws directly to the underlying surface's cairo
context. This may have problems if the @racket[dc<%>] is not backed by cairo or
if you perform different transformations (like cropping or blurring).
