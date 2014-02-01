#lang scribble/doc
@(require scribble/manual
          planet/scribble
          (for-label racket)
          (for-label racket/gui)
          (for-label slideshow/pict)
          (for-label (this-package-in main)))

@title{pdf-read: Read and render PDF files}
@author{gcr, based on code by Jens Axel Søgaard}

@defmodule/this-package[main]{

This library lets Racket render PDF files. You can also gather information
about the text or layout.

This requires @tt{libpoppler}, and thus likely only works on Linux or Mac OSX.
Windows might work if you insctall Poppler, but I can't make promises.

}

@table-of-contents[]

@section{Examples}

Showing a PDF as a pict is as easy as you'd expect:
@codeblock{
#lang racket
(require slideshow/pict
         pdf-read)

(show-pict (page->pict "oopsla04-gff.pdf"))
}
@image{imgs/pageone.png}

By default, @racket[page->pict] shows the first page of the given PDF filename.
You can also say something like @racket[(show-pict (page->pict (pdf-page
"oopsla04-gff.pdf" 5)))] to show the 6th page (pages are zero-indexed).

You can also search for text. This snippet searches for every occurance of the
word "the" and overlays a yellow rectangle on each:
@codeblock{
;; The first page of a PDF file. (Pages are zero-indexed)
(define page (pdf-page "oopsla04-gff.pdf" 0))

;; Overlay each box over the PDF.
(for/fold ([pageview (page->pict page)])
   ([bounding-box (in-list (page-find-text page "the"))])
 (match-define (list x1 y1 x2 y2) bounding-box)
 ;; Each match's bounding box ^
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

All functions that accept pages or documents also accept filenames. This is
more convenient for you, but it is also less efficient because the document
must be re-opened every time. You can make this faster by keeping the result of
@racket[pdf-page] or @racket[open-pdf-uri] to ensure that this library only
opens the document once.

@defproc[(pdf-document? [maybe-doc any/c]) boolean?]{
Returns @racket[#t] if @racket[maybe-doc] is a PDF document. This can be a
string (filename) or the result of @racket[open-pdf-uri].
}
@defproc[(open-pdf-uri [uri string?] [password (or/c string? false?)])
         (or/c pdf-document? false?)]{
Opens the given PDF file with the given uri and the given password. The
@racket[password] may be @racket[#f] if the document does not have a password.
The @racket[uri] is usually something like @racket["file:/tmp/test.pdf"];
anything libpoppler accepts. Don't forget the @tt{file:} at the beginning for
local documents!

This function will throw an error if the PDF file does not exist.

For example, if you wish to show the tenth page from an encrypted document,
use:
@codeblock{
(define document (open-pdf-uri "file:/tmp/secret.pdf" "some_password"))
(define page (pdf-page document 9))
(show-pict (page->pict page))
}
}
As a shortcut, @racket[(page->pict "/tmp/filename.pdf")] is a shortcut for
@codeblock{
(page->pict (pdf-page (open-pdf-uri "file:/tmp/filename.pdf" #f) 0))
}
Most of these functinos honor shortcuts like this.

@defproc[(pdf-page? [maybe-page any/c]) boolean?]{
Returns true if @racket[maybe-page] is a page. Pages can be filenames (in which
case the first page of the document will be used), @racket[pdf-document?]s, or
the result of @racket[pdf-page].
}

@defproc[(pdf-page [maybe-doc pdf-document?] [page-index
exact-nonnegative-integer?]) pdf-page?]{
Opens the @racket[page-index]th page in @racket[maybe-doc]. Pages are
zero-indexed, so if you want the
third page of @racket["/tmp/oopsla04-gff.pdf"], for example, use:
@racketblock[(pdf-page "/tmp/oopsla04-gff.pdf" 2)]
}


@section{Rendering}
@defproc[(page->pict [page pdf-page?]) pict?]{
Renders the given @racket[page] to a @racket[pict] from Racket's
@racket[slideshow/pict] library. This pict can be transformed any way you like
without rasterization.
}

@defproc[(page->bitmap [page pdf-page?]) (is-a?/c bitmap%)]{
Renders the @racket[page] at 72 DPI to a @racket[bitmap%].
}

@defproc[(page-render-to-dc! [page pdf-page?] [dc (is-a?/c dc<%>)]) any/c]{
Renders the given @racket[page] to the given @racket[dc<%>] context. This only
works if @racket[dc] is backed by Cairo and the moons are aligned properly.
}

@defproc[(page-render-to-cairo! [page pdf-page?] [_cairo_t any/c]) any/c]{
Render the given @racket[page] to the given @racket[_cairo_t] Cairo context with
the current transformation.
}

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

@defproc[(page-text-in-rect
[page pdf-page?]
[mode (one-of/c 'glyph 'word 'line)]
[x1 (and/c inexact? (not/c negative?))]
[y1 (and/c inexact? (not/c negative?))]
[x2 (and/c inexact? (not/c negative?))]
[y2 (and/c inexact? (not/c negative?))])
string?]{

Returns the string inside the given selection bounding box. If @racket[mode] is
@racket['glyph], the closest-matching characters are returned; if @racket[mode]
is @racket['word], the returned string will be the words lying in the
rectangle; if @racket[mode] is @racket['line], an entire line of text will be
returned.

When specifying the rectangle, @tt{x1,y1} should be the point of the beginning
of the selection and @tt{x2,y2} should be the end. Coordinates are in points
(1/72 of an inch).

}

@defproc[(page-text [page pdf-page?]) string?]{
Returns all the text in @racket[page] as a string.
}

@defproc[(page-text-layout [page pdf-page?])
(listof (list/c (and/c real? (not/c negative?))
                (and/c real? (not/c negative?))
                (and/c real? (not/c negative?))
                (and/c real? (not/c negative?))))]{

Returns bounding boxes for each of the letters in @racket[page]. The poppler
documentation says that the @racket[N]th character in @racket[(page-text page)]
should correspond with the @racket[N]th bounding box, but this may or may not
be accurate.

Each bounding box is a @racket[(list x1 y1 x2 y2)], where @tt{x1,y1} is the top
left corner and @tt{x2,y2} is the bottom right. Coordinates are in points (1/72
of an inch).
}

@defproc[(page-text-with-layout [page pdf-page?])
(listof (list/c string
                (list/c (and/c real? (not/c negative?))
                        (and/c real? (not/c negative?))
                        (and/c real? (not/c negative?))
                        (and/c real? (not/c negative?)))))]{

Returns each letter in the document along with its bounding box for each of the
letters in @racket[page]. Note that each letter's string may be longer than one
letter, and it often includes newlines.

Each bounding box is a @racket[(list x1 y1 x2 y2)], where @tt{x1,y1} is the top
left corner and @tt{x2,y2} is the bottom right. Coordinates are in points (1/72
of an inch).

For example,
@codeblock{
(take (page-text-with-layout "oopsla04-gff.pdf") 5)
}
will return the bounding boxes of the first five letters on the first page of
@racket["oopsla04-gff.pdf"], namely:
@codeblock{
'(("S\n" (150.738 71.302 162.699 87.890))
  ("u\n" (162.699 71.302 173.656 87.890))
  ("p\n" (173.656 71.302 184.613 87.890))
  ("e\n" (184.613 71.302 194.584 87.890))
  ("r\n" (194.584 71.302 201.560 87.890)))
}
}

@section{Searching}
@defproc[(page-find-text [page pdf-page?] [text string?])
(listof (list/c (and/c real? (not/c negative?))
                (and/c real? (not/c negative?))
                (and/c real? (not/c negative?))
                (and/c real? (not/c negative?))))]{
Searches for the given @racket[text] on @racket[page]. This is a
case-insensitive search. Returns a list of rectangles. Each
rectangle is a @racket[(list x1 y1 x2 y2)], where @tt{x1,y1} is the top left
corner and @tt{x2,y2} is the bottom right. Coordinates are in points (1/72 of
an inch).
}


@section{Metadata}
@defproc[(pdf-count-pages [doc pdf-document?]) exact-nonnegative-integer?]{
Returns the number of pages in @racket[doc].
}

@defproc*[(
[(pdf-title [doc pdf-document?]) (or/c false? string?)]
[(pdf-author [doc pdf-document?]) (or/c false? string?)]
[(pdf-subject [doc pdf-document?]) (or/c false? string?)]
[(pdf-keywords [doc pdf-document?]) (or/c false? string?)]
[(pdf-creator [doc pdf-document?]) (or/c false? string?)]
[(pdf-producer [doc pdf-document?]) (or/c false? string?)])]{
Returns metadata about @racket[doc].
}

@defproc[(page-label [page pdf-page?]) (or/c false? string?)]{
Returns the @racket[page]'s "label", which is often a numeral like @racket["1"]
or @racket["2"], but it may be a Roman numeral like @racket["xii"] for certain
sections of the PDF. It may even be some other exotic name like @racket["Table of Contents"].
}

@section{Bugs and Issues}
Note that @racket[pdf->pict] draws directly to the underlying surface's cairo
context. This may have problems if the @racket[dc<%>] is not backed by cairo or
if you perform different transformations (like cropping or blurring).
