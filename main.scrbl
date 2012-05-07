#lang scribble/doc
@(require scribble/manual
          planet/scribble
          (for-label racket)
          (for-label racket/gui)
          (for-label slideshow/pict)
          (for-label (this-package-in main)))

@title{@bold{pdf-render}: Render PDF files to dc% and picts}
@author{gcr, based on code by Jens Axel Søgaard}

@defmodule/this-package[main]{

This library lets Racket draw PDF files to picts and to @racket[dc%]
objects. It's quite hacky and minimal; only including auxiliary
functions to get the page count and page size for now.

This requires @tt{libpoppler}, and likely only works on Linux or Mac
OSX. Windows might work if you insctall Poppler.
}

@section{PDF file info}
@defstruct*[pdf-file ([uri string?] [pw (or/c string? false/c)])]{
This struct allows you to open PDF files from an arbitrary Poppler URI
(@racket["file:/tmp/test.pdf"] for example) and optionally protected
with a password. Note that the URI must be an absolute path.
}

@defproc[(page-count [path (or/c pdf-file? path?)])
         exact-integer?]{
Returns the number of pages in @racket[path].
}

@defproc[(page-size [doc (or/c pdf-file? path?)] [page_index (exact-integer?)])
         (list/c real? real?)]{
Returns the width and height of the @racket[page_index]th page. Note that
@racket[page_index] starts at 0.

This is the page size, not the bounding/crop box! We don't respect
that; sorry.
}

@defproc[(render-doc-to-dc! [doc (or/c pdf-file? path?)]
                            [page_index (exact-integer?)]
                            [dc (is-a?/c dc%)])
         any/c]{
Renders the @racket[page_index]th page, without rasterization, to the
given drawing context.
}

@defproc[(pdf-page->pict [doc (or/c pdf-file? path?)]
                         [page_index (exact-integer?)])
         pict?]{
Produces a @racket[pict] containing the @racket[page_index]th page.
}

@section{Bugs and Issues}

Rendering works by reaching into the @racket[dc%] and pulling out the
cairo drawing context. This is inappropriately low-level.

