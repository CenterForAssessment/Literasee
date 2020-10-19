\name{placeFigure}
\alias{placeFigure}
\title{Places figure and caption appropriately within the supplied file.}
\usage{
placeFigure(
    files,
    rows=1,
    columns=1,
    figure.id=NULL,
    caption=NULL,
    caption.position="top",
    pdf.width=NULL,
    html.width=NULL,
    page.break=FALSE,
    normalize.path=FALSE
  )
}
\arguments{
  \item{files}{Character string of files in which figures are placed.  Multiple files will be put in a \code{rows} x \code{columns} grid by
          row (i.e. with four files in a 2x2 grid, the first two files will be the top row and the last two files will be the bottom row.)}
  \item{rows}{Postive integer, defaults to 1.}
  \item{columns}{Positive integer defining number of columsns, defaults to 1.}
  \item{figure.id}{Figure identifier.}
  \item{caption}{Figure caption.}
  \item{caption.position}{Figure caption position. Either \sQuote{top} (the default) or \sQuote{bottom}.}
  \item{pdf.width}{The width of the PDF output.}
  \item{html.width}{The width of the HTML output (if single image in the figure - ignored for multiple image grid).}
  \item{page.break}{Boolean variable indicating whether there should be page breaks (defaults to FALSE).}
  \item{normalize.path}{Boolean variable indicating whether the should paths in \code{files} should be normalized with
          \link[base:normalizePath]{normalizePath} (defaults to FALSE).}
}
\description{
Places figure appropriately within file with caption.
}
