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
    caption.position="top", # "bottom"
    pdf.width=NULL,
    html.width=NULL,
    page.break=FALSE
  )
}
\arguments{
  \item{files}{Character string of files in which figures are placed.}
  \item{rows}{Postive integer, defaults to 1.}
  \item{columns}{Positive integer defining number of columsns, defaults to 1.}
  \item{figure.id}{Figure identifier.}
  \item{caption}{Figure caption.}
  \item{caption.position}{Figure caption position.Either 'top' (the default) or bottom.}
  \item{pdf.width}{The width of the PDF output.}
  \item{html.width}{The width of the HTML output.}
  \item{page.break}{Boolean variable indicating whether there should be page breaks (defaults to FALSE).}
}
\description{
Places figure appropriately within file with caption.
}
