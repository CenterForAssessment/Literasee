\name{startAppendix}
\alias{startAppendix}
\title{Starts a new appendix}
\usage{
startAppendix(
  appendix.number=1L,
  use.alpha=TRUE,
  set.counters=TRUE,
  table.name=NULL,
  figure.name=NULL)
}
\arguments{
  \item{appendix.number}{Numeric.  Number to set to the appendix number.  Default is 1, meaning the next appendix will be the first
    (Appendix A if \code{use.alpha} is true).}
  \item{use.alpha}{Boolean.  Defaults to TRUE, meaning the appendix will will be the alphabetical letter corresponding to \code{appendix.number}.}
  \item{set.counters}{Boolean.  Defaults to TRUE, meaning the \code{\link{setCounters}} function will be used to reset all
    table, figure and equation numbers to 0.}
  \item{table.name}{A character string indicating how tables should be prefaced.  Defaults to NULL, which provides
    \sQuote{**Table A\%s:**} for tables in Appendix A, etc.  Note that \sQuote{\%s} will be substituted for the table number.}
  \item{figure.name}{A character string indicating how figures should be prefaced.  Defaults to NULL, which provides
    \sQuote{**Figure A\%d:** \%s} for figures in Appendix A (results in \sQuote{Figure A1: Caption text}), etc.  Note that \sQuote{\%d}
    will be substituted for the table number and \sQuote{\%s} will be substituted for the caption text provided (if any).}
}
\description{
The function sets up the document to transition to appendix formatting, including section number resets (usually as a letter - e.g. Appendix A) and counter clearing.
}

\examples{
  \dontrun{
    startAppendix()
  }
}
