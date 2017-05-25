\name{setCounters}
\alias{setCounters}
\title{Sets the table, figure and equation counter numbers.}
\usage{
setCounters(
  tbl.counter=0,
  fig.counter=0,
  eqn.counter=0)
}
\arguments{
  \item{tbl.counter}{Numeric.  Number to set to the present table number.  Default is 0, meaning the next (first) table will be number 1.}
  \item{fig.counter}{Numeric.  Number to set to the present figure number.  Default is 0, meaning the next (first) figure will be number 1.}
  \item{eqn.counter}{Numeric.  Number to set to the present equation number.  Default is 0, meaning the next (first) equation will be number 1.}
}
\description{
The function relies on \code{options("table_number")}, \code{options("fig_caption_no")} and \code{options("equation_counter")} as counters.
}

\examples{
setCounters()
}
