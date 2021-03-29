\name{figNumIncrement}
\alias{figNumIncrement}
\title{Increase the figure number counter}
\usage{
figNumIncrement(advance.counter=1)
}
\arguments{
  \item{advance.counter}{Numeric.  Number to be added to the present figure number.  Default is 1, advancing figure count by 1.}
}
\description{
Bump the figure number by one.  The function relies
on \code{options("fig_caption_no")} in order to keep track of the last number.
}
\examples{
options(fig_caption_no=1)
getOption("fig_caption_no")
figNumIncrement()
getOption("fig_caption_no")
}
