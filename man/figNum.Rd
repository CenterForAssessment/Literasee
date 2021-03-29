\name{figNum}
\alias{figNum}
\title{Gets the figure number}
\usage{
figNum(advance.counter=0)
}
\arguments{
  \item{advance.counter}{Numeric.  Number to be added to the present figure number.  Default is 0, giving the present figure.}
}
\description{
The function relies on \code{options("fig_caption_no")} in
order to keep track of the current number.
}
\examples{
options(fig_caption_no=1)
figNum()
}
