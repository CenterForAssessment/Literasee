\name{reportSGP}
\alias{reportSGP}
\title{Function for automatic technical report generation from SGP analyses.}
\description{
The function reportSGP is used to create automatic or customized technical reports associated with SGP analyses using the SGP object produced as part of the SGP analysis.
}

\usage{
reportSGP(
  state,
	sgp_object,
	report_params,
	output_knit=TRUE)
}

\arguments{
  \item{state}{Acronym indicating state associated with the data. This can be supplied to the function
  automatically by including the full state name (e.g, New_Hampshire_SGP) in the name of the object supplied to \code{sgp_object}.
  }
  \item{sgp_object}{An SGP object resulting from SGP analyses.
  }
  \item{report_params}{list - parameters to use for the report. To be built out!
  }
  \item{output_knit}{Boolean -- indicating whether to knit Rmd file. Default is TRUE.
  }
}

\value{Returns a technical report in multiple output formats including HTML and PDF.
}

\author{Adam Van Iwaarden \email{avaniwaarden@nciea.org} and Damian W. Betebenner \email{dbetebenner@nciea.org}}

\examples{
  \dontrun{
    reportSGP()
  }
}

\keyword{documentation}
