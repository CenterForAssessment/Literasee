\name{gofPrint}
\alias{gofPrint}
\title{Function to produce Goodness of Fit plots from a SGP object.}
\description{
The function gofPrint is used to output Goodness of Fit plots from R grobs stored in a SGP object.
}

\usage{
  gofPrint(
    sgp_object,
    years=NULL,
    content_areas=NULL,
    grades=NULL,
    norm_group=NULL,
    sgp_types=NULL,
    output_format=c("PDF", "PNG", "DECILE_TABLES"))
}

\arguments{
  \item{sgp_object}{An SGP object resulting from SGP analyses.
  }
  \item{years}{The academic year(s) for which fit plots should be produced
  }
  \item{content_areas}{The content area(s) for which fit plots should be produced
  }
  \item{grades}{The grade(s) for which fit plots should be produced.
  }
  \item{norm_group}{The specific norm group (cohort or course progression) for which fit plots should be produced
  }
  \item{sgp_types}{The SGP type for which fit plots should be produdced (e.g., cohort or baseline referenced, SIMEX adjusted, etc.).
  }
  \item{output_format}{The graphical file type that should be produced (PDF or PNG), or the decile tables as a .Rdata file.
  }
}

\value{Returns a graphical output of the fit plot(s) requested based on the arguments provided.  Can also save the decile table data in Rdata format.
}

\author{Adam Van Iwaarden \email{avaniwaarden@nciea.org} and Damian W. Betebenner \email{dbetebenner@nciea.org}}

\examples{
  \dontrun{
    gofPrint(Demonstration_SGP)
  }
}

\keyword{documentation}
