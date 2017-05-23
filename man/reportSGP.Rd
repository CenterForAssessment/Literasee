\name{reportSGP}
\alias{reportSGP}
\title{Function for automatic technical report generation from SGP analyses.}
\description{
The function reportSGP is used to create automatic or customized technical reports associated with SGP analyses using the SGP object produced as part of the SGP analysis.
}

\usage{
reportSGP(
  sgp_object,
  sgp.year,
  output_file="docs/SGP_Report.Rmd",
  content="tech_report",
  references=TRUE)
}

\arguments{
  \item{sgp_object}{An SGP object resulting from SGP analyses.
  }
  \item{sgp.year}{The value of the YEAR in the SGP object @Data to use for the report.
  }
  \item{output_file}{Character - the name that should be assigned to the .Rmd output file.
    May be a (relative) file path, in which case directories will be created as needed.
  }
  \item{content}{Character - the type of report to create.  Pulls content .Rmd files ('bones') from one of the
    content repositories located at \code{system.file('rmarkdown', 'content', package = 'Literasee')}.
  }
  \item{references}{Boolean. Include a 'References' section in the report.
  }
}

\value{Returns a technical report in multiple output formats including HTML and PDF.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{avaniwaarden@gmail.com}}

\examples{
  \dontrun{
    reportSGP()
  }
}

\keyword{documentation}
