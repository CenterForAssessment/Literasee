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
  state=NULL,
  output_directory="Documents/reports",
  output_file_name=NULL,
  content="tech_report",
  references=TRUE)
}

\arguments{
  \item{sgp_object}{An SGP object resulting from SGP analyses.
  }
  \item{sgp.year}{The value of the YEAR in the SGP object @Data to use for the report.
  }
  \item{state}{Acronym indicating state associated with the data. This can be supplied to the function
  automatically by including the full state name (e.g, New_Hampshire_SGP) in the name of the object supplied to \code{sgp_object}.
  }
  \item{output_directory}{Character - the directory path name where the output file is saved.
    May be a (relative) file path, in which case directories will be created as needed.
  }
  \item{output_file_name}{Character - the name of the output file to be saved in the \code{output_directory}.
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
