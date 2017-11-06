\name{reportSGP}
\alias{reportSGP}
\title{Function for automatic technical report generation from SGP analyses.}
\description{
The function reportSGP is used to create automatic or customized technical reports associated with SGP analyses using the SGP object produced as part of the SGP analysis.
}

\usage{
reportSGP(
  sgp_object,
  year,
  state=NULL,
  output_directory="Documents/reports",
  output_file_name=NULL,
  output_knit=TRUE,
  content="sgp_report",
  license="default",
  references=TRUE,
  cover_page="default")
}

\arguments{
  \item{sgp_object}{An SGP object resulting from SGP analyses.
  }
  \item{year}{The value of the YEAR in the SGP object @Data to use for the report.
  }
  \item{state}{Acronym indicating state associated with the data. This can be supplied to the function
  automatically by including the full state name (e.g, New_Hampshire_SGP) in the name of the object supplied to \code{sgp_object}.
  }
  \item{output_directory}{Character - the directory path name where the output file is saved. Defaults to \code{documents/reports}.
    May be a (relative) file path, in which case directories will be created as needed.
  }
  \item{output_file_name}{Character - the name of the output file to be saved in the \code{output_directory}.
  }
  \item{output_knit}{Boolean -- indicating whether to knit Rmd file. Default is TRUE.
  }
  \item{content}{Character - the type of report to create.  Pulls content .Rmd files ('bones') from one of the
    content repositories located at \code{system.file('rmarkdown', 'content', package = 'Literasee')}.
  }
  \item{license}{Path to .tex file or "default" providing a licence page following the cover page.
  }
  \item{references}{Boolean. Include a 'References' section in the report.
  }
  \item{cover_page}{Path to .tex file providing cover page or "default". File is placed in student report YAML: \code{includes: in_header: cover_page.tex}
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
