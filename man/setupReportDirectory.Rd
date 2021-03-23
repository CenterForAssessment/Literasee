\name{setupReportDirectory}
\alias{setupReportDirectory}
\title{Create a new NCIEA report directory}
\usage{
setupReportDirectory(
  dir=getwd(),
  new.report=TRUE,
  update.assets=FALSE,
  custom.content=TRUE,
  custom.content.path,
  overwrite.custom=FALSE)
}
\arguments{
  \item{dir}{The directory path to establish the new report in.  Defaults to the current working directory.}
  \item{new.report}{Logical.  Whether the report is new and new \sQuote{nciea_report} assets should be copied.}
  \item{update.assets}{Logical.  If report is not new, simply update Literasee package assets using \code{\link{updateAssets}}.}
  \item{custom.content}{Logical.  Whether the templates for custom content (.Rmd files) should be copied from some directory (custom.content.path).}
  \item{custom.content.path}{Character.  The file path location from which custom content templates will be copied.}
  \item{overwrite.custom}{Logical.  Whether the templates for custom content should all be overwritten. Defaults to FALSE - meaning only new items will be copied.}
}
\value{
Copies assets (css, javascript, Rmd and other source files) and custom content necessary for creating a new Center for Assessment themed report.
}
\description{
Create a new report using the NCIEA RMarkdown template and custom content templates.  Wrapper function for \code{\link{new_nciea_report}} and \code{file.copy}.
}
