\name{createReportScripts}
\alias{createReportScripts}
\title{Create a new NCIEA report directory}
\usage{
createReportScripts(report_config, rmd_file_list, bookdown=TRUE, pagedown=TRUE)
}
\arguments{
  \item{report_config}{List. A named list with elements to be turned into YAML and RMD master/parent files in the creation of a NCIEA report.}
  \item{rmd_file_list}{List. A named list of RMD child documents to be included in the report.  Additional information such as file order and paths also included.}
  \item{bookdown}{Logical. Should bookdown files (\sQuote{_bookdown_site.yml} and \sQuote{index.Rmd}) be created.}
  \item{pagedown}{Logical. Should pagedown files (master/parent named by report/appendix title) be created.}
}
\value{
Returns one or more .yml and/or .Rmd files that contain the required report meta-data and child file structure/order to produce an NCIEA themed report/bookdown site.
}
\description{
Create master/parent documents appropriate for use with the NCIEA RMarkdown template.
}
