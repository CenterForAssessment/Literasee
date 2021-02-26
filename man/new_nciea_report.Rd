\name{new_nciea_report}
\alias{new_nciea_report}
\title{Create a new report using the NCIEA RMarkdown template}
\usage{
new_nciea_report(file_name = "nciea_report", type_of_report = NULL)
}
\arguments{
\item{file_name}{The name of the file for your RMarkdown document. Defaults to \sQuote{report} and creates a file called report.Rmd}

\item{type_of_report}{The type of report you are creating (currently only supports \sQuote{nciea_report})}
}
\value{
Copies assets (css, javascript, Rmd and other source files) necessary for creating a new Center for Assessment themed report.
}
\description{
Create a new report using the NCIEA RMarkdown template
}
