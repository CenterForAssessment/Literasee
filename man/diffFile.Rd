\name{diffFile}
\alias{diffFile}
\title{Render the differences between two files as an HTML document using the \sQuote{git} version control software package}
\usage{
diffFile(
  current_file,
  reference_file = "HEAD",
  diff_algorithm="minimal",
  clean_empyt_lines=FALSE,
  keep_diff = FALSE,
  view = TRUE,
  return_results=TRUE)
}

\arguments{
  \item{current_file}{string: path to file after changes}
  \item{reference_file}{string: path to file before changes.}
  \item{diff_algorithm}{string: The algorithm to use in \sQuote{git diff}.  See
          \href{https://git-scm.com/docs/git-diff}{git diff} for more information. Possible choices are
          \sQuote{patience}, \sQuote{minimal}, \sQuote{histogram}, or \sQuote{myers} (\sQuote{myers} is git default).}
  \item{clean_empyt_lines}{logical: Should both files be \sQuote{cleaned} by removing additional lines between text chunks.
          Only an option for reference_file if it is an actual file (not a git repo).}
  \item{keep_diff}{logical: Keep a copy of the HTML file.  If TRUE a file is saved to the current_file directory and named after it.}
  \item{view}{logical: View the HTML results automatically using \code{\link[utils]{browseURL}}.}
  \item{return_results}{logical: Return the character string with the location of the HTML results.}
}
\value{
\code{string} Returns a character string containing the path for a git diff output file that has been post-processed as an
          HTML document using \link[rmarkdown]{render}.
}
\description{
This is a function for rendering the differences between two files as an HTML document.  The function depends on the user
having \href{https://git-scm.com}{git} version control software installed. See \href{https://git-scm.com/doc}{git documentation} for more details.
}

\examples{
  \dontrun{
  # Check differences between two 'Draft' .Rmd files (not in a git repo)
  diffRmd(current_file='Draft/My_Report-Compiled_Draft_v2.Rmd',
          reference_file='Draft/My_Report-Compiled_Draft_v1.Rmd',
          clean_empyt_lines = TRUE,
          keep_diff = TRUE)
  }
  # An advanced output
  \dontrun{
  # From the Literacy repo directory
  # check the current placeFigure.R file against an old commit
  diffRmd(current_file='R/placeFigure.R',
          reference_file='d47a9e2bdb1fde5723af3877ef21ea18d229ced7')
  }
}
