\name{multi_document}
\alias{multi_document}
\title{Formatter wrapper for \code{rmarkdown::html_document} to facilitate multiple output format porting.}
\usage{
multi_document(
  ...,
  number_sections = TRUE,
  number_section_depth=3,
  toc = TRUE,
  toc_depth = 2,
  self_contained = TRUE,
  dev = "png",
  literasee_template = "sgp_report",
  html_template = "default",
  css = "default",
  bibliography = "default",
  csl = "default",
  md_extensions=NULL,
  pandoc_args = NULL)
}
\arguments{
  \item{...}{Passed onto \code{\link[rmarkdown]{html_document}}.}
  \item{number_sections}{Boolean. Should sections, subsections, and subsubsections be numbered.  Default \code{TRUE}}
  \item{number_section_depth}{Numeric. Depth of headers to include in section numbering. Default is 3 (subsubsections).}
  \item{toc}{Boolean. \code{TRUE} to include a table of contents in the output}
  \item{toc_depth}{Numeric. Depth of headers to include in table of
      contents. Default is 2 (subsections).}
  \item{self_contained}{Produce a standalone HTML file with no external dependencies, using data:
      URIs to incorporate the contents of linked scripts, stylesheets, images, and videos.}
  \item{dev}{Graphics device to use for figure output (defaults to png).}
  \item{literasee_template}{Name of Literasee package style template to use for rendering. Default is \sQuote{sgp_report}
      (currently the only option available).  Located at \code{system.file('rmarkdown', 'templates', 'sgp_report', 'resources', package = 'Literasee')}.
      Note that alternative templates must adopt the same naming conventions (\sQuote{html_report.html}, \sQuote{pdf_report.tex}, etc).}
  \item{html_template}{Pandoc HTML template to use for rendering. Pass \code{"default"} to use the Literasee package default
      template.  Note that if you do not use the default template then the features of \code{multi_document} will not be available.}
  \item{css}{The CSS if other that the default within the package.}
  \item{bibliography}{The LaTeX style bibliography file (.bib, .bibtex, etc.) to include. The default file included
      in the package includes references often used by the author and will likely be insufficient for most users.}
  \item{csl}{The CSL (citation style language) to use to format bibliography (if included).  Defaults to APA 5th edition.  See
      \href{https://github.com/citation-style-language/styles}{this Github repository} for alternatives.
      If NULL pandoc defaults will be used.}
  \item{md_extensions}{Markdown extensions to be added or removed from the default definition or R Markdown.}
  \item{pandoc_args}{Additional command line options to pass to pandoc}
}
\value{
R Markdown output format to pass to \code{\link[rmarkdown]{render}}
}
\description{
This function produces a markdown file suitable for rendering as a
stylized, standalone HTML document tailored for academic and technical
reports, as well as  alternative output formats.  Available formats
include PDF, EPUB and DOCX.

The DOCX format is an adaptation of the  \code{\link[Gmisc]{docx_document}}
processor which allows for seamless integration for importing
html-documents in the MS Word .docx-format. The advantage of html
documents is the ability to create advanced formatting
frequently needed in medical publications and that is
available in the \code{\link[htmlTable]{htmlTable}} function.
You can view
\href{http://gforge.se/2014-07/fast-track-publishing-using-rmarkdown}{the
series} for more details regarding how to achieve
fast-track-publishing (ftp) together with knitr.
}

\examples{
\dontrun{
## Produce HTML document from My_Report.Rmd

  render("My_Report.Rmd",
      multi_document(pandoc_args = "--webtex"))

}
}
