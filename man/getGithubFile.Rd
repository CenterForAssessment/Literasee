\name{getGithubFile}
\alias{getGithubFile}
\title{Convenience wrapper to get (download) one or more files from a Github repo.}
\usage{
getGithubFile(
  github.user,
	repo,
	branch="master",
	github.directory=NULL,
  file.to.get=NULL,
	local.directory="github_files",
	file.type = "[.]R$|[.]Rmd$",
	quiet=FALSE)
}
\arguments{
  \item{github.user}{Character. Name of github user or organization who owns the repo you want to get file(s) from.}
  \item{repo}{Character. The repo in which file(s) are located.}
  \item{branch}{Character. Repo branch in which to look.  Default is \sQuote{master}.}
  \item{github.directory}{Character. Additional directory structure within the repo branch in which to look.
              Default is NULL, meaning the whole repo.}
  \item{file.to.get}{Character. Specific (single) file to get within the \code{repo/branch/github.directory}.
              Default is NULL, meaning all files in the \code{repo/branch/github.directory} path.}
  \item{local.directory}{Character. Local directory in which to download files.  Default is \sQuote{github_files}.
              A directory of this name will be created in the working directory if it does not already exist.}
  \item{file.type}{Character. Specific file type (or other special text in which to \link[base]{grep} from the list of files in
              the repo) to limit files that are retrieved.  Default is \sQuote{[.]R$|[.]Rmd$}, which will get all .R and .Rmd files.}
  \item{quiet}{Boolean. Logical - should download progress be shown. Defaults to \sQuote{FALSE}. See
              \link[utils]{download.file} for details.}
}
\description{
The function retrieves one or more files from a Github repo via \link[utils]{download.file}.
}

\examples{
\dontrun{
getGithubFile(github.user = "CenterForAssessment", repo = "SGP_Research",
              github.directory = "Master_Documents/Skip_Year_Analysis/SGP_Report",
              local.directory = file.path("assets", "github_children"), file.type = ".Rmd$")
}
}
