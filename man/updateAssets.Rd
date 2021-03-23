\name{updateAssets}
\alias{updateAssets}
\title{Update the Literasee NCIEA report assets in and existing report directory}
\usage{
updateAssets(asset.type=c("css", "js", "pandoc", "rmd"), asset.directory = "assets")
}
\arguments{
  \item{asset.type}{Character. Which of the assets to update. Defaults to \sQuote{css}, \sQuote{js}, \sQuote{pandoc} and \sQuote{rmd}. The \sQuote{images} can also be updated.}
  \item{asset.directory}{Character. The \sQuote{assets} directory in which to copy/overwrite the requested assets.  This directory should already include the appropriate subdirectories (\sQuote{css}, \sQuote{js}, etc.).}
}
\value{
Updates the requested assets in a local report directory to the most current installed version of the Literasee package (may require package update).
}
\description{
Update local assets for use with the NCIEA RMarkdown template.
}
