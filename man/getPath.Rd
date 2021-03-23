\name{getPath}
\alias{getPath}
\title{Utility function that returns a file path relative to another.}
\usage{
getPath(path=".", relation=".")
}
\arguments{
  \item{path}{Character. File path from which to find relative path. Defaults to the current (working) directory}
  \item{relation}{Character. Relationship from path to return. Defaults to the path provided.}
}
\value{
File path relative to path provided.
}
\description{
Helper function to find one path relative to another.  Returns the current working dierectory by default.
}
