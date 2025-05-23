% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{writeResultToDisk}
\alias{writeResultToDisk}
\title{Write diagnostics results to a zip file on disk in given output folder.}
\usage{
writeResultToDisk(resultList, databaseId, outputFolder, filename = NULL)
}
\arguments{
\item{resultList}{named list with results}

\item{databaseId}{database identifier}

\item{outputFolder}{folder to write to}

\item{filename}{output filename, if NULL it will be equal to databaseId}
}
\value{
No return value, called for side effects
}
\description{
Write diagnostics results to a zip file on disk in given output folder.
}
\examples{
\dontrun{
resultList <- list("mtcars" = mtcars)
result <- writeResultToDisk(
  resultList = resultList,
  databaseId = "mtcars",
  outputFolder = here::here()
)
}
}
