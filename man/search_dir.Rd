% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_tools.R
\name{search_dir}
\alias{search_dir}
\title{Search a Directory of R Files for a Pattern
Looks through all of the .R files in a directory for a given pattern in the code.}
\usage{
search_dir(.dir, pattern, as_list = FALSE, drop_null = TRUE, ...)
}
\arguments{
\item{.dir}{character. The path to the directory containing the files to search.}

\item{pattern}{characte. The pattern, in RegExp format, for which to search the files.}

\item{as_list}{logical. Should search_dir return a list instead of a data frame?}

\item{drop_null}{logical. Drop the NULL values? Only applies when as_list is TRUE.}

\item{...}{pass through additional options.}
}
\value{
a data frame containing the filename, line number, and code line text for any lines of code matching the pattern.
}
\description{
Search a Directory of R Files for a Pattern
Looks through all of the .R files in a directory for a given pattern in the code.
}
\examples{
search_dir("./", "read.csv")
}
