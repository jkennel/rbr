% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_rbr_files.R
\name{read_rbr_files}
\alias{read_rbr_files}
\title{obtain data from a list of rbr sqlite3 databases}
\usage{
read_rbr_files(files, use_rbr_tz = TRUE)
}
\arguments{
\item{files}{the path to the rbr database ( rsk )}

\item{use_rbr_tz}{boolean use time zone information from the rbr file?}
}
\value{
data.table of results
}
\description{
import sqlite data to R
}
\author{
Jonathan Kennel \email{jkennel@uoguelph.ca}
}
