% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fst_from_rbr.R
\name{fst_from_rbr}
\alias{fst_from_rbr}
\title{obtain data from rbr sqlite3 database and save in fst format}
\usage{
fst_from_rbr(db_name, well_name)
}
\arguments{
\item{db_name}{character the path to the rbr database ( rsk )}

\item{well_name}{character the well_name to add to the file}
}
\value{
the dataset
}
\description{
convert sqlite data to fst file
}
\author{
Jonathan Kennel \email{jkennel@uoguelph.ca}
}
