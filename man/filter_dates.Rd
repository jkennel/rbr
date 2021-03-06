% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_dates.R
\name{filter_dates}
\alias{filter_dates}
\title{filter_dates}
\usage{
filter_dates(all, subsets, keep = FALSE, include_filt_cols = FALSE)
}
\arguments{
\item{all}{data.table to filter (name, datetime)}

\item{subsets}{data.table of filter start and end times (start, end)}

\item{keep}{include or exclude the subsets}

\item{include_filt_cols}{include the columns in the filter table when keep is TRUE}
}
\value{
filtered data.table
}
\description{
remove values from a data.table (non-equi join filter)
}
\author{
Jonathan Kennel \email{jkennel@uoguelph.ca}
}
