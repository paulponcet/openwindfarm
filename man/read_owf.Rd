% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_owf.R
\name{read_owf}
\alias{read_owf}
\title{Read a csv file containing the open wind farm data}
\usage{
read_owf(file)
}
\arguments{
\item{file}{character. One of \code{"la-haute-borne-data-2009-2012.csv"},
\code{"la-haute-borne-data-2013-2016.csv"}, \code{"la-haute-borne-data-2017-2020.csv"}.}
}
\value{
The dataset read from the csv file.
}
\description{
This function is made for users who would like to download directly datasets
as csv files from the web site \url{https://opendata-renewables.engie.com/} (instead of
using the API though the \code{\link[openwindfarm]{get_owf}} function).
}
