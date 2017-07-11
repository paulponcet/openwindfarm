#' @title
#' Read a csv file containing the open wind farm data
#'
#' @description
#' This function is made for users who would like to download directly datasets
#' as csv files from the web site \url{https://opendata-renewables.engie.com/} (instead of
#' using the API though the \code{\link[openwindfarm]{get_owf}} function).
#'
#' @param file
#' character. One of \code{"la-haute-borne-data-2009-2012.csv"},
#' \code{"la-haute-borne-data-2013-2016.csv"}, \code{"la-haute-borne-data-2017-2020.csv"}.
#'
#' @return
#' The dataset read from the csv file.
#'
#' @import readr
#' @export
#'
read_owf <-
function(file)
{
  col_types <- readr::cols(
    .default = readr::col_double(),
    wind_turbine_name = readr::col_character(),
    date_time = readr::col_datetime(format = ""))
  readr::read_delim(file, delim = ";", col_types = col_types)
}
