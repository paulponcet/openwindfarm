#' @title
#' Retrieve Scada data from ENGIE's open wind farm
#'
#' @description
#' The wind farm called `La Haute Borne' is located in the Meuse department in France,
#' and is made of four Senvion MM82 wind turbines commissioned in January, 15th, 2009.
#'
#' In addition to the columns 'Wind_turbine_name' and the 'Date_time',
#' the dataset is made of 136 columns, which record every 10 minutes the average, the minimum,
#' the maximum, and the standard deviation over 10 minutes of 34 measurements
#' related to the operational behavior of the 4 wind turbines.
#'
#' The dataset describing these 34 measurements can be retrieved with the
#' \code{\link[openwindfarm]{get_info}} function.
#'
#' @note
#' The dataset (not directly part of the \pkg{openwindfarm} package) is distributed
#' by \href{http://www.engie.com}{the ENGIE Group} under the terms of the
#' \href{https://www.etalab.gouv.fr/wp-content/uploads/2017/04/ETALAB-Licence-Ouverte-v2.0.pdf}{Open Licence 2.0},
#' provided by \href{https://www.etalab.gouv.fr/en/qui-sommes-nous}{Etalab} and
#' designed to be compatible notably with the "Creative Commons Attribution 4.0"
#' (CC-BY 4.0) license of Creative Commons.
#'
#' @param wind_turbine
#' character or numeric. The name or the number of the desired wind turbine,
#' among the four wind turbines available called \code{"R80711"} (or \code{1}),
#' \code{"R80790"} (or \code{2}), \code{"R80721"} (or \code{3}),
#' and \code{"R80736"} (or \code{4}).
#'
#' @param start_date,end_date
#' character or \code{\link{POSIXct}}.
#' Can have any format recognized by the \code{\link{anytime}} function.
#' Should be between \code{"2009-01-01 00:10:00"} and the current date.
#'
#' @param ...
#' Additional parameters to be passed to \code{\link[httr]{GET}}.
#'
#' @return
#' The dataset downloaded is returned invisibly.
#'
#' @seealso
#' \code{\link[openwindfarm]{get_info}}.
#'
#' @import httr
#' @import readr
#' @importFrom anytime anytime
#' @importFrom dplyr arrange rename
#' @importFrom foreach foreach %do%
#' @importFrom magrittr %>%
#' @export
#'
get_owf <-
function(wind_turbine = c("R80711", "R80790", "R80721", "R80736"),
         start_date,
         end_date,
         ...)
{
  wind_turbine_values <- c("R80711", "R80790", "R80721", "R80736")
  if (is.numeric(wind_turbine)) wind_turbine <- wind_turbine_values[wind_turbine]
  wind_turbine <- match.arg(wind_turbine, wind_turbine_values)

  start_date <- anytime::anytime(start_date, tz = "UTC", asUTC = TRUE)
  end_date <- anytime::anytime(end_date, tz = "UTC", asUTC = TRUE)

  ds <- datasets_list()
  n  <- length(ds)
  stopifnot(start_date <= end_date)
  stopifnot(end_date > ds[[1L]][1L])
  stopifnot(start_date <= ds[[n]][2L])

  rm_ds <- c()
  for (i in seq_len(n)) {
    if (start_date > ds[[i]][2L]) {
      rm_ds <- c(rm_ds, i)
    } else {
      ds[[i]][1L] <- max(ds[[i]][1L], start_date)
    }
    if (end_date <= ds[[i]][2L]) {
      ds[[i]][2L] <- max(ds[[i]][1L], end_date)
      if (i < n) rm_ds <- c(rm_ds, i+1L)
    }
  }
  ds <- ds[setdiff(seq_along(ds), rm_ds)]

  df <- foreach::foreach(d = names(ds), .combine = rbind) %do% {
    a = gsub(" ", "T", as.character(ds[[d]][1L]))
    b = gsub(" ", "T", as.character(ds[[d]][2L]))
    r <- httr::GET(url_owf(),
                   query = list(dataset = d,
                                facet = "wind_turbine_name",
                                refine.wind_turbine_name = wind_turbine,
                                q = paste0("date_time >= ", a, " AND date_time <= ", b)),
                   ...)
    httr::stop_for_status(r)
    read_owf(r$content)
  }

  df <- df %>%
    dplyr::rename(DCs_avg = dcs_avg,
                  DCs_min = dcs_min,
                  DCs_max = dcs_max,
                  DCs_std = dcs_std) %>%
    dplyr::arrange(wind_turbine_name, date_time)
  names(df) <- capitalize(names(df))

  invisible(df)
}
