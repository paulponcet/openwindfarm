
datasets_list <- 
function(y)
{
  now <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  l <- list(c("2009-01-01 00:10:00", "2012-12-31 23:50:00"), 
              c("2013-01-01 00:00:00", "2016-12-31 23:50:00"), 
              c("2017-01-01 00:00:00", as.character(now)))
  l <- lapply(l, FUN = anytime::anytime, tz = "UTC", asUTC = TRUE)
  names(l) <- paste0("la-haute-borne-data-", c("2009-2012", "2013-2016", "2017-2020"))
  l
}
