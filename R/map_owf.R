#' @title
#' Map of the wind farm
#'
#' @description
#' This function plots the map of the wind farm
#' with the names of the four wind turbines.
#'
#' @import leaflet
#' @importFrom magrittr %>%
#' @export
#'
map_owf <-
function()
{
  lat <- c(48.4461, 48.4569, 48.4536, 48.4497)
  lng <- c(5.5925, 5.5847, 5.5875, 5.5869)
  wts <- c("R80736", "R80711", "R80790", "R80721")

  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPopups(lng = lng, lat = lat, popup = wts) %>%
    leaflet::setView(mean(lng), mean(lat), zoom = 14)
}
