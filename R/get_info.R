#' @title 
#' Retrieve descriptive information from ENGIE's open wind farm 
#' 
#' @description 
#' The \code{\link[openwindfarm]{get_info}} function downloads the dataset 
#' describing the 34 measurements that compose the . 
#' 
#' @param ...
#' Additional parameters to be passed to \code{\link[httr]{GET}}. 
#' 
#' @return
#' The dataset downloaded is returned invisibly. 
#' 
#' @seealso 
#' \code{\link[openwindfarm]{get_owf}}. 
#' 
#' @import readr
#' @importFrom httr GET stop_for_status
#' @importFrom dplyr arrange
#' @importFrom magrittr %>% 
#' @export
#' 
get_info <- 
function(...)
{
  r <- httr::GET(url_owf(), query = list(dataset = "data_description"), ...)
  httr::stop_for_status(r)
  
  col_types <- readr::cols(.default = readr::col_character())
  df <- readr::read_delim(r$content, delim = ";", col_types = col_types) %>% 
    dplyr::arrange(variable_name)
  names(df) <- capitalize(names(df))
  
  invisible(df)
}
