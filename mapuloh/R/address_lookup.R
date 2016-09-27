#' @title Address finder
#'
#' @description The 'address_lookup' function is a function that provides the user the address of a given
#' a set of coordinates The funtion uses the google geocode API to retrieve the result.
#'
#' @param latlong A character string that includes the latitude and the longitude.
#'
#' @return A dataframe cointaining Full address names as well as the coordinates
#'
#' @export
address_lookup <- function(latlong = NULL){
  require("httr")
  #Intial
  if(is.null(latlong) | !is.character(latlong)){
    stop("Use character values")
  }

  #Creating query and using GET verb
  latlong <- gsub("[[:space:]]",",",latlong)
  url <- "https://maps.googleapis.com/maps/api/geocode/json?latlng="
  key <- "&key=AIzaSyCcRPdN_sAcwiovz7EPAq31l5cFIxp-aW4"
  url <- paste0(url,latlong,key)
  get_res <- httr::GET(url)

  #Checking for status code
  if(!grepl("^2",httr::status_code(get_res))){
    stop(httr::message_for_status(get_res))
  } else {


    #Handlig result and returning
    content_res <- httr::content(get_res)
    geometry_res <- lapply(content_res[["results"]], "[[", "geometry")
    coord <- as.data.frame(t(sapply(geometry_res, "[[", "location")))
    full_adress <- unlist(lapply(content_res[["results"]], "[[", "formatted_address"))

    res_df <- data.frame("Full_address" = full_adress,
                         "lat" = unlist(coord[,1]),
                         "lng" = unlist(coord[,2]),
                         stringsAsFactors = FALSE)

    rownames(res_df) <- NULL

    return(res_df)
  }

}
