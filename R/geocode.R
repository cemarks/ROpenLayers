#' Geocode an Address
#'
#' Get Lat/Lon Cooridinates for an Address.
#'
#' This function uses the geocoding utility at
#' \href{http://origin-services.gvs.nga.smil.mil/arcgis/rest/services/Locators/CompositeLocator/GeoCodeServer/}{NGA.smil.mil}
#' to get coordinates for an address or list of addresses.
#'
#' @param address.strings character vector of addresses to geocode.
#'
#' @return A data frame with a \code{location} column providing latitude and longitude values for
#' each geo-coded location.
#'
#' @seealso \code{\link{ol_geom_point}},
#' \code{\link{ol_geom_icon}},
#'
#' @export
#'
#' @examples
#' addresses <- c(
#'     "1600 Pennsylvania Ave NW, Washington, DC 20500",
#'     "One 1st ST NE, Washington, DC  20543"
#' )
#' \dontrun{
#' g <- geocode(addresses)
#' point.matrix <- g$location
#' point.df <- data.frame(
#'     pt.type=c("White House","Supreme Court")
#' )
#' mymap <- ol_map(
#'     center=c(-77.03196,38.89037),
#'    zoom=12
#' ) +
#'     streetmap() +
#' ol_geom_point(
#'     point.matrix,
#'     name="Points of Interest",
#'     marker="pin",
#'     toggle.control=TRUE,
#'     tooltip=point.df$pt.type
#' )
#'
#' # Output to file and view
#' # ol_map2HTML(mymap,'map.html')
#' # browseURL('map.html')
#' }
geocode <- function(address.strings){
  if(requireNamespace("httr",quietly=TRUE)){
    url <- "http://origin-services.gvs.nga.smil.mil/arcgis/rest/services/Locators/CompositeLocator/GeoCodeServer/geocodeAddresses"
    query.url <- paste(
      url,
      "?addresses={\"records\":[",
      paste(
        "{\"attributes\": {\"OBJECTID\":",
        1:length(address.strings),
        ",\"SingleLine\":\"",
        address.strings,
        "\"}}",
        sep="",
        collapse=","
      ),
      "]}&f=json",
      sep=""
    )
    query.encode <- utils::URLencode(query.url)
    result <- httr::GET(query.encode)
    con <- jsonlite::fromJSON(httr::content(result,as='text'))
    locs <- con$locations
    names(locs$location)<-c("lon","lat")
    return(locs)
  } else {
    stop("Use of geo-code requires installation of package 'httr.'")
  }
}
