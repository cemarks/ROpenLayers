#' Geocode an Address
#'
#' Get Lat/Lon Cooridinates for an Address.
#'
#' This function uses the \code{findAddressCandidates} utility at
#' \href{https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer}{Arcgis.com}
#' to get coordinates for a single address.
#'
#' @param address.string character vector of addresses to geocode.
#'
#' @return A data frame with probable address locations. 
#'
#' @seealso \code{\link{ol_geom_point}},
#' \code{\link{ol_geom_icon}},
#'
#' @export
#'
#' @examples
#' address <- "1600 Pennsylvania Ave NW, Washington, DC 20500"

#' \dontrun{
#' g <- geocode(address)
#' point.matrix <- matrix(
#'   c(
#'     as.numeric(g$location[1,1]),
#'     as.numeric(g$location[1,2])
#'   ),
#'   nrow=1
#' )
#' point.df <- data.frame(
#'     pt.type="White House"
#' )
#' mymap <- ol_map(
#'    center=c(-77.03196,38.89037),
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
#' ol_map2HTML(mymap,'map.html')
#' browseURL('map.html')
#' }
geocode <- function(address.string){
  if(requireNamespace("httr",quietly=TRUE)){
    url <- "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates"
    query.url <- paste(
      url,
      sprintf(
        "?SingleLine=%s",
        address.string
      ),
      "&f=json",
      sep=""
    )
    query.encode <- utils::URLencode(query.url)
    result <- httr::GET(query.encode)
    con <- jsonlite::fromJSON(httr::content(result,as='text'))
    return(con$candidates)
  } else {
    stop("Use of geo-code requires installation of package 'httr.'")
  }
}
