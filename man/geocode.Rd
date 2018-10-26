% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocode.R
\name{geocode}
\alias{geocode}
\title{Geocode an Address}
\usage{
geocode(address.strings)
}
\arguments{
\item{address.strings}{character vector of addresses to geocode.}
}
\value{
A data frame with a \code{location} column providing latitude and longitude values for
each geo-coded location.
}
\description{
Get Lat/Lon Cooridinates for an Address.
}
\details{
This function uses the geocoding utility at
\href{http://origin-services.gvs.nga.smil.mil/arcgis/rest/services/Locators/CompositeLocator/GeoCodeServer/}{NGA.smil.mil}
to get coordinates for an address or list of addresses.
}
\examples{
addresses <- c(
    "1600 Pennsylvania Ave NW, Washington, DC 20500",
    "1 1st ST NE, Washington, DC  20543"
)
## Not Run for NIPR Compile
# g <- geocode(addresses)
# point.matrix <- g$location
# point.df <- data.frame(
#     pt.type=c("White House","Supreme Court")
# )
# mymap <- ol_map(
#     center=c(-77.03196,38.89037),
#    zoom=12
# ) +
 #    nga_basemap("WSM") +
# ol_geom_point(
 #    point.matrix,
 #    name="Points of Interest",
 #    marker="pin",
 #    toggle.control=TRUE,
 #    tooltip=point.df$pt.type
# )

## Not Run: output to file and view
# ol_map2HTML(mymap,'map.html')
# browseURL('map.html')
}
\seealso{
\code{\link{ol_geom_point}},
\code{\link{ol_geom_icon}},
}