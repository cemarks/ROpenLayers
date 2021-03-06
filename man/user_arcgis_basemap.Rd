% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basemaps.R
\name{user_arcgis_basemap}
\alias{user_arcgis_basemap}
\title{User ArcGIS Basemap Layer}
\usage{
user_arcgis_basemap(url, name = "", attributions = "",
  toggle.control = FALSE)
}
\arguments{
\item{url}{character url string where the map server is located.  Typically these
urls end with "/MapServer".}

\item{name}{character layer name.}

\item{attributions}{character HTML.  This HTML will render as attributional text at the
bottom-right corner of the map.  At a minimum, this text should include the
copyright text provided on the map server.}

\item{toggle.control}{logical.  If \code{TRUE}, a checkbox will appear on the
map allowing the viewer to toggle its visibility in the browser.}
}
\value{
A \code{Layer.ArcGIS} S3 object.
}
\description{
Create a basemap layer linking to an User-supplied ArcGIS mapserver.
}
\details{
Creates and returns an OpenLayers ArcGIS Tile layer that sources a
map server at a user-supplied URL.
}
\examples{
server.url <- "http://server.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer"
mymap <- ol_map()
attrib <- paste(
    "Content may not reflect National Geographic's current map policy.",
    "Sources: National Geographic, Esri, Garmin, HERE,",
    "UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, increment P Corp",
    sep=" " # long attribution!
)
base.layer <- user_arcgis_basemap(
    server.url,
    attributions = attrib,
    toggle.control=TRUE
)
mymap <- mymap + base.layer
\dontrun{
ol_map2HTML(
  mymap,
  "SanDiego_NatGeo.html",
  map.note = sprintf(
    "I found this at <a href='\%s'>arcgisonline.com</a>",
    server.url
  )
)
browseURL("SanDiego_NatGeo.html")
}
}
\seealso{
\code{\link{ol_map}}, 
\code{\link{+.Ol.Map}}, 
\code{\link{nga_basemap}}, 
\code{\link{public_OSM_basemap}}, 
\code{\link{public_arcgis_basemap}}
}
