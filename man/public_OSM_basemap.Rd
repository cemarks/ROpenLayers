% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basemaps.R, R/swrap.R
\name{public_OSM_basemap}
\alias{public_OSM_basemap}
\alias{streetmap}
\title{Public OpenStreetMap Basemap Layer}
\usage{
public_OSM_basemap(name = NULL, toggle.control = FALSE)

streetmap(toggle.control = FALSE)
}
\arguments{
\item{name}{character layer name.}

\item{toggle.control}{logical.  If \code{TRUE}, a checkbox will appear on the
map allowing the viewer to toggle its visibility in the browser.}
}
\value{
A \code{Layer.ArcGIS} S3 object.
}
\description{
Create a basemap layer linking to \href{https://www.openstreetmap.org}{OpenStreetMap}.
}
\details{
Creates and returns an OpenLayers OpenStreetMap Tile layer.
}
\section{Functions}{
\itemize{
\item \code{streetmap}: Call \code{public_OSM_basemap} with default
settings.
}}

\examples{
mymap <- ol_map()
base.layer <- public_OSM_basemap()
mymap <- mymap + base.layer
\dontrun{
ol_map2HTML(mymap,"SanDiego_OSM.html")
browseURL("SanDiego_OSM.html")
}
}
\seealso{
\code{\link{ol_map}}, 
\code{\link{+.Ol.Map}}, 
\code{\link{nga_basemap}}, 
\code{\link{public_arcgis_basemap}}, 
\code{\link{user_arcgis_basemap}}
}
