% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ol_map_base.R
\name{+.Ol.Map}
\alias{+.Ol.Map}
\title{+.Ol.Map}
\usage{
\method{+}{Ol.Map}(ol.map.obj, other.obj)
}
\arguments{
\item{ol.map.obj}{S3 object of class \code{Ol.Map}.}

\item{other.obj}{A map layer or scale component.}
}
\value{
Ol.Map object with updated layers or scales.
}
\description{
Add components to a OpenLayers Map.
}
\details{
Similar to the \code{ggplot2} package, + provides functionality to add layers to
an existing OpenLayers Map object.
Layers are simply appended to the Ol.Map objects layers list.
When adding scales, this method searches
through map layers in reverse order for scales with matching aesthetics.  When
a matching scale is found, it is updated according to the parameters of the
added scale.
In general, continuous scales can be coerced into discrete scales.
}
\section{What can you add?}{

You can add the following types of objects:
\itemize{
\item A layer object generated by one of the \code{ol_geom_*} layer functions.
\item A scale object generated by one of the \code{ol_scale_*} functions.
}
}

\examples{
mymap <- ol_map()
base.layer <- lightgray()
mymap <- mymap + base.layer
\dontrun{
ol_map2HTML(mymap,"SanDiego.html")
browseURL("SanDiego.html")
}
}
\seealso{
\code{\link{ol_map}}
}
