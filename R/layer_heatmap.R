#' OpenLayers Heatmap Layer
#'
#' Function to create a Heatmap layer to add to an OpenLayers Map object.
#'
#' This function stores the data required to generate an OpenLayers 
#' vector layer with features using \code{Point} 
#' geometries.  
#' See OpenLayers \href{http://geoadmin.github.io/ol3/apidoc/ol.layer.Heatmap.html}{Heatmap Documentation}
#' for details.
#'
#' @section Aesthetics:
#' \itemize{
#' \item \code{fill}
#' \item \code{size}
#' }
#' 
#' @param point.obj SpatialPointsDataframe, SpatialPoints, or a matrix 
#' containing columns of point longitudes and latitudes, respectively. 
#' @param name character Layer name.
#' @param toggle.control logical indicating whether this layer will have
#' a visibility toggle.
#' @param gradient character color gradient of heatmap.  See OpenLayers 
#' \href{http://geoadmin.github.io/ol3/apidoc/ol.layer.Heatmap.html}{Heatmap Documentation}
#' Enclose gradient array in single character string.
#' @param opacity numeric Heatmap opacity.  See OpenLayers 
#' \href{http://geoadmin.github.io/ol3/apidoc/ol.layer.Heatmap.html}{Heatmap Documentation}.
#' @param radius numeric Heatmap radius size in pixels.See OpenLayers 
#' \href{http://geoadmin.github.io/ol3/apidoc/ol.layer.Heatmap.html}{Heatmap Documentation}.
#' @param blur  numeric Heatmap blur.  See OpenLayers 
#' \href{http://geoadmin.github.io/ol3/apidoc/ol.layer.Heatmap.html}{Heatmap Documentation}.
#' @param shadow numeric Heatmap shadow.  See OpenLayers 
#' \href{http://geoadmin.github.io/ol3/apidoc/ol.layer.Heatmap.html}{Heatmap Documentation}.
#' @param weight.values numeric vector of weights to be assigned to the
#' points in \code{point.obj}.  Values should be in [0,1].
#'
#' @return A list object of class \code{Layer.HeatMap}.
#'
#' @seealso 
#' \code{\link{ol_map}}, 
#' \code{\link{ol_geom_polygon}}, 
#' \code{\link{ol_geom_circle}},
#' \code{\link{ol_geom_point}},
#' \code{\link{ol_geom_icon}}
#' 
#' @export
#'
#' @examples
#' heatmap.pts <- matrix(
#'     c(
#'         rnorm(100,-80.385,1), #Miami Longitudes
#'         rnorm(100,-117.1611,3), #San Diego Longitudes
#'         rnorm(100,25.782618,1), #Miami Latitudes
#'         rnorm(100,32.7157,3) # San Diego Latitudes
#'     ),ncol=2
#' )
#' mymap <- ol_map(
#'     center=c(-98.5,28.5),
#'     zoom=4,
#'     map.note="Heatmap of random points centered on Miami and San Diego."
#' ) + 
#'     nga_basemap("WSM") +
#'     ol_geom_heatmap(
#'         heatmap.pts,
#'         name="Random Heatmap",
#'         toggle.control=TRUE,
#'         opacity=0.25
#'         )
#' ## Not run: write to file and view in browser
#' # ol_map2HTML(mymap, "heatmap.html")
#' # browseURL("heatmap.html")
ol_geom_heatmap <- function(
    point.obj,
    name=NULL,
    toggle.control=FALSE,
    gradient=NULL,
    opacity=1,
    radius=8,
    blur=15,
    shadow=250,
    weight.values=NULL
){
    if(class(point.obj)=="SpatialPointsDataFrame"){
        sp.point <- sp::spTransform(point.obj,wgs84.proj4str)
        if(is.null(df)){
            df <- sp.point@data
        }
    } else if(class(point.obj)=="SpatialPoints"){
        sp.point <- sp::spTransform(point.obj,wgs84.proj4str)
    } else {
        sp.point <- sp::SpatialPoints(point.obj,proj4string=wgs84.proj4str)
    }
    o <- list()
    o[["name"]] <- name
    o[['toggle.control']] <- toggle.control
    o[["features"]]<-sp.point@coords
    o[['feature.count']]<-nrow(o[["features"]])
    o[['heatmap.params']] <- list()
    if(!is.null(opacity)){
        o[['heatmap.params']][['opacity']]<-opacity
    }
    if(!is.null(gradient)){
        o[['heatmap.params']][['gradient']]<-gradient
    }
    if(!is.null(radius)){
        o[['heatmap.params']][['radius']]<-radius
    }
    if(!is.null(blur)){
        o[['heatmap.params']][['blur']]<-blur
    }
    if(!is.null(shadow)){
        o[['heatmap.params']][['shadow']]<-shadow
    }
    o[['weight.values']]<-weight.values
    class(o) <- "Layer.HeatMap"
    return(o)
}

writeLayer.Layer.HeatMap <- function(layer,suffix="basemap",nice.format=TRUE,self.contained=TRUE,initial.indent=6,...){
    inid <- initial.indent
    if(nice.format){
        write_function <- function(s){
            cat(paste(strrep(" ",inid),s,"\n",sep=""))
        }
    } else {
        write_function <- function(s){
            cat(s)
        }
    }
    write_function(sprintf("var layer_%s = new ol.layer.Heatmap({",suffix))
    inid <- inid + 2
    write_function(sprintf("name: \"%s\",",gsub('"',"'",layer[['name']])))
    for(n in names(layer[['heatmap.params']])){
        write_function(sprintf("%s: %s,",n,as.character(layer[['heatmap.params']][[n]])))
    }
    write_function("source: new ol.source.Vector({")
    inid <- inid + 2
    write_function("features: [")
    inid <- inid + 2
    for(i in 1:nrow(layer[['features']])){
        write_function("new ol.Feature({")
        inid <- inid + 2
        if(!is.null(layer[['weight.values']])){
            j <- i %% length(layer[['weight.values']])
            if(j==0) j <- length(layer[['weight.values']])
            write_function(sprintf("weight: %1.3f,",layer[['weight.values']][j]))
        }
        write_function(sprintf("name: \"Point%i\",",i))
        write_function("geometry: new ol.geom.Point(")
        inid <- inid + 2
        write_function(
            sprintf(
                "%s",
                write_coordinate(as.numeric(layer[['features']][i,1:2]))
            )
        )
        inid <- inid - 2
        write_function(")")
        inid <- inid - 2
        if(i < nrow(layer[['features']])){
            write_function("}),")
        } else {
            write_function("})")
        }
    }
    inid <- inid - 2
    write_function("]")
    inid <- inid - 2
    write_function("})")
    inid <- inid - 2
    write_function("});")
    if(nice.format) cat("\n")
}
