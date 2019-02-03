#' OpenLayers Text Layer
#'
#' Function to create a Text layer to add to an OpenLayers Map object.
#'
#' This function stores the data required to generate an OpenLayers 
#' vector layer with text features using \code{Point} 
#' geometries.  It does not enable aesthetic mappings to variables.
#'
#' @section Formatting Labels With \code{label.params}:
#' The \code{label.params} parameter provide direct access to OpenLayers
#' feature text styling (see \href{http://geoadmin.github.io/ol3/apidoc/ol.style.Text.html}{OpenLayers Documentation}).
#' Multiple values for any of these properties is not supported.
#' The following ol/style/Text properties are supported:
#' \tabular{ll}{
#' \code{font} \tab character label font CSS string \cr
#' \code{offsetX} \tab numeric label x-offset \cr
#' \code{offsetY} \tab numeric label y-offset \cr
#' \code{rotation} \tab numeric label rotation  \cr
#' \code{textAlign} \tab character label text horizontal alighment \cr
#' \code{textBaseline} \tab  character label text vertical alignment\cr
#' \code{stroke_color} \tab character text color\cr
#' \code{fill_color} \tab  character text fill color\cr
#' }
#'
#' 
#' @param point.obj SpatialPointsDataframe, SpatialPoints, or a matrix 
#' containing columns of point longitudes and latitudes, respectively. 
#' @param label character vector of text labels to put at points.
#' @param name character Layer name.
#' @param toggle.control logical indicating whether this layer will have
#' a visibility toggle.
#' @param tooltip character vector of point feature tooltip popups.
#' @param label.params named list (e.g., \code{list(property=value)}) of 
#' label position and format parameters.  See below. 
#' @param tooltip.params named list (e.g., \code{list(property=value)}) of 
#' tooltip position and format parameters.  See \link{ol_geom_polygon} documentation. 
#'
#' @return A list object of class \code{Layer.Text}.
#'
#' @seealso 
#' \code{\link{ol_map}}, 
#' \code{\link{ol_geom_point}}
#' 
#' @export
#'
#' @examples
#' text.pts <- matrix(
#'     c(
#'         -101.5, 39.2,
#'         -101.1, 54,
#'         -101.1, 21.4
#'     ),
#'     byrow=TRUE,
#'     ncol=2
#' )
#' text.labels <- c("USA","Canada","Mexico")
#' mymap <- ol_map(
#'     center=c(-100,25),
#'     zoom=3
#' ) +
#'     oceanbase() +
#'     ol_geom_text(
#'         text.pts,
#'         text.labels,
#'         toggle.control=TRUE,
#'         label.params=list(
#'             font="16px sans-serif",
#'             stroke_color="#FF0000",
#'             fill_color="#FFFFFF00"
#'         )
#'     )
#' \dontrun{
#' # Write to file and view in browser
#' ol_map2HTML(mymap, "textmap.html")
#' browseURL("textmap.html")
#' }
ol_geom_text <- function(
    point.obj,
    label,
    name=NULL,
    toggle.control=FALSE,
    label.params=list(),
    tooltip=NULL,
    tooltip.params=list()
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
    default.label.params <- list(offsetX=0,offsetY=0,textAlign='center',textBaseline='middle')
    for(lp in setdiff(names(default.label.params),names(label.params))){
        label.params[[lp]]<-default.label.params[[lp]]
    }
    o[['label']] <- get_ol_layer_label(df,list(),label,label.params)
    o[['tooltip']] <- get_ol_layer_tooltip(df,list(),tooltip,tooltip.params)
    class(o) <- "Layer.Text"
    return(o)
}

writeLayer.Layer.Text <- function(layer,suffix="basemap",nice.format=TRUE,initial.indent=6,...){
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
    pts.df <- data.frame(x=layer[['features']][,1],y=layer[['features']][,2])
    write_function(sprintf("var layer_%s = new ol.layer.Vector({",suffix))
    inid <- inid + 2
    write_function(sprintf("name: \"%s\",",gsub('"',"'",layer[['name']])))
    write_function("source: new ol.source.Vector({")
    inid <- inid + 2
    write_function("features: [")
    inid <- inid + 2
    for(i in 1:nrow(pts.df)){
        write_function("new ol.Feature({")
        inid <- inid + 2
        write_function(sprintf("name: \"Text%i\",",i))
        write_tooltip_attr(layer[['tooltip']],i,nice.format=nice.format,initial.indent=inid)
        write_label_attr_vec(layer[["label"]],i,nice.format,inid)
        write_function("geometry: new ol.geom.Point(")
        inid <- inid + 2
        write_function(
            sprintf(
                "%s",
                write_coordinate(as.numeric(pts.df[i,1:2]))
            )
        )
        inid <- inid - 2
        write_function(")")
        inid <- inid - 2
        if(i < nrow(pts.df)){
            write_function("}),")
        } else {
            write_function("})")
        }
    }
    inid <- inid - 2
    write_function("]")
    inid <- inid - 2
    write_function("}),")
    write_function("style: function(feature){")
    inid <- inid + 2
    write_function("var style = new ol.style.Style({")
    inid <- inid + 2
    if(!(is.null(layer[['label']])) && !all(is.na(layer[['label']])) && is.list(layer[['label']])){
        write_function("text: new ol.style.Text({")
        inid <- inid + 2
        write_label_style_obj(layer[['label']],nice.format=nice.format,initial.indent=inid)
        inid <- inid - 2
        write_function(sprintf("}),"))
    }
    inid <- inid - 2
    write_function("});")
    write_function("return style;")
    inid <- inid - 2
    write_function("}")
    inid <- inid - 2
    write_function("});")
    if(nice.format) cat("\n")
}