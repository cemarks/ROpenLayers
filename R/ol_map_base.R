#' ROpenLayers: A pacakge for Geo-Visualization
#'
#' ROpenLayers leverages the power of OpenLayers javascript libraries
#' and web-based Mapservers to enable informative visualization.
#'
#' @section What this package does:
#' The purpose of this package is to make it easy for a user to visualize
#' geo-spatial data and analyses using the open source
#' \href{https://openlayers.org/}{OpenLayers} javascript library and
#' online map servers.  The process for creating a visualization
#' imitates the process of creating a plot in R package
#' \href{https://ggplot2.tidyverse.org/}{\code{ggplot2}}.
#' \enumerate{
#' \item First, an OpenLayers Map object is created with a
#' call to the \code{\link{ol_map}} method.
#' \item Next, layers and scales are created and added.  Layers can reference
#' map servers to provide underlying base maps or vector features
#' (polygons, lines, or points) created or imported in R.  These capabilities
#' are described in the following sections.
#' \item Finally, the updated map object is exported to HTML/javascript for
#' viewing in a browser, hosting on a server, or embedding into another
#' application or format.  Export methods are \code{\link{ol_map2HTML}} and
#' \code{\link{ol_map2Strings}}.
#' }
#'
#' @section OpenLayers:
#' \href{https://openlayers.org}{OpenLayers}
#'  is an open source javascript library that makes it easy to put a
#' dynamic map on any web page.  It is licensed under the 2-clause BSD
#' license (see \href{https://github.com/openlayers/openlayers/blob/master/LICENSE.md}{OpenLayers Licence}).
#' This license will appear commented within OpenLayers CSS code in the
#' HTML exports created by this package. However, this package does not
#' contain any of the OpenLayers javascript source code; rather, it exports
#' HTML code that source these libraries when loaded.  Therefore, these
#' products will not render without network access to the OpenLayers
#' javascript library.  By default, the products exported by this package
#' source OpenLayers 3.16.0, but the user has the option to set the
#' source URL (see \code{\link{ol_map}}).
#'
#' @section Public ArcGIS Servers:
#' \href{https://www.arcgis.com/index.html}{ESRI ArcGIS} hosts several
#' publicly available map servers at
#' \href{https://server.arcgisonline.com/arcgis/rest/services}{arcgisonline.com},
#'  which can accessed via REST APIs and rendered using OpenLayers javascript methods.
#' A subset of these are made available in this package through the
#' \code{\link{public_arcgis_basemap}} method.  Alternatively, a user can
#' specify any ArcGIS map server using the \code{\link{user_arcgis_basemap}}
#' method.  Note that while these maps servers are publicly available, they are
#' not necessarily open-licensed.  Users must ensure they comply with each
#' map server's license and terms of use.
#'
#' @section OpenStreetMap:
#' \href{https://www.openstreetmap.org/}{OpenStreetMap} also hosts a public and
#' open license map server that can be imported as a layer using OpenLayers.
#' See \code{\link{public_OSM_basemap}}.
#'
#' @section Other Servers:
#' As stated above, the \code{\link{user_arcgis_basemap}} method allows the user
#' to manually specify any available ArcGIS map server.  This package also provides
#' access to US National Geospatial-Intelligence Agency servers hosted
#' at \href{https://www.nga.mil}{NGA.mil} through the \code{\link{nga_basemap}}
#' method.  Note that these servers require authentication, which will be requested
#' at the time of access (i.e., when the HTML page is opened in a browser).
#'
#' @section Vector Layers:
#' This package enables users to rapidly access and write OpenLayers
#' vector layers in javascript.  The following methods enable that functionality.
#' \itemize{
#' \item \code{\link{ol_geom_polygon}}
#' \item \code{\link{ol_geom_line}}
#' \item \code{\link{ol_geom_point}}
#' \item \code{\link{ol_geom_icon}}
#' \item \code{\link{ol_geom_circle}}
#' \item \code{\link{ol_geom_heatmap}}
#' \item \code{\link{ol_geom_text}}
#' }
#'
#' @section Geocode:
#' New in version 1.0.0--geocode addresses using \href{http://www.arcgis.com}{ArcGIS}
#' geocoding service.  See \code{\link{geocode}}.
#' 
#' @docType package
#' @name ROpenLayers
#' @examples
#' data(quakes)
#' center <- c(mean(quakes$long),mean(quakes$lat))
#' quakes$long[which(quakes$long>180)]<-quakes$long[which(quakes$long>180)]-360
#' tooltips <- paste("Depth",quakes$depth,sep=": ")
#' mymap <- ol_map(
#'     zoom = 5,
#'     center = center
#' )
#' basemap.layer <- oceanbase()
#' point.layer <- ol_geom_point(
#'     quakes[,c("long","lat")],
#'     mapping = ol_aes(fill=mag),
#'     df = quakes,
#'     name = "Earthquake Points",
#'     toggle.control=TRUE,
#'     tooltip = tooltips
#' )
#' heatmap.layer <- ol_geom_heatmap(
#'     quakes[,c("long","lat")],
#'     name = "Earthquake Heatmap",
#'     toggle.control=TRUE,
#'     weight.values = quakes$mag,
#'     opacity = 0.25
#' )
#' mymap <- mymap +
#'     basemap.layer +
#'     point.layer +
#'     ol_scale_fill_continuous(name="Magnitude",display=TRUE) +
#'     heatmap.layer
#' \dontrun{
#' # Save to file and open in browser
#' ol_map2HTML(
#'   mymap,
#'   "Quakes.html",
#'   map.heading = "Earthquake Data Visualization"
#' )
#' browseURL("Quakes.html")
#' }
NULL






#'
#' OpenLayers Map
#'
#' Create an OpenLayers Map Object.
#'
#' This function creates a new S3 OpenLayers Map object with no layers.
#' If \code{ol.source.url} is \code{NULL} and \code{nga.olsource} is
#' \code{FALSE}, OpenLayers Javascript source will be embedded directly
#' into the HTML when \code{\link{ol_map2HTML}} or
#' \code{\link{ol_map2Strings}} is called.
#' Otherwise, the output HTML/Javascript with source the OpenLayers library
#' according to the value of \code{ol.source.url}, or the NGA hosted
#' OpenLayers library if \code{nga.olsource} is \code{TRUE}.
#'
#' @param zoom integer map initial zoom level.
#' @param center numeric vector of length 2 containing decimal longitude and
#' latitude coordinates for initial map center.
#'
#' @return A list object of class \code{Ol.Map}.
#'
#' @seealso \code{\link{ol_map2HTML}},
#' \code{\link{ol_map2Strings}},
#' \code{\link{public_OSM_basemap}},
#' \code{\link{nga_basemap}},
#' \code{\link{public_arcgis_basemap}},
#' \code{\link{user_arcgis_basemap}}
#'
#' @export
#'
#' @examples
#' miami.OSM.basemap <- ol_map(
#'     center=c(-80.385790,25.782618),
#'     zoom=9
#'     ) +
#'    streetmap()
#' \dontrun{
#' ol_map2HTML(
#'   miami.OSM.basemap,
#'   'miami.html',
#'   map.heading="Miami, FL"
#' )
#' browseURL("miami.html")
#' }
ol_map <- function(
    zoom=10,
    center=c(-117.1611,32.7157)
){
    toggle.control.df <- data.frame(matrix(nrow=0,ncol=3))
    names(toggle.control.df) <- c("layer.id","layer.var","name")
    o = list(
        center=center,
        zoom=zoom,
        layer.control.df=toggle.control.df,
        tooltips=FALSE,
        tooltips.param.vector=c(
            fill_color=hex2rgba_func('#105555B0'),
            border='solid black 1px',
            borderradius="3px",
            font=NULL,
            padding="3px",
            stroke_color=hex2rgba_func("#000000"),
            offsetX=6,
            offsetY=0,
            positioning="bottom-left"
        ),
        layers=list()
    )
    class(o) <- 'Ol.Map'
    return(o)
}


#' +.Ol.Map
#'
#' Add components to a OpenLayers Map.
#'
#' Similar to the \code{ggplot2} package, + provides functionality to add layers to
#' an existing OpenLayers Map object.
#' Layers are simply appended to the Ol.Map objects layers list.
#' When adding scales, this method searches
#' through map layers in reverse order for scales with matching aesthetics.  When
#' a matching scale is found, it is updated according to the parameters of the
#' added scale.
#' In general, continuous scales can be coerced into discrete scales.
#'
#' @section What can you add?:
#' You can add the following types of objects:
#' \itemize{
#' \item A layer object generated by one of the \code{ol_geom_*} layer functions.
#' \item A scale object generated by one of the \code{ol_scale_*} functions.
#' }
#'
#' @param ol.map.obj S3 object of class \code{Ol.Map}.
#' @param other.obj A map layer or scale component.
#'
#' @return Ol.Map object with updated layers or scales.
#'
#' @seealso
#' \code{\link{ol_map}}
#'
#' @export
#'
#' @examples
#' mymap <- ol_map()
#' base.layer <- lightgray()
#' mymap <- mymap + base.layer
#' \dontrun{
#' ol_map2HTML(mymap,"SanDiego.html")
#' browseURL("SanDiego.html")
#' }
`+.Ol.Map` <- function(ol.map.obj,other.obj){
    cl <- class(other.obj)
    cl.split <- strsplit(cl,".",fixed=TRUE)[[1]]
    class.1 <- cl.split[1]
    class.2 <- cl.split[2]
    if(class.1=='Layer'){
        len <- length(ol.map.obj[['layers']])
        if(is.null(other.obj[['name']]) || other.obj[['name']]==""){
            other.obj[['name']] <- sprintf("Layer %i",(len+1))
        }
        ol.map.obj[['layers']][[as.character(len+1)]] <- other.obj
        for(i in 1:length(other.obj[['scale']])){
            if(!is.null(other.obj[['scale']][[i]][['display']]) && other.obj[['scale']][[i]][['display']]  && other.obj[['scale']][[i]][['type']] != 'fixed'){
                new.img <- draw_scale(other.obj[['scale']][[i]])
                ol.map.obj[['scale.container']]<-TRUE
                ol.map.obj[['scale.img.vector']]<-c(ol.map.obj[['scale.img.vector']],new.img)
            }
        }
        if(!(is.null(other.obj[['toggle.control']])) && other.obj[['toggle.control']]){
            ol.map.obj[['layer.control.df']] <- rbind(
                ol.map.obj[['layer.control.df']],
                data.frame(
                    layer.id=as.character(len+1),
                    layer.var=paste("layer",as.character(len+1),sep="_"),
                    name=other.obj[['name']],
                    stringsAsFactors=FALSE
                )
            )
        }
        if(!(is.null(other.obj[['tooltip']]))){
            ol.map.obj[['tooltips']]<-TRUE
            for(tooltip.property in c(
                "fill_color",
                "border",
                "borderradius",
                "font",
                "padding",
                "stroke_color",
                "offsetX",
                "offsetY",
                "positioning")
            ){
                if(!is.null(other.obj[['tooltip']][[tooltip.property]])){
                    ol.map.obj[['tooltips.param.vector']][[tooltip.property]] <- other.obj[['tooltip']][[tooltip.property]]
                }
            }
        }
    } else if (class.1== 'Scale' && class.2 == 'Fixed'){
        nl <- length(ol.map.obj[["layers"]])
        i <- nl
        scale.attr <- other.obj[["attribute"]]
        layer.ind <- NA
        while(is.na(layer.ind) && i > 0){
            if('scale' %in% names(ol.map.obj[['layers']][[i]])){
                layer.scale.attrs <- sapply(
                    1:length(ol.map.obj[['layers']][[i]][['scale']]),
                    function(j) return(ol.map.obj[['layers']][[i]][['scale']][[j]][['attribute']])
                )
                if(scale.attr %in% layer.scale.attrs){
                    w <- which(layer.scale.attrs==scale.attr)
                    ol.map.obj[['layers']][[i]][['scale']][[w]] <- other.obj
                    layer.ind <- i
                }
            }
            i <- i-1
        }
        if (i==0) warning(sprintf("Unable to find matching fixed scale in Ol.Map object. No %s scale assigned.",scale.attr))
    } else if(class.1 == 'Scale'){
        nl <- length(ol.map.obj[["layers"]])
        i <- nl
        scale.attr <- other.obj[["attribute"]]
        layer.ind <- NA
        while(is.na(layer.ind) && i > 0){
            if('scale' %in% names(ol.map.obj[['layers']][[i]])){
                layer.scale.attrs <- sapply(
                    1:length(ol.map.obj[['layers']][[i]][['scale']]),
                    function(j) return(ol.map.obj[['layers']][[i]][['scale']][[j]][['attribute']])
                )
                if(scale.attr %in% layer.scale.attrs){
                    w <- which(layer.scale.attrs==scale.attr)
                    layer.scale <- ol.map.obj[['layers']][[i]][['scale']][[w]]
                    scale.values <- layer.scale[['variable.values']]
                    if(is.null(other.obj[['name']]) || is.na(other.obj[['name']])){
                        scale.name <- layer.scale[['name']]
                    } else {
                        scale.name <- other.obj[['name']]
                    }
                    if(!is.null(scale.values) && length(scale.values)==ol.map.obj[['layers']][[i]][['feature.count']]){
                        if(ol.map.obj[['layers']][[i]][['scale']][[w]][['type']]=='discrete' && other.obj[['type']]=='continuous'){
                            stop("Cannot coerce discrete scale to continuous scale.")
                        } else if(ol.map.obj[['layers']][[i]][['scale']][[w]][['type']]=='continuous' && other.obj[['type']]=='discrete'){
                            unique.vals <- sort(unique(stats::na.omit(ol.map.obj[['layers']][[i]][['scale']][[w]][['variable.values']])))
                            ol.map.obj[['layers']][[i]][['scale']][[w]][['vector']] <- sapply(unique.vals,ol.map.obj[['layers']][[i]][['scale']][[w]][['function']])
                            names(ol.map.obj[['layers']][[i]][['scale']][[w]][['vector']]) <- as.character(unique.vals)
                        }
                        for(nme in names(other.obj)){
                            if(!is.null(other.obj[[nme]])){
                                ol.map.obj[['layers']][[i]][['scale']][[w]][[nme]] <- other.obj[[nme]]
                            }
                            class(ol.map.obj[['layers']][[i]][['scale']][[w]])<-class(other.obj)
                        }
                        layer.ind <- i
                        if(class.2=="IconImage"){
                            if(length(other.obj[['vector']]) > 0){
                                for(j in 1:length(other.obj[['vector']])){
                                    img.src <- other.obj[['vector']][[j]]
                                    if(!(img.src %in% ol.map.obj[['layers']][[i]][['icons']]$src)){
                                        if(!is.null(other.obj[['icon.width']])){
                                            z.ind <- j %% length(other.obj[['icon.width']])
                                            if(z.ind==0) z.ind <- length(other.obj[['icon.width']])
                                            image.width <- image_width(img.src)
                                            image.scalar <- other.obj[['icon.width']][z.ind]/image.width
                                            d <- data.frame(src=img.src,scalar=image.scalar,width=image.width,stringsAsFactors=FALSE)
                                            ol.map.obj[['layers']][[i]][['icons']] <- rbind(ol.map.obj[['layers']][[i]][['icons']],d)
                                            for(k in 1:length(ol.map.obj[['layers']][[i]][['scale']])){
                                                ol.map.obj[['layers']][[i]][['scale']][[k]][['icons']] <- rbind(ol.map.obj[['layers']][[i]][['scale']][[k]][['icons']],d)
                                            }
                                        } else {
                                            z.ind <- 1
                                            image.width <- image_width(img.src)
                                            image.scalar <- ol.map.obj[['layers']][[i]][['target.icon.width']][z.ind]/image.width
                                            d <- data.frame(src=img.src,scalar=image.scalar,width=image.width,stringsAsFactors=FALSE)
                                            ol.map.obj[['layers']][[i]][['icons']] <- rbind(ol.map.obj[['layers']][[i]][['icons']],d)
                                            for(k in 1:length(ol.map.obj[['layers']][[i]][['scale']])){
                                                ol.map.obj[['layers']][[i]][['scale']][[k]][['icons']] <- rbind(ol.map.obj[['layers']][[i]][['scale']][[k]][['icons']],d)
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        i <- i-1
                    }
                } else {
                    i <- i - 1
                }
            } else {
                i <- i - 1
            }
        }
        if (i==0) warning(sprintf("Unable to find matching scale in Ol.Map object. No %s scale assigned.",scale.attr))
    } else {
        stop("Incorrect object class for `+.Ol.Map`")
    }
    return(ol.map.obj)
}



#' Aesthetic Mappings
#'
#' Map variables to layer aesthetics.
#'
#' This function replicates a subset of the functionality of the ggplot2 \code{aes} function.
#' It \emph{does not} allow for variable transformations or functions of multpile varuables.
#' These operations must be completed a priori by the user.
#'
#' @param ... comma-separated mappings of the form 'aesthetic=variable'.  Available aesthetics for mapping
#' are layer specific and are listed in the documentation for each layer type.  Unavailable or
#' unrecognized aesthetics are ignored.  Variables must correspond to names in the layer's input data.frame,
#' otherwise an error is thrown.
#'
#' @return A list of aesthetic mappings.
#'
#' @seealso
#' \code{\link{ol_geom_polygon}},
#' \code{\link{ol_geom_line}},
#' \code{\link{ol_geom_point}},
#' \code{\link{ol_geom_icon}},
#' \code{\link{ol_geom_circle}}
#'
#' @export
#'
#' @examples
#' polygon.matrix1 <- matrix(
#'     c(
#'         -80.385+c(0,0.05,0.05,0,0),
#'         25.782618+c(0,0,0.05,0.05,0)
#'     ),
#'     ncol=2
#' )
#' polygon.matrix2 <- matrix(
#'     c(
#'         -80.34+c(0,0.05,0.025,0),
#'         25.73++c(0,0,0.025*sqrt(3),0)
#'     ),
#'     ncol=2
#' )
#' polygon.list<-list(polygon.matrix1,polygon.matrix2)
#' polygon.df <- data.frame(shape=c("rectangle","triangle"),no=c(1,2))
#' miami.OSM.basemap <- ol_map(
#'     center=c(-80.385790,25.782618),
#'     zoom=9
#'     ) +
#'    streetmap()
#' polygon.layer <- ol_geom_polygon(
#'     polygon.list,
#'     mapping=ol_aes(
#'         fill=no,
#'         lwd=shape
#'     ),
#'     df=polygon.df,
#'     name="Miami Polygons",
#'     toggle.control=TRUE,
#'     tooltip=polygon.df$no
#' )
#' polygon.fill.scale <- ol_scale_fill_discrete(
#'     c("1"="red","2"="green"),
#'     opacity=0.5,
#'     display=TRUE,
#'     name="Number"
#' )
#' polygon.linewidth.scale <- ol_scale_lwd_discrete(
#'     display=TRUE,
#'     name="Shape"
#' )
#' polygons.over.miami <- miami.OSM.basemap +
#'     polygon.layer +
#'     polygon.fill.scale +
#'     polygon.linewidth.scale
#'
#' \dontrun{
#' ol_map2HTML(
#'   polygons.over.miami,
#'   'miami_polygons.html',
#'   map.heading="Miami Shapes",
#'   map.note="Note: Mouseover popup values are
#'     independent of shape size &amp; color."
#' )
#' browseURL("miami_polygons.html")
#' }
ol_aes <- function(...){
    z <- eval(substitute(alist(...)))
    return(z)
}


