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
#' \href{https://www.arcgis.com}{ESRI ArcGIS} hosts several 
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
#' @docType package
#' @name ROpenLayers
#' @examples
#' data(quakes)
#' center <- c(mean(quakes$long),mean(quakes$lat))
#' quakes$long[which(quakes$long>180)]<-quakes$long[which(quakes$long>180)]-360
#' tooltips <- paste("Depth",quakes$depth,sep=": ")
#' mymap <- ol_map(
#'     zoom = 5,
#'     center = center,
#'     map.heading = "Earthquake Data Visualization"
#' )
#' basemap.layer <- public_arcgis_basemap(
#'     "OceanBase",
#'     toggle.control=FALSE
#' )
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
#' ## Not run: save to file and open in browser
#' # ol_map2HTML(mymap,"Quakes.html")
#' # browseURL("file:Quakes.html")
NULL






#' 
#' OpenLayers Map
#'
#' Create an OpenLayers Map Object.
#'
#' This function creates a new S3 OpenLayers Map object with no layers.  
#'
#' @param zoom integer map initial zoom level.
#' @param center numeric vector of length 2 containing decimal longitude and
#' latitude coordinates for initial map center.
#' @param width numeric or character CSS value width of map container.
#' @param height numeric or character CSS value height of map container.
#' @param ol.source.url character string containing the url to the OpenLayers
#' javascript library.
#' @param nga.olsource logical.  \code{TRUE} will use the OpenLayers 3.16.0 javascript
#' library from \url{https://home.gvs.nga.mil} (requires authentication);
#' \code{FALSE} uses the public library at 
#' \href{https://cdn.rawgit.com/openlayers/openlayers.github.io/master/en/v3.16.0/build/ol.js}{cdn.rawgit.com}.
#' Only used if \code{ol.source.url} is missing or \code{NULL}.
#' @param map.heading character heading to be placed over map in html h1 tag.
#' @param map.note character note placed in html paragraph (<p>) tag centered
#' under map container. 
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
#'     zoom=9,
#'     map.heading="Miami Shapes",
#'     map.note="Note: Mouseover popup values are 
#'         independent of shape size &amp; color."
#'     ) + 
#'    public_OSM_basemap() 
#' ## Not Run
#' # ol_map2HTML(miami.OSM.basemap,'miami.html')
#' # browseURL("miami.html")
ol_map <- function(
    zoom=10,
    center=c(-117.1611,32.7157),
    width=NULL,
    height=NULL,
    ol.source.url=NULL,
    nga.olsource = FALSE,
    map.heading = NULL,
    map.note=NULL
){
    if(missing(ol.source.url) || is.null(ol.source.url)){
        if(nga.olsource){
            ol.source.url <- "https://home.gvs.nga.mil/libs/openlayers/3.16.0/build/ol.js"
        } else {
            ol.source.url <- "https://cdn.rawgit.com/openlayers/openlayers.github.io/master/en/v3.16.0/build/ol.js"
        }
    }
    toggle.control.df <- data.frame(matrix(nrow=0,ncol=3))
    names(toggle.control.df) <- c("layer.id","layer.var","name")
    o = list(
        ol.source.url=ol.source.url,
        map.width=width,
        map.height=height,
        center=center,
        zoom=zoom,
        layer.control.df=toggle.control.df,
        tooltips=FALSE,
        tooltips.param.vector=c(
            backgroundFill='#105555B0',
            border='solid #000000FF 1px',
            borderradius="3px",
            font=NULL,
            padding="3px",
            stroke="#000000",
            offsetX=6,
            offsetY=0,
            positioning="bottom-left"
        ),
        layers=list(),
        map.heading=map.heading,
        map.note=map.note
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
#' base.layer <- public_arcgis_basemap('LightGray')
#' mymap <- mymap + base.layer
#' ## Not run
#' # ol_map2HTML(mymap,"SanDiego.html")
#' # browseURL("SanDiego.html")
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
                "backgroundFill",
                "border",
                "borderradius",
                "font",
                "padding",
                "stroke",
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



#' NGA Basemap Layer
#'
#' Create a basemap layer linking to an NGA ArcGIS mapserver.
#' 
#' Creates and returns an OpenLayers ArcGIS Tile layer that sources a
#' map server hosted at \url{https://home.gvs.nga.mil}.  These map servers
#' are owned by the US Government and require authentication.  If the 
#' \code{basemap.identifier} parameter is unrecognized the function will
#' default to the NGA OpenStreetMap map server.
#' 
#' @section Available Base Maps:
#' The following basemap.identifiers are currently supported by this method.
#' \tabular{ll}{
#' "ABM" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/CanvasMaps/Analytic_Basemap/MapServer}{Analytic Base Map}\cr
#' "LightGray" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/CanvasMaps/LightGray/MapServer}{Analytic Base Map (Light Gray)}\cr
#' "Light_LightGray" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/CanvasMaps/Lite_LightGray/MapServer}{Analytic Base Map (Light Light Gray)}\cr
#' "LightMidnight" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/CanvasMaps/Lite_Midnight/MapServer}{Analytic Base Map (Light Midnight)}\cr
#' "Light_Slate" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/CanvasMaps/Lite_Slate/MapServer}{Analytic Base Map (Light Slate)}\cr
#' "Midnight" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/CanvasMaps/Midnight/MapServer}{Analytic Base Map (Midnight)}\cr
#' "Slate" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/CanvasMaps/Slate/MapServer}{Analytic Base Map (Slate)}\cr
#' "CARDG" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/Basemap/ScannedMaps/MapServer}{Scanned CARDG Maps}\cr
#' "DNC" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/Basemap/DNC/MapServer}{Digital Nautical Charts}\cr
#' "Imagery" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/Basemap/NGA_World_Imagery_2D/MapServer}{Satellite Imagery}\cr
#' "Hillshade" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/Basemap/NGA_Hillshade_2D/MapServer}{Hillshade Map}\cr
#' "ShadedRelief" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/Basemap/NGA_ShadedRelief_2D/MapServer}{Shaded Relief Map}\cr
#' "TintedHillshade" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/Basemap/NGA_Tinted_Hillshade/MapServer}{Tinted Hillshade Map}\cr
#' "WorldBoundaries" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/Basemap/World_Boundaries_2D/MapServer}{World Boundaries (WSM)}\cr
#' "WorldBoundaries_Places" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/Basemap/World_Boundaries_Places_2D/MapServer}{World Boundaries, Places (WSM)}\cr
#' "WorldPlaceNames" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/Basemap/World_Place_Names_2D/MapServer}{World Place Names (WSM)}\cr
#' "WorldTransportation" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/Basemap/World_Transportation_2D/MapServer}{World Transportation (WSM)}\cr
#' "WorldCities" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/SampleWorldCities/MapServer}{Sample World Cities}\cr
#' "WSM" \tab \href{https://maps.gvs.nga.mil/arcgis/rest/services/Basemap/World_StreetMap_2D/MapServer}{World Street Map}\cr
#' }
#' 
#' @param basemap.identifier character indicating which NGA mapserver to use.
#' See 'Available Base Maps'.
#' @param name character layer name.
#' @param toggle.control logical.  If \code{TRUE}, a checkbox will appear on the
#' map allowing the viewer to toggle its visibility in the browser.
#' 
#' @return A \code{Layer.ArcGIS} S3 object. 
#'
#' @seealso 
#' \code{\link{ol_map}}, 
#' \code{\link{+.Ol.Map}}, 
#' \code{\link{public_arcgis_basemap}}, 
#' \code{\link{public_OSM_basemap}}, 
#' \code{\link{user_arcgis_basemap}}
#'
#' @export
#'
#' @examples
#' mymap <- ol_map(
#'     nga.olsource = TRUE ## Not required; can also use public OpenLayers
#'                         ## javascript source with NGA mapservers.
#' )
#' base.layer <- nga_basemap('Midnight')
#' mymap <- mymap + base.layer
#' ## Not run
#' # ol_map2HTML(mymap,"SanDiegoMidnight.html")
#' # browseURL("SanDiegoMidnight.html")
nga_basemap<-function(basemap.identifier="WSM",name=NULL,toggle.control=FALSE){
    if(basemap.identifier %in% nga.mapserver.df$identifier){
        w <- which(nga.mapserver.df$identifier==basemap.identifier)
        if(missing(name) || is.null(name) || name==""){
            use.name <- nga.mapserver.df$name[w]
        } else {
            use.name <- name
        }
        o <- list(
            name=use.name,
            attributions = attribution_str(nga.mapserver.df,w),
            url = nga.mapserver.df$url[w],
            toggle.control=toggle.control
        )
        class(o) <- "Layer.ArcGIS"
        return(o)
    } else {
        w <- which(nga.mapserver.df$identifier=="WSM")
        if(missing(name) || is.null(name) || name==""){
            use.name <- nga.mapserver.df$name[w]
        } else {
            use.name <- name
        }
        o <- list(
            name=use.name,
            attributions = attribution_str(nga.mapserver.df,w),
            url = nga.mapserver.df$url[w],
            toggle.control=toggle.control
        )
        class(o) <- "Layer.ArcGIS"
        return(o)
    }
}

### Does not work well with OL3--requires more recent openlayers version
user_arcgis_vectortile <- function(url,name="",attributions="",toggle.control=FALSE){
    o <- list(
        name=name,
        attributions=attributions,
        url=url,
        toggle.control=toggle.control
    )
    class(o)<-"Layer.ArcGISVector"
    return(o)
}

#' Public ArcGIS Basemap Layer
#'
#' Create a basemap layer linking to an Public ArcGIS mapserver.
#' 
#' Creates and returns an OpenLayers ArcGIS Tile layer that sources a
#' map server hosted at \url{http://server.arcgisonline.com}.  If the 
#' \code{basemap.identifier} parameter is unrecognized the function will
#' default to the \href{http://server.arcgisonline.com/ArcGIS/rest/services/Specialty/DeLorme_World_Base_Map/MapServer}{DeLorme} map server.
#' 
#' @section Available Base Maps:
#' The following basemap.identifiers are currently supported by this method.
#' \tabular{ll}{
#' "LightGray" \tab \href{http://server.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer}{World Light Gray Base}\cr
#' "USAPOP2010" \tab \href{http://server.arcgisonline.com/arcgis/rest/services/Demographics/USA_2000-2010_Population_Change/MapServer}{USA Population Change 2000-2010}\cr
#' "Hillshade" \tab \href{http://server.arcgisonline.com/arcgis/rest/services/Elevation/World_Hillshade/MapServer}{World Hillshade}\cr
#' "OceanBase" \tab \href{http://server.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Base/MapServer}{World Ocean Base}\cr
#' "WorldBoundaries" \tab \href{http://server.arcgisonline.com/arcgis/rest/services/Reference/World_Boundaries_and_Places/MapServer}{World Boundaries and Places}\cr
#' "WorldRefOverlay" \tab \href{http://server.arcgisonline.com/arcgis/rest/services/Reference/World_Reference_Overlay/MapServer}{World Reference Overlay}\cr
#' "WorldTrans" \tab \href{http://server.arcgisonline.com/arcgis/rest/services/Reference/World_Transportation/MapServer}{World Transportation}\cr
#' "WorldNav" \tab \href{http://server.arcgisonline.com/arcgis/rest/services/Specialty/World_Navigation_Charts/MapServer}{World Navigation Charts}\cr
#' "Imagery" \tab \href{https://server.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer}{World Imagery} \cr
#' "DeLorme" \tab \href{http://server.arcgisonline.com/ArcGIS/rest/services/Specialty/DeLorme_World_Base_Map/MapServer}{World Imagery}\cr
#' }
#' 
#' @param basemap.identifier character indicating which Public ArcGIS mapserver to use.
#' See 'Available Base Maps'.
#' @param name character layer name.
#' @param toggle.control logical.  If \code{TRUE}, a checkbox will appear on the
#' map allowing the viewer to toggle its visibility in the browser.
#'
#' @return A \code{Layer.ArcGIS} S3 object. 
#'
#' @seealso 
#' \code{\link{ol_map}}, 
#' \code{\link{+.Ol.Map}}, 
#' \code{\link{nga_basemap}}, 
#' \code{\link{public_OSM_basemap}}, 
#' \code{\link{user_arcgis_basemap}}
#'
#' @export
#'
#' @examples
#' mymap <- ol_map()
#' base.layer <- public_arcgis_basemap('LightGray')
#' mymap <- mymap + base.layer
#' ## Not run
#' # ol_map2HTML(mymap,"SanDiego.html")
#' # browseURL("SanDiego.html")
public_arcgis_basemap<-function(basemap.identifier="DeLorme",name=NULL,toggle.control=FALSE){
    if(basemap.identifier %in% public.mapserver.df$identifier){
        w <- which(public.mapserver.df$identifier==basemap.identifier)
        if(missing(name) || is.null(name) || name==""){
            use.name <- public.mapserver.df$name[w]
        } else {
            use.name <- name
        }
        o <- list(
            name=use.name,
            attributions = attribution_str(public.mapserver.df,w),
            url = public.mapserver.df$url[w],
            toggle.control=toggle.control
        )
        class(o) <- "Layer.ArcGIS"
        return(o)
    } else {
        w <- which(public.mapserver.df$identifier=="DeLorme")
        if(missing(name) || is.null(name) || name==""){
            use.name <- attribution_str(public.mapserver.df,w)
        } else {
            use.name <- name
        }
        o <- list(
            name=use.name,
            attributions = public.mapserver.df$attributions[w],
            url = public.mapserver.df$url[w],
            toggle.control=toggle.control
        )
        class(o) <- "Layer.ArcGIS"
        return(o)
    }
}

#' User ArcGIS Basemap Layer
#'
#' Create a basemap layer linking to an User-supplied ArcGIS mapserver.
#' 
#' Creates and returns an OpenLayers ArcGIS Tile layer that sources a
#' map server at a user-supplied URL.  
#'  
#' @param url character url string where the map server is located.  Typically these
#' urls end with "/MapServer".
#' @param name character layer name.
#' @param attributions character HTML.  This HTML will render as attributional text at the
#' bottom-right corner of the map.  At a minimum, this text should include the
#' copyright text provided on the map server.
#' @param toggle.control logical.  If \code{TRUE}, a checkbox will appear on the
#' map allowing the viewer to toggle its visibility in the browser.
#'
#' @return A \code{Layer.ArcGIS} S3 object. 
#'
#' @seealso 
#' \code{\link{ol_map}}, 
#' \code{\link{+.Ol.Map}}, 
#' \code{\link{nga_basemap}}, 
#' \code{\link{public_OSM_basemap}}, 
#' \code{\link{public_arcgis_basemap}}
#'
#' @export
#'
#' @examples
#' mymap <- ol_map()
#' server.url <- "http://server.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer"
#' base.layer <- user_arcgis_basemap(
#'     server.url,
#'     attributions = sprintf(
#'         "I found this at <a href='%s'>arcgisonline.com</a>",
#'         server.url
#'     ),
#'     toggle.control=TRUE
#' )
#' mymap <- mymap + base.layer
#' ## Not run
#' # ol_map2HTML(mymap,"SanDiego_NatGeo.html")
#' # browseURL("SanDiego_NatGeo.html")
user_arcgis_basemap <- function(url,name="",attributions="",toggle.control=FALSE){
    o <- list(
        name=name,
        attributions=gsub('"',"'",attributions),
        url=url,
        toggle.control=toggle.control
    )
    class(o) <- "Layer.ArcGIS"
    return(o)
}

#' Public OpenStreetMap Basemap Layer
#'
#' Create a basemap layer linking to \href{https://www.openstreetmap.org}{OpenStreetMap}.
#' 
#' Creates and returns an OpenLayers OpenStreetMap Tile layer.  
#'  
#' @param name character layer name.
#' @param toggle.control logical.  If \code{TRUE}, a checkbox will appear on the
#' map allowing the viewer to toggle its visibility in the browser.
#'
#' @return A \code{Layer.ArcGIS} S3 object. 
#'
#' @seealso 
#' \code{\link{ol_map}}, 
#' \code{\link{+.Ol.Map}}, 
#' \code{\link{nga_basemap}}, 
#' \code{\link{public_arcgis_basemap}}, 
#' \code{\link{user_arcgis_basemap}}
#'
#' @export
#'
#' @examples
#' mymap <- ol_map()
#' base.layer <- public_OSM_basemap()
#' mymap <- mymap + base.layer
#' ## Not run
#' # ol_map2HTML(mymap,"SanDiego_OSM.html")
#' # browseURL("SanDiego_OSM.html")
public_OSM_basemap <- function(name=NULL,toggle.control = FALSE){
    if(missing(name) || is.null(name) || name==""){
        use.name <- "Open Street Map"
    } else {
        use.name <- name
    }
    o <- list(
        name=use.name,
        attributions="<a href=\\\"http://www.openstreetmap.org\\\">OpenStreetMap</a> - &copy; OpenStreetMap contributors."
        )
    class(o) <- "Layer.OSM"
    return(o)
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
#'     zoom=9,
#'     map.heading="Miami Shapes",
#'     map.note="Note: Mouseover popup values are 
#'         independent of shape size &amp; color."
#'     ) + 
#'    public_OSM_basemap() 
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
#' ## Not Run
#' # ol_map2HTML(polygons.over.miami,'miami_polygons.html')
#' # browseURL("miami_polygons.html")
ol_aes <- function(...){
    z <- eval(substitute(alist(...)))
    return(z)
}


