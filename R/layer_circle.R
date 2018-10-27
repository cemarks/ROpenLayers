#' OpenLayers Circle Layer
#'
#' Function to create a circle layer to add to an OpenLayers Map object.
#'
#' This function stores the data required to generate an OpenLayers 
#' vector layer with features using \code{circle} 
#' geometries.  
#' See OpenLayers \href{https://openlayers.org/en/latest/apidoc/module-ol_geom_Circle-Circle.html}{Circle Documentation} for details. 
#' @section Aesthetics:
#' \itemize{
#' \item \code{fill}
#' \item \code{color}
#' \item \code{lwd}
#' \item \code{ol_lty} (experimental; See OpenLayers
#' \href{https://openlayers.org/en/latest/apidoc/module-ol_style_Stroke-Stroke.html}{ol/style/Stroke Documentation},
#' 'lineDash' property for more information.)
#' }
#'
#' @param circle.obj matrix containing three columns: center longitude,
#' center latitude, and radius respectively.  Each row yields a 
#' single \code{circle} feature in the resulting layer.
#' @param mapping list created by ol_aes.
#' @param name character Layer name.
#' @param df data.frame with same number of rows as \code{circle.obj}.
#' Used for aestheic mapping. 
#' @param toggle.control logical indicating whether this layer will have
#' a visibility toggle.
#' @param fill character color string, or vector of color strings.  Used
#' only if no \code{fill} aesthetic is provided in \code{mapping}
#' @param fill.opacity numeric in [0,1]. Controls circle opacity if
#' no opacity provided in \code{fill} or \code{fill} aesthetic.
#' @param lwd numeric circle border width. Used only if no \code{lwd}
#' aesthetic is provided in \code{mapping}
#' @param ol.lty (experimental) numeric vector with length > 1, or 
#' \code{list} of such vectors. Used only if no \code{ol.lty} aesthetic is
#' provided in \code{mapping}.  See OpenLayers
#' \href{https://openlayers.org/en/latest/apidoc/module-ol_style_Stroke-Stroke.html}{ol/style/Stroke Documentation},
#' 'lineDash' property for more information.
#' @param color character border color string, or vector of color strings.  Used
#' only if no \code{color} aesthetic is provided in \code{mapping}
#' @param label character vector of length \code{nrow(circle.obj)} of 
#' feature labels.
#' @param tooltip character vector of length \code{nrow(circle.obj)} of 
#' feature tooltip popups.
#' @param label.params,tooltip.params named lists (e.g., \code{list(property=value)}) of 
#' label and tooltip position and format parameters.  See \link{ol_geom_polygon} documentation. 
#'
#' @return A list object of class \code{Layer.Circle}.
#'
#' @seealso \code{\link{ol_aes}}, 
#' \code{\link{ol_map}}, 
#' \code{\link{ol_geom_polygon}} 
#' 
#' @export
#'
#' @examples
#' miami.circles <- matrix(
#'     c(
#'         -80.885+runif(10), #Longitudes
#'         25.282618+runif(10), #Latitudes
#'         rnorm(10,2000,500) # Radii in meters
#'     ),
#'     ncol=3
#' )
#' aesthetic.df <- data.frame(
#'     type=sample(c("A","B"),10,replace=TRUE),
#'     value=runif(10)*10
#' )
#' miami.OSM.basemap <- ol_map(
#'     center=c(-80.385790,25.782618),
#'     zoom=9,
#'     map.heading="Miami Shapes",
#'     map.note="Note: Mouseover popup values are 
#'         independent of shape size &amp; color."
#'     ) + 
#'    public_OSM_basemap() 
#' circle.layer<-ol_geom_circle(
#'         miami.circles,
#'         df = aesthetic.df,
#'         mapping=ol_aes(fill=type),
#'         lwd=2,
#'         name="Meaningless Miami Circles",
#'         toggle.control=TRUE,
#'         color="#000000FF",
#'         tooltip=sprintf("%1.2f",aesthetic.df$value)
#'         ) 
#' circle.fill <- ol_scale_fill_discrete(
#'         display=TRUE,
#'         preserve.opacity=TRUE
#'     )
#' circles <- miami.OSM.basemap + circle.layer + circle.fill
#' 
#' ## Not Run: output to file and view
#' # ol_map2HTML(circles,'miami_circles.html')
#' # browseURL("miami_circles.html")
ol_geom_circle <- function(
    circle.obj,
    mapping=ol_aes(),
    name=NULL,
    df=NULL,
    toggle.control=FALSE,
    fill="#00FF0090",
    fill.opacity = 0.5,
    lwd=1,
    ol.lty=list(),
    color='#000000',
    label=NULL,
    label.params=list(),
    tooltip=NULL,
    tooltip.params=list()
){
    lty <- ol.lty
    if('ol.lty' %in% names(mapping)){
        mapping[['lty']]<-mapping[['ol.lty']]
    }
    if(length(mapping)>0){
        for(i in 1:length(mapping)){
            mapping.col<-as.character(mapping[[i]])
            if(!(mapping.col %in% names(df)) && mapping.col != ""){
                stop(sprintf("No column %s in data frame to map to %s aesthetic.",mapping.col,names(mapping)[i]))
            }
        }
    }
    o <- list()
    o[["name"]] <- name
    o[['toggle.control']] <- toggle.control
    n <- names(mapping)
    o[['scale']] <- list()
    i <- 1
    l.created <- FALSE
    if('fill' %in% names(mapping)){
        fill.column = as.character(mapping['fill'])
        if(is.numeric(df[,fill.column])){
            o[['scale']][[i]] <- ol_scale_fill_continuous(
                low.val=min(df[,fill.column],na.rm=TRUE),
                high.val=max(df[,fill.column],na.rm=TRUE),
                name=fill.column,
                opacity=fill.opacity
            )
            if(!is.null(o[['scale']][[i]])){
                l.created <- TRUE
            } else {
                warning("Continuous fill scale not created, trying discrete scale")
            }
        }
        if(!l.created){
            fill.vector <- get_default_color_vec_discrete(
                df[,fill.column],
                opacity=fill.opacity
            )
            o[['scale']][[i]] <- ol_scale_fill_discrete(
                color.vector=fill.vector,
                name=fill.column,
                opacity=fill.opacity,
                draw.lines=TRUE
            )
            if(!is.null(o[['scale']][[i]])){
                l.created <- TRUE
                o[['scale']][[i]][['marker']]<-"circle"
            } else {
                warning("Discrete fill scale not created, trying fixed scale")
            }
        }
        if(l.created){
            o[['scale']][[i]][['variable.values']] <- df[,fill.column]
            o[['scale']][[i]][['variable.name']] <- fill.column
        }
    }
    if(!l.created){
        o[['scale']][[i]] <- ol_scale_fixed(
            attribute <- "fill",
            values <- color_check(fill,fill.opacity)
        )
    }
    i <- i+1
    l.created <- FALSE
    if('color' %in% names(mapping)){
        color.column = as.character(mapping['color'])
        if(is.numeric(df[,color.column])){
            o[['scale']][[i]] <- ol_scale_color_continuous(
                low.val=min(df[,color.column],na.rm=TRUE),
                high.val=max(df[,color.column],na.rm=TRUE),
                name=color.column,
            )
            if(!is.null(o[['scale']][[i]])){
                l.created <- TRUE
            } else {
                warning("Continuous color scale not created, trying discrete scale")
            }
        }
        if(!l.created){
            color.vector <- get_default_color_vec_discrete(
                df[,color.column]
            )
            o[['scale']][[i]] <- ol_scale_color_discrete(
                color.vector=color.vector,
                name=color.column,
            )
            if(!is.null(o[['scale']][[i]])){
                l.created <- TRUE
            } else {
                warning("Discrete color scale not created, trying fixed scale")
            }
        }
        if(l.created){
            o[['scale']][[i]][['variable.values']] <- df[,color.column]
            o[['scale']][[i]][['variable.name']] <- color.column
        }
    }
    if(!l.created){
        o[['scale']][[i]] <- ol_scale_fixed(
            attribute <- "color",
            values <- color_check(color,1)
        )
    }
    i <- i+1
    l.created <- FALSE
    if('lwd' %in% names(mapping)){
        lwd.column = as.character(mapping['lwd'])
        lwd.vector <- get_default_num_vec_discrete(
            df[,lwd.column],
            c(1,5)
        )
        o[['scale']][[i]] <- ol_scale_lwd_discrete(
            lwd.vector=lwd.vector,
            name=lwd.column,
        )
        if(!is.null(o[['scale']][[i]])){
            l.created <- TRUE
        } else {
            warning("Discrete lwd scale not created, trying fixed scale")
        }
        if(l.created){
            o[['scale']][[i]][['variable.values']] <- df[,lwd.column]
            o[['scale']][[i]][['variable.name']] <- lwd.column
        }
    }
    if(!l.created){
        o[['scale']][[i]] <- ol_scale_fixed(
            attribute <- "lwd",
            values <- lwd
        )
    }
    i <- i+1
    l.created <- FALSE
    if('lty' %in% names(mapping)){
        lty.column = as.character(mapping['lty'])
        lty.list <- get_default_lty_list(
            df[,lty.column]
        )
        o[['scale']][[i]] <- ol_scale_lty_discrete(
            lty.list=lty.list,
            name=lty.column,
        )
        if(!is.null(o[['scale']][[i]])){
            l.created <- TRUE
        } else {
            warning("Discrete lty scale not created, trying fixed scale")
        }
        if(l.created){
            o[['scale']][[i]][['variable.values']] <- df[,lty.column]
            o[['scale']][[i]][['variable.name']] <- lty.column
        }
    }
    if(!l.created && (length(lty)>0)){
        o[['scale']][[i]] <- ol_scale_fixed(
            attribute <- "lty",
            values <- lty
        )
    }
    o[["features"]]<-circle.obj
    o[['feature.count']]<-nrow(o[["features"]])
    o[['label']] <- get_ol_layer_label(df,mapping,label,label.params)
    o[['tooltip']] <- get_ol_layer_tooltip(df,mapping,tooltip,tooltip.params)
    class(o) <- "Layer.Circle"
    return(o)
}

writeLayer.Layer.Circle <- function(layer,suffix="basemap",nice.format=TRUE,self.contained=TRUE,initial.indent=6,...){
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
    write_function(sprintf("var layer_%s = new ol.layer.Vector({",suffix))
    inid <- inid + 2
    write_function(sprintf("name: \"%s\",",gsub('"',"'",layer[['name']])))
    write_function("source: new ol.source.Vector({")
    inid <- inid + 2
    write_function("features: [")
    inid <- inid + 2
    if(layer[["feature.count"]] > 0){
        for(i in 1:nrow(layer[['features']])){
            write_function("new ol.Feature({")
            inid <- inid + 2
            write_function(sprintf("name: 'Circle%i',",i))
            if(length(layer[['scale']]) > 0){
                for(j in 1:length(layer[['scale']])){
                    write_scale_attr_polygons_lines(layer[['scale']][[j]],i,nice.format=nice.format,initial.indent=inid)
                }
            }
            write_tooltip_attr(layer[['tooltip']],i,nice.format=nice.format,initial.indent=inid)
            write_label_attr_vec(layer[["label"]],i,nice.format,inid)
            write_function("geometry: new ol.geom.Circle(")
            inid <- inid + 2
            write_function(
                sprintf(
                    "%s,",
                    write_coordinate(as.numeric(layer[['features']][i,1:2]))
                )
            )
            write_function(
                sprintf(
                    "%i",
                    as.integer(layer[['features']][i,3])
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
    }
    inid <- inid - 2
    write_function("]")
    inid <- inid - 2
    write_function("}),")
    write_function("style: function(feature){")
    inid <- inid + 2
    write_function("var style = new ol.style.Style({")
    inid <- inid + 2
    if(!(is.null(layer[['label']])) && !(is.na(layer[['label']])) && is.list(layer[['label']])){
        write_function("text: new ol.style.Text({")
        inid <- inid + 2
        write_label_style_obj(layer[['label']],nice.format=nice.format,initial.indent=inid)
        inid <- inid - 2
        write_function(sprintf("}),"))
    }
    write_function("stroke: new ol.style.Stroke({")
    inid <- inid + 2
    if(length(layer[['scale']])>0){
        scale.attributes <- sapply(
            1:length(layer[['scale']]),
            function(j) return(layer[['scale']][[j]][['attribute']])
        )
    } else {
        scale.attributes <- NULL
    }
    if('lty' %in% scale.attributes){
        w <- which(scale.attributes=='lty')
        if(!(layer[['scale']][[w]][['type']]=='fixed') || (length(layer[['scale']][[w]][['value']]) > 1)){
            write_function("lineDash: feature.get('lty'),")
            write_function("lineDashOffset: 3,")
        } else if(length(layer[['scale']][[w]][['value']]) == 1){
            write_function(sprintf("lineDash: [%s],",paste(layer[['scale']][[w]][['value']][[1]],collapse=",")))
            write_function("lineDashOffset: 3,")
        }
    }
    w <- which(scale.attributes=='lwd')
    if((length(w)==1) && layer[['scale']][[w]][['type']] == 'fixed' && length(layer[['scale']][[w]][['value']])==1){
        write_function(sprintf("width: %1.1f,",layer[['scale']][[w]][['value']]))
    } else {
        write_function("width: feature.get('lwd'),")
    }
    w <- which(scale.attributes=='color')
    if((length(w)==1) && layer[['scale']][[w]][['type']] == 'fixed' && length(layer[['scale']][[w]][['value']])==1){
        write_function(sprintf("color: %s",hex2rgb_arraystr(layer[['scale']][[w]][['value']])))
    } else {
        write_function("color: feature.get('color')")
    }
    inid <- inid - 2
    write_function("}),")
    write_function("fill: new ol.style.Fill({")
    inid <- inid + 2
    w <- which(scale.attributes=='fill')
    if((length(w)==1) && layer[['scale']][[w]][['type']] == 'fixed' && length(layer[['scale']][[w]][['value']])==1){
        write_function(sprintf("color: %s",hex2rgb_arraystr(layer[['scale']][[w]][['value']])))
    } else {
        write_function("color: feature.get('fill')")
    }
    inid <- inid - 2
    write_function("})")
    inid <- inid - 2
    write_function("});")
    write_function("return style;")
    inid <- inid - 2
    write_function("}")
    inid <- inid - 2
    write_function("});")
    if(nice.format) cat("\n")
}


