#' OpenLayers Polygon Layer
#'
#' Function to create a polygon layer to add to an OpenLayers Map object.
#' 
#' This function creates a list object containing the data required 
#' to generate an OpenLayers vector layer with features using \code{MultiPolygon}.  
#' See OpenLayers 
#' \href{https://openlayers.org/en/latest/apidoc/module-ol_geom_MultiPolygon-MultiPolygon.html}{MultiPolygon Documentation} for details.
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
#' @section Formatting Tooltips With \code{label.params}:
#' The \code{tooltip.params} parameter enable the user to control tooltip
#' formats.  Unlike the \code{label.params}, not all \code{tooltip.params} are 
#' embedded in Openlayers javascript objects; some are translated
#' to corresponding CSS properties.  The 
#' table below provides a list of supported properties and their descriptions.
#' OpenLayers \href{https://openlayers.org/en/latest/apidoc/module-ol_Overlay.html}{Overlay Documentation}
#' provides additional information about Overlay Properties.
#' \tabular{ll}{
#' \code{font} \tab character tootltip CSS font \cr
#' \code{offsetX} \tab numeric OpenLayers Overlay x-offset \cr
#' \code{offsetY} \tab numeric OpenLayers Overlay y-offset \cr
#' \code{positioning} \tab character OpenLayers Overlay positioning string \cr
#' \code{stroke_color} \tab character Tooltip CSS font-color \cr
#' \code{fill_color} character \tab Tooltip CSS background-color \cr
#' \code{padding}, \tab character Tooltip CSS padding \cr
#' \code{border}, \tab character Tooltip CSS border \cr
#' \code{borderradius} character \tab Tooltip CSS border-radius \cr
#' }
#' 
#' @param polygon.obj SpatialPolygonsDataFrame, SpatialPolygons, list
#' of polygon-like objects, or a two-column matrix of longitude-latitude  
#' coordinates to be used as ordered polygon vertices.
#' @param mapping list created by ol_aes.
#' @param name character Layer name.
#' @param df data.frame with same number of polygon objects as \code{polygon.obj}.
#' Used for aestheic mapping.  Defaults to polygon.obj@@data if 
#' \code{class(polygon.obj)==SpatialPolygonsDataFrame)} and df is
#' not provided.
#' @param toggle.control logical indicating whether this layer will have
#' a visibility toggle.
#' @param fill character color string, or vector of color strings.  Used
#' only if no \code{fill} aesthetic is provided in \code{mapping}
#' @param fill.opacity numeric in [0,1]. Controls circle opacity if
#' no opacity provided in \code{fill} or \code{fill} aesthetic.
#' @param lwd numeric polygon border width. Used only if no \code{lwd}
#' aesthetic is provided in \code{mapping}
#' @param ol.lty (experimental) numeric vector with length > 1, or 
#' \code{list} of such vectors. Used only if no \code{ol.lty} aesthetic is
#' provided in \code{mapping}.  See OpenLayers
#' \href{https://openlayers.org/en/latest/apidoc/module-ol_style_Stroke-Stroke.html}{ol/style/Stroke Documentation},
#' 'lineDash' property for more information.
#' @param color character border color string, or vector of color strings.  Used
#' only if no \code{color} aesthetic is provided in \code{mapping}
#' @param label character vector of polygon feature labels.
#' @param label.params named list (e.g., \code{list(property=value)}) of 
#' label position and format parameters.  See below. 
#' @param tooltip character vector polygon feature tooltip popups.
#' @param tooltip.params named list (e.g., \code{list(property=value)}) of 
#' tooltip position and format parameters.  See below.
#'
#' @return A list object of class \code{Layer.SpatialPolygon}.
#'
#' @seealso \code{\link{ol_aes}}, \code{\link{ol_map}}, \code{\link{ol_geom_point}}, 
#' \code{\link{ol_geom_line}}, \code{\link{ol_geom_circle}}
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
#'         fill=shape,
#'     ),
#'     df=polygon.df,
#'     lwd=1,
#'     name="Miami Polygons",
#'     toggle.control=TRUE,
#'     tooltip=polygon.df$no
#' )
#' polygon.fill.scale <- ol_scale_fill_discrete(display=TRUE)
#' polygons.over.miami <- miami.OSM.basemap + 
#'     polygon.layer + 
#'     polygon.fill.scale
#'
#' ## Not Run: output to file and view
#' # ol_map2HTML(polygons.over.miami,'miami_polygons.html')
#' # browseURL("miami_polygons.html")
ol_geom_polygon <- function(
    polygon.obj,
    mapping=ol_aes(),
    name=NULL,
    df=NULL,
    toggle.control=FALSE,
    fill="#00FF00",
    fill.opacity = 0.5,
    lwd=1,
    ol.lty=list(),
    color='#000000',
    label=NULL,
    label.params=list(),
    tooltip=NULL,
    tooltip.params=list()
){
   sp_poly2layer <- function(sppoly){
        PGS <- sppoly@Polygons
        n <- length(PGS)
        holes <- which(sapply(1:n,function(z)return(PGS[[z]]@hole)))
        nonholes <- setdiff(1:n,holes)
        unexplored.holes <- holes
        o <- list()
        for(i in 1:length(nonholes)){
            o[[i]] <- list()
            o[[i]][[1]] <-PGS[[nonholes[i]]]@coords
            inex <- 2
            for(j in unexplored.holes){
                if(sp::point.in.polygon(
                    PGS[[j]]@coords[1,1],
                    PGS[[j]]@coords[1,2],
                    PGS[[nonholes[i]]]@coords[,1],
                    PGS[[nonholes[i]]]@coords[,2]) > 0
                ){
                    unexplored.holes <- setdiff(unexplored.holes,j)
                    o[[i]][[inex]]<-PGS[[j]]@coords
                    inex <- inex + 1
                }
            }
        }
        return(o)
    }
    lty <- ol.lty
    if('ol.lty' %in% names(mapping)){
        mapping[['lty']]<-mapping[['ol.lty']]
    }
    if(class(polygon.obj)=="SpatialPolygonsDataFrame"){
        sp.poly <- sp::spTransform(polygon.obj,wgs84.proj4str)
        if(is.null(df)){
            df <- sp.poly@data
        }
    } else if(class(polygon.obj)=="SpatialPolygons"){
        sp.poly <- sp::spTransform(polygon.obj,wgs84.proj4str)
    } else {
        if(!is.list(polygon.obj)){
            polygon.obj <- list(polygon.obj)
        }
        for(i in 1:length(polygon.obj)){
            if(class(polygon.obj[[i]]) != 'Polygons'){
                polygon.obj[[i]] <- sp::Polygons(list(sp::Polygon(as.matrix(polygon.obj[[i]]))),i-1)
            }
        }
        sp.poly <- sp::SpatialPolygons(polygon.obj,proj4string=wgs84.proj4str)
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
    o[["features"]] <- list()
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
    for(i in 1:length(sp.poly@polygons)){
        o[["features"]][[i]]<-list()
        o[["features"]][[i]][['Polygon']] <- sp_poly2layer(sp.poly@polygons[[i]])
        o[["features"]][[i]][['labpt']] <- sp.poly@polygons[[i]]@labpt
    }
    o[['feature.count']]<-length(o[["features"]])
    o[['label']] <- get_ol_layer_label(df,mapping,label,label.params)
    o[['tooltip']] <- get_ol_layer_tooltip(df,mapping,tooltip,tooltip.params)
    class(o) <- "Layer.SpatialPolygon"
    return(o)
}

writeLayer.Layer.SpatialPolygon <- function(layer,suffix="basemap",nice.format=TRUE,self.contained=TRUE,initial.indent=6,...){
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
    if(layer[["feature.count"]] > 1){
        for(i in 1:(layer[["feature.count"]]-1)){
            write_function("new ol.Feature({")
            inid <- inid + 2
            write_poly_feature_vec(layer,i,nice.format,initial.indent=inid)
            inid <- inid - 2
            write_function("}),")
        }
    }
    i <- layer[["feature.count"]]
    write_function("new ol.Feature({")
    inid <- inid + 2
    write_poly_feature_vec(layer,i,nice.format,initial.indent=inid)
    inid <- inid - 2
    write_function("})")
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


write_poly_feature_vec <- function(layer,ind,nice.format=TRUE,initial.indent=14){
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
    write_function(sprintf("name: 'P%i',",ind))
    if(length(layer[['scale']]) > 0){
        for(i in 1:length(layer[['scale']])){
            write_scale_attr_polygons_lines(layer[['scale']][[i]],ind,nice.format,inid)
        }
    }
    write_tooltip_attr(layer[['tooltip']],ind,nice.format,inid)
    write_label_attr_vec(layer[["label"]],ind,nice.format,inid)
    write_multi_poly_geom_vec(layer,ind,nice.format,inid)
}



write_single_poly_geom_vec <- function(single.polygon.matrix,nice.format=TRUE,initial.indent=22){
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
    nr <- nrow(single.polygon.matrix)
    if(any(single.polygon.matrix[1,] != single.polygon.matrix[nr,])){
        single.polygon.matrix <- rbind(single.polygon.matrix,single.polygon.matrix[1,])
    }
    nr <- nrow(single.polygon.matrix)
    for(i in 1:(nr-1)){
        write_function(sprintf("%s,",
            write_coordinate(as.numeric(single.polygon.matrix[i,]))
            )
        )
    }
    i <- nr
    write_function(sprintf("%s",
        write_coordinate(as.numeric(single.polygon.matrix[i,]))
        )
    )
}

write_poly_geom_vec <- function(poly,nice.format=TRUE,initial.indent=20){
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
    if(length(poly) > 1){
        for(i in 1:(length(poly)-1)){
            write_function('[')
            write_single_poly_geom_vec(poly[[i]],nice.format,initial.indent=inid + 2)
            write_function('],')
        }
    }
    i <- length(poly)
    write_function('[')
    write_single_poly_geom_vec(poly[[i]],nice.format,initial.indent=inid + 2)
    write_function(']')
}

write_multi_poly_geom_vec <- function(layer,ind,nice.format=TRUE,initial.indent=14){
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
    write_function('geometry: new ol.geom.MultiPolygon(')
    inid <- inid + 2
    pgs <- layer[['features']][[ind]][['Polygon']]
    write_function('[')
    inid <- inid + 2
    if(length(pgs) > 1){
        for(i in 1:(length(pgs)-1)){
            write_function('[')
            inid <- inid + 2
            write_poly_geom_vec(pgs[[i]],nice.format,initial.indent=inid)
            inid <- inid - 2
            write_function('],')
        }
    }
    i <- length(pgs)
    write_function('[')
    inid <- inid + 2
    write_poly_geom_vec(pgs[[i]],nice.format,initial.indent=inid)
    inid <- inid - 2
    write_function(']')
    inid <- inid - 2
    write_function(']')
    inid <- inid - 2
    write_function(')')
}


