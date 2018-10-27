#' OpenLayers Line Layer
#'
#' Function to create a line layer to add to an OpenLayers Map object.
#' 
#' This function creates a list object containing the data required 
#' to generate an OpenLayers vector layer with features using \code{MultiLineString}.  
#' See Openlayers 
#' \href{https://openlayers.org/en/latest/apidoc/module-ol_geom_MultiLineString-MultiLineString.html}{MultiLineString Documentation} for details. 
#'
#' @section Aesthetics:
#' \itemize{
#' \item \code{color}
#' \item \code{lwd}
#' \item \code{ol_lty} (experimental; See OpenLayers
#' \href{https://openlayers.org/en/latest/apidoc/module-ol_style_Stroke-Stroke.html}{ol/style/Stroke Documentation},
#' 'lineDash' property for more information.)
#' }
#'
#' @param line.obj SpatialLinesDataFrame, SpatialLines, list
#' of lines-like objects, or a two-column matrix of longitude-latitude  
#' coordinates to be used as ordered line object coordinates.
#' @param mapping list created by ol_aes.
#' @param name character Layer name.
#' @param df data.frame with same number of lines-like objects as \code{line.obj}.
#' Used for aestheic mapping.  Defaults to line.obj@@data if 
#' \code{class(polygon.obj)==SpatialLinesDataFrame)} and df is
#' not provided.
#' @param toggle.control logical indicating whether this layer will have
#' a visibility toggle.
#' @param lwd numeric line feature width. Used only if no \code{lwd}
#' aesthetic is provided in \code{mapping}
#' @param ol.lty (experimental) numeric vector with length > 1, or 
#' \code{list} of such vectors. Used only if no \code{ol.lty} aesthetic is
#' provided in \code{mapping}.  See OpenLayers
#' \href{https://openlayers.org/en/latest/apidoc/module-ol_style_Stroke-Stroke.html}{ol/style/Stroke Documentation},
#' 'lineDash' property for more information.
#' @param color character line color string, or vector of color strings.  Used
#' only if no \code{color} aesthetic is provided in \code{mapping}
#' @param label character vector of line feature labels.
#' @param tooltip character vector of line feature tooltip popups.
#' @param label.params,tooltip.params named lists (e.g., \code{list(property=value)}) of 
#' label and tooltip position and format parameters.  See \link{ol_geom_polygon} documentation. 
#'
#' @return A list object of class \code{Layer.SpatialLine}.
#'
#' @seealso \code{\link{ol_aes}}, \code{\link{ol_map}}, \code{\link{ol_geom_point}}, 
#' \code{\link{ol_geom_polygon}}, \code{\link{ol_geom_circle}}
#'
#' @export
#'
#' @examples
#' line.matrix1 <- matrix(
#'     c(
#'         -80.4,-80.4,
#'         25.78,25.88
#'     ),
#'     ncol=2
#' )
#' line.matrix2 <- matrix(
#'     c(
#'         -80.25,-80.35,
#'         25.65,25.65
#'     ),
#'     ncol=2
#' )
#' line.list <- list(line.matrix1,line.matrix2)
#' line.df <- data.frame(
#'     direction=c("vertical","horizontal"),
#'     no=c(1,2)
#' )
#' miami.gray.basemap <- ol_map(
#'     center=c(-80.385790,25.782618),
#'     zoom=9,
#'     map.heading="Miami Lines"
#'     ) + 
#'    public_arcgis_basemap("LightGray") 
#' line.layer <- ol_geom_line(
#'     line.list,
#'     mapping=ol_aes(
#'         color=no,
#'         lwd=direction
#'     ),
#'     df=line.df,
#'     name="Miami Lines",
#'     toggle.control=TRUE,
#'     tooltip=line.df$direction
#' )
#' line.color.scale <- ol_scale_color_continuous(
#'     name="Number",
#'     display=TRUE
#' )
#' line.width.scale <- ol_scale_lwd_discrete(
#'     lwd.vector=c(
#'         horizontal=2,
#'         vertical=4
#'     ),
#'     name="Direction",
#'     display=TRUE
#' )
#' line.map.miami <- miami.gray.basemap + 
#'     line.layer + 
#'     line.color.scale +
#'     line.width.scale
#' ## Not Run: output to file and view
#' # ol_map2HTML(line.map.miami,'miami_lines.html')
#' # browseURL("miami_lines.html")
ol_geom_line <- function(
    line.obj,
    mapping=ol_aes(),
    name=NULL,
    df=NULL,
    toggle.control=FALSE,
    lwd=1,
    ol.lty=list(),
    color='#000000',
    label=NULL,
    label.params=list(),
    tooltip=NULL,
    tooltip.params=list()
    ){
    sp_line2layer <- function(sp.line){
        LNS <- sp.line@Lines
        o <- list()
        for(i in 1:length(LNS)){
            o[[i]] <- sp.line@Lines[[i]]@coords
        }
        return(o)
    }
    lty <- ol.lty
    if(length(lty)==1){
        lty <- NULL
    }
    if('ol.lty' %in% names(mapping)){
        mapping[['lty']]<-mapping[['ol.lty']]
    }
    if(class(line.obj)=="SpatialLinesDataFrame"){
        sp.line <- sp::spTransform(line.obj,wgs84.proj4str)
        if(is.null(df)){
            df <- sp.line@data
        }
    } else if(class(line.obj)=="SpatialLines"){
        sp.line <- sp::spTransform(line.obj,wgs84.proj4str)
    } else {
        if(!is.list(line.obj)){
            line.obj <- list(line.obj)
        }
        for(i in 1:length(line.obj)){
            if(class(line.obj[[i]]) != 'Lines'){
                line.obj[[i]] <- sp::Lines(list(sp::Line(as.matrix(line.obj[[i]]))),i-1)
            }
        }
        sp.line <- sp::SpatialLines(line.obj,proj4string=wgs84.proj4str)
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
    for(i in 1:length(sp.line@lines)){
        o[["features"]][[i]]<-list()
        o[["features"]][[i]][['Line']] <- sp_line2layer(sp.line@lines[[i]])
    }
    o[['feature.count']]<-length(o[["features"]])
    o[['label']] <- get_ol_layer_label(df,mapping,label,label.params)
    o[['tooltip']] <- get_ol_layer_tooltip(df,mapping,tooltip,tooltip.params)
    class(o) <- "Layer.SpatialLine"
    return(o)
}


writeLayer.Layer.SpatialLine <- function(layer,suffix="basemap",nice.format=TRUE,self.contained=TRUE,initial.indent=6,...){
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
            write_line_feature_vec(layer,i,nice.format=nice.format,initial.indent=inid)
            inid <- inid - 2
            write_function("}),")
        }
    }
    i <- layer[["feature.count"]]
    write_function("new ol.Feature({")
    inid <- inid + 2
    write_line_feature_vec(layer,i,nice.format=nice.format,initial.indent=inid)
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


write_line_feature_vec <- function(layer,ind,nice.format=TRUE,initial.indent=14){
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
    write_function(sprintf("name: 'L%i',",ind))
    if(length(layer[['scale']]) > 0){
        for(i in 1:length(layer[['scale']])){
            write_scale_attr_polygons_lines(layer[['scale']][[i]],ind,nice.format,inid)
        }
    }
    write_tooltip_attr(layer[['tooltip']],ind,nice.format=nice.format,initial.indent=inid)
    write_label_attr_vec(layer[["label"]],ind,nice.format,inid)
    write_multi_line_geom_vec(layer,ind,nice.format=nice.format,initial.indent=inid)
}



write_line_geom_vec <- function(single.line.matrix,nice.format=TRUE,initial.indent=20){
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
    nr <- nrow(single.line.matrix)
    if(any(single.line.matrix[1,] != single.line.matrix[nr,])){
        single.line.matrix <- rbind(single.line.matrix,single.line.matrix[1,])
    }
    nr <- nrow(single.line.matrix)
    for(i in 1:(nr-1)){
        write_function(sprintf("%s,",
            write_coordinate(as.numeric(single.line.matrix[i,]))
            )
        )
    }
    i <- nr
    write_function(sprintf("%s",
        write_coordinate(as.numeric(single.line.matrix[i,]))
        )
    )
}


write_multi_line_geom_vec <- function(layer,ind,nice.format=TRUE,initial.indent=14){
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
    write_function('geometry: new ol.geom.MultiLineString(')
    inid <- inid + 2
    lns <- layer[['features']][[ind]][['Line']]
    write_function('[')
    inid <- inid + 2
    if(length(lns) > 1){
        for(i in 1:(length(lns)-1)){
            write_function('[')
            inid <- inid + 2
            write_line_geom_vec(lns[[i]],nice.format=nice.format,initial.indent=20)
            inid <- inid - 2
            write_function('],')
        }
    }
    i <- length(lns)
    write_function('[')
    inid <- inid + 2
    write_line_geom_vec(lns[[i]],nice.format=nice.format,initial.indent=20)
    inid <- inid - 2
    write_function(']')
    inid <- inid - 2
    write_function(']')
    inid <- inid - 2
    write_function(')')
}



