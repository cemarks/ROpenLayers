#' OpenLayers Point Layer
#'
#' Function to create a points layer to add to an OpenLayers Map object.
#'
#' This function stores the data required to generate an OpenLayers
#' vector layer with features using \code{Point}
#' geometries.
#' See OpenLayers \href{https://openlayers.org/en/latest/apidoc/module-ol_geom_Point-Point.html}{Point Documentation}
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
#' @param mapping list created by ol_aes. Used for aestheic mapping.
#' @param name character Layer name.
#' @param df data.frame with same number of rows as \code{point.obj} coordinate
#' matrix.
#' @param toggle.control logical indicating whether this layer will have
#' a visibility toggle.
#' @param fill character color string, or vector of color strings.  Used
#' only if no \code{fill} aesthetic is provided in \code{mapping}
#' @param fill.opacity numeric in [0,1]. Controls circle opacity if
#' no opacity provided in \code{fill} or \code{fill} aesthetic.
#' @param marker character.  The 'pin' marker draws map pointers similar to most web map
#' applications.  The 'dot' or 'point' markers render as cicular points on the map.
#' Other marker types are not supported by this method.
#' @param size numeric point icon size scalar or vector scalars.  Used
#' only if no \code{size} aesthetic is provided in \code{mapping}.  A value of
#' 1 translates to an icon width of 40 pixels for "pin" markers, or 20 pixels for
#' "dot" markers.
#' @param label character vector of point feature labels.
#' @param tooltip character vector of point feature tooltip popups.
#' @param label.params,tooltip.params named lists (e.g., \code{list(property=value)}) of
#' label and tooltip position and format parameters.  See \link{ol_geom_polygon} documentation.
#'
#' @return A list object of class \code{Layer.SpatialPoint}.
#'
#' @seealso \code{\link{ol_aes}},
#' \code{\link{ol_map}},
#' \code{\link{ol_geom_polygon}},
#' \code{\link{ol_geom_circle}},
#' \code{\link{ol_geom_line}},
#' \code{\link{ol_geom_icon}},
#' \code{\link{geocode}}
#'
#' @export
#'
#' @examples
#' point.matrix <- matrix(
#'     c(
#'         -80.885+runif(10),
#'         25.282618+runif(10)
#'     ),
#'     ncol=2
#' )
#' point.df <- data.frame(
#'     pt.type=sample(c("A","B"),10,replace=TRUE),
#'     pt.value=runif(10)*10
#' )
#' miami.map <- ol_map(
#'     center=c(-80.385790,25.782618),
#'     zoom=9
#' ) +
#'     nga_basemap("WSM")
#' miami.points <- ol_geom_point(
#'     point.matrix,
#'     df=point.df,
#'     mapping=ol_aes(fill=pt.type,size=pt.value),
#'     name="Random Points of Interest",
#'     marker="pin",
#'     toggle.control=TRUE,
#'     tooltip=point.df$pt.type
#' )
#' size.scale <- ol_scale_size_continuous(
#'     display=TRUE,
#'     draw.fill='green'
#' )
#' fill.scale <- ol_scale_fill_discrete(
#'     c(B='red',A='green'),
#'     display=TRUE
#' )
#' miami.points.map <- miami.map +
#'     miami.points +
#'     size.scale +
#'     fill.scale
#'
#' ## Not Run: output to file and view
#' # ol_map2HTML(miami.points.map,'Miami_points.html')
#' # browseURL('Miami_points.html')
ol_geom_point <- function(
    point.obj,
    mapping=ol_aes(),
    name=NULL,
    df=NULL,
    toggle.control=FALSE,
    fill='#00FF00',
    fill.opacity=1,
    marker='pin',
    size=0.5,
    label=NULL,
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
    o[["marker"]] <- marker
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
                df[,fill.column]
            )
            o[['scale']][[i]] <- ol_scale_fill_discrete(
                color.vector=fill.vector,
                name=fill.column,
                opacity=fill.opacity
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
    o[["scale"]][[i]][["marker"]]<-marker
    i <- i+1
    l.created <- FALSE
    if('size' %in% names(mapping)){
        size.column = as.character(mapping['size'])
        if(is.numeric(df[,size.column])){
            o[['scale']][[i]] <- ol_scale_size_continuous(
                low.val=min(df[,size.column],na.rm=TRUE),
                high.val=max(df[,size.column],na.rm=TRUE),
                name=size.column,
                draw.fill = '#00FF00FF'
            )
            if(!is.null(o[['scale']][[i]])){
                l.created <- TRUE
            } else {
                warning("Continuous fill scale not created, trying discrete scale")
            }
        }
        if(!l.created){
            size.vector <- get_default_num_vec_discrete(
                df[,size.column],
                c(0.33,0.75)
            )
            o[['scale']][[i]] <- ol_scale_size_discrete(
                size.vector=size.vector,
                name=size.column,
                draw.fill = '#00FF00FF'
            )
            if(!is.null(o[['scale']][[i]])){
                l.created <- TRUE
            } else {
                warning("Discrete size scale not created, trying fixed scale")
            }
        }
        if(l.created){
            o[['scale']][[i]][['variable.values']] <- df[,size.column]
            o[['scale']][[i]][['variable.name']] <- size.column
        }
    }
    if(!l.created){
        o[['scale']][[i]] <- ol_scale_fixed(
            attribute <- "size",
            values <- size
        )
    }
    o[["scale"]][[i]][["marker"]]<-marker
    o[["features"]]<-sp.point@coords
    o[['feature.count']]<-nrow(o[["features"]])
    default.label.params <- list(offsetX=8,offsetY=2,textAlign='left',textBaseline='bottom')
    for(lp in setdiff(names(default.label.params),names(label.params))){
        label.params[[lp]]<-default.label.params[[lp]]
    }
    o[['label']] <- get_ol_layer_label(df,mapping,label,label.params)
    o[['tooltip']] <- get_ol_layer_tooltip(df,mapping,tooltip,tooltip.params)
    class(o) <- "Layer.SpatialPoint"
    return(o)
}

writeLayer.Layer.SpatialPoint <- function(layer,suffix="basemap",nice.format=TRUE,self.contained=TRUE,initial.indent=6,image.path=".",...){
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
    marker <- layer[['marker']]
    if(marker=="pin"){
        marker.width <- 40
    } else {
        marker.width <- 20
    }
    filename.prefix <- paste(image.path,
        paste(format(Sys.time(),"%Y%m%d%H%M%S"),
            sample(1:1000,1),
            sep=""
        ),
        sep="/"
    )
    pts.df <- data.frame(x=layer[['features']][,1],y=layer[['features']][,2])
    for(s in 1:length(layer[['scale']])){
        s.attr <- layer[['scale']][[s]][['attribute']]
        if(layer[['scale']][[s]][['type']] == 'fixed'){
            v1 <- rep(layer[['scale']][[s]][['value']],ceiling(nrow(pts.df)/length(layer[['scale']][[s]][['value']])))
            new.col<-data.frame(v1[1:nrow(pts.df)])
        } else {
            new.col <- data.frame(sapply(layer[['scale']][[s]][['variable.values']],layer[['scale']][[s]][['function']]))
        }
        names(new.col) <- s.attr
        pts.df<-cbind(pts.df,new.col)
    }
    write_function(sprintf("var layer_%s = new ol.layer.Vector({",suffix))
    inid <- inid + 2
    write_function(sprintf("name: \"%s\",",gsub('"',"'",layer[['name']])))
    write_function("source: new ol.source.Vector({")
    inid <- inid + 2
    write_function("features: [")
    inid <- inid + 2
    plotted.colors <- NULL
    for(i in 1:nrow(pts.df)){
        point.color <- substr(pts.df$fill[i],start=1,stop=7)
        point.opacity <- as.numeric(as.hexmode(substr(pts.df$fill[i],start=8,stop=9)))/255
        file.name <- paste(
            paste(
                filename.prefix,
                substr(
                    point.color,
                    start=2,
                    stop=7
                ),
                sep="_"
            ),
            "png",
            sep="."
        )
        if(!(point.color %in% plotted.colors)){
            draw_marker(
                marker,
                file.name,
                point.color,
                w=marker.width
            )
            plotted.colors <- c(plotted.colors,point.color)
        }
        write_function("new ol.Feature({")
        inid <- inid + 2
        if(self.contained){
            image.str <- paste("data:image/png;base64,",base64enc::base64encode(file.name))
        } else {
            image.str <- file.name
        }
        write_function(sprintf("img: \"%s\",",image.str))
        write_function(sprintf("name: \"Point%i\",",i))
        write_function(sprintf("scale: %1.2f,",pts.df$size[i]))
        write_function(sprintf("opacity: %1.2f,",point.opacity))
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
    write_function("image: new ol.style.Icon({")
    inid <- inid + 2
    write_function("scale: feature.get('scale'),")
    write_function("opacity: feature.get('opacity'),")
    write_function("src: feature.get('img'),")
    if(marker=='pin'){
        write_function("anchor: [0.5,0],")
    } else {
        write_function("anchor: [0.5,0.5],")
    }
    write_function("anchorOrigin: \"bottom-left\"")
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



draw_marker <- function(marker,file.name=NULL,color='green',opacity=1,w=40,theta=7*pi/24,scale.factor=1){
    if(missing(marker)) marker <- "pin"
    if(marker=="pin"){
        pts1<-50
        pts2<-150
        x0 <- (1/2/tan(theta))^2
        x1 <- (0:pts1)*x0/pts1
        r <- sqrt(4*x0^3 + x0^2)
        a <- (0:pts2)*(2*(pi-theta))/pts2-(pi/2-theta)
        d <- data.frame(
            x=scale.factor*c(
                x1,
                r*cos(a),
                rev(-x1)
            ),
            y=scale.factor*c(
                sqrt(x1),
                (r*sin(a) + sqrt(x0)*(2*x0+1)),
                rev(sqrt(x1))
            )
        )
        xmax <- scale.factor*r
        ymax <- scale.factor*2*sqrt(x0)*x0+sqrt(x0) + r
        xmin <- -scale.factor*r
        ymin <- scale.factor*0
        if(!is.null(file.name)){
            grDevices::png(file.name,bg=NA,width=w,height=round(w/(xmax-xmin)*(ymax-ymin)))
        }
        graphics::par(mar=c(0,0,0,0),oma=c(0,0,0,0))
        graphics::plot.new()
        graphics::plot.window(xlim=c(xmin,xmax),ylim=c(ymin,ymax),asp=1)
        graphics::polygon(d[,1],d[,2],col=color,border="black")
        if(!is.null(file.name)){
            grDevices::dev.off()
        }
    } else {
        pts <- 200
        r <- 0.075
        th <- (0:(pts-1))/(pts-1)*2*pi
        xmax <- r
        ymax <- r
        xmin <- -r
        ymin <- -r
        if(!is.null(file.name)){
            grDevices::png(file.name,bg=NA,width=w,height=w)
        }
        graphics::par(mar=c(0,0,0,0),oma=c(0,0,0,0))
        graphics::plot.new()
        graphics::plot.window(xlim=c(xmin,xmax),ylim=c(ymin,ymax),asp=1)
        graphics::polygon(r*cos(th),r*sin(th),col=color,border=color)
        if(!is.null(file.name)){
            grDevices::dev.off()
        }
    }
}
