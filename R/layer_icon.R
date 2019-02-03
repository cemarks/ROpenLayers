#' OpenLayers Icon Layer
#'
#' Function to create a point-icon layer to add to an OpenLayers Map object.
#'
#' This function stores the data required to generate an OpenLayers 
#' vector layer with features using \code{Point} 
#' geometries and user-supplied point icons.
#'
#' @section Aesthetics:
#' \itemize{
#' \item \code{iconimage}
#' \item \code{iconsize}
#' }
#' 
#' @param point.obj SpatialPointsDataframe, SpatialPoints, or a matrix 
#' containing columns of point longitudes and latitudes, respectively. 
#' @param src.img character vector of image file paths.
#' @param mapping list created by ol_aes. Used for aestheic mapping. 
#' @param name character Layer name.
#' @param df data.frame with same number of rows as \code{point.obj} coordinate
#' matrix.
#' @param toggle.control logical indicating whether this layer will have
#' a visibility toggle.
#' @param icon.size.scalar numeric scalar vector or 'autoscale'.  
#' The width of the icon on the map will be scaled by this input from the original
#' image width.  The default 'autoscale' uses the png, jpeg, or tiff package to
#' scale each image to \code{target.icon.width}.
#' @param src.img.width numeric vector of widths of user-supplied images, in pixels.
#' If \code{icon.size.scalar} is not supplied and \code{src.img.width} is not provided,
#' image widths will be detected using the png, jpeg, or tiff package.
#' @param target.icon.width numeric desired width of icons on map, in pixels.  Only used
#' if \code{icon.size.scalar} is 'autoscale'.
#' @param size.scale.lims numeric vector containing the minimum and maximum image scaling
#' for size aesthetic mappings.  A value of 1 results renders the image at the default size,
#' determined by \code{target.icon.width} or \code{icon.size.scalar}.
#' @param label character vector of point feature labels.
#' @param tooltip character vector of point feature tooltip popups.
#' @param label.params,tooltip.params named lists (e.g., \code{list(property=value)}) of 
#' label and tooltip position and format parameters.  See \link{ol_geom_polygon} documentation. 
#'
#' @return A list object of class \code{Layer.SpatialIcon}.
#'
#' @seealso 
#' \code{\link{ol_map}}, 
#' \code{\link{ol_geom_polygon}}, 
#' \code{\link{ol_geom_circle}},
#' \code{\link{ol_geom_line}}, 
#' \code{\link{ol_geom_point}}
#' 
#' @export
#'
#' @examples
#' some.r.servers <- matrix(
#'     c(
#'         144.964,-37.798,
#'         -122.920,49.278,
#'         121.494,31.307,
#'         25.083,35.307,
#'         -21.930,64.149,
#'         11.877,45.407,
#'         -99.200,19.345,
#'         5.322,60.388,
#'         -8.224,39.400,
#'         -8.616,41.147,
#'         -73.953,40.768,
#'         20.304,63.821,
#'         8.548,47.376,
#'         33.031,35.247,
#'         -78.938,36.001,
#'         -123.279,44.564,
#'         -96.797,32.777
#'     ),
#'     byrow=TRUE,
#'     ncol=2
#' )
#' r.server.names <- c(
#'     'School of Mathematics and Statistics, University of Melbourne',
#'     'Simon Fraser University, Burnaby',
#'     'Shanghai University',
#'     'University of Crete',
#'     'Marine Research Institute',
#'     'University of Padua',
#'     'Instituto Tecnologico Autonomo de Mexico',
#'     'University of Bergen',
#'     'RadicalDevelop, Lda',
#'     'University of Porto',
#'     'Four Dots',
#'     'Academic Computer Club, Umeå University',
#'     'ETH Zurich',
#'     'Middle East Technical University Northern Cyprus Campus, Mersin',
#'     'Duke University, Durham, NC',
#'     'Oregon State University',
#'     'Revolution Analytics, Dallas, TX'
#' )
#' r.icon <- "https://www.r-project.org/Rlogo.png"
#' ## If width is not provided image must be local
#' ## and png package must be installed.
#' r.icon.width <- 200 
#' r.map <- ol_map(
#'     center=c(-100,30),
#'     zoom=3
#' ) + 
#'     streetmap()+
#'     ol_geom_icon(
#'         some.r.servers,
#'         r.icon,
#'         name="R Servers",
#'         icon.size.scalar='autoscale',
#'         src.img.width=r.icon.width,
#'         toggle.control=TRUE,
#'         tooltip=r.server.names
#' ) 
#' \dontrun{
#' # Save as HTML and open in browser
#' ol_map2HTML(r.map,'R-servers.html')
#' browseURL("R-servers.html")
#' }
ol_geom_icon <- function(
    point.obj,
    src.img=NULL,
    mapping=ol_aes(),
    name=NULL,
    df=NULL,
    toggle.control=FALSE,
    icon.size.scalar='autoscale',
    src.img.width=NULL,
    target.icon.width=30,
    size.scale.lims=c(0.5,1.25),
    label=NULL,
    label.params=list(),
    tooltip=NULL,
    tooltip.params=list()
){
    icons <- data.frame(matrix(nrow=0,ncol=3))
    names(icons) <- c('src','scalar','width')
    get_repeat_vector_val <- function(src.vector,ind){
        if(is.null(src.vector)){
            return(NULL)
        } else {
            l <- length(src.vector)
            m <- ind %% l
            if(m==0) m <- l
            return(src.vector[m])
        }
    }
    if(length(src.img)>0){
        for(i in 1:length(src.img)){
            if(icon.size.scalar=='autoscale'){
                scale.and.width <- icon_autoscale(src.img[i],get_repeat_vector_val(src.img.width,i),get_repeat_vector_val(target.icon.width,i))
                s <- scale.and.width['scale.size']
                w <- scale.and.width['image.width']
            } else {
                s <- get_repeat_vector_val(icon.size.scalar,i)
                w <- get_repeat_vector_val(src.img.width,i)
            }
            if(is.null(w)){
                w <- NA
            }
            icons<- rbind(icons, data.frame(src=src.img[i],scalar=s, width=w,stringsAsFactors=FALSE))
        }
    }
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
    o[['toggle.control']] <- toggle.control
    o[['icons']] <- icons
    o[['target.icon.width']] <- target.icon.width
    n <- names(mapping)
    o[['scale']] <- list()
    i <- 1
    l.created <- FALSE
    if('iconsize' %in% names(mapping)){
        mapping[['size']]=mapping[['iconsize']]
    }
    if('iconimage' %in% names(mapping)){
        mapping[['icon']]=mapping[['iconimage']]
    }
    if('size' %in% names(mapping)){
        size.column = as.character(mapping['size'])
        if(is.numeric(df[,size.column])){
            o[['scale']][[i]] <- ol_scale_iconsize_continuous(
                low.val=min(df[,size.column],na.rm=TRUE),
                high.val=max(df[,size.column],na.rm=TRUE),
                low.size=size.scale.lims[1],
                high.size=size.scale.lims[2],
                name=size.column
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
                size.scale.lims
            )
            o[['scale']][[i]] <- ol_scale_iconsize_discrete(
                size.vector=size.vector,
                name=size.column
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
            attribute <- "iconsize",
            values <- 1
        )
    }
    o[['scale']][[i]][['icons']] <- icons
    i <- i+1
    l.created <- FALSE
    if('icon' %in% names(mapping)){
        icon.column = as.character(mapping['icon'])
        if(!l.created){
            icon.vector.names <- unique(
                stats::na.omit(as.character(df[,icon.column]))
            )
            if(length(src.img)>0){
                img.src.repeated <- rep(src.img,ceiling(length(icon.vector.names)/length(src.img)))
                icon.vector <- img.src.repeated[1:length(icon.vector.names)]
                names(icon.vector) <- icon.vector.names
            } else{
                icon.vector <- NULL
            }
            o[['scale']][[i]] <- ol_scale_iconimage_discrete(
                icon.img.vector=icon.vector,
                name=icon.column
            )
            if(!is.null(o[['scale']][[i]])){
                l.created <- TRUE
            } else {
                warning("Discrete icon scale not created, trying fixed scale")
            }
        }
        if(l.created){
            o[['scale']][[i]][['variable.values']] <- df[,icon.column]
            o[['scale']][[i]][['variable.name']] <- icon.column
        }
    }
    if(!l.created){
        o[['scale']][[i]] <- ol_scale_fixed(
            attribute <- "iconimage",
            values <- src.img
        )
    }
    o[['scale']][[i]][['icons']] <- icons
    o[["features"]]<-sp.point@coords
    o[['feature.count']]<-nrow(o[["features"]])
    default.label.params <- list(offsetX=8,offsetY=2,textAlign='left',textBaseline='bottom')
    for(lp in setdiff(names(default.label.params),names(label.params))){
        label.params[[lp]]<-default.label.params[[lp]]
    }
    o[['label']] <- get_ol_layer_label(df,mapping,label,label.params)
    o[['tooltip']] <- get_ol_layer_tooltip(df,mapping,tooltip,tooltip.params)
    class(o) <- "Layer.SpatialPointIcon"
    return(o)
}

writeLayer.Layer.SpatialPointIcon <- function(layer,suffix="basemap",nice.format=TRUE,initial.indent=6,...){
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
    for(s in 1:length(layer[['scale']])){
        s.attr <- layer[['scale']][[s]][['attribute']]
        if(layer[['scale']][[s]][['type']] == 'fixed'){
            v1 <- rep(layer[['scale']][[s]][['value']],ceiling(nrow(pts.df)/length(layer[['scale']][[s]][['value']])))
            new.col<-data.frame(v1[1:nrow(pts.df)])
        } else if(!(is.null(layer[['scale']][[s]][['function']]))){
            new.col <- data.frame(sapply(layer[['scale']][[s]][['variable.values']],layer[['scale']][[s]][['function']]))
        } else {
            warning(sprintf("Unable to assign %s aesthetic.",layer[['scale']][[s]][['attribute']]))
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
    for(i in 1:nrow(pts.df)){
        file.name <- as.character(pts.df$iconimage[i])
        scale.multiplier <- layer[['icons']]$scalar[which(layer[['icons']]$src==file.name)]
        write_function("new ol.Feature({")
        inid <- inid + 2
        image.str <- paste("data:image/png;base64,",base64enc::base64encode(file.name))
        write_function(sprintf("img: \"%s\",",image.str))
        write_function(sprintf("name: \"Icon%i\",",i))
        write_function(sprintf("scale: %1.6f,",pts.df$iconsize[i]*scale.multiplier))
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
    write_function("anchor: [0.5,0.5],")
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

image_width <- function(file.name){
    file.name.split<-strsplit(file.name,".",fixed=TRUE)[[1]]
    file.name.ext <- file.name.split[length(file.name.split)]
    file.width.found<-FALSE
    if(tolower(file.name.ext)=='png'){
        if(requireNamespace("png",quietly=TRUE)){
            img <- png::readPNG(file.name)
            image.width <- dim(img)[2]
            file.width.found <- TRUE
        } else {
            stop(sprintf("PNG file detected, but 'png' package not available.  Unable to detect image width for %s.",file.name))
        }
    } else if(tolower(file.name.ext)=='jpg' || tolower(file.name.ext)=='jpeg'){
        if(requireNamespace("jpeg",quietly=TRUE)){
            img <- jpeg::readJPEG(file.name)
            image.width <- dim(img)[2]
            file.width.found <- TRUE
        } else {
            stop(sprintf("JPEG file detected, but 'jpeg' package not available.  Unable to detect image width for %s.",file.name))
        }
    } else if(tolower(file.name.ext)=='tiff'){
        if(requireNamespace("tiff",quietly=TRUE)){
            img <- tiff::readTIFF(file.name)
            image.width <- dim(img)[2]
            file.width.found <- TRUE
        } else {
            stop(sprintf("TIFF file detected, but 'tiff' package not available.  Unable to detect image width for %s.",file.name))
        }
    }
    if(!file.width.found){
        stop(sprintf("Unsupported format.  Unable to detect image width for %s.",file.name))
    }
    return(image.width)
}

icon_autoscale <- function(img.src,img.width,target.width=30){
    if(missing(img.src) && (missing(img.width) || is.null(img.width))){
        stop("img.src or img.width must be supplied to icon_autoscale")
    } else if(missing(img.width) || is.null(img.width)){
        img.width <- image_width(img.src)
    }
    scale.size <- target.width/img.width
    return(c(scale.size=scale.size,image.width=img.width))
}
