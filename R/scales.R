get_color_scale_continuous <- function(
    low.val,
    high.val,
    low.col=NULL,
    high.col=NULL,
    na.col.val = "#FFFFFF00",
    opacity=1,
    rotate.clockwise=TRUE
    ){
    if(opacity > 1){
        op <- 1
    } else if(opacity < 0){
        op <- 0
    } else {
        op <- opacity
    }
    if(is.null(low.col) || is.null(high.col)){
        l.h <- 0
        l.s <- 0.75
        l.v <- 0.75
        h.h <- 0.4
        h.s <- 0.85
        h.v <- 0.9
        get_color_gen1 <- function(lv,hv,h.min,s.min,v.min,h.max,s.max,v.max,opac,na.col.gen){
            o <- function(df.var,na.col=na.col.gen){
                if(is.na(df.var) || is.null(df.var) || !(is.numeric(df.var))){
                    color <- color_check(na.col,opac)
                } else {
                    if(df.var > hv){
                        color <- grDevices::hsv(h.max,s.max,v.max)
                    } else if(df.var < lv){
                        color <- grDevices::hsv(h.min,s.min,v.min)
                    } else {
                        if(rotate.clockwise){
                            if(h.max >= h.min){
                                l <- 1-(df.var-lv)/(hv-lv)
                                hh <- h.min*l + h.max*(1-l)
                                ss <- s.min*l + s.max*(1-l)
                                vv <- v.min*l + v.max*(1-l)
                                color <- grDevices::hsv(hh,ss,vv)
                            } else {
                                h.max <- h.max + 1
                                l <- 1-(df.var-lv)/(hv-lv)
                                hh <- h.min*l + h.max*(1-l)
                                ss <- s.min*l + s.max*(1-l)
                                vv <- v.min*l + v.max*(1-l)
                                if(hh >=1){
                                    hh <- hh - 1
                                }
                                color <- grDevices::hsv(hh,ss,vv)
                            }
                        } else {
                            if(h.max <= h.min){
                                l <- 1-(df.var-lv)/(hv-lv)
                                hh <- h.min*l + h.max*(1-l)
                                ss <- s.min*l + s.max*(1-l)
                                vv <- v.min*l + v.max*(1-l)
                                color <- grDevices::hsv(hh,ss,vv)
                            } else {
                                h.min <- h.min + 1
                                l <- 1-(df.var-lv)/(hv-lv)
                                hh <- h.min*l + h.max*(1-l)
                                ss <- s.min*l + s.max*(1-l)
                                vv <- v.min*l + v.max*(1-l)
                                if(hh >= 1){
                                    hh <- hh - 1
                                }
                                color <- grDevices::hsv(hh,ss,vv)
                            }
                        }
                    }
                    color <- paste(
                        color,
                        format(as.hexmode(round((opac)*255)),width=2,upper.case=TRUE),
                        sep=""
                        )
                }
                return(color)
            }
            return(o)
        }
        get_color <- get_color_gen1(low.val,high.val,l.h,l.s,l.v,h.h,h.s,h.v,opacity,na.col.val)
        return(get_color)
    } else {
        if(
            is.character(low.col) &&
            substr(low.col,1,1) == "#" &&
            nchar(low.col) == 9 &&
            is.character(high.col) &&
            substr(high.col,1,1) == "#" &&
            nchar(high.col) == 9
            ){
            op.min <- strtoi(paste("0x",substr(low.col,8,9),sep=""))/255
            op.max <- strtoi(paste("0x",substr(high.col,8,9),sep=""))/255
        } else {
            op.min <- opacity
            op.max <- opacity
        }
        get_color_gen2 <- function(lv,hv,lc,hc,min.opac,max.opac,na.col.gen){
            hsv.min <- rgb2hsv(t(lc))
            hsv.max <- rgb2hsv(t(hc))
            if(hsv.max[1] < hsv.min[1] && rotate.clockwise){
                hsv.max[1] <- hsv.max[1] + 1
            } else if(hsv.max[1] > hsv.min[1] && !rotate.clockwise) {
                hsv.min[1] <- hsv.min[1] + 1
            }
            o <- function(df.var,na.col=na.col.gen){
                if(is.na(df.var) || is.null(df.var) || !(is.numeric(df.var))){
                    color <- color_check(na.col,mean(c(min.opac,max.opac)))
                } else {
                    l <- 1-(df.var-lv)/(hv-lv)
                    if (l > 1){
                        new.c <- lc
                        new.op <- min.opac
                    } else if(l < 0){
                        new.c <- hc
                        new.op <- max.opac
                    } else {
                        linear.comb <- hsv.min*l + hsv.max*(1-l)
                        if(linear.comb[1]>1) linear.comb[1] <- linear.comb[1]-1
                        new.c <- grDevices::hsv(linear.comb[1],linear.comb[2],linear.comb[3])
                        new.op <- min.opac*l + max.opac*(1-l)
                    }
                    color <- color_check(new.c,new.op)
                }
                return(color)
            }
            return(o)
        }
        get_color <- get_color_gen2(low.val,high.val,low.col,high.col,op.min,op.max,na.col.val)
        return(get_color)
    }
}

get_default_color_vec_discrete <- function(
    values,
    low.col=NULL,
    high.col=NULL,
    opacity = 1,
    rotate.clockwise=TRUE
    ){
    if(is.null(values)){
        return(NULL)
    } else {
        uv <- sort(stats::na.omit(unique(values)),decreasing=TRUE)
        f <- get_color_scale_continuous(
            0,
            1,
            low.col=low.col,
            high.col=high.col,
            opacity=opacity,
            rotate.clockwise=rotate.clockwise
        )
        color.vector <- sapply((0:(1-length(uv)))/(1-length(uv)),f)
        names(color.vector)<- uv
        return(color.vector)
    }
}

get_color_scale_discrete <- function(
    color.vector,
    na.col.val="#FFFFFF00",
    opacity = 1
    ){
    f <- function(color.vector,opac,na.col.gen){
        o <- function(val){
            if(is.na(val) || is.null(val) || !(as.character(val) %in% names(color.vector))){
                return(color_check(na.col.gen,opac))
            }else{
                return(color_check(color.vector[as.character(val)],opac))
            }
        }
        return(o)
    }
    return(f(color.vector,opacity,na.col.val))
}


get_default_num_vec_discrete <- function(
    values,
    num.range
){
    u.vals <- sort(stats::na.omit(unique(values)),decreasing=TRUE)
    if(missing(num.range) || length(num.range) != 2){
        num.range = c(1,5)
    }
    if(is.numeric(u.vals)){
        u.vals <- sort(u.vals)
    }
    n <- length(u.vals)
    o <- (0:(n-1))/(n-1)*(num.range[2]-num.range[1]) + num.range[1]
    names(o) <- u.vals
    return(o)
}

get_num_scale_continuous <- function(
    low.val,
    high.val,
    low.num=1,
    high.num=5,
    na.num.val=0
    ){
    f <- function(low.v,high.v,low.n,high.n,na.num){
        o <- function(val){
            if(is.na(val) || is.null(val)){
                return(na.num)
            }else if(val < low.v){
                return(low.n)
            } else if(val > high.v){
                return(high.n)
            } else {
                l <- (high.v-val)/(high.v-low.v)
                return(l*low.n + (1-l)*high.n)
            }
        }
        return(o)
    }
    return(f(low.val,high.val,low.num,high.num,na.num.val))
}


get_num_scale_discrete <- function(
    num.vector,
    na.num.val=1
    ){
    f <- function(num.vec,na.num){
        o <- function(val){
            if(is.na(val) || is.null(val) || !(as.character(val) %in% names(num.vec))){
                return(na.num)
            }else{
                return(num.vec[as.character(val)])
            }
        }
        return(o)
    }
    return(f(num.vector,na.num.val))
}

get_default_lty_list <- function(
    values
){
    ll <- list(NA,c(8,8),c(8,16),c(8,24),c(16,25),c(5,20),c(10,20),c(8,20))
    i<-1
    u.vals <- sort(stats::na.omit(unique(values)),decreasing=TRUE)
    n <- length(u.vals)
    lty.list<-list()
    for(i in 1:n){
        if(i %% length(ll) == 0){
            j <- length(ll)
        } else {
            j <- i %% length(ll)
        }
        lty.list[[as.character(u.vals[[i]])]] <- ll[[j]]
    }
    return(lty.list)
}



get_lty_scale_discrete <- function(
    lty.list,
    na.lty.val = NA
    ){
    f <- function(lty.list,na.lty){
        o <- function(val){
            if(is.na(val) || is.null(val) || !(as.character(val) %in% names(lty.list))){
                return.lty <- na.lty
            }else{
                return.lty <- lty.list[[as.character(val)]]
            }
            if(length(return.lty) > 1){
                return(return.lty)
            } else {
                return(NA)
            }
        }
        return(o)
    }
    return(f(lty.list,na.lty.val))
}


#' Fill Color Scale (Continuous)
#'
#' Specify a continuous fill color mapping scale.
#'
#' This method maps OpenLayers feature fill colors to continuous 
#' variable values.  This scale can be added to an Ol.Map S3 object
#' only if the Ol.Map object has a layer with a "fill" mapping to a
#' numeric variable.  If no such layer exists, attempts to add this type
#' of scale will result in a warning.  Attempts to add this scale
#' to a discrete variable mapping will throw an error.
#' 
#' @param low.val numeric the minimum variable value to be mapped 
#' to the lowest color.
#' @param high.val numeric the maximum variable value to be mapped
#' to the highest color.
#' @param low.col character the "low" color.
#' @param high.col character the "high" color.
#' @param rotate.clockwise logical.  If \code{TRUE}, continuous scale
#' will map to colors on a clockwise rotation from \code{low.col} to
#' \code{high.col} on the hue-saturation-value (HSV) color space.  If 
#' \code{FALSE}, rotation will be counter-clockwise.
#' @param name character the scale name.
#' @param na.col.val character the color assigned to non-numeric or
#' NA values.
#' @param opacity numeric in [0,1]. The fill opacity, if not specified in
#' the \code{low.col} and \code{high.col} colors.
#' @param preserve.opacity logical indicating whether to draw the legend
#' with the same opacity as the feature fills on the map.
#' @param display logical indicating whether to draw the scale for output
#' in the HTML.  If \code{TRUE}, a bitmap will be created and sourced in 
#' the HTML in an \code{img} tag.
#'
#' @return list of class \code{Scale.Fill.Continuous}.
#'
#' @seealso 
#' \code{\link{ol_map}},
#' \code{\link{ol_geom_polygon}},
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
#' polygon.df <- data.frame(shape=c("rectangle","triangle"),no=runif(2))
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
#'         fill=no #numeric mapping
#'     ),
#'     df=polygon.df,
#'     lwd=1,
#'     name="Miami Polygons",
#'     toggle.control=TRUE,
#'     tooltip=polygon.df$no
#' )
#' polygon.fill.scale <- ol_scale_fill_continuous(
#'     low.val=0,
#'     high.val=1,
#'     low.col='red',
#'     high.col='green',
#'     opacity=0.5,
#'     preserve.opacity=FALSE,
#'     display=TRUE
#' )
#' polygons.over.miami <- miami.OSM.basemap + 
#'     polygon.layer + 
#'     polygon.fill.scale
#'
#' ## Not Run: output to file and view
#' # ol_map2HTML(polygons.over.miami,'miami_polygons.html')
#' # browseURL("miami_polygons.html")
ol_scale_fill_continuous <- function(
        low.val,
        high.val,
        low.col=NULL,
        high.col=NULL,
        rotate.clockwise=TRUE,
        name=NULL,
        na.col.val = "#FFFFFF00",
        opacity=1,
        preserve.opacity=FALSE,
        display=FALSE
    ) {
    if(missing(low.val) || missing(high.val)){
        f <- NULL
        low.val <- NULL
        high.val <- NULL
    } else {
        f <- get_color_scale_continuous(
                low.val,
                high.val,
                low.col,
                high.col,
                na.col.val,
                opacity,
                rotate.clockwise
        )
    }
    o <- list(
        low.val=low.val,
        high.val=high.val,
        "attribute"="fill",
        "function"=f,
        display=display,
        type="continuous",
        name=name,
        preserve.opacity=preserve.opacity
    )
    class(o) <-'Scale.Fill.Continuous'
    return(o)
}

#' Fill Color Scale (Discrete)
#'
#' Specify a discrete fill color mapping scale.
#'
#' This method maps OpenLayers feature fill colors to discrete 
#' variable values.  This scale can be added to an Ol.Map S3 object
#' only if the Ol.Map object has a layer with a "fill" mapping to a
#' numeric, character, or factor type variable.  If no such layer 
#' exists, attempts to add this type of scale will result in a warning.
#' The \code{color.vector} input enables the user to specify the exact 
#' mapping, assigning colors to specific variable values.
#' 
#' @param color.vector character named vector of the form 
#' \code{c(value=color)}.  If \code{NULL}, a default color mapping
#' is assigned.
#' @param name character the scale name.
#' @param na.col.val character the color assigned to unrecognized or
#' NA values.
#' @param ordered.values character, numeric, or factor vector containing the 
#' ordered unique discrete variable values.  This input is used to determine
#' the order of the values appearing in the legend.  If not supplied, the 
#' order is taken from \code{names(color.vector)}.
#' @param ordinal.scale logical.  If \code{TRUE}, the colors in the legend
#' will not have spaces between them.
#' @param opacity numeric in [0,1]. The fill opacity, if not specified in
#' the \code{color.vector} colors.
#' @param preserve.opacity logical indicating whether to draw the legend
#' with the same opacity as the feature fills on the map.
#' @param draw.lines logical indicating whether to draw a border
#' around each color in the legend.  If \code{NULL}, a default is assigned
#' according to the type of layer containing the scale.
#' @param draw.color character color of the border in the legend.
#' @param draw.lty character indicating the border line type for the legend
#' only.  This will be passed to an R plot command.  See \code{\link{par}}.
#' @param draw.lwd numeric width of border only used in drawing the legend.
#' @param display logical indicating whether to draw the scale for output
#' in the HTML.  If \code{TRUE}, a bitmap will be created and sourced in 
#' the HTML in an \code{img} tag.  If \code{FALSE}, the \code{draw.*} 
#' inputs are ignored.
#'
#' @return list of class \code{Scale.Fill.Discrete}.
#'
#' @seealso 
#' \code{\link{ol_map}},
#' \code{\link{ol_geom_polygon}},
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
#' polygon.df <- data.frame(shape=c("rectangle","triangle"),no=runif(2))
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
#'         fill=shape #discrete mapping
#'     ),
#'     df=polygon.df,
#'     lwd=1,
#'     name="Miami Polygons",
#'     toggle.control=TRUE
#' )
#' polygon.fill.scale <- ol_scale_fill_discrete(
#'     color.vector=c(
#'         rectangle = 'red',
#'         triangle = 'blue'
#'     ),
#'     name = "Shape",
#'     opacity = 0.5,
#'     preserve.opacity = FALSE,
#'     display = TRUE
#' )
#' polygons.over.miami <- miami.OSM.basemap + 
#'     polygon.layer + 
#'     polygon.fill.scale
#'
#' ## Not Run: output to file and view
#' # ol_map2HTML(polygons.over.miami,'miami_polygons.html')
#' # browseURL("miami_polygons.html")
ol_scale_fill_discrete <- function(
        color.vector=NULL,
        name=NULL,
        na.col.val="#FFFFFF00",
        ordered.values = NULL,
        ordinal.scale=FALSE,
        opacity = 1,
        preserve.opacity=FALSE,
        draw.lines=NULL,
        draw.color='black',
        draw.lty = 'solid',
        draw.lwd = 1,
        display=FALSE
    ) {
    if(is.null(color.vector)){
        f <- NULL
    } else {
        f <- get_color_scale_discrete(
            color.vector=color.vector,
            na.col.val=na.col.val,
            opacity = opacity
        )
        if(is.null(ordered.values)){
            ordered.values <- names(color.vector)
        }
    }   
    o <- list(
        "attribute"="fill",
        'function'=f,
        vector = color.vector[ordered.values],
        display=display,
        type="discrete",
        name=name,
        ordered.values=ordered.values,
        ordinal.scale=ordinal.scale,
        preserve.opacity=preserve.opacity,
        draw.lines=draw.lines,
        draw.color=draw.color,
        draw.lty=draw.lty,
        draw.lwd=draw.lwd
    )
    class(o) <-'Scale.Fill.Discrete'
    return(o)
}


ol_scale_fixed <- function(attribute,values){
    o <- list(
        "attribute"=attribute,
        "type"="fixed",
        "value"=values
        )
    class(o)<-"Scale.Fixed"
    return(o)
}



#' Line Color Scale (Continuous)
#'
#' Specify a line (or border) color mapping scale.
#'
#' This method maps OpenLayers feature line or border colors to continuous 
#' variable values.  This scale can be added to an Ol.Map S3 object
#' only if the Ol.Map object has a layer with a "color" mapping to a
#' numeric variable.  If no such layer exists, attempts to add this type
#' of scale will result in a warning.  Attempts to apply this scale to a 
#' non-numeric variable will throw an error.
#' 
#' @param low.val numeric the minimum variable value to be mapped 
#' to the lowest color.
#' @param high.val numeric the maximum variable value to be mapped
#' to the highest color.
#' @param low.col character the "low" color.
#' @param high.col character the "high" color.
#' @param rotate.clockwise logical.  If \code{TRUE}, continuous scale
#' will map to colors on a clockwise rotation from \code{low.col} to
#' \code{high.col} on the hue-saturation-value (HSV) color space.  If 
#' \code{FALSE}, rotation will be counter-clockwise.
#' @param name character the scale name.
#' @param na.col.val character the color assigned to non-numeric or
#' NA values.
#' @param opacity numeric in [0,1]. The fill opacity, if not specified in
#' the \code{low.col} and \code{high.col} colors.
#' @param preserve.opacity logical indicating whether to draw the legend
#' with the same opacity as the feature fills on the map.
#' @param display logical indicating whether to draw the scale for output
#' in the HTML.  If \code{TRUE}, a bitmap will be created and sourced in 
#' the HTML in an \code{img} tag.
#'
#' @return list of class \code{Scale.Color.Continuous}.
#'
#' @seealso
#' \code{\link{ol_map}},
#' \code{\link{ol_geom_polygon}},
#' \code{\link{ol_geom_line}} 
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
#'     no=runif(2)
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
#'         color=no #continuous mapping
#'     ),
#'     df=line.df,
#'     name="Miami Lines",
#'     toggle.control=TRUE,
#'     lwd=5,
#'     tooltip=line.df$no
#' )
#' line.color.scale <- ol_scale_color_continuous(
#'     low.val = 0,
#'     high.val = 1,
#'     low.col = 'red',
#'     high.col= 'green',
#'     opacity = 1,
#'     preserve.opacity = TRUE,
#'     name = "Number",
#'     display = TRUE
#' )
#' line.map.miami <- miami.gray.basemap + 
#'     line.layer + 
#'     line.color.scale
#' ## Not Run: output to file and view
#' # ol_map2HTML(line.map.miami,'miami_lines.html')
#' # browseURL("miami_lines.html")
ol_scale_color_continuous <- function(
        low.val,
        high.val,
        low.col=NULL,
        high.col=NULL,
        rotate.clockwise=TRUE,
        name=NULL,
        na.col.val = "#FFFFFF00",
        opacity=1,
        preserve.opacity=NULL,
        display=FALSE
    ) {
    if(missing(low.val) || missing(high.val)){
        f <- NULL
        low.val <- NULL
        high.val <- NULL
    } else {
        f <- get_color_scale_continuous(
            low.val,
            high.val,
            low.col,
            high.col,
            na.col.val,
            opacity,
            rotate.clockwise
        )
    }
    o <- list(
        low.val=low.val,
        high.val=high.val,
        "attribute"="color",
        "function"=f,
        display=display,
        type="continuous",
        name=name,
        preserve.opacity=preserve.opacity
    )
    class(o) <-'Scale.Color.Continuous'
    return(o)
}


#' Line Color Scale (Discrete)
#'
#' Specify a discrete line color mapping scale.
#'
#' This method maps OpenLayers feature line colors to discrete 
#' variable values.  This scale can be added to an Ol.Map S3 object
#' only if the Ol.Map object has a layer with a "color" mapping to a
#' numeric, character, or factor type variable.  If no such layer 
#' exists, attempts to add this type of scale will result in a warning.
#' The \code{color.vector} input enables the user to specify the exact 
#' mapping, assigning colors to specific variable values.
#' 
#' @param color.vector character named vector of the form 
#' \code{c(value=color)}.  If \code{NULL}, a default color mapping
#' is assigned.
#' @param name character the scale name.
#' @param na.col.val character the color assigned to unrecognized or
#' NA values.
#' @param ordered.values character, numeric, or factor vector containing the 
#' ordered unique discrete variable values.  This input is used to determine
#' the order of the values appearing in the legend.  If not supplied, the 
#' order is taken from \code{names(color.vector)}.
#' @param opacity numeric in [0,1]. The color opacity, if not specified in
#' the \code{color.vector} colors.
#' @param preserve.opacity logical indicating whether to draw the legend
#' with the same opacity as the feature fills on the map.
#' @param draw.lty character indicating the line type for the legend
#' only.  This will be passed to an R plot command.  See \code{\link{par}}.
#' @param draw.lwd numeric width of lines only used in drawing the legend.
#' @param display logical indicating whether to draw the scale for output
#' in the HTML.  If \code{TRUE}, a bitmap will be created and sourced in 
#' the HTML in an \code{img} tag.  If \code{FALSE}, the \code{draw.*} 
#' inputs are ignored.
#'
#' @return list of class \code{Scale.Color.Discrete}.
#'
#' @seealso 
#' \code{\link{ol_map}},
#' \code{\link{ol_geom_polygon}},
#' \code{\link{ol_geom_line}} 
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
#'     no=runif(2)
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
#'         color=direction # discrete mapping
#'     ),
#'     df=line.df,
#'     name="Miami Lines",
#'     toggle.control=TRUE,
#'     lwd=5
#' )
#' line.color.scale <- ol_scale_color_discrete(
#'     color.vector=c(
#'         vertical = 'red',
#'         horizontal = 'blue'
#'     ),
#'     name="Direction",
#'     display=TRUE
#' )
#' line.map.miami <- miami.gray.basemap + 
#'     line.layer + 
#'     line.color.scale
#' ## Not Run: output to file and view
#' # ol_map2HTML(line.map.miami,'miami_lines.html')
#' # browseURL("miami_lines.html")
ol_scale_color_discrete <- function(
        color.vector=NULL,
        name=NULL,
        na.col.val="#FFFFFF00",
        ordered.values = NULL,
        opacity = 1,
        preserve.opacity=FALSE,
        draw.lty = 'solid',
        draw.lwd = 3,
        display=FALSE
    ) {
    if(is.null(color.vector)){
        f <- NULL
    } else {
        f <- get_color_scale_discrete(
            color.vector=color.vector,
            na.col.val=na.col.val,
            opacity = opacity
        )
        if(is.null(ordered.values)){
            ordered.values <- names(color.vector)
        }
    }   
    o <- list(
        "attribute"="color",
        "function"=f,
        vector = color.vector[ordered.values],
        display=display,
        type="discrete",
        name=name,
        ordered.values=ordered.values,
        preserve.opacity=preserve.opacity,
        draw.lty=draw.lty,
        draw.lwd=draw.lwd
    )
    class(o) <-'Scale.Color.Discrete'
    return(o)
}


#' Line Width Scale
#'
#' Specify a discrete line width mapping scale.
#'
#' This method maps OpenLayers feature line widths to discrete 
#' variable values.  This scale can be added to an Ol.Map S3 object
#' only if the Ol.Map object has a layer with a "lwd" mapping to a
#' numeric, character, or factor type variable.  If no such layer 
#' exists, attempts to add this type of scale will result in a warning.
#' The \code{lwd.vector} input enables the user to specify the exact 
#' mapping, assigning widths to specific variable values.  The width
#' aesthetic does not have a continuous scale.
#' 
#' @param lwd.vector numeric named vector of the form 
#' \code{c(value=width)}.  If \code{NULL}, a default width mapping
#' is assigned.
#' @param name character the scale name.
#' @param na.lwd.val numeric the width assigned to unrecognized or
#' NA values.
#' @param ordered.values character, numeric, or factor vector containing the 
#' ordered unique discrete variable values.  This input is used to determine
#' the order of the values appearing in the legend.  If not supplied, the 
#' order is taken from \code{names(lwd.vector)}.
#' @param opacity numeric in [0,1]. The line opacity used in the legend only.
#' @param draw.color character color used in the legend lines only.
#' @param draw.lty character indicating the line type for the legend
#' only.  This will be passed to an R plot command.  See \code{\link{par}}.
#' @param display logical indicating whether to draw the scale for output
#' in the HTML.  If \code{TRUE}, a bitmap will be created and sourced in 
#' the HTML in an \code{img} tag.  If \code{FALSE}, the \code{draw.*} 
#' inputs are ignored.
#'
#' @return list of class \code{Scale.Lwd.Discrete}.
#'
#' @seealso 
#' \code{\link{ol_map}},
#' \code{\link{ol_geom_polygon}},
#' \code{\link{ol_geom_line}} 
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
#'     no=runif(2)
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
#'         lwd=direction # discrete mapping
#'     ),
#'     df=line.df,
#'     name="Miami Lines",
#'     toggle.control=TRUE
#' )
#' line.width.scale <- ol_scale_lwd_discrete(
#'     lwd.vector=c(
#'         vertical = 2,
#'         horizontal = 5
#'     ),
#'     name="Direction",
#'     display=TRUE
#' )
#' line.map.miami <- miami.gray.basemap + 
#'     line.layer + 
#'     line.width.scale
#' ## Not Run: output to file and view
#' # ol_map2HTML(line.map.miami,'miami_lines.html')
#' # browseURL("miami_lines.html")
ol_scale_lwd_discrete <- function(
        lwd.vector=NULL,
        name=NULL,
        na.lwd.val=1,
        ordered.values = NULL,
        opacity = 1,
        draw.color='black',
        draw.lty = 'solid',
        display=FALSE
    ) {
    if(is.null(lwd.vector)){
        f <- NULL
    } else {
        f <- get_num_scale_discrete(
            num.vector=lwd.vector,
            na.num.val=na.lwd.val
        )
        if(is.null(ordered.values)){
            ordered.values <- names(lwd.vector)
        }
    }   
    o <- list(
        "attribute"="lwd",
        "function"=f,
        vector = lwd.vector[ordered.values],
        display=display,
        type="discrete",
        name=name,
        ordered.values=ordered.values,
        draw.color=draw.color,
        draw.lty=draw.lty
    )
    class(o) <-'Scale.Lwd.Discrete'
    return(o)
}



#' Point Size Scale (Continuous)
#'
#' Specify a continuous size mapping for a point layer.
#'
#' This method maps OpenLayers point feature sizes to continuous 
#' variable values.  This scale can be added to an Ol.Map S3 object
#' only if the Ol.Map object has a layer with a "size" mapping to a
#' numeric variable.  If no such layer exists, attempts to add this type
#' of scale will result in a warning.  Attempt to apply this scale
#' to a non-numeric variable will throw an error.  
#' 
#' Size inputs to this method are applied as scalars to the icon widths.
#' A size value of 1 translates to an icon width of 40 pixels for 
#' "pin" markers, or 20 pixels for "dot" markers.
#' 
#' @param low.val numeric the minimum variable value to be mapped 
#' to the smallest size.
#' @param high.val numeric the maximum variable value to be mapped
#' to the largest size.
#' @param low.size numeric smallest size scalar.
#' @param high.size numeric largest size scalar.
#' @param name character the scale name.
#' @param na.size.val numeric the size scalar assigned to non-numeric or
#' NA values.
#' @param draw.fill character fill color for drawing points in the legend
#' only. 
#' @param legend.breaks numeric ordered vector of variable values to display
#' in the legend.
#' @param display logical indicating whether to draw the scale for output
#' in the HTML.  If \code{TRUE}, a bitmap will be created and sourced in 
#' the HTML in an \code{img} tag.
#'
#' @return list of class \code{Scale.Size.Continuous}.
#'
#' @seealso
#' \code{\link{ol_map}},
#' \code{\link{ol_geom_point}},
#'
#' @export
#'
#' @examples
#' point.matrix <- matrix(
#'     c(
#'         -80.885+runif(10),
#'         25.223+runif(10)
#'     ),
#'     ncol=2
#' )
#' point.df <- data.frame(
#'     pt.type=sample(c("A","B"),10,replace=TRUE),
#'     pt.numeric=runif(10)*10
#' )
#' miami.points.map <- ol_map(
#'     center=c(-80.385790,25.782618),
#'     zoom=10
#' ) + 
#'     public_OSM_basemap()+
#'     ol_geom_point(
#'         point.matrix,
#'         df=point.df,
#'         mapping=ol_aes(
#'             size=pt.numeric # continuous mapping
#'         ), 
#'         name="Point",
#'         marker="pin",
#'         fill='green',
#'         toggle.control=TRUE,
#'         tooltip=sprintf("%1.2f",point.df$pt.numeric)
#' ) + 
#'     ol_scale_size_continuous(
#'     high.val=10,
#'     low.val=0,
#'     high.size=0.66,
#'     low.size=0.33,
#'     draw.fill='green',
#'     legend.breaks=c(0,3.33,6.67,10),
#'     display=TRUE
#' )
#' ## Not run: save to file and open on browser
#' # ol_map2HTML(miami.points.map,'pointsizes.html')
#' # browseURL('pointsizes.html')
ol_scale_size_continuous <- function(
        low.val,
        high.val,
        low.size=0.33,
        high.size=0.75,
        name=NULL,
        na.size.val = 0.33,
        draw.fill=NULL,
        legend.breaks=NULL,
        display=FALSE
    ) {
    if(missing(low.val) || missing(high.val)){
        f <- NULL
        low.val <- NULL
        high.val <- NULL
    } else {
        f <- get_num_scale_continuous(
            low.val,
            high.val,
            low.size,
            high.size,
            na.size.val
        )
        if(is.null(legend.breaks)){
            legend.breaks <- (0:4)*(high.val-low.val)/4+low.val
        }
    }
    o <- list(
        low.val=low.val,
        high.val=high.val,
        "attribute"="size",
        "function"=f,
        display=display,
        type="continuous",
        name=name,
        draw.fill=draw.fill,
        legend.breaks = legend.breaks
    )
    class(o) <-'Scale.Size.Continuous'
    return(o)
}

#' Point Size Scale (Discrete)
#'
#' Specify a discrete size mapping for a point layer.
#'
#' This method maps OpenLayers point feature sizes to a discrete set of
#' variable values.  This scale can be added to an Ol.Map S3 object
#' only if the Ol.Map object has a layer with a "size" mapping to a
#' numeric, character, or factor variable.  If no such layer exists, 
#' attempts to add this type of scale will result in a warning.  
#' 
#' Size inputs to this method are applied as scalars to the icon widths.
#' A size value of 1 translates to an icon width of 40 pixels for 
#' "pin" markers, or 20 pixels for "dot" markers.
#' 
#' @param size.vector numeric named vector of the form 
#' \code{c(value=width)}.  If \code{NULL}, a default size mapping
#' is assigned.
#' @param name character scale name.
#' @param na.size.val numeric the size scalar assigned to unrecognized or
#' NA values.
#' @param draw.fill character fill color for drawing points in the legend
#' only. 
#' @param legend.breaks numeric ordered vector of variable values to display
#' in the legend.
#' @param display logical indicating whether to draw the scale for output
#' in the HTML.  If \code{TRUE}, a bitmap will be created and sourced in 
#' the HTML in one or more \code{img} tags.
#'
#' @return list of class \code{Scale.Size.Discrete}.
#'
#' @seealso
#' \code{\link{ol_map}},
#' \code{\link{ol_geom_point}},
#'
#' @export
#'
#' @examples
#' point.matrix <- matrix(
#'     c(
#'         -80.885+runif(10),
#'         25.223+runif(10)
#'     ),
#'     ncol=2
#' )
#' point.df <- data.frame(
#'     pt.type=sample(c("A","B"),10,replace=TRUE),
#'     pt.numeric=runif(10)*10
#' )
#' miami.points.map <- ol_map(
#'     center=c(-80.385790,25.782618),
#'     zoom=10
#' ) + 
#'     public_OSM_basemap()+
#'     ol_geom_point(
#'         point.matrix,
#'         df=point.df,
#'         mapping=ol_aes(
#'             size=pt.type # continuous mapping
#'         ), 
#'         name="Point",
#'         marker="pin",
#'         fill='green',
#'         toggle.control=TRUE,
#'         tooltip=point.df$pt.type
#' ) + 
#'     ol_scale_size_discrete(
#'     c(A=1,B=0.5),
#'     name="Point Type",
#'     draw.fill='black',
#'     display=TRUE
#' )
#' ## Not run: save to file and open on browser
#' # ol_map2HTML(miami.points.map,'pointsizes.html')
#' # browseURL('pointsizes.html')
ol_scale_size_discrete <- function(
        size.vector=NULL,
        name=NULL,
        na.size.val=0.33,
        draw.fill=NULL,
        legend.breaks = NULL,
        display=FALSE
    ) {
    if(is.null(size.vector)){
        f <- NULL
    } else {
        f <- get_num_scale_discrete(
            num.vector=size.vector,
            na.num.val=na.size.val
        )
    }   
    o <- list(
        "attribute"="size",
        "function"=f,
        vector = size.vector,
        display=display,
        type="discrete",
        name=name,
        draw.fill=draw.fill,
        legend.breaks = legend.breaks
    )
    class(o) <-'Scale.Size.Discrete'
    return(o)
}


#' Icon Size Scale (Continuous)
#'
#' Specify a continuous size mapping for an icon layer.
#'
#' This method maps OpenLayers point (icon) feature sizes to continuous 
#' variable values.  This scale can be added to an Ol.Map S3 object
#' only if the Ol.Map object has a layer with an "iconsize" mapping to a
#' numeric variable.  If no such layer exists, attempts to add this type
#' of scale will result in a warning.  Attempt to apply this scale
#' to a non-numeric variable will throw an error.  
#' 
#' Size inputs to this method are applied as scalars to the icon widths.
#' A size value of 1 translates to the \code{target.icon.width} assigned
#' to the layer. 
#' 
#' @param low.val numeric the minimum variable value to be mapped 
#' to the smallest size.
#' @param high.val numeric the maximum variable value to be mapped
#' to the largest size.
#' @param low.size numeric smallest size scalar.
#' @param high.size numeric largest size scalar.
#' @param name character the scale name.
#' @param na.size.val numeric the size scalar assigned to non-numeric or
#' NA values.
#' @param legend.breaks numeric ordered vector of variable values to display
#' in the legend.
#' @param display logical indicating whether to draw the scale for output
#' in the HTML.  If \code{TRUE}, a bitmap will be created and sourced in 
#' the HTML in one or more \code{img} tags.
#' @param display.icon.img.src character path to image file to use in size 
#' legend.  If NULL, the first image supplied to the icon layer will be
#' used.
#'
#' @return list of class \code{Scale.IconSize.Continuous}.
#'
#' @seealso
#' \code{\link{ol_map}},
#' \code{\link{ol_geom_icon}},
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
#' r.server.df <- data.frame(
#'     server.name=c(
#'         'School of Mathematics and Statistics, University of Melbourne',
#'         'Simon Fraser University, Burnaby',
#'         'Shanghai University',
#'         'University of Crete',
#'         'Marine Research Institute',
#'         'University of Padua',
#'         'Instituto Tecnologico Autonomo de Mexico',
#'         'University of Bergen',
#'         'RadicalDevelop, Lda',
#'         'University of Porto',
#'         'Four Dots',
#'         'Academic Computer Club, Umeå University',
#'         'ETH Zurich',
#'         'Middle East Technical University Northern Cyprus Campus, Mersin',
#'         'Duke University, Durham, NC',
#'         'Oregon State University',
#'         'Revolution Analytics, Dallas, TX'
#'     ),
#'     server.value = runif(17)*10,
#'     stringsAsFactors=FALSE
#' )
#' r.icon <- "https://www.r-project.org/Rlogo.png"
#' ## If width is not provided image must be local
#' ## and png package must be installed.
#' r.icon.width <- 200 
#' r.map <- ol_map(
#'     center=c(-100,30),
#'     zoom=3
#' ) + 
#'     public_OSM_basemap()+
#'     ol_geom_icon(
#'         some.r.servers,
#'         r.icon,
#'         mapping=ol_aes(iconsize=server.value),
#'         df = r.server.df,
#'         name="R Servers",
#'         icon.size.scalar='autoscale',
#'         src.img.width=r.icon.width,
#'         toggle.control=TRUE,
#'         tooltip=r.server.df$server.name
#' ) +
#'     ol_scale_iconsize_continuous(
#'     low.val=0,
#'     high.val=10,
#'     legend.breaks=c(0,2.5,5,7.5,10),
#'     display=TRUE
#' )
#' ## Not run: save as HTML and open in browser
#' # ol_map2HTML(r.map,'R-servers.html')
#' # browseURL("R-servers.html")
ol_scale_iconsize_continuous <- function(
        low.val,
        high.val,
        low.size=0.33,
        high.size=0.75,
        name=NULL,
        na.size.val = 0.33,
        legend.breaks=NULL,
        display=FALSE,
        display.icon.img.src=NULL
    ) {
    if(missing(low.val) || missing(high.val)){
        f <- NULL
        low.val <- NULL
        high.val <- NULL
    } else {
        f <- get_num_scale_continuous(
            low.val,
            high.val,
            low.size,
            high.size,
            na.size.val
        )
        if(is.null(legend.breaks)){
            legend.breaks <- (0:4)*(high.val-low.val)/4+low.val
        }
    }
    o <- list(
        low.val=low.val,
        high.val=high.val,
        "attribute"="iconsize",
        "function"=f,
        display=display,
        type="continuous",
        name=name,
        legend.breaks = legend.breaks,
        display.icon.img.src=display.icon.img.src
    )
    class(o) <-'Scale.IconSize.Continuous'
    return(o)
}

#' Icon Size Scale (Discrete)
#'
#' Specify a discrete size mapping for an icon layer.
#'
#' This method maps OpenLayers point (icon) feature sizes to discrete 
#' variable values.  This scale can be added to an Ol.Map S3 object
#' only if the Ol.Map object has a layer with a "iconsize" mapping to a
#' numeric, character, or factor variable.  
#' If no such layer exists, attempts to add this type
#' of scale will result in a warning.  
#' 
#' Size inputs to this method are applied as scalars to the icon widths.
#' A size value of 1 translates to the \code{target.icon.width} assigned
#' to the layer. 
#' 
#' @param size.vector numeric named vector of the form 
#' \code{c(value=width)}.  If \code{NULL}, a default size mapping
#' is assigned.
#' @param name character the scale name.
#' @param na.size.val numeric the size scalar assigned to non-numeric or
#' NA values.
#' @param legend.breaks numeric ordered vector of variable values to display
#' in the legend.
#' @param display logical indicating whether to draw the scale for output
#' in the HTML.  If \code{TRUE}, a bitmap will be created and sourced in 
#' the HTML in one or more \code{img} tags.
#' @param display.icon.img.src character path to image file to use in size 
#' legend.  If NULL, the first image supplied to the icon layer will be
#' used.
#'
#' @return list of class \code{Scale.IconSize.Discrete}.
#'
#' @seealso
#' \code{\link{ol_map}},
#' \code{\link{ol_geom_icon}},
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
#' r.server.df <- data.frame(
#'     server.name=c(
#'         'School of Mathematics and Statistics, University of Melbourne',
#'         'Simon Fraser University, Burnaby',
#'         'Shanghai University',
#'         'University of Crete',
#'         'Marine Research Institute',
#'         'University of Padua',
#'         'Instituto Tecnologico Autonomo de Mexico',
#'         'University of Bergen',
#'         'RadicalDevelop, Lda',
#'         'University of Porto',
#'         'Four Dots',
#'         'Academic Computer Club, Umeå University',
#'         'ETH Zurich',
#'         'Middle East Technical University Northern Cyprus Campus, Mersin',
#'         'Duke University, Durham, NC',
#'         'Oregon State University',
#'         'Revolution Analytics, Dallas, TX'
#'     ),
#'     server.type = sample(c("A","B","C"),17,replace=TRUE),
#'     stringsAsFactors=FALSE
#' )
#' r.icon <- "https://www.r-project.org/Rlogo.png"
#' ## If width is not provided image must be local
#' ## and png package must be installed.
#' r.icon.width <- 200 
#' r.map <- ol_map(
#'     center=c(-100,30),
#'     zoom=3
#' ) + 
#'     public_OSM_basemap()+
#'     ol_geom_icon(
#'         some.r.servers,
#'         r.icon,
#'         mapping=ol_aes(iconsize=server.type),
#'         df = r.server.df,
#'         name="R Servers",
#'         icon.size.scalar='autoscale',
#'         src.img.width=r.icon.width,
#'         toggle.control=TRUE,
#'         tooltip=r.server.df$server.type
#' ) +
#'     ol_scale_iconsize_discrete(
#'     display=TRUE
#' )
#' ## Not run: save as HTML and open in browser
#' # ol_map2HTML(r.map,'R-servers.html')
#' # browseURL("R-servers.html")
ol_scale_iconsize_discrete <- function(
        size.vector=NULL,
        name=NULL,
        na.size.val=0.33,
        legend.breaks = NULL,
        display=FALSE,
        display.icon.img.src = NULL
    ) {
    if(is.null(size.vector)){
        f <- NULL
    } else {
        f <- get_num_scale_discrete(
            num.vector=size.vector,
            na.num.val=na.size.val
        )
    }   
    o <- list(
        "attribute"="iconsize",
        "function"=f,
        vector = size.vector,
        display=display,
        type="discrete",
        name=name,
        legend.breaks = legend.breaks,
        display.icon.img.src=display.icon.img.src
    )
    class(o) <-'Scale.IconSize.Discrete'
    return(o)
}

#' Icon Image Scale
#'
#' Map icon images to discrete variable values.
#'
#' This method maps OpenLayers point (icon) feature images to discrete 
#' variable values.  This scale can be added to an Ol.Map S3 object
#' only if the Ol.Map object has a layer with an "iconimage" mapping to a
#' numeric, character, or factor variable.  
#' If no such layer exists, attempts to add this type
#' of scale will result in a warning.  
#' 
#' 
#' @param icon.img.vector character named vector of the form 
#' \code{c(value=image.path)}.  If \code{NULL}, a default mapping
#' is assigned to the images available in the layer.
#' @param name character the scale name.
#' @param na.img.src character image path assigned to unrecognized or
#' NA values.
#' @param ordered.values character, numeric, or factor vector giving the
#' ordering for the legend display.  If \code{NULL}, the ordering from
#' \code{icon.img.vector} is used.
#' @param display logical indicating whether to draw the scale for output
#' in the HTML.  If \code{TRUE}, a bitmap will be created and sourced in 
#' the HTML in one or more \code{img} tags.
#' @param icon.width numeric width(s) of icons for the legend display only.
#' Icon widths for the map overlay are defined in \code{\link{ol_geom_icon}}.
#'
#' @return list of class \code{Scale.IconImage.Discrete}.
#'
#' @seealso
#' \code{\link{ol_map}},
#' \code{\link{ol_geom_icon}},
#'
#' @export
#'
#' @examples
#' freebsd.icon <- "https://www.freebsd.org/gifs/daemon-phk.png"
#' freebsd.icon.width <- 191
#' r.icon <- "https://www.r-project.org/Rlogo.png"
#' r.icon.width <- 200
#' loc.df <- data.frame(
#'     lon=c(
#'         -73.953,
#'         -78.938,
#'         -74.007
#'     ),
#'     lat=c(
#'         40.768,
#'         36.001,
#'         40.708
#'     ),
#'     type=c(
#'         "R",
#'         "R",
#'         "BSD"
#'     )
#' 
#' )
#' icon.map <- ol_map(
#'     center=c(-75,38),
#'     zoom=5
#' ) + 
#'     public_OSM_basemap()+
#'     ol_geom_icon(
#'         loc.df[,1:2],
#'         c(r.icon,freebsd.icon),
#'         mapping=ol_aes(iconimage=type),
#'         df = loc.df,
#'         name="Some Open Source Locations",
#'         icon.size.scalar='autoscale',
#'         src.img.width=c(r.icon.width,freebsd.icon.width),
#'         toggle.control=TRUE
#' ) +
#'     ol_scale_iconimage_discrete(
#'     c(R=r.icon,BSD=freebsd.icon),
#'     display=TRUE
#' )
#' ## Not run: save as HTML and open in browser
#' # ol_map2HTML(icon.map,'servers.html')
#' # browseURL("servers.html")
ol_scale_iconimage_discrete <- function(
        icon.img.vector = NULL,
        name=NULL,
        na.img.src=NULL,
        ordered.values = NULL,
        display=FALSE,
        icon.width = NULL
    ) {
    if(is.null(icon.img.vector)){
        f <- NULL
    } else {
        f <- function(val,na.img=na.img.src){
            v <- as.character(val)
            if(is.null(na.img) || is.na(na.img)){
                na.img <- icon.img.vector[1]
            }
            if(v %in% names(icon.img.vector)){
                return(icon.img.vector[v])
            } else {
                return(na.img)
            }
        }
        if(is.null(ordered.values)){
            ordered.values <- names(icon.img.vector)
        }
    }   
    o <- list(
        "attribute"="iconimage",
        "function"=f,
        vector = icon.img.vector[ordered.values],
        display=display,
        type="discrete",
        name=name,
        ordered.values=ordered.values,
        icon.width=icon.width
    )
    class(o) <-'Scale.IconImage.Discrete'
    return(o)
}



#' Line Type Scale (Experimental)
#'
#' Specify a discrete line type mapping scale.
#'
#' This method maps OpenLayers feature line dash types to discrete 
#' variable values.  This scale can be added to an Ol.Map S3 object
#' only if the Ol.Map object has a layer with a "lty" mapping to a
#' numeric, character, or factor type variable.  If no such layer 
#' exists, attempts to add this type of scale will result in a warning.
#' The \code{lty.list} input enables the user to specify the exact 
#' mapping, assigning line dash types to specific variable values.  The 
#' \code{lty} aesthetic does not have a continuous scale.
#' 
#' Note: this method does not result in consistant rendering for 
#' different browsers or map zoom levels.
#' 
#' @param lty.list numeric named list of the form 
#' \code{list(value=numeric.vector)}.  If \code{NULL}, a default line type mapping
#' is assigned.  The input vectors will be converted to javascript
#' numeric arrays.  See the \code{lineDash} propery in the OpenLayers
#' \href{https://openlayers.org/en/latest/apidoc/module-ol_style_Stroke-Stroke.html}{Stroke Documentation}
#' for interpretation of these inputs.
#' @param name character the scale name.
#' @param na.lty.val numeric the \code{lineDash} vector assigned to
#' unrecognized or NA values.
#' @param ordered.values character, numeric, or factor vector containing the 
#' ordered unique discrete variable values.  This input is used to determine
#' the order of the values appearing in the legend.  If not supplied, the 
#' order is taken from \code{names(lwd.vector)}.
#' @param opacity numeric in [0,1]. The line opacity used in the legend only.
#' @param draw.color character color used in the legend lines only.
#' @param draw.lwd numeric indicating the line width for the legend
#' only.  This will be passed to an R plot command.  See \code{\link{par}}.
#' @param display logical indicating whether to draw the scale for output
#' in the HTML.  If \code{TRUE}, a bitmap will be created and sourced in 
#' the HTML in an \code{img} tag.  If \code{FALSE}, the \code{draw.*} 
#' inputs are ignored.
#'
#' @return list of class \code{Scale.Lwd.Discrete}.
#'
#' @seealso 
#' \code{\link{ol_map}},
#' \code{\link{ol_geom_polygon}},
#' \code{\link{ol_geom_line}} 
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
#'     no=runif(2)
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
#'         lty=direction # discrete mapping
#'     ),
#'     df=line.df,
#'     name="Miami Lines",
#'     toggle.control=TRUE
#' )
#' line.type.scale <- ol_scale_lty_discrete(
#'     lty.list=list(
#'         vertical = 1,
#'         horizontal = c(5,5)
#'     ),
#'     name="Direction",
#'     display=TRUE
#' )
#' line.map.miami <- miami.gray.basemap + 
#'     line.layer + 
#'     line.type.scale
#' ## Not Run: output to file and view
#' # ol_map2HTML(line.map.miami,'miami_lines.html')
#' # browseURL("miami_lines.html")
ol_scale_lty_discrete <- function(
        lty.list=NULL,
        name=NULL,
        na.lty.val = NA,
        ordered.values = NULL,
        opacity=1,
        draw.color='black',
        draw.lwd = 1,
        display=FALSE
    ) {
    if(is.null(lty.list)){
        f <- NULL
    } else {
        for(i in 1:length(lty.list)){
            if(length(lty.list[[i]])<2){
                lty.list[[i]] <- NA
            }
        }
        f <- get_lty_scale_discrete(
            lty.list,
            na.lty.val
        )
        if(is.null(ordered.values)){
            ordered.values <- names(lty.list)
        }
    }   
    o <- list(
        "attribute"="lty",
        "function"=f,
        vector = lty.list[ordered.values],
        display=display,
        type="discrete",
        name=name,
        ordered.values=ordered.values,
        opacity=opacity,
        draw.color=draw.color,
        draw.lwd=draw.lwd
    )
    class(o) <-'Scale.Lty.Discrete'
    return(o)
}

