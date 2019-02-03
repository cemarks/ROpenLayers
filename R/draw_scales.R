# Draw a Legend

# Generic method for drawing legends.

# This generic method produces legends for aesthetic mappings 
# (i.e., scales) in \code{Ol.Map} layers.  For \code{Scale.IconSize.Continuous},
# \code{Scale.IconSize.Discrete}, and \code{Scale.IconImage.Discrete} scales,
# this method formats the legend as an HTML table.  For all other scale-type
# objects, this method saves a bitmap (PNG) image and formats the legend
#  as an HTML\code{img} tag sourcing the saved image.  

# If a file name is not specified, the image is rendered in the default device.

# @param scale.obj \code{Scale.*.*} object to be rendered as a legend in HTML.
# @param file.name character file name to write the legend image bitmap (PNG).
# This parameter is ignored for Icon-type scales.

# @return HTML string to render the legend.

# @seealso 
# \code{\link{ol_scale_fill_continuous}}, 
# \code{\link{ol_scale_color_continuous}}, 
# \code{\link{ol_scale_size_continuous}}, 
# \code{\link{ol_scale_fill_discrete}}, 
# \code{\link{ol_scale_color_discrete}}, 
# \code{\link{ol_scale_size_discrete}}, 
# \code{\link{ol_scale_lwd_discrete}}, 
# \code{\link{ol_scale_lty_continuous}}, 
# \code{\link{ol_scale_iconsize_continuous}}, 
# \code{\link{ol_scale_iconsize_discrete}}, 
# \code{\link{ol_scale_icon_discrete}}, 
# \code{\link{ol_map2HTML}}, 
# \code{\link{ol_map2HTMLstrings}}, 

### @export ## Not ready for export

# @examples
draw_scale <- function(scale.obj,scale.count=sample(1:100000,1),nice.format=TRUE,initial.indent=6) UseMethod("draw_scale")
draw_scale.default <- function(scale.obj,scale.count=sample(1:100000,1),nice.format=TRUE,initial.indent=6) return(NULL)

plot_color_scale_continuous <- function(
    scale.obj,
    file.name = NULL
){
    if(!is.null(file.name)){
        grDevices::png(file.name,width=200,height=250)
    }
    graphics::par(mar=c(2,0.5,3,8))
    if(is.null(scale.obj[['preserve.opacity']])){
        scale.obj[['preserve.opacity']] <- FALSE
    }
    if(scale.obj[['preserve.opacity']]){
        graphics::image(c(0,0.2),(0:100)/100,matrix((0:99),nrow=1),col=sapply(scale.obj[['low.val']]+(0:99)/99*(scale.obj[['high.val']]-scale.obj[['low.val']]),scale.obj[['function']]),axes=FALSE,ann=FALSE)
    } else {
        graphics::image(c(0,0.2),(0:100)/100,matrix((0:99),nrow=1),col=sapply(scale.obj[['low.val']]+(0:99)/99*(scale.obj[['high.val']]-scale.obj[['low.val']]),function(x) return(substr(scale.obj[['function']](x),1,7))),axes=FALSE,ann=FALSE)
    }
    graphics::axis(side=4,at=(0:5)/5,labels=sprintf("%1.2f",scale.obj[['low.val']]+(0:5)/5*(scale.obj[['high.val']]-scale.obj[['low.val']])),las=2,cex.axis=1.2)
    graphics::mtext(scale.obj[['name']],side=3,at=0,adj=0,line=1,cex=1.25,font=2)
    if(!is.null(file.name)){
        grDevices::dev.off()
    }
}


draw_scale.Scale.Fill.Continuous <- function(scale.obj,scale.count=sample(1:100000,1),nice.format=TRUE,initial.indent=6){
    inid <- initial.indent
    if(nice.format){
        write_function <- function(s,inid.t){
            return(paste(strrep(" ",inid.t),s,"\n",sep=""))
        }
    } else {
        write_function <- function(s,inid.t){
            return(s)
        }
    }
    file.name <- tempfile()
    file.name <- paste(file.name,"png",sep=".")
    plot_color_scale_continuous(
            scale.obj,
            file.name
    )
    image.str <- paste("data:image/png;base64,",base64enc::base64encode(file.name))
    zzz <- file.remove(file.name)
    o <- paste(
        write_function("<div class=\"single-scale\">",inid),
        write_function(sprintf("<img class=\"scale\" src=\"%s\" alt=\"scale_png\" width=\"200px\"/>",image.str),inid+2),
        write_function("</div>",inid),
        sep=""
    )
    return(o)
}
draw_scale.Scale.Color.Continuous <- function(scale.obj,scale.count=sample(1:100000,1),nice.format=TRUE,initial.indent=6){
    inid <- initial.indent
    if(nice.format){
        write_function <- function(s,inid.t){
            return(paste(strrep(" ",inid.t),s,"\n",sep=""))
        }
    } else {
        write_function <- function(s,inid.t){
            return(s)
        }
    }
    file.name <- tempfile()
    file.name <- paste(file.name,"png",sep=".")
    plot_color_scale_continuous(
        scale.obj,
        file.name
    )
    image.str <- paste("data:image/png;base64,",base64enc::base64encode(file.name))
    zzz <- file.remove(file.name)
    o <- paste(
        write_function("<div class=\"single-scale\">",inid),
        write_function(sprintf("<img class=\"scale\" src=\"%s\" alt=\"scale_png\" width=\"200px\"/>",image.str),inid+2),
        write_function("</div>",inid),
        sep=""
    )
    return(o)
}

plot_fill_scale_discrete <- function(
    scale.obj,
    file.name=NULL
    ){
    if(is.null(scale.obj[['vector']])){
        scale.obj[['vector']] <- get_default_color_vec_discrete(
            scale.obj[['variable.values']],
            opacity=0.5
        )
    }
    if(is.null(scale.obj[['draw.lines']])) scale.obj[['draw.lines']] <- TRUE
    if(is.null(scale.obj[['ordered.values']]) || !identical(sort(as.character(scale.obj[["ordered.values"]])),sort(names(scale.obj[["vector"]])))) scale.obj[['ordered.values']] <- names(scale.obj[['vector']])
    n <- length(scale.obj[["ordered.values"]])
    if(scale.obj[["ordinal.scale"]]){
        y <- seq(from=0,to=(n)*0.15,by=0.15)
    } else {
        y <- seq(from=0,to=(n)*0.2,by=0.2)
    }
    if(is.null(scale.obj[['marker']]) || !(scale.obj[['marker']] %in% c("pin","point","dot","circle"))) {
        draw_shape <- function(i){
            if(scale.obj[["preserve.opacity"]]){
                color <- color_check(scale.obj[["vector"]][scale.obj[["ordered.values"]][i]])
            } else {
                color <- substr(color_check(scale.obj[["vector"]][scale.obj[["ordered.values"]][i]]),1,7)
            }
            if(scale.obj[["draw.lines"]]){
                graphics::rect(0,y[i],0.2,y[i]+0.15,col=color,lty=scale.obj[["draw.lty"]],lwd=scale.obj[["draw.lwd"]],border=scale.obj[["draw.color"]])
            }else{
                graphics::rect(0,y[i],0.2,y[i]+0.15,col=color,border=NA)
            }
            graphics::text(0.3,y[i]+0.075,scale.obj[["ordered.values"]][i],adj=0,cex=1.2)
        }
    } else if(scale.obj[['marker']]=="point" || scale.obj[['marker']]=="dot"){
        draw_shape <- function(i){
            if(scale.obj[["preserve.opacity"]]){
                color <- color_check(scale.obj[["vector"]][scale.obj[["ordered.values"]][i]])
            } else {
                color <- substr(color_check(scale.obj[["vector"]][scale.obj[["ordered.values"]][i]]),1,7)
            }
            pts <- 100
            a <- (0:(pts-1))/(pts-1)*2*pi
            r <- 0.025
            xx <- r*cos(a)+max(r,0.1)
            yy <- r*sin(a)+y[i]+0.075
            if(scale.obj[["draw.lines"]]){
                graphics::polygon(xx,yy,col=color,border=scale.obj[["draw.color"]])
            } else {
                graphics::polygon(xx,yy,col=color,border=color)
            }
            graphics::text(0.3,y[i]+0.075,scale.obj[["ordered.values"]][i],adj=0,cex=1.2)
        }
    } else if(scale.obj[['marker']]=="circle"){
        draw_shape <- function(i){
            if(scale.obj[["preserve.opacity"]]){
                color <- color_check(scale.obj[["vector"]][scale.obj[["ordered.values"]][i]])
            } else {
                color <- substr(color_check(scale.obj[["vector"]][scale.obj[["ordered.values"]][i]]),1,7)
            }
            pts <- 100
            a <- (0:(pts-1))/(pts-1)*2*pi
            r <- 0.075
            xx <- r*cos(a)+max(r,0.1)
            yy <- r*sin(a)+y[i]+0.075
            if(scale.obj[["draw.lines"]]){
                graphics::polygon(xx,yy,col=color,border=scale.obj[["draw.color"]])
            } else {
                graphics::polygon(xx,yy,col=color,border=color)
            }
            graphics::text(0.3,y[i]+0.075,scale.obj[["ordered.values"]][i],adj=0,cex=1.2)
        }
    } else if(scale.obj[['marker']]=="pin") {
        draw_shape <- function(i){
            theta <- 7*pi/24
            if(scale.obj[["preserve.opacity"]]){
                color <- color_check(scale.obj[["vector"]][scale.obj[["ordered.values"]][i]])
            } else {
                color <- substr(color_check(scale.obj[["vector"]][scale.obj[["ordered.values"]][i]]),1,7)
            }
            pts1 <- 20
            pts2 <- 80
            x0 <- (1/2/tan(theta))^2
            x1 <- (0:pts1)*x0/pts1
            r <- sqrt(4*x0^3 + x0^2)
            a <- (0:pts2)*(2*(pi-theta))/pts2-(pi/2-theta)
            height <- 2*sqrt(x0)*x0+sqrt(x0) + r
            xmin <- -r
            xadj <- max(-xmin,0.1)
            ymin <- y[i]
            scale.factor <- 0.15/height
            d <- data.frame(
                x=c(
                    x1,
                    r*cos(a),
                    rev(-x1)
                ) * scale.factor + xadj,
                y=c(
                    sqrt(x1),
                    (r*sin(a) + sqrt(x0)*(2*x0+1)),
                    rev(sqrt(x1))
                )*scale.factor+ymin
            )
            if(scale.obj[["draw.lines"]]){
                graphics::polygon(d[,1],d[,2],col=color,border=scale.obj[["draw.color"]])
            } else {
                graphics::polygon(d[,1],d[,2],col=color,border=color)
            }
            graphics::text(0.3,y[i]+0.075,scale.obj[["ordered.values"]][i],adj=0,cex=1.2)
        }
    }
    if(!is.null(file.name)){
        grDevices::png(file.name,width=200,height=min(250,0+n*50))
    }
    graphics::par(mar=c(0,0,1,0))
    graphics::plot(0,type='n',axes=FALSE,ann=FALSE,xlim=c(0,1),ylim=c(y[1],y[n+1]))
    for(i in 1:(n)){
        draw_shape(i)
    }
    graphics::mtext(scale.obj[['name']],side=3,at=0,adj=0,line=-0.5,cex=1.25,font=2)
    if(!is.null(file.name)){
        grDevices::dev.off()
    }
}

draw_scale.Scale.Fill.Discrete <- function(scale.obj,scale.count=sample(1:100000,1),nice.format=TRUE,initial.indent=6){
    inid <- initial.indent
    if(nice.format){
        write_function <- function(s,inid.t){
            return(paste(strrep(" ",inid.t),s,"\n",sep=""))
        }
    } else {
        write_function <- function(s,inid.t){
            return(s)
        }
    }
    file.name <- tempfile()
    file.name <- paste(file.name,"png",sep=".")
    plot_fill_scale_discrete(
        scale.obj,
        file.name
    )
    image.str <- paste("data:image/png;base64,",base64enc::base64encode(file.name))
    zzz <- file.remove(file.name)
    o <- paste(
        write_function("<div class=\"single-scale\">",inid),
        write_function(sprintf("<img class=\"scale\" src=\"%s\" alt=\"scale_png\" width=\"200px\"/>",image.str),inid+2),
        write_function("</div>",inid),
        sep=""
    )
    return(o)
}


plot_line_scale_discrete <- function(
    color.vector,
    width.vector,
    lty.list,
    ordered.values,
    main.title=NULL,
    preserve.opacity=FALSE,
    col = '#000000FF',
    lty = 'solid',
    lwd = 5,
    file.name=NULL
    ){
    if(missing(color.vector) && missing(width.vector)){
        color.vector <- rep('#000000FF',length(lty.list))
        names(color.vector) <- names(lty.list)
    }
    if(missing(color.vector)){
        color.vector <- rep(col,length(width.vector))
        names(color.vector) <- names(width.vector)
    }
    if(missing(width.vector)){
        width.vector <- rep(lwd,length(color.vector))
        names(width.vector) <- names(color.vector)
    } else {
        if(missing(ordered.values)) ordered.values <- names(sort(width.vector))
    }
    if(is.null(preserve.opacity)) preserve.opacity <- FALSE
    if(missing(ordered.values)) ordered.values <- names(color.vector)
    if(!identical(sort(as.character(ordered.values)),sort(names(color.vector)))) ordered.values <- names(color.vector)
    if(missing(lty.list)){
        lty.list <- NULL
    }
    ordered.values <- stats::na.omit(ordered.values)
    n <- length(ordered.values)
    y <- seq(from=0,to=(n)*0.15,by=0.15)
    draw_line <- function(i){
        if(preserve.opacity){
            color <- color_check(color.vector[ordered.values[i]])
        } else {
            color <- substr(color_check(color.vector[ordered.values[i]]),1,7)
        }
        w <- width.vector[ordered.values[i]]
        if(!is.null(lty.list) && length(lty.list[[ordered.values[i]]])>1){
            lty.vec <- lty.list[[ordered.values[i]]]
            s <- sum(lty.vec) + length(lty.vec)
            unit.length <- 0.2/2/s
            start.point <- 0
            for(k in 1:2){
                for(j in 1:length(lty.vec)){
                    graphics::lines(c(start.point,start.point+lty.vec[j]*unit.length),c(y[i]+0.075,y[i]+0.075),col=color,lwd=w,lty=lty)
                    start.point <- start.point + lty.vec[j]*unit.length+unit.length
                    # cat(start.point, "")
                }
            }
        } else {
            graphics::lines(c(0,0.2),c(y[i]+0.075,y[i]+0.075),col=color,lwd=w,lty=lty)
        }
        graphics::text(0.3,y[i]+0.075,ordered.values[i],adj=0,cex=1.2)
    }
    if(!is.null(file.name)){
        grDevices::png(file.name,width=200,height=min(250,20+n*45))
    }
    graphics::par(mar=c(0,0,1,0))
    graphics::plot(0,type='n',axes=FALSE,ann=FALSE,xlim=c(0,1),ylim=c(y[1],y[n+1]))
    for(i in 1:(n)){
        draw_line(i)
    }
    graphics::mtext(main.title,side=3,at=0,adj=0,line=-0.5,cex=1.25,font=2)
    if(!is.null(file.name)){
        grDevices::dev.off()
    }
}


draw_scale.Scale.Color.Discrete <- function(scale.obj,scale.count=sample(1:100000,1),nice.format=TRUE,initial.indent=6){
    inid <- initial.indent
    if(nice.format){
        write_function <- function(s,inid.t){
            return(paste(strrep(" ",inid.t),s,"\n",sep=""))
        }
    } else {
        write_function <- function(s,inid.t){
            return(s)
        }
    }
    if(is.null(scale.obj[['vector']])){
        scale.obj[['vector']] <- get_default_color_vec_discrete(
            scale.obj[['variable.values']],
            opacity=1
        )
    }
    file.name <- tempfile()
    file.name <- paste(file.name,"png",sep=".")
    plot_line_scale_discrete(    
        color.vector=scale.obj[['vector']],
        ordered.values=scale.obj[['ordered.values']],
        main.title=scale.obj[['name']],
        preserve.opacity=scale.obj[['preserve.opacity']],
        col=scale.obj[['draw.color']],
        lty=scale.obj[['draw.lty']],
        lwd=scale.obj[['draw.lwd']],
        file.name=file.name
    )
    image.str <- paste("data:image/png;base64,",base64enc::base64encode(file.name))
    zzz <- file.remove(file.name)
    o <- paste(
        write_function("<div class=\"single-scale\">",inid),
        write_function(sprintf("<img class=\"scale\" src=\"%s\" alt=\"scale_png\" width=\"200px\"/>",image.str),inid+2),
        write_function("</div>",inid),
        sep=""
    )
    return(o)
}


draw_scale.Scale.Lwd.Discrete <- function(scale.obj,scale.count=sample(1:100000,1),nice.format=TRUE,initial.indent=6){
    inid <- initial.indent
    if(nice.format){
        write_function <- function(s,inid.t){
            return(paste(strrep(" ",inid.t),s,"\n",sep=""))
        }
    } else {
        write_function <- function(s,inid.t){
            return(s)
        }
    }
    if(is.null(scale.obj[['vector']])){
        scale.obj[['vector']] <- get_default_num_vec_discrete(
            scale.obj[['variable.values']],
            c(1,5)
        )
    }
    file.name <- tempfile()
    file.name <- paste(file.name,"png",sep=".")
    plot_line_scale_discrete(    
        width.vector=scale.obj[['vector']],
        ordered.values=scale.obj[['ordered.values']],
        main.title=scale.obj[['name']],
        preserve.opacity=scale.obj[['preserve.opacity']],
        col=scale.obj[['draw.color']],
        lty=scale.obj[['draw.lty']],
        lwd=scale.obj[['draw.lwd']],
        file.name=file.name
    )
    image.str <- paste("data:image/png;base64,",base64enc::base64encode(file.name))
    zzz <- file.remove(file.name)
    o <- paste(
        write_function("<div class=\"single-scale\">",inid),
        write_function(sprintf("<img class=\"scale\" src=\"%s\" alt=\"scale_png\" width=\"200px\"/>",image.str),inid+2),
        write_function("</div>",inid),
        sep=""
    )
    return(o)
}

draw_scale.Scale.Lty.Discrete <- function(scale.obj,scale.count=sample(1:100000,1),nice.format=TRUE,initial.indent=6){
    inid <- initial.indent
    if(nice.format){
        write_function <- function(s,inid.t){
            return(paste(strrep(" ",inid.t),s,"\n",sep=""))
        }
    } else {
        write_function <- function(s,inid.t){
            return(s)
        }
    }
    file.name <- tempfile()
    file.name <- paste(file.name,"png",sep=".")
    plot_line_scale_discrete(    
        lty.list=scale.obj[['vector']],
        ordered.values=scale.obj[['ordered.values']],
        main.title=scale.obj[['name']],
        preserve.opacity=scale.obj[['preserve.opacity']],
        col=scale.obj[['draw.color']],
        lty=scale.obj[['draw.lty']],
        lwd=scale.obj[['draw.lwd']],
        file.name=file.name
    )
    image.str <- paste("data:image/png;base64,",base64enc::base64encode(file.name))
    zzz <- file.remove(file.name)
    o <- paste(
        write_function("<div class=\"single-scale\">",inid),
        write_function(sprintf("<img class=\"scale\" src=\"%s\" alt=\"scale_png\" width=\"200px\"/>",image.str),inid+2),
        write_function("</div>",inid),
        sep=""
    )
    return(o)
}


html_table_size_scale <- function(scale.obj,file.name,nice.format=TRUE,initial.indent=6){
    inid <- initial.indent
    if(nice.format){
        write_function <- function(s,inid.t){
            return(paste(strrep(" ",inid.t),s,"\n",sep=""))
        }
    } else {
        write_function <- function(s,inid.t){
            return(s)
        }
    }
    if(scale.obj[['marker']]=='pin'){
        base.img.width<-40
    } else {
        base.img.width <- 20
    }
    if(is.null(scale.obj[["legend.breaks"]])){
        if(is.null(scale.obj[['vector']])){
            high.val <- max(scale.obj[['variable.values']],na.rm=TRUE)
            low.val <- max(scale.obj[['variable.values']],na.rm=TRUE)
            legend.breaks <- (0:4)*(high.val-low.val)/4+low.val
        } else {
            legend.breaks <- names(scale.obj[['vector']])
        }
    } else {
        legend.breaks <- scale.obj[['legend.breaks']]
    }
    draw_marker(
        marker=scale.obj[['marker']],
        file.name=file.name,
        color=scale.obj[['draw.fill']],
        opacity=1,
        w=base.img.width,
        theta=7*pi/24
    )
    get_img_width <- function(val){
        scale.size <- scale.obj[['function']](val)
        return(base.img.width*scale.size)
    }
    table.rows <- NULL
    image.str <- paste("data:image/png;base64,",base64enc::base64encode(file.name))
    if(is.numeric(legend.breaks)){
        for(i in 1:length(legend.breaks)){
            table.rows <- c(
                table.rows,
                paste(
                    write_function("<tr>",inid+4),
                    write_function(sprintf("<td> <img src=\"%s\" alt=\"scale image\" width=\"%1.0f\"/> </td>",image.str,get_img_width(legend.breaks[i])),inid+6),
                    write_function(sprintf("<td> %1.2f </td>",legend.breaks[i]),inid+6),
                    write_function("</tr>",inid+4),
                    sep=""
                )
            )
        }
    } else {
        for(i in 1:length(legend.breaks)){
            table.rows <- c(
                table.rows,
                paste(
                    write_function("<tr>",inid+4),
                    write_function(sprintf("<td> <img src=\"%s\" alt=\"scale image\" width=\"%1.0f\"/> </td>",image.str,get_img_width(legend.breaks[i])),inid+6),
                    write_function(sprintf("<td> %s </td>",legend.breaks[i]),inid+6),
                    write_function("</tr>",inid+4),
                    sep=""
                )
            )
        }
    }
    o <- paste(
        write_function("<div class=\"scale\">",inid),
        write_function(sprintf("<p class=\"scale-table-title\"><b>%s</b></p>",scale.obj[['name']]),inid+2),
        write_function("<table class=\"scale-table\">",inid+2),
        write_function("<col align=\"center\"/>",inid+4),
        write_function("<col align=\"left\"/>",inid+4),
        paste(table.rows,collapse=""),
        write_function("</table>",inid+2),
        write_function("</div>",inid),
        sep=""
    )
    return(o)
}

draw_scale.Scale.Size.Discrete <- function(scale.obj,scale.count=sample(1:100000,1),nice.format=TRUE,initial.indent=6){
    file.name <- tempfile()
    file.name <- paste(file.name,"png",sep=".")
    o <- html_table_size_scale(scale.obj,file.name,nice.format=nice.format,initial.indent=initial.indent)
    zzz <- file.remove(file.name)
    return(o)
}

draw_scale.Scale.Size.Continuous <- function(scale.obj,scale.count=sample(1:100000,1),nice.format=TRUE,initial.indent=6){
    file.name <- tempfile()
    file.name <- paste(file.name,"png",sep=".")
    o <- html_table_size_scale(scale.obj,file.name,nice.format=nice.format,initial.indent=initial.indent)
    zzz <- file.remove(file.name)
    return(o)
}


html_table_iconsize_scale <- function(scale.obj,nice.format=TRUE,initial.indent=6){
    inid <- initial.indent
    if(nice.format){
        write_function <- function(s,inid.t){
            return(paste(strrep(" ",inid.t),s,"\n",sep=""))
        }
    } else {
        write_function <- function(s,inid.t){
            return(s)
        }
    }
    if(is.null(scale.obj[["legend.breaks"]])){
        if(is.null(scale.obj[['vector']])){
            high.val <- max(scale.obj[['variable.values']],na.rm=TRUE)
            low.val <- max(scale.obj[['variable.values']],na.rm=TRUE)
            legend.breaks <- (0:4)*(high.val-low.val)/4+low.val
        } else {
            legend.breaks <- names(scale.obj[['vector']])
        }
    } else {
        legend.breaks <- scale.obj[['legend.breaks']]
    }
    if(is.null(scale.obj[['display.icon.img.src']]) || is.na(scale.obj[['display.icon.img.src']])){
        if(nrow(scale.obj[['icons']])>0){
            file.name <- scale.obj[['icons']]$src[1]
            image.width <- scale.obj[['icons']]$width[1]
            image.scalar <- scale.obj[['icons']]$scalar[1]
        } else {
            file.name <- NULL
            image.width <- 30
            image.scalar <- 1
        }
    } else {
        file.name <- scale.obj[['display.icon.img.src']]
        if(file.name %in% scale.obj[['icons']]$src){
            j <- which(scale.obj[['icons']]$src == file.name)
            image.width <- scale.obj[['icons']]$width[j]
            image.scalar <- scale.obj[['icons']]$scalar[j]
        } else {
            if(is.null(scale.obj[['display.icon.img.width']]) || is.na(scale.obj[['display.icon.img.width']])){
                image.width <- image_width(file.name)
            } else {
                image.width <- scale.obj[['display.icon.img.width']]
            }
            if(is.null(scale.obj[['display.icon.img.scalar']]) || is.na(scale.obj[['display.icon.img.scalar']])){
                width.indices <- which(!is.na(scale.obj[['icons']]$width))
                if(length(width.indices)>1){
                    j <- width.indices[1]
                    target.width <- scale.obj[['icons']]$width[j] * scale.obj[['icons']]$scalar[j]
                    image.scalar <- target.width/image.width
                } else {
                    test.file <- scale.obj[['icons']]$src[1]
                    test.width <- image_width(test.file)
                    target.width <- test.width * scale.obj[['icons']]$scalar[1]
                    image.scalar <- target.width/image.width
                }
            } else {
                image.scalar <- scale.obj[['display.icon.img.scalar']]
            }
        }
    }
    if(is.na(image.width) || is.null(image.width)){
        image.width <- image_width(file.name)
    }
    get_size_scale_width <- function(val){
        scale.size <- scale.obj[['function']](val)*image.scalar
        return(image.width*scale.size)
    }
    table.rows <- NULL
    image.str <- paste("data:image/png;base64,",base64enc::base64encode(file.name))
    if(is.numeric(legend.breaks)){
        for(i in 1:length(legend.breaks)){
            table.rows <- c(
                table.rows,
                paste(
                    write_function("<tr>",inid+4),
                    write_function(sprintf("<td> <img src=\"%s\" alt=\"scale image\" width=\"%1.0f\"/> </td>",image.str,get_size_scale_width(legend.breaks[i])),inid+6),
                    write_function(sprintf("<td> %1.2f </td>",legend.breaks[i]),inid+6),
                    write_function("</tr>",inid+4),
                    sep=""
                )
            )
        }
    } else {
        for(i in 1:length(legend.breaks)){
            table.rows <- c(
                table.rows,
                paste(
                    write_function("<tr>",inid+4),
                    write_function(sprintf("<td> <img src=\"%s\" alt=\"scale image\" width=\"%1.0f\"/> </td>",image.str,get_size_scale_width(legend.breaks[i])),inid+6),
                    write_function(sprintf("<td> %s </td>",legend.breaks[i]),inid+6),
                    write_function("</tr>",inid+4),
                    sep=""
                )
            )
        }
    }
    o <- paste(
        write_function("<div class=\"scale\">",inid),
        write_function(sprintf("<p class=\"scale-table-title\"><b>%s</b></p>",scale.obj[['name']]),inid+2),
        write_function("<table class=\"scale-table\">",inid+2),
        write_function("<col align=\"center\"/>",inid+4),
        write_function("<col align=\"left\"/>",inid+4),
        paste(table.rows,collapse=""),
        write_function("</table>",inid+2),
        write_function("</div>",inid),
        sep=""
    )
    return(o)
}

draw_scale.Scale.IconSize.Discrete <- function(scale.obj,scale.count=sample(1:100000,1),nice.format=TRUE,initial.indent=6){
    o <- html_table_iconsize_scale(scale.obj,nice.format=nice.format,initial.indent=initial.indent)
    return(o)
}

draw_scale.Scale.IconSize.Continuous <- function(scale.obj,scale.count=sample(1:100000,1),nice.format=TRUE,initial.indent=6){
    o <- html_table_iconsize_scale(scale.obj,nice.format=nice.format,initial.indent=initial.indent)
    return(o)
}


html_table_iconimage_scale <- function(scale.obj,nice.format=TRUE,initial.indent=6){
    inid <- initial.indent
    if(nice.format){
        write_function <- function(s,inid.t){
            return(paste(strrep(" ",inid.t),s,"\n",sep=""))
        }
    } else {
        write_function <- function(s,inid.t){
            return(s)
        }
    }
    get_icon_scale_width <- function(scale.obj,ind){
        if(!is.null(scale.obj[['icon.width']]) && !is.na(scale.obj[['icon.width']])){
            z.ind <- ind %% length(scale.obj[['icon.width']])
            if(z.ind==0) z.ind <- length(scale.obj[['icon.width']])
            return(scale.obj[['icon.width']][z.ind])
        } else {
            img.src <- scale.obj[['vector']][ind]
            w <- which(scale.obj[['icons']]$src == img.src)
            if(!is.na(scale.obj[['icons']]$width[w])){
                return(scale.obj[['icons']]$width[w]*scale.obj[['icons']]$scalar[w])
            } else {
                image.width <- image_width(img.src)
                return(image.width*scale.obj[['icons']]$scalar[w])
            }
        }
    }
    table.rows <- NULL
    if(length(scale.obj[['vector']])>0){
        for(i in 1:length(scale.obj[['vector']])){
            image.str <- paste("data:image/png;base64,",base64enc::base64encode(scale.obj[['vector']][i]))
            file.name <- scale.obj[['vector']][i]
            table.rows <- c(
                table.rows,
                paste(
                    write_function("<tr>",inid+4),
                    write_function(sprintf("<td> <img src=\"%s\" alt=\"scale image\" width=\"%1.0f\"/> </td>",image.str,get_icon_scale_width(scale.obj,i)),inid+6),
                    write_function(sprintf("<td> %s </td>",names(scale.obj[['vector']])[i]),inid+6),
                    write_function("</tr>",inid+4),
                    sep=""
                )
            )
        }
    }
    o <- paste(
        write_function("<div class=\"scale\">",inid),
        write_function(sprintf("<p class=\"scale-table-title\"><b>%s</b></p>",scale.obj[['name']]),inid+2),
        write_function("<table class=\"scale-table\">",inid+2),
        write_function("<col align=\"center\"/>",inid+4),
        write_function("<col align=\"left\"/>",inid+4),
        paste(table.rows,collapse=""),
        write_function("</table>",inid+2),
        write_function("</div>",inid),
        sep=""
    )
    return(o)
}


draw_scale.Scale.IconImage.Discrete <- function(scale.obj,scale.count=sample(1:100000,1),nice.format=TRUE,initial.indent=6){
    o <- html_table_iconimage_scale(scale.obj,nice.format=nice.format,initial.indent=initial.indent)
    return(o)
}
