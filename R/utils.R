col2hex <- function(color.str,alpha=1){
    rgb.vector <- as.numeric(grDevices::col2rgb(color.str))
    hex.out <- grDevices::rgb(rgb.vector[1],rgb.vector[2],rgb.vector[3],alpha*255,maxColorValue=255)
    return(hex.out)
}

color_check <- function(color.vector,alpha.vector=1){
    ncols <- length(color.vector)
    nalphs <- length(alpha.vector)
    alpha.index <- 1
    o <- NULL
    for(i in 1:ncols){
        color.str <- color.vector[i]
        alpha <- alpha.vector[alpha.index]
        if(nchar(color.str)==7 && grepl("#[0-9,A-F][0-9,A-F][0-9,A-F][0-9,A-F][0-9,A-F][0-9,A-F]",color.str)){
            o <- append(o,col2hex(color.str,alpha))
        } else if(nchar(color.str)==9 && grepl("#[0-9,A-F][0-9,A-F][0-9,A-F][0-9,A-F][0-9,A-F][0-9,A-F][0-9,A-F][0-9,A-F]",color.str)){
            o <- append(o,color.str)
        } else {
            o <- append(o,col2hex(color.str,alpha))
        }
        if(alpha.index == nalphs){
            alpha.index <- 1
        } else {
            alpha.index <- alpha.index + 1
        }
    }
    return(o)
}

hex2rgb_arraystr <- function(input.hex){
    col <- color_check(input.hex)
    v <- as.numeric(
        grDevices::col2rgb(
            col,
            alpha=TRUE
        )
    )
    v[4] <- v[4]/255
    return(
        paste(
            "[",
            paste(
                v,
                collapse=","
            ),
            "]",
            sep=""
        )
    )    
}

hex2rgba_func <- function(input.hex){
    col <- color_check(input.hex)
    v <- as.numeric(
        grDevices::col2rgb(
            col,
            alpha=TRUE
        )
    )
    v[4] <- v[4]/255
    return(
        paste(
            "rgba(",
            paste(
                v,
                collapse=","
            ),
            ")",
            sep=""
        )
    )    
}

rgb2hsv <- function(color.str){
    rr <- t(grDevices::col2rgb(color.str))/255
    rgb.min<-min(rr)
    rgb.max<-max(rr)
    V <- rgb.max
    if(rgb.max==0){
        S <- 0
        H <- 0
    } else {
        delta <- rgb.max-rgb.min
        S <- delta/V
        if(rr[1]==rgb.max){
            H <- 0+1/6*(rr[2]-rr[3])/delta
        } else if(rr[2]==rgb.max){
            H <- 1/3 + 1/6*(rr[3]-rr[1])/delta
        } else {
            H <- 2/3+1/6*(rr[1]-rr[2])
        }
    }
    if(H<0){
        H <- H + 1
    }
    return(c(H,S,V))
}


get_vind <- function(layer.ind,list.len){
    vind <- layer.ind %% list.len
    if (vind==0) vind <- list.len
    return(vind)
}



get_ol_layer_label <- function(df,mapping,label,label.params){
    default.values <- list(
        font=NULL,
        offsetX=0,
        offsetY=0,
        rotation=0,
        textAlign='center',
        textBaseline='middle',
        stroke_color='#000000FF',
        fill_color=NULL
    )
    for(i in 1:length(default.values)){
        if(!(names(default.values)[i] %in% names(label.params))){
            label.params[[names(default.values)[i]]] <- default.values[[i]]
        }
    }
    n <- names(mapping)
    if('label' %in% n){ #### Some of these attributes can be set through mapping later
        label.values <- df[,as.character(n$mapping$label)]
    } else if(!is.null(label)) {
        label.values <- label
    } else {
        label.values <- NULL
    }
    if(is.null(label.params[['fill_color']])) label.params[['fill_color']] <- label.params[['stroke_color']]
    if(is.null(label.values)){
        o <- NULL
    } else {
        o <- list(
            text=label.values,
            font=label.params[['font']],
            offsetX=label.params[['offsetX']],
            offsetY=label.params[['offsetY']],
            rotation=label.params[['rotation']],
            textAlign=label.params[['textAlign']],
            textBaseline=label.params[['textBaseline']],
            stroke_color=sapply(label.params[['stroke_color']],hex2rgb_arraystr),
            fill_color=sapply(label.params[['fill_color']],hex2rgb_arraystr)
        )
        class(o) <- "Label"
    }
    return(o)
}

get_ol_layer_tooltip <- function(df,mapping,tooltip,tooltip.params){
    default.values <- list(
        font=NULL,
        offsetX=5,
        offsetY=5,
        positioning='bottom-left',
        stroke_color='#000000FF',
        fill_color="#FFFFFFA0",
        padding="3px",
        border="solid black 1px",
        borderradius="3px"
    )
    for(i in 1:length(default.values)){
        if(!(names(default.values)[i] %in% names(tooltip.params))){
            tooltip.params[[names(default.values)[i]]] <- default.values[[i]]
        }
    }
    n <- names(mapping)
    if('tooltip' %in% n){ #### Some of these attributes can be set through mapping later
        tooltip.values <- df[,as.character(mapping$tooltip)]
    } else if(!is.null(tooltip)) {
        tooltip.values <- tooltip
    } else {
        tooltip.values <- NULL
    }
    if(is.null(tooltip.values)){
        o <- NULL
    } else {
        o <- list(
            text=tooltip.values,
            font=tooltip.params[["font"]],
            offsetX=tooltip.params[["offsetX"]],
            offsetY=tooltip.params[["offsetY"]],
            positioning=tooltip.params[["positioning"]],
            stroke_color=hex2rgba_func(tooltip.params[["stroke_color"]]),
            fill_color=hex2rgba_func(tooltip.params[["fill_color"]]),
            padding=tooltip.params[["padding"]],
            border=tooltip.params[["border"]],
            borderradius=tooltip.params[["borderradius"]]
        )
        class(o) <- "tooltip"
    }
    return(o)
}

attribution_str <- function(df,row.index){
    link <- sprintf("<a href=\\\"%s\\\">%s</a>",df$url[row.index],df$name[row.index])
    o <- paste(link,df$copyright[row.index],sep=" - ")
    return(o)
}