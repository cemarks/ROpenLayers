#' Export OpenLayers Map to file.
#'
#' Writes Ol.Map object to HTML file.
#'
#' Ol.Map object is written to HTML file with embedded javascript.  The file
#' will source the OpenLayers javascript library identified by the Ol.Map 
#' object and call any REST APIs required for each layer.  The intent is to
#' produce an output file with supporting images, as required, that can
#' be placed directly into a directory hosted by a minimal http server.      
#' 
#' @param ol.map.obj Ol.Map object to be exported.
#' @param file.name character output HTML file name.  This name will be appended
#' to the \code{file.path}.
#' @param file.path character output HTML file path.  The output HTML file and
#' supporting directories (e.g., 'images') will be in this path.
#' @param page.name character page title to be included in the HTML head section.
#' @param image.path character \emph{relative} path from \code{file.path} to 
#' directory that will contain page images. 
#' @param nice.format logical.  If \code{TRUE}, output file will be formated with
#' new lines and indentation for human readability.
#' @param IE.compatability.view logical.  If \code{TRUE}, the statement\cr
#' \code{<meta http-equiv="X-UA-Compatible" content="IE=edge"/>}\cr
#' to the HTML document head.  This statement is required for some browsers to 
#' render the map.
#'
#' @return NULL
#'
#' @seealso 
#' \code{\link{ol_map}}, 
#' \code{\link{ol_map2Strings}}, 
#'
#' @export
#'
#' @examples
#' mymap <- ol_map()
#' base.layer <- public_arcgis_basemap('LightGray')
#' mymap <- mymap + base.layer
#' ## The following writes HTML and needed images (not run)
#' # ol_map2HTML(mymap,"SanDiego.html", nice.format=TRUE)
#' ## Open in browser (not run)
#' # browseURL("SanDiego.html")
ol_map2HTML <- function(
    ol.map.obj,
    file.name,
    file.path=".",
    page.name="ROpenLayers Map",
    image.path="images",
    nice.format=FALSE,
    IE.compatability.view=TRUE
){
    if(grepl("/",file.name)){
        warning("File name string contains path identifier.  This could result in broken image links.")
        str.sp <- strsplit(file.name,"/")[[1]]
        partial.path <- paste(file.path,paste(str.sp[1:(length(str.sp)-1)],collapse="/"),sep="/")
        if(!dir.exists(partial.path)){
            stop(sprintf("Directory does not exist! %s",partial.path))
        }
    }
    if(!dir.exists(paste(file.path,image.path,sep="/"))){
        image.dir.exists <- FALSE
    } else {
        image.dir.exists <- TRUE
    }
    ###### CHECK FOR IMAGES & DRAW SCALES
    if(any(sapply(ol.map.obj[['layers']],function(x) if(class(x)=="Layer.SpatialPoint") return(TRUE) else return(FALSE)))){
        if(!image.dir.exists){
            dir.create(paste(file.path,image.path,sep="/"))
            image.dir.exists <- TRUE
        }
    }
    scale.div.vector <- NULL
    display.scale <- FALSE
    for(i in 1:length(ol.map.obj[['layers']])){
        if(length(ol.map.obj[['layers']][[i]][['scale']])>0){
            for(j in 1:length(ol.map.obj[['layers']][[i]][['scale']])){
                if(ol.map.obj[['layers']][[i]][['scale']][[j]][["type"]]!="fixed" && ol.map.obj[['layers']][[i]][['scale']][[j]][['display']]){
                    if(!image.dir.exists){
                        dir.create(paste(file.path,image.path,sep="/"))
                        image.dir.exists <- TRUE
                    }
                    new.scale.html <- draw_scale(ol.map.obj[['layers']][[i]][['scale']][[j]],image.path,nice.format=nice.format,initial.indent=8)
                    display.scale<-TRUE
                    scale.div.vector<-c(scale.div.vector,new.scale.html)
                }
            }
        }
    }
    if(nrow(ol.map.obj[['layer.control.df']])>0){
        layer.control <- TRUE
    } else {
        layer.control <- FALSE
    }
    lst2indent_string <- function(input.list){
        o<-""
        if(length(input.list[['text']])>0){
            for(i in 1:length(input.list[['text']])){
                o<-paste(o,strrep(" ",input.list[['indent']][[i]]),input.list[['text']][[i]],sep="")
            }
        }
        return(o)
    }
    inid <- 0
    if(nice.format){
        write_function <- function(s){
            cat(paste(strrep(" ",inid),s,"\n",sep=""))
        }
    } else {
        write_function <- function(s){
            cat(s)
        }
    }
    warnings <- NULL
    sink(paste(file.path,file.name,sep="/"))
    tryCatch({
        write_function("<!DOCTYPE html>")
        write_function("<html>")
        inid <- inid + 2
        write_function("<head>")
        inid <- inid + 2
        if(IE.compatability.view){
            write_function("<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"/>")
        }
        write_function(sprintf("<title>%s</title>",page.name))
        write_headscript(ol.map.obj[['ol.source.url']],nice.format,inid)
        write_function("<style>")
        inid <- inid + 2
        write_style(
            width=ol.map.obj[['map.width']],
            height=ol.map.obj[['map.height']],
            display.scale=display.scale,
            layer.control=layer.control,
            tooltips.param.vector=ol.map.obj[['tooltips.param.vector']],
            nice.format = nice.format,
            initial.indent=inid
        )
        inid <- inid - 2
        write_function("</style>")
        inid <- inid - 2
        write_function("</head>")
        write_function("<body>")
        inid <- inid + 2
        write_body_html(
            display.scale=display.scale,
            scale.divs=scale.div.vector,
            layer.control.df=ol.map.obj[['layer.control.df']],
            tooltips.bool=ol.map.obj[['tooltips']],
            tooltips.param.vector=ol.map.obj[['tooltips.param.vector']],
            map.heading=ol.map.obj[['map.heading']],
            map.note=ol.map.obj[['map.note']],
            nice.format=nice.format,
            initial.indent = inid
        )
        write_function("<script>")
        inid <- inid + 2
        write_body_script(
            ol.map.obj,
            layer.control,
            image.path,
            nice.format=nice.format,
            initial.indent=inid
        )
        inid <- inid - 2
        write_function("</script>")
        inid <- inid - 2 
        write_function("</body>")
        inid <- inid - 2
        write_function("</html>")
        sink()
    }, warning = function(w) {
        warnings <- c(warnings,w)
    }, error = function(e) {
        sink()
        cat(paste(e,"\n",sep=""))
    }, finally = {
        if(length(warnings)>0){
            for(i in warnings) cat(paste("Warning: ",i,"\n",sep=""))
        }
    })
}


#' OpenLayers Map HTML to List
#'
#' Assigns Ol.Map HTML content to list.
#'
#' Ol.Map object HTML is exported to a list object that can be deployed in a 
#' variety of applications or server environments.  See exmaples for a minimal
#' example using RShiny.  This method writes image files to the directory specified
#' in \code{image.path} if they are needed for any layers in \code{ol.map.obj}.  
#' This method does not currently support adding multiple maps to the same 
#' web page, as javascript variable names would be replicated.
#' 
#' @param ol.map.obj Ol.Map object to be exported.
#' @param image.path character \emph{relative} path from \code{file.path} to 
#' directory that will contain page images. 
#' @param deployment.image.path character path to images on server, if different
#' from \code{image.path}.  The \code{image.path} will be replaced with this 
#' path in the returned strings.
#'
#' @return list object with the following character elements:
#' \tabular{ll}{
#' $headscript \tab HTML script block sourcing the OpenLayers javascript library identify in \code{ol.map.obj}.\cr
#' $style \tab  CSS code for styling the map and legends. \cr
#' $body.html \tab HTML map and legend containers, and associated elements. \cr
#' $body.script \tab Javascript code writing the layer and map objects. \cr
#' }
#'
#' @seealso 
#' \code{\link{ol_map}}, 
#' \code{\link{ol_map2HTML}}, 
#'
#' @export
#'
#' @examples
#' heatmap.pts <- matrix(
#'     c(
#'         rnorm(100,-80.385,1), #Miami Longitudes
#'         rnorm(100,-117.1611,3), #San Diego Longitudes
#'         rnorm(100,25.782618,1), #Miami Latitudes
#'         rnorm(100,32.7157,3) # San Diego Latitudes
#'     ),ncol=2
#' )
#' mymap <- ol_map(
#'     center=c(-98.5,28.5),
#'     zoom=4,
#'     nga.olsource=FALSE,
#'     map.note="Heatmap of random points centered on Miami and San Diego.") + 
#'     public_OSM_basemap() +
#'     ol_geom_heatmap(
#'         heatmap.pts,
#'         name="Random Heatmap",
#'         toggle.control=TRUE,
#'         opacity=0.25
#'         )
#' ## The following line will create image files
#' ## as needed for point layers and legends.
#' ## None are required in this example.
#' HTML.strings <- ol_map2Strings(mymap)
#' ## Minimal shiny example
#' ## Not Run
#' # library(shiny)
#' # ui <- shinyUI(
#' #     fluidPage(
#' #         ## Add OpenLayers Javascript source & CSS to head
#' #         tags$head(
#' #             HTML("<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"/>")
#' #             HTML(HTML.strings[[1]]),
#' #             tags$style(HTML(HTML.strings[[2]]))
#' #         ),
#' #         titlePanel("Random Heatmap"),
#' #         mainPanel(
#' #             tags$div(HTML(HTML.strings[[3]]))
#' #         ),
#' #         tags$script(HTML(HTML.strings[[4]]))
#' #     )
#' # )
#' # server <- function(input,output){
#' # }
#' # shinyApp(ui=ui,server)
ol_map2Strings <- function(ol.map.obj,image.path="images",deployment.image.path=NULL){
    nice.format=FALSE
    if(!dir.exists(image.path)){
        image.dir.exists <- FALSE
    } else {
        image.dir.exists <- TRUE
    }
    ###### CHECK FOR IMAGES & DRAW SCALES
    if(any(sapply(ol.map.obj[['layers']],function(x) if(class(x)=="Layer.SpatialPoint") return(TRUE) else return(FALSE)))){
        if(!image.dir.exists){
            dir.create(image.path)
            image.dir.exists <- TRUE
        }
    }
    scale.div.vector <- NULL
    display.scale <- FALSE
    for(i in 1:length(ol.map.obj[['layers']])){
        if(length(ol.map.obj[['layers']][[i]][['scale']])>0){
            for(j in 1:length(ol.map.obj[['layers']][[i]][['scale']])){
                if(ol.map.obj[['layers']][[i]][['scale']][[j]][["type"]]!="fixed" && ol.map.obj[['layers']][[i]][['scale']][[j]][['display']]){
                    if(!image.dir.exists){
                        dir.create(image.path)
                        image.dir.exists <- TRUE
                    }
                    new.scale.html <- draw_scale(ol.map.obj[['layers']][[i]][['scale']][[j]],image.path,nice.format=nice.format,initial.indent=8)
                    display.scale<-TRUE
                    scale.div.vector<-c(scale.div.vector,new.scale.html)
                }
            }
        }
    }
    if(nrow(ol.map.obj[['layer.control.df']])>0){
        layer.control <- TRUE
    } else {
        layer.control <- FALSE
    }
    head.script <- utils::capture.output(
        write_headscript(ol.map.obj[['ol.source.url']],nice.format,0)
    )
    style <- utils::capture.output(
        write_style(
            width=ol.map.obj[['map.width']],
            height=ol.map.obj[['map.height']],
            display.scale=display.scale,
            layer.control=layer.control,
            tooltips.param.vector=ol.map.obj[['tooltips.param.vector']],
            nice.format = nice.format,
            initial.indent=0
        )
    )
    body.html <- utils::capture.output(
        write_body_html(
            display.scale=display.scale,
            scale.divs=scale.div.vector,
            layer.control.df=ol.map.obj[['layer.control.df']],
            tooltips.bool=ol.map.obj[['tooltips']],
            tooltips.param.vector=ol.map.obj[['tooltips.param.vector']],
            map.heading=ol.map.obj[['map.heading']],
            map.note=ol.map.obj[['map.note']],
            nice.format=nice.format,
            initial.indent = 0
        )
    )
    body.script <- utils::capture.output(
        write_body_script(
            ol.map.obj,
            layer.control,
            image.path,
            nice.format=nice.format,
            initial.indent=0
        )        
    )
    if(!is.null(deployment.image.path)){
        body.html <- gsub(
            paste(image.path,"/",sep=""),
            paste(deployment.image.path,"/",sep=""),
            body.html,
            fixed=TRUE
        )
        body.script <- gsub(
            paste(image.path,"/",sep=""),
            paste(deployment.image.path,"/",sep=""),
            body.script,
            fixed=TRUE
        )
    }
    return(
        list(
            headscript = head.script,
            style=style,
            body.html=body.html,
            body.script=body.script
        )
    )
}





write_headscript <- function(ol.source.url,nice.format=TRUE,initial.indent=4){
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
    write_function(sprintf("<script type=\"text/javascript\" src=\"%s\"></script>",ol.source.url))
}

write_style <- function(width,height,display.scale=FALSE,layer.control=FALSE,scale.width="200px",tooltips.param.vector,nice.format=TRUE,initial.indent=6){
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
    if(is.numeric(scale.width)){
        lw <- paste(round(scale.width),"px",sep="")
    } else {
        lw <- as.character(scale.width)
    }
    cat(ol.stylesheet)
    write_function("a.skiplink {")
    inid <- inid + 2
    write_function("position: absolute;")
    write_function("clip: rect(1px, 1px, 1px, 1px);")
    write_function("padding: 0;")
    write_function("border: 0;")
    write_function("height: 1px;")
    write_function("width: 1px;")
    write_function("overflow: hidden;")
    inid <- inid -2
    write_function("}")
    write_function("a.skiplink:focus {")
    inid <- inid + 2
    write_function("clip: auto;")
    write_function("height: auto;")
    write_function("width: auto;")
    write_function("background-color: #fff;")
    write_function("padding: 0.3em;")
    inid <- inid -2
    write_function("}")
    write_function(".map-container{")
    inid <- inid + 2
    write_function("position: relative;")
    write_function("overflow: hidden;")
    if(!is.null(width)){
        if(is.numeric(width)){
            w <- paste(round(width),"px",sep="")
        } else {
            w <- as.character(width)
        }
        write_function(sprintf("width: %s;",w))
    }
    if(!is.null(height)){
        if(is.numeric(height)){
            h <- paste(round(height),"px",sep="")
        } else {
            h <- as.character(height)
        }
        write_function(sprintf("height: %s;",h))
    }
    inid <- inid -2
    write_function("}")
    write_function(".postmappar-container{")
    inid <- inid + 2
    write_function("text-align: center;")
    write_function("position: relative;")
    write_function("overflow: hidden;")
    if(!is.null(width)){
        if(is.numeric(width)){
            w <- paste(round(width),"px",sep="")
        } else {
            w <- as.character(width)
        }
        write_function(sprintf("width: %s;",w))
    }
    inid <- inid -2
    write_function("}")
    if(display.scale){
        write_function(".map{")
        inid <- inid + 2
        write_function(sprintf("margin-right: %s;",lw))
        write_function("padding-right: 10px;")
        inid <- inid -2
        write_function("}")
    }
    write_function(".scale-container{")
    inid <- inid + 2
    write_function(sprintf("width: %s;",lw))
    write_function("float: right;")
    inid <- inid -2
    write_function("}")
    write_function(".layer-control{")
    inid <- inid + 2
    write_function("position:absolute;")
    write_function("top: 0px;")
    write_function("background-color: rgba(255,255,255,0.6);")
    if(display.scale){
        write_function(sprintf("right: %s;",lw))
        write_function("padding-right: 25px;")
    } else {
        write_function("right: 0px;")
    }
    inid <- inid -2
    write_function("}")
    write_function(".scale-table-title{")
    inid <- inid + 2
    write_function("margin-bottom: 3px;")
    write_function("margin-left: 6px;")
    inid <- inid -2
    write_function("}")
    write_function(".scale-table{")
    inid <- inid + 2
    write_function("margin-top: 3px;")
    inid <- inid -2
    write_function("}")
    write_function(".scale-table td{")
    inid <- inid + 2
    write_function("padding: 0 15px 0 15px;")
    write_function("text-align: center;")
    inid <- inid -2
    write_function("}")
    write_function(".scale{")
    inid <- inid + 2
    write_function("display:block;")
    write_function("margin-bottom: 10px;")
    write_function("width: 200px;")
    inid <- inid -2
    write_function("}")
    write_function(".ol-tooltip{")
    inid <- inid + 2
    write_tooltip_css(tooltips.param.vector,nice.format,initial.indent=inid)
    inid <- inid -2
    write_function("}")
    write_function("#map:focus {")
    inid <- inid + 2
    write_function("outline: #4A74A8 solid 0.15em;")
    inid <- inid -2
    write_function("}")
}

write_tooltip_css <- function(tooltip.params,nice.format=TRUE,initial.indent=8){
    params.translation <- c(
        fill.color="background-color",
        border="border",
        borderradius="border-radius",
        font="font",
        padding="padding",
        stroke.color="font-color"
        )
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
    for(param in names(params.translation)){
        if(!is.null(tooltip.params[param]) && !is.na(tooltip.params[param])){
            write_function(sprintf("%s:%s;",params.translation[param],tooltip.params[param]))
        }
    }
}

write_body_html <- function(display.scale=FALSE,
    scale.divs=NULL,
    layer.control.df=NULL,
    tooltips.bool=NULL,
    tooltips.param.vector=NULL,
    map.heading=NULL,
    map.note=NULL,
    nice.format=TRUE,
    initial.indent=4){
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
    if(!is.null(map.heading)){
        write_function(sprintf("<h1>%s</h1>",map.heading))
    }
    write_function("<a class=\"skiplink\" href=\"#map\">Go to map</a>")
    write_function("<div class=\"map-container\">")
    inid <- inid + 2
    if(display.scale){
        write_function("<div class=\"scale-container\"> ")
        for(i in scale.divs){
            cat(i)
        }
        write_function("</div>")
    }
    if(tooltips.bool){
        write_function("<div id=\"map\" class=\"map\">")
        inid <- inid + 2
        write_function("<div id=\"map-tooltip\" class=\"ol-tooltip\"></div>")
        inid <- inid -2
        write_function("</div>")
    } else {
        write_function("<div id=\"map\" class=\"map\"></div>")
    }
    if(nrow(layer.control.df)>0){
        write_function("<div class=\"layer-control\">")
        inid <- inid + 2
        if(nrow(layer.control.df)>0){
            nlayers <- nrow(layer.control.df)
            for(i in 1:nlayers){
                write_function(sprintf("<input type=\"checkbox\" id=\"%s\" checked/> %s <br/>",layer.control.df$layer.var[i],layer.control.df$name[i]))
            }
        }
        inid <- inid -2
        write_function("</div>")
    }
    inid <- inid -2
    write_function("</div>")
    if(!is.null(map.note)){
        write_function("<div class=\"postmappar-container\">")
        inid <- inid + 2
        write_function(sprintf("<p>%s</p>",map.note))
        inid <- inid -2
        write_function("</div>")
    }
}


write_body_script <- function(ol.map.obj,layer.control,image.path,nice.format=TRUE,initial.indent=6){
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
    nl <- length(ol.map.obj[['layers']])
    if(nl >= 1){
        for(i in 1:nl){
            writeLayer(ol.map.obj[['layers']][[names(ol.map.obj[['layers']])[i]]],names(ol.map.obj[['layers']][i]),nice.format=nice.format,initial.indent = inid,image.path=image.path)
        }
    }
    if(nice.format) cat("\n\n")
    write_function("var map = new ol.Map({")
    inid <- inid + 2
    write_function("layers: [")
    inid <- inid + 2
    if(nl > 1){
        for(i in 1:(nl-1)){
            write_function(sprintf("layer_%s,",names(ol.map.obj[['layers']])[i]))
        }
    }
    i<-nl
    write_function(sprintf("layer_%s",names(ol.map.obj[['layers']])[i]))
    inid <- inid - 2
    write_function("],")
    write_function("target: 'map',")
    write_function("controls: ol.control.defaults({")
    inid <- inid + 2
    write_function("attributionOptions: /** @type {olx.control.AttributionOptions} */ ({")
    inid <- inid + 2
    write_function("collapsible: false")
    inid <- inid - 2
    write_function("})")
    inid <- inid - 2
    write_function("}),")
    write_function("view: new ol.View({")
    inid <- inid + 2
    write_function(sprintf("center: %s,",write_coordinate(ol.map.obj[['center']])))
    write_function(sprintf("zoom: %s",as.character(as.integer(ol.map.obj[['zoom']]))))
    inid <- inid - 2
    write_function("})")
    inid <- inid - 2
    write_function("});")
    if(layer.control){
        for(i in 1:nrow(ol.map.obj[['layer.control.df']])){
            layer.var.name <- ol.map.obj[['layer.control.df']]$layer.var[i]
            write_function(sprintf("document.getElementById(\"%s\").onchange = function(){",layer.var.name))
            inid <- inid + 2
            write_function(sprintf("%s.setVisible(document.getElementById(\"%s\").checked);",layer.var.name,layer.var.name))
            inid <- inid - 2
            write_function("};")
            if(nice.format) cat("\n")
        }
    }
    if(ol.map.obj[['tooltips']]){
        write_function("var ol_tooltip = document.getElementById('map-tooltip');")
        write_function("var overlay = new ol.Overlay({")
        inid <- inid + 2
        write_function("element: ol_tooltip,")
        write_function(sprintf("offset: [%i, %i],",
            as.integer(ol.map.obj[['tooltips.param.vector']]['offsetX']),
            as.integer(ol.map.obj[['tooltips.param.vector']]['offsetY'])))
        write_function(sprintf("positioning: '%s'",ol.map.obj[['tooltips.param.vector']][['positioning']]))
        inid <- inid - 2
        write_function("});")
        write_function("map.addOverlay(overlay);")
        if(nice.format) cat("\n")
        write_function("function displayOlTooltip(evt) {")
        inid <- inid + 2
        write_function("var pixel = evt.pixel;")
        write_function("var feature = map.forEachFeatureAtPixel(pixel, function(feature) {")
        inid <- inid + 2
        write_function("return feature;")
        inid <- inid - 2
        write_function("});")
        write_function("if (feature) {")
        inid <- inid + 2
        write_function("var feature_text = feature.get(\"tooltip_text\");")
        write_function("ol_tooltip.style.display = feature_text ? '' : 'none';")
        write_function("if(feature_text){")
        inid <- inid + 2
        write_function("overlay.setPosition(evt.coordinate);")
        write_function("ol_tooltip.innerHTML = feature_text;")
        inid <- inid - 2
        write_function("}")
        inid <- inid - 2
        write_function("} else {")
        inid <- inid + 2
        write_function("ol_tooltip.style.display = 'none';")
        inid <- inid - 2
        write_function("}")
        inid <- inid - 2
        write_function("};")
        write_function("map.on('pointermove',displayOlTooltip);")
        if(nice.format) cat("\n")
    }
}

writeLayer <- function(layer,suffix="basemap",nice.format=TRUE,initial.indent=6,...) UseMethod("writeLayer")
writeLayer.default <- function(layer,suffix="basemap",nice.format=TRUE,initial.indent=6,...) return(0)
writeLayer.Layer.ArcGIS <- function(layer,suffix="basemap",nice.format=TRUE,initial.indent=6,...){
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
    write_function(sprintf("var layer_%s = new ol.layer.Tile({",suffix))
    inid <- inid + 2
    write_function(sprintf("name: \"%s\",",gsub('"',"'",layer[['name']])))
    write_function("source: new ol.source.TileArcGISRest({")
    inid <- inid + 2
    write_function(sprintf("attributions: \"%s\",",layer[['attributions']]))
    write_function(sprintf("url: \"%s\"",layer[['url']]))
    inid <- inid - 2
    write_function("})")
    inid <- inid - 2
    write_function("});")
    if(nice.format) cat("\n")
}

writeLayer.Layer.OSM <- function(layer,suffix="basemap",nice.format=TRUE,initial.indent=6,...){
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
    write_function(sprintf("var layer_%s = new ol.layer.Tile({",suffix))
    inid <- inid + 2
    write_function(sprintf("name: \"%s\",",gsub('"',"'",layer[['name']])))
    write_function("source: new ol.source.OSM({")
    inid <- inid + 2
    write_function(sprintf("attributions: \"%s\"",layer[['attributions']]))
    inid <- inid - 2
    write_function("})")
    inid <- inid - 2
    write_function("});")
    if(nice.format) cat("\n")
}

# writeLayer.layer.bing <- function(layer,suffix="basemap"){
#     cat(sprintf("      var layer_%s = new ol.layer.Tile({\n",suffix))
#     cat("        source: new ol.source.BingMaps({\n")
#     cat("        })\n")
#     cat("      });\n\n")
# }

#Not used; needs development
# writeLayer.Layer.ArcGISVector <- function(layer,suffix="basemap",nice.format=TRUE,initial.indent=6,...){
#     inid <- initial.indent
#     if(nice.format){
#         write_function <- function(s){
#             cat(paste(strrep(" ",inid),s,"\n",sep=""))
#         }
#     } else {
#         write_function <- function(s){
#             cat(s)
#         }
#     }
#     write_function(sprintf("var layer_%s = new ol.layer.VectorTile({",suffix))
#     inid <- inid + 2
#     write_function(sprintf("name: \"%s\",",gsub('"',"'",layer[['name']])))
#     write_function("source: new ol.source.VectorTile({")
#     inid <- inid + 2
#     write_function(sprintf("attributions: \"%s\",",layer[['attributions']]))
#     write_function("format: new ol.format.MVT(),")
#     write_function(sprintf("url: '%s/tile/{z}/{y}/{x}.pbf'",layer[['url']]))
#     inid <- inid - 2
#     write_function("})")
#     inid <- inid - 2
#     write_function("});")
#     if(nice.format) cat("\n")
# }


write_coordinate <- function(coord.vector){
    return(sprintf("ol.proj.fromLonLat([%0.4f,%0.4f],'EPSG:3857')",coord.vector[1],coord.vector[2]))
}


write_scale_attr_polygons_lines <- function(scale.list,ind,nice.format=TRUE,initial.indent = 14){
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
    scale.attr <- scale.list[['attribute']]
    if(!(is.null(scale.list[['type']])) && !(is.na(scale.list[['type']])) && (scale.list[['type']] %in% c('continuous','discrete','fixed'))){
        if(scale.list[['attribute']]=='lty'){
            if(scale.list[['type']]=='continuous' || scale.list[['type']]=='discrete'){
                feature.value <- scale.list[['function']](scale.list[['variable.values']][ind])
            } else if((scale.list[['type']]=='fixed') && (length(scale.list[['value']]) > 1) && (!is.na(scale.list[['value']]) && (!is.null(scale.list[['value']])))){
                vind <- get_vind(ind,length(scale.list[['value']]))
                if(is.list(scale.list[['value']])){
                    feature.value <- scale.list[['value']][[vind]]
                } else {
                    feature.value <- NA
                }
            } else {
                feature.value <- NA
            }
            if(!is.na(feature.value)  && !is.null(feature.value) && length(feature.value)>1){
                write_function(sprintf("%s: [%s],",scale.attr,paste(feature.value,collapse=",")))
            }
        } else if(scale.list[['attribute']] %in% c('fill','color')){
            if(scale.list[['type']]=='continuous' || scale.list[['type']]=='discrete'){
                feature.value <- scale.list[['function']](scale.list[['variable.values']][ind])
            } else if((scale.list[['type']]=='fixed') && !is.na(scale.list[['value']]) && !is.null(scale.list[['value']]) && (length(scale.list[['value']]) > 1)){
                vind <- get_vind(ind,length(scale.list[['value']]))
                feature.value <- scale.list[['value']][vind]
            } else {
                feature.value <- NA
            }
            if(!is.na(feature.value)  && !is.null(feature.value)){
                write_function(sprintf("%s: %s,",scale.attr,hex2rgb_arraystr(feature.value)))
            }
        } else if(scale.list[['attribute']] == 'lwd'){
            if(scale.list[['type']]=='continuous' || scale.list[['type']]=='discrete'){
                feature.value <- scale.list[['function']](scale.list[['variable.values']][ind])
            } else if((scale.list[['type']]=='fixed') && !is.na(scale.list[['value']]) && !is.null(scale.list[['value']]) && (length(scale.list[['value']]) > 1)){
                vind <- get_vind(ind,length(scale.list[['value']]))
                feature.value <- scale.list[['value']][vind]
            } else {
                feature.value <- NA
            }
            if(!is.na(feature.value)  && !is.null(feature.value)){
                write_function(sprintf("%s: %1.1f,",scale.attr,feature.value))
            }
        }
    }
}

write_tooltip_attr <- function(tooltip,ind,nice.format=TRUE,initial.indent=14){
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
    if(!(is.null(tooltip)) && !(all(is.na(tooltip)))){
        if(!(is.null(tooltip[['text']])) && !(all(is.na(tooltip[['text']])))){
            tt <- gsub('"',"'",tooltip[['text']][ind])
            write_function(sprintf("tooltip_text: \"%s\",",tt))
        }
    }
}

write_label_attr_vec <- function(label.list,ind,nice.format=TRUE,initial.indent=14){
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
    if(!(is.null(label.list)) && !(all(is.na(label.list)))){
        label.list[["text"]] <- gsub('"',"'",label.list[["text"]])
        fields <- which(sapply(1:length(label.list), function(x) if(length(label.list[[x]])>1) return(TRUE) else return(FALSE)))
        for(str in names(label.list)[fields]){
            object.name <- paste('label',str,sep="_")
            if(!(is.null(label.list[[str]])) && !(is.na(label.list[[str]]))){
                vind <- get_vind(ind,length(label.list[[str]]))
                if(is.list(label.list[[str]])){
                    attr.val <- label.list[[str]][[vind]]
                } else {
                    attr.val <- label.list[[str]][vind]
                }
                if(length(attr.val) > 1){
                    if(is.numeric(attr.val)){
                        write_function(paste(sprintf("%s: [",object.name),paste(sprintf("%1.2f",attr.val),collapse=","),"],",sep=""))
                    } else {
                        write_function(paste(sprintf("%s: [",object.name),paste(sprintf("\"%s\"",gsub("\"","'",as.character(attr.val))),collapse=","),"],",sep=""))
                    }
                } else {
                    if(is.numeric(attr.val)){
                        write_function(sprintf("%s: %1.2f,",object.name,attr.val))
                    } else {
                        write_function(sprintf("%s: \"%s\",",object.name,gsub("\"","'",as.character(attr.val))))
                    }
                }
            }
        }
    }
}

write_label_style_obj <- function(label.list,nice.format=TRUE,initial.indent=14){
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
    fields.all <- names(label.list)[which(sapply(1:length(label.list), function(x) if(length(label.list[[x]])>1) return(TRUE) else return(FALSE)))]
    fields <- setdiff(fields.all,"text")
    for(text.property in c(
            "font",
            "offsetX",
            "offsetY",
            "rotation",
            "textAlign",
            "textBaseline"
        )
    ){
        if(!(is.null(label.list[[text.property]])) && (text.property %in% fields)){
            write_function(sprintf("%s: feature.get('label_%s'),",text.property,text.property))
        } else if(!(is.null(label.list[[text.property]]))){
            if(is.numeric(label.list[[text.property]])){
                write_function(sprintf("%s: %1.2f,",text.property,label.list[[text.property]]))
            } else {
                write_function(sprintf("%s: \"%s\",",text.property,gsub("\"","'",as.character(label.list[[text.property]]))))
            }
        }
    }
    if(!is.null(label.list[["stroke_color"]]) && (length(label.list[["stroke_color"]]) > 1)){
        write_function("stroke: new ol.style.Stroke({")
        inid <- inid + 2
        write_function("color: feature.get('label_stroke_color')")
        inid <- inid - 2
        write_function("}),")
    } else if(!is.null(label.list[["stroke_color"]])){
        write_function("stroke: new ol.style.Stroke({")
        inid <- inid + 2
        write_function(sprintf("color: \"%s\"",label.list[["stroke_color"]]))
        inid <- inid - 2
        write_function("}),")
    }
    if(!is.null(label.list[["fill_color"]]) && (length(label.list[["fill_color"]]) > 1)){
        write_function("fill: new ol.style.Fill({")
        inid <- inid + 2
        write_function("color: feature.get('label_fill_color')")
        inid <- inid - 2
        write_function("}),")
    } else if(!is.null(label.list[["fill_color"]])){
        write_function("fill: new ol.style.Fill({")
        inid <- inid + 2
        write_function(sprintf("color: \"%s\"",label.list[["fill_color"]]))
        inid <- inid - 2
        write_function("}),")
    }
    if('text' %in% fields.all){
        write_function("text: feature.get('label_text')")
    } else if(!(is.null(label.list[['text']]))){
        write_function(sprintf("text: \"%s\"",gsub("\"","'",as.character(label.list[['text']]))))
    } else {
        write_function("text: \"\"")
    }
}