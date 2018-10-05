# ROpenLayers

A pacakge for Geo-Visualization in R

ROpenLayers leverages the power of [OpenLayers](https://openlayers.org/) JavaScript libraries and web-based map servers to enable informative visualization. 

## What this package does

The purpose of this package is to make it easy for a user to visualize geo-spatial data and analyses using the open source [OpenLayers](https://openlayers.org/) JavaScript library and online map servers. The process for creating a visualization imitates the process of creating a plot in R package [ggplot2](https://ggplot2.tidyverse.org/). 
1. First, an OpenLayers Map object is created with a call to the `ol_map` method. 
1. Next, layers and scales are created and added. Layers can reference map servers to provide underlying base maps or vector features (polygons, lines, or points) created or imported in R. These capabilities are described in the following sections. 
1. Finally, the updated map object is exported to HTML/JavaScript for viewing in a browser, hosting on a server, or embedding into another application or format. Export methods are `ol_map2HTML` and `ol_map2Strings`. 

## OpenLayers

[OpenLayers](https://openlayers.org) is an open source JavaScript library that makes it easy to put a dynamic map on any web page. It is licensed under the 2-clause BSD license (see [OpenLayers Licence](https://github.com/openlayers/openlayers/blob/master/LICENSE.md)). This license will appear commented within OpenLayers CSS code in the HTML exports created by this package. However, this package does not contain any of the OpenLayers JavaScript source code; rather, it exports HTML code that source these libraries when loaded. Therefore, these products will not render without network access to the OpenLayers JavaScript library. By default, the products exported by this package source OpenLayers 3.16.0, but the user has the option to set the JavaScript source URL (see `ol_map` documentation). 

## Public ArcGIS Servers

[ESRI ArcGIS](https://www.arcgis.com) hosts several publicly available map servers at [arcgisonline.com](https://server.arcgisonline.com/arcgis/rest/services), which can accessed via REST APIs and rendered using OpenLayers JavaScript methods. A subset of these are made available in this package through the `public_arcgis_basemap` method. Alternatively, a user can specify any ArcGIS map server using the `user_arcgis_basemap` method. Note that while these maps servers are publicly available, they are not necessarily open-licensed. Users must ensure they comply with each map server's license and terms of use. 

## OpenStreetMap

[OpenStreetMap](https://www.openstreetmap.org/) also hosts a public and open license map server that can be imported as a layer using OpenLayers. See `public_OSM_basemap` documentation. 

## Other Servers

As stated above, the `user_arcgis_basemap` method allows the user to manually specify any available ArcGIS map server. This package also provides access to US National Geospatial-Intelligence Agency servers hosted at [NGA.mil](https://www.nga.mil) through the `nga_basemap` method. Note that these servers require authentication, which will be requested at the time of access (i.e., when the HTML page is opened in a browser). 

## Vector Layers

This package enables users to rapidly access and write OpenLayers vector layers in JavaScript. The following methods enable that functionality.

* `ol_geom_polygon` 
* `ol_geom_line` 
* `ol_geom_point` 
* `ol_geom_icon` 
* `ol_geom_circle` 
* `ol_geom_heatmap` 
* `ol_geom_text` 

### Aesthetic Mappings

Most vector layer types support some aesthetic mappings.  For a list of which aesthetic mappings are supported by a layer, consult the documentation for that layer, e.g.,

```r
?ROpenLayers::ol_geom_polygon
```

Similar to the ggplot2 package, aesthetic mappings are made by including a `mapping` parameter to the layer call.  This parameter generally takes the form
```r
mapping=ol_aes(aesthetic1=variable1,aesthetic2=variable2,...)
```

This parameter will create a default mapping in the layer.

### Scales

Once a default aesthetic mapping has been created, it can be manually updated by adding a scale object to the `Ol.Map` object.  A comprehensive list of scales follows.  For more information, consult the documentation on each.

* `ol_scale_fill_continuous` 
* `ol_scale_fill_discrete` 
* `ol_scale_color_continuous` 
* `ol_scale_color_discrete` 
* `ol_scale_lwd_discrete` 
* `ol_scale_lty_discrete` (experimental) 
* `ol_scale_size_continuous` 
* `ol_scale_size_discrete` 
* `ol_scale_iconsize_continuous` 
* `ol_scale_iconsize_discrete` 
* `ol_scale_iconimage_discrete` 

The example below shows how to map a `fill` aesthetic to a `ol_geom_polygon` layer and then to set the name and display options by calling `ol_scale_fill_continuous`.


## Installation

Download the pre-compiled Windows binary and install from local.

```r 
install.packages("[local path to binary]/ROpenLayers_0.0.7-99.zip",repos=NULL)
```

## Example

```r
library(ROpenLayers)
data(quakes)
center <- c(
	mean(quakes$long),mean(quakes$lat)
)
quakes$long[which(quakes$long>180)] <- quakes$long[which(quakes$long>180)]-360
tooltips <- paste("Depth",quakes$depth,sep=": ")
mymap <- ol_map(
	zoom = 5,
	center = center,
	map.heading = "Earthquake Data Visualization"
)
basemap.layer <- public_arcgis_basemap(
	"OceanBase",
	toggle.control=FALSE
)
point.layer <- ol_geom_point(
	quakes[,c("long","lat")],
	mapping = ol_aes(fill=mag),
	df = quakes,
	name = "Earthquake Points",
	toggle.control=TRUE,
	tooltip = tooltips
)
heatmap.layer <- ol_geom_heatmap(
	quakes[,c("long","lat")],
	name = "Earthquake Heatmap",
	toggle.control=TRUE,
	weight.values = quakes$mag,
	opacity = 0.25
)
mymap <- mymap +
	basemap.layer +
	point.layer +
	ol_scale_fill_continuous(name="Magnitude",display=TRUE) +
	heatmap.layer 
## Save to file (requires write permission)
ol_map2HTML(mymap,"Quakes.html")
## Open in browser
browseURL("Quakes.html") 
```

### Integration with Shiny

While not developed for Shiny, ROpenLayers output can be hosted in a Shiny server.  Currently, this requires writing supporting image files to a 'www' directory.  Shiny automatically looks in this directory for source files, so the 'www' must be overwritten in the HTML output.  The example below provides one over several ways the same map from the above example could be integrated into a minimal Shiny application.  Note that the code will create a "shinyApp" folder in the working directory for the Shiny application.

```r
library(shiny)
dir.create('shinyApp',showWarnings=FALSE)
setwd('shinyApp')
#Write to App.R file
sink("App.R")
cat(
"	library(ROpenLayers)
	data(quakes)
	center <- c(
		mean(quakes$long),mean(quakes$lat)
	)
	quakes$long[which(quakes$long>180)] <- quakes$long[which(quakes$long>180)]-360
	tooltips <- paste('Depth',quakes$depth,sep=': ')
	mymap <- ol_map(
		zoom = 5,
		center = center,
		map.heading = 'Earthquake Data Visualization'
	)
	basemap.layer <- public_arcgis_basemap(
		'OceanBase',
		toggle.control=FALSE
	)
	point.layer <- ol_geom_point(
		quakes[,c('long','lat')],
		mapping = ol_aes(fill=mag),
		df = quakes,
		name = 'Earthquake Points',
		toggle.control=TRUE,
		tooltip = tooltips
	)
	heatmap.layer <- ol_geom_heatmap(
		quakes[,c('long','lat')],
		name = 'Earthquake Heatmap',
		toggle.control=TRUE,
		weight.values = quakes$mag,
		opacity = 0.25
	)
	mymap <- mymap +
		basemap.layer +
		point.layer +
		ol_scale_fill_continuous(name='Magnitude',display=TRUE) +
		heatmap.layer 
	## Save to file (requires write permission)
	HTML.strings <- ol_map2Strings(mymap,image.path='www')
	### Shiny integration ---
	### replace www with current working directory 
	HTML.strings[[3]] <- gsub('www','.',HTML.strings[[3]],fixed=TRUE)
	HTML.strings[[4]] <- gsub('www','.',HTML.strings[[4]],fixed=TRUE)
	ui <- shinyUI(
	    fluidPage(
	        ## Add OpenLayers Javascript source & CSS to head
	        tags$head(
	            HTML('<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"/>'),
	            HTML(HTML.strings[[1]]),
	            tags$style(HTML(HTML.strings[[2]]))
	        ),
	        titlePanel('Earthquakes'),
	        mainPanel(
	            tags$div(HTML(HTML.strings[[3]]))
	        ),
	        tags$script(HTML(HTML.strings[[4]]))
	    )
	)
	server <- function(input,output){
	}
	shinyApp(ui=ui,server)"
)
sink()
setwd("..")
shiny::runApp("./shinyApp")
```