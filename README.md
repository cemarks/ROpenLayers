# ROpenLayers

A pacakge for Geo-Visualization in R

ROpenLayers leverages the power of [OpenLayers](https://openlayers.org/) JavaScript libraries and web-based Mapservers to enable informative visualization. 

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

## Installation

Install in R directly from GitHub:

`> library(devtools)`
`> install_github("cemarks/ROpenLayers")`

## Example

```r
data(quakes)
center <- c(
	mean(quakes$long),mean(quakes$lat)
)
quakes$long[which(quakes$long>180)] <- quakes$long[which(quakes$long>180)]-360
tooltips <- paste("Depth",quakes$depth,sep="")
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
browseURL("file:Quakes.html") 
```