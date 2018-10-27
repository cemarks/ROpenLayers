############### 

server.url <- "http://server.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer"
ol.source.url <- "https://cdn.rawgit.com/openlayers/openlayers.github.io/master/en/v3.16.0/build/ol.js"
mymap <- ol_map(
    map.note = sprintf(
         "I found this at <a href='%s'>arcgisonline.com</a>",
         server.url
    ),
    ol.source.url = ol.source.url

)
attrib <- paste(
    "Content may not reflect National Geographic's current map policy.",
    "Sources: National Geographic, Esri, Garmin, HERE,",
    "UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, increment P Corp",
    sep=" " # long attribution!
)
base.layer <- user_arcgis_basemap(
    server.url,
    attributions = attrib,
    toggle.control=TRUE
)
mymap <- mymap + base.layer
ol_map2HTML(mymap,"SanDiego_NatGeo.html")
browseURL("SanDiego_NatGeo.html")



##########################

data(quakes)
center <- c(mean(quakes$long),mean(quakes$lat))
quakes$long[which(quakes$long>180)]<-quakes$long[which(quakes$long>180)]-360
tooltips <- paste("Depth",quakes$depth,sep=": ")
mymap <- ol_map(
 zoom = 5,
 center = center,
 map.heading = "Earthquake Data Visualization",
 ol.source.url = ol.source.url
)
basemap.layer <- user_arcgis_basemap(
    server.url,
    attributions = attrib,
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

ol_map2HTML(mymap,"Quakes.html")
browseURL("Quakes.html")


##########################

data(quakes)
center <- c(mean(quakes$long),mean(quakes$lat))
quakes$long[which(quakes$long>180)]<-quakes$long[which(quakes$long>180)]-360
tooltips <- paste("Depth",quakes$depth,sep=": ")
mymap <- ol_map(
 zoom = 5,
 center = center,
 map.heading = "Earthquake Data Visualization",
 ol.source.url = ol.source.url
)
basemap.layer <- user_arcgis_basemap(
    server.url,
    attributions = attrib,
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

ol_map2HTML(mymap,"Quakes2.html",self.contained=FALSE)
browseURL("Quakes2.html")

####

point.matrix <- matrix(
 c(
     -80.885+runif(10),
     25.282618+runif(10)
 ),
 ncol=2
)
point.df <- data.frame(
 pt.type=sample(c("A","B"),10,replace=TRUE),
 pt.value=runif(10)*10
)
miami.map <- ol_map(
 center=c(-80.385790,25.782618),
 zoom=9,
 ol.source.url = ol.source.url
) +
 user_arcgis_basemap(
    server.url,
    attributions = attrib,
    toggle.control=FALSE
)

miami.points <- ol_geom_point(
 point.matrix,
 df=point.df,
 mapping=ol_aes(fill=pt.type,size=pt.value),
 name="Random Points of Interest",
 marker="pin",
 toggle.control=TRUE,
 tooltip=point.df$pt.type
)
size.scale <- ol_scale_size_continuous(
 display=TRUE,
 draw.fill='green'
)
fill.scale <- ol_scale_fill_discrete(
 c(B='red',A='green'),
 display=TRUE
)
miami.points.map <- miami.map +
 miami.points +
 size.scale +
 fill.scale

ol_map2HTML(miami.points.map,'Miami_points.html')
browseURL('Miami_points.html')

################3

polygon.matrix1 <- matrix(
 c(
     -80.385+c(0,0.05,0.05,0,0),
     25.782618+c(0,0,0.05,0.05,0)
 ),
 ncol=2
)
polygon.matrix2 <- matrix(
 c(
     -80.34+c(0,0.05,0.025,0),
     25.73++c(0,0,0.025*sqrt(3),0)
 ),
 ncol=2
)
polygon.list<-list(polygon.matrix1,polygon.matrix2)
polygon.df <- data.frame(shape=c("rectangle","triangle"),no=c(1,2))
miami.OSM.basemap <- ol_map(
 center=c(-80.385790,25.782618),
 zoom=9,
 map.heading="Miami Shapes",
 map.note="Note: Mouseover popup values are 
     independent of shape size &amp; color.",
 ol.source.url = ol.source.url
 ) + 
user_arcgis_basemap(
    server.url,
    attributions = attrib,
    toggle.control=FALSE
)

polygon.layer <- ol_geom_polygon(
 polygon.list,
 mapping=ol_aes(
     fill=shape,
 ),
 df=polygon.df,
 lwd=1,
 name="Miami Polygons",
 toggle.control=TRUE,
 tooltip=polygon.df$no
)
polygon.fill.scale <- ol_scale_fill_discrete(display=TRUE)
polygons.over.miami <- miami.OSM.basemap + 
 polygon.layer + 
 polygon.fill.scale

# Not Run: output to file and view
ol_map2HTML(polygons.over.miami,'miami_polygons.html')
browseURL("miami_polygons.html")


################3

polygon.matrix1 <- matrix(
 c(
     -80.385+c(0,0.05,0.05,0,0),
     25.782618+c(0,0,0.05,0.05,0)
 ),
 ncol=2
)
polygon.matrix2 <- matrix(
 c(
     -80.34+c(0,0.05,0.025,0),
     25.73++c(0,0,0.025*sqrt(3),0)
 ),
 ncol=2
)
polygon.list<-list(polygon.matrix1,polygon.matrix2)
polygon.df <- data.frame(shape=c("rectangle","triangle"),no=c(1,2))
miami.OSM.basemap <- ol_map(
 center=c(-80.385790,25.782618),
 zoom=9,
 map.heading="Miami Shapes",
 map.note="Note: Mouseover popup values are 
     independent of shape size &amp; color.",
 ol.source.url = ol.source.url
 ) + 
user_arcgis_basemap(
    server.url,
    attributions = attrib,
    toggle.control=FALSE
)

polygon.layer <- ol_geom_polygon(
 polygon.list,
 mapping=ol_aes(
     fill=shape,
 ),
 df=polygon.df,
 lwd=1,
 name="Miami Polygons",
 toggle.control=TRUE,
 tooltip=polygon.df$no
)
polygon.fill.scale <- ol_scale_fill_discrete(display=TRUE)
polygons.over.miami <- miami.OSM.basemap + 
 polygon.layer + 
 polygon.fill.scale

# Not Run: output to file and view
ol_map2HTML(polygons.over.miami,'miami_polygons.html',self.contained=FALSE)
browseURL("miami_polygons.html")


#################33

miami.circles <- matrix(
 c(
     -80.885+runif(10), #Longitudes
     25.282618+runif(10), #Latitudes
     rnorm(10,2000,500) # Radii in meters
 ),
 ncol=3
)
aesthetic.df <- data.frame(
 type=sample(c("A","B"),10,replace=TRUE),
 value=runif(10)*10
)
miami.OSM.basemap <- ol_map(
 center=c(-80.385790,25.782618),
 zoom=9,
 map.heading="Miami Shapes",
 map.note="Note: Mouseover popup values are 
     independent of shape size &amp; color.",
 ol.source.url = ol.source.url
 ) + 
user_arcgis_basemap(
    server.url,
    attributions = attrib,
    toggle.control=FALSE
)
circle.layer<-ol_geom_circle(
     miami.circles,
     df = aesthetic.df,
     mapping=ol_aes(fill=type),
     lwd=2,
     name="Meaningless Miami Circles",
     toggle.control=TRUE,
     color="#000000FF",
     tooltip=sprintf("%1.2f",aesthetic.df$value)
     ) 
circle.fill <- ol_scale_fill_discrete(
     display=TRUE,
     preserve.opacity=TRUE
 )
circles <- miami.OSM.basemap + circle.layer + circle.fill

ol_map2HTML(circles,'miami_circles.html')
browseURL("miami_circles.html")
     
###################3

text.pts <- matrix(
 c(
     -101.5, 39.2,
     -101.1, 54,
     -101.1, 21.4
 ),
 byrow=TRUE,
 ncol=2
)
text.labels <- c("USA","Canada","Mexico")
mymap <- ol_map(
 center=c(-100,25),
 zoom=3,
 ol.source.url = ol.source.url
) +
user_arcgis_basemap(
    server.url,
    attributions = attrib,
    toggle.control=FALSE
) +
 ol_geom_text(
     text.pts,
     text.labels,
     toggle.control=TRUE,
     label.params=list(
         font="16px sans-serif",
         stroke_color=c("#FF0000","#00FF00","#0000FF"),
         fill_color=c("#FF0000","#00FF00","#0000FF")
     )
 )
ol_map2HTML(mymap, "textmap.html")
browseURL("textmap.html")
