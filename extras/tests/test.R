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
     polygon.df <- data.frame(shape=c("rectangle","triangle"),no=c(NA,runif(1)))
     miami.OSM.basemap <- ol_map(
         center=c(-80.385790,25.782618),
         zoom=9,
         map.heading="Miami Shapes",
         map.note="Note: Mouseover popup values are 
             independent of shape size &amp; color."
         ) + 
        public_OSM_basemap() 
     polygon.layer <- ol_geom_polygon(
         polygon.list,
         mapping=ol_aes(
             fill=no, #numeric mapping
             color=no
         ),
         df=polygon.df,
         lwd=8,
         name="Miami Polygons",
         toggle.control=TRUE,
         tooltip=polygon.df$no
     )
     polygon.fill.scale <- ol_scale_fill_continuous(
         low.val=0,
         high.val=1,
         low.col='red',
         high.col='green',
         opacity=0.5,
         preserve.opacity=FALSE,
         display=TRUE,
         na.col.val="#0000FF55"
         )
     polygon.color.scale <- ol_scale_color_continuous(
         low.val=0,
         high.val=1,
         low.col='red',
         high.col='green',
         opacity=1,
         preserve.opacity=FALSE,
         display=TRUE,
         na.col.val='#FF000010',
         rotate.clockwise=FALSE
         )
     polygons.over.miami <- miami.OSM.basemap + 
         polygon.layer + 
         polygon.fill.scale +
         polygon.color.scale
     
     ## Not Run: output to file and view
     ol_map2HTML(polygons.over.miami,'miami_polygons.html')
     browseURL("miami_polygons.html")


setwd("/home/cemarks/Projects/SIPR")
install.packages("ROpenLayers_0.0.11-99.tar.gz",repos=NULL)
library(ROpenLayers)

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
     polygon.df <- data.frame(shape=c("rectangle",NA),no=runif(2))
     miami.OSM.basemap <- ol_map(
         center=c(-80.385790,25.782618),
         zoom=9,
         ol.source.url=NULL,
         map.heading="Miami Shapes",
         map.note="Note: Mouseover popup values are 
             independent of shape size &amp; color.",
         height='600px',
         width=1024) + 
        user_arcgis_basemap("http://server.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer") 
     polygon.layer <- ol_geom_polygon(
         polygon.list,
         mapping=ol_aes(
             fill=shape, #discrete mapping
             color=shape
         ),
         df=polygon.df,
         lwd=4,
         name="Miami Polygons",
         toggle.control=TRUE
     )
     polygon.fill.scale <- ol_scale_fill_discrete(
         color.vector=c(
             rectangle = 'red',
             triangle = 'blue'
         ),
         name = "Shape",
         opacity = 0.5,
         preserve.opacity = FALSE,
         display = TRUE,
         na.col.val="#FFFFFFFF"
     )
     polygon.color.scale <- ol_scale_color_discrete(
         color.vector=c(
             rectangle = 'red',
             triangle = 'blue'
         ),
         name = "Shape",
         opacity = 0.15,
         preserve.opacity = FALSE,
         display = TRUE,
         na.col.val="#0000FFA0"
     )
     polygons.over.miami <- miami.OSM.basemap + 
         polygon.layer + 
         polygon.fill.scale +
         polygon.color.scale
     
     ## Not Run: output to file and view
     ol_map2HTML(polygons.over.miami,'miami_polygons.html')
     browseURL("miami_polygons.html")


     heatmap.pts <- matrix(
         c(
             rnorm(100,-80.385,1), #Miami Longitudes
             rnorm(100,-117.1611,3), #San Diego Longitudes
             rnorm(100,25.782618,1), #Miami Latitudes
             rnorm(100,32.7157,3) # San Diego Latitudes
         ),ncol=2
     )
     mymap <- ol_map(
         center=c(-98.5,28.5),
         zoom=4,
         ol.source.url=NULL,
         map.note="Heatmap of random points centered on Miami and San Diego.") + 
         user_arcgis_basemap("http://server.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer") +
         ol_geom_heatmap(
             heatmap.pts,
             name="Random Heatmap",
             toggle.control=TRUE,
             opacity=0.25
             )
     ## The following line will create image files
     ## as needed for point layers and legends.
     ## None are required in this example.
     HTML.strings <- ol_map2Strings(mymap)
     # Minimal shiny example
     # Not Run
     library(shiny)
     ui <- shinyUI(
         fluidPage(
             ## Add OpenLayers Javascript source & CSS to head
             tags$head(
                 HTML(HTML.strings[[1]]),
                 HTML(HTML.strings[[2]]),
                 tags$style(HTML(HTML.strings[[3]]))
             ),
             titlePanel("Random Heatmap"),
             mainPanel(
                 tags$div(HTML(HTML.strings[[4]]))
             ),
             tags$script(HTML(HTML.strings[[5]]))
         )
     )
     server <- function(input,output){
     }
     shinyApp(ui=ui,server)
