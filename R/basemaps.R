#' NGA Basemap Layer
#'
#' Create a basemap layer linking to an NGA ArcGIS mapserver.
#'
#' Creates and returns an OpenLayers ArcGIS Tile layer that sources a
#' map server hosted at \url{http://home.gvs.nga.smil.mil}.  These map servers
#' are owned by the US Government and require authentication.  If the
#' \code{basemap.identifier} parameter is unrecognized the function will
#' default to the NGA OpenStreetMap map server.
#'
#' @section Available Base Maps:
#' The following basemap.identifiers are currently supported by this method.
#' \tabular{ll}{
#' "ABM" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/CanvasMaps/Analytic_Basemap/MapServer}{Analytic Base Map}\cr
#' "LightGray" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/CanvasMaps/LightGray/MapServer}{Analytic Base Map (Light Gray)}\cr
#' "Light_LightGray" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/CanvasMaps/Lite_LightGray/MapServer}{Analytic Base Map (Light Light Gray)}\cr
#' "LightMidnight" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/CanvasMaps/Lite_Midnight/MapServer}{Analytic Base Map (Light Midnight)}\cr
#' "Light_Slate" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/CanvasMaps/Lite_Slate/MapServer}{Analytic Base Map (Light Slate)}\cr
#' "Midnight" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/CanvasMaps/Midnight/MapServer}{Analytic Base Map (Midnight)}\cr
#' "Slate" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/CanvasMaps/Slate/MapServer}{Analytic Base Map (Slate)}\cr
#' "CARDG" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/Basemap/ScannedMaps/MapServer}{Scanned CARDG Maps}\cr
#' "DNC" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/Basemap/DNC/MapServer}{Digital Nautical Charts}\cr
#' "Imagery" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/Basemap/NGA_World_Imagery_2D/MapServer}{Satellite Imagery}\cr
#' "Hillshade" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/Basemap/NGA_Hillshade_2D/MapServer}{Hillshade Map}\cr
#' "ShadedRelief" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/Basemap/NGA_ShadedRelief_2D/MapServer}{Shaded Relief Map}\cr
#' "TintedHillshade" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/Basemap/NGA_Tinted_Hillshade/MapServer}{Tinted Hillshade Map}\cr
#' "WorldBoundaries" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/Basemap/World_Boundaries_2D/MapServer}{World Boundaries (WSM)}\cr
#' "WorldBoundaries_Places" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/Basemap/World_Boundaries_Places_2D/MapServer}{World Boundaries, Places (WSM)}\cr
#' "WorldPlaceNames" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/Basemap/World_Place_Names_2D/MapServer}{World Place Names (WSM)}\cr
#' "WorldTransportation" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/Basemap/World_Transportation_2D/MapServer}{World Transportation (WSM)}\cr
#' "WSM" \tab \href{http://origin-maps.gvs.nga.smil.mil/arcgis/rest/services/Basemap/World_StreetMap_2D/MapServer}{World Street Map}\cr
#' }
#'
#' @param basemap.identifier character indicating which NGA mapserver to use.
#' See 'Available Base Maps'.
#' @param name character layer name.
#' @param toggle.control logical.  If \code{TRUE}, a checkbox will appear on the
#' map allowing the viewer to toggle its visibility in the browser.
#'
#' @return A \code{Layer.ArcGIS} S3 object.
#'
#' @seealso
#' \code{\link{ol_map}},
#' \code{\link{+.Ol.Map}},
#' \code{\link{nga_basemap}},
#' \code{\link{user_arcgis_basemap}}
#'
#' @export
#'
#' @examples
#' mymap <- ol_map()
#' base.layer <- nga_basemap('Midnight')
#' mymap <- mymap + base.layer
#' \dontrun{
#' ol_map2HTML(mymap,"SanDiegoMidnight.html")
#' browseURL("SanDiegoMidnight.html")
#' }
nga_basemap<-function(basemap.identifier="WSM",name=NULL,toggle.control=FALSE){
    if(basemap.identifier %in% nga.mapserver.df$identifier){
        w <- which(nga.mapserver.df$identifier==basemap.identifier)
        if(missing(name) || is.null(name) || name==""){
            use.name <- nga.mapserver.df$name[w]
        } else {
            use.name <- name
        }
        o <- list(
            name=use.name,
            attributions = attribution_str(nga.mapserver.df,w),
            url = nga.mapserver.df$url[w],
            toggle.control=toggle.control
        )
        class(o) <- "Layer.ArcGIS"
        return(o)
    } else {
        w <- which(nga.mapserver.df$identifier=="WSM")
        if(missing(name) || is.null(name) || name==""){
            use.name <- nga.mapserver.df$name[w]
        } else {
            use.name <- name
        }
        o <- list(
            name=use.name,
            attributions = attribution_str(nga.mapserver.df,w),
            url = nga.mapserver.df$url[w],
            toggle.control=toggle.control
        )
        class(o) <- "Layer.ArcGIS"
        return(o)
    }
}


#' User ArcGIS Basemap Layer
#'
#' Create a basemap layer linking to an User-supplied ArcGIS mapserver.
#'
#' Creates and returns an OpenLayers ArcGIS Tile layer that sources a
#' map server at a user-supplied URL.
#'
#' @param url character url string where the map server is located.  Typically these
#' urls end with "/MapServer".
#' @param name character layer name.
#' @param attributions character HTML.  This HTML will render as attributional text at the
#' bottom-right corner of the map.  At a minimum, this text should include the
#' copyright text provided on the map server.
#' @param toggle.control logical.  If \code{TRUE}, a checkbox will appear on the
#' map allowing the viewer to toggle its visibility in the browser.
#'
#' @return A \code{Layer.ArcGIS} S3 object.
#'
#' @seealso
#' \code{\link{ol_map}},
#' \code{\link{+.Ol.Map}},
#' \code{\link{nga_basemap}},
#'
#' @export
#'
#' @examples
#' ## NOTE: Example for public URL accessible on NIPR.
#' server.url <- "http://server.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer"
#' mymap <- ol_map()
#' attrib <- paste(
#'     "Content may not reflect National Geographic's current map policy.",
#'     "Sources: National Geographic, Esri, Garmin, HERE,",
#'     "UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, increment P Corp",
#'     sep=" " # long attribution!
#' )
#' base.layer <- user_arcgis_basemap(
#'     server.url,
#'     attributions = attrib,
#'     toggle.control=TRUE
#' )
#' mymap <- mymap + base.layer
#' \dontrun{
#' ol_map2HTML(
#'   mymap,
#'   "SanDiego_NatGeo.html",
#'   map.note = sprintf(
#'     "I found this at <a href='%s'>arcgisonline.com</a>",
#'     server.url
#'   )
#' )
#' browseURL("SanDiego_NatGeo.html")
#' }
user_arcgis_basemap <- function(url,name="",attributions="",toggle.control=FALSE){
    o <- list(
        name=name,
        attributions=gsub('"',"'",attributions),
        url=url,
        toggle.control=toggle.control
    )
    class(o) <- "Layer.ArcGIS"
    return(o)
}
