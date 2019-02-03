#' @describeIn public_OSM_basemap Call \code{public_OSM_basemap} with default
#' settings.
#' @export
streetmap <- function(toggle.control = FALSE){
    return(public_OSM_basemap(toggle.control=toggle.control))
}


#' @describeIn public_arcgis_basemap Call "LightGray" \code{public_arcgis_basemap}.
#' @export
lightgray <-function(toggle.control = FALSE){
    return(
        public_arcgis_basemap(
            basemap.identifier="LightGray",
            toggle.control=toggle.control
        )
    )
}

#' @describeIn public_arcgis_basemap Call "OceanBase" \code{public_arcgis_basemap}.
#' @export
oceanbase <-function(toggle.control = FALSE){
    return(
        public_arcgis_basemap(
            basemap.identifier="OceanBase",
            toggle.control=toggle.control
        )
    )
}
