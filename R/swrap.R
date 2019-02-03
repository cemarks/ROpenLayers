#' @describeIn nga_basemap Call "WSM" \code{nga_basemap} 
#' @export
streetmap <- function(toggle.control = FALSE){
    return(
        nga_basemap(
            basemap.identifier="WSM",
            toggle.control=toggle.control
        )
    )
}


#' @describeIn nga_basemap Call "LightGray" \code{nga_basemap}.
#' @export
lightgray <-function(toggle.control = FALSE){
    return(
        nga_basemap(
            basemap.identifier="LightGray",
            toggle.control=toggle.control
        )
    )
}

#' @describeIn nga_basemap Call "LightMidnight" \code{nga_basemap}.
#' @export
oceanbase <-function(toggle.control = FALSE){
    return(
        nga_basemap(
            basemap.identifier="LightMidnight",
            toggle.control=toggle.control
        )
    )
}
