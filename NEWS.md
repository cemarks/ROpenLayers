# ROpenLayers 1.0.0-99

* Updated Javascript to OL5.  There is still an option to link to a URL.
* Removed option for non-stand alone pages, i.e., all pages are self-contained.
* Moved the HTML page options from the `ol_map` function to the HTML production functions.
* Added `geocode` function using [https://www.arcgis.com](ArcGIS) online geocoding service.

# ROpenLayers 0.0.11-99

* Fixed continuous color scale so that rotate.clockwise has an effect and na.col.val is correctly interpreted (#10)
* Placed map height property in the .map CSS class.  The width property remains in the .map-container class (#11).
* Included HTML meta tag in ol_map2Strings output for IE compatibility (#12).
* Included OpenLayers 3.21.1 source code in the /inst/extdata folder.  The default behavior is now to include this script in the HTML outputs unless otherwise specified by user inputs (#14).
* Checked scale_color_discrete; opacity appears to be working fine for discrete and continuous color and fill scales (#15).

# ROpenLayers 0.0.10-99

* Added functionality for base-64 standalone web page exports.  This is now the default behavior.

# ROpenLayers 0.0.9-99

* Added geocoding capability using NGA services portal.

# ROpenLayers 0.0.8-99

* Fixed several bugs in label and tooltip CSS.

# ROpenLayers 0.0.7-99

* Initial SIPR branch and commit.
* Capability to control the direction of continuous color scales.
* Removed SSL from public OpenLayers JavaScript library request.
* Created "inst" folder for documentation, binaries.

# ROpenLayers 0.0.6

* Converted CSS colors to rgba format for compatibility.
* Enabled deployment image path in ol_map2Strings.


# ROpenLayers 0.0.5

* Included IE compatibility statement in ol_map2HTML by default and in shiny examples (#1).
* Fixed unneeded comma in public_OSM_basemap layer (#2).
* Fixed color formats for browser compatibility (#3).
* Removed linedash property when lty not set (#4).
* Moved property settings to style function when fixed to single value (#5).

# ROpenLayers 0.0.4

* Update Shiny example so that full code is written to App (make it stand alone)
* Update README.md to include ol_aes and scales.
* Update icon layer aesthetics to match scale names.
* Add PDF documentation to repository.
* Fix user_arcgis_basemap documentation
* Include package tar.gz

# ROpenLayers 0.0.3

* Updated README.md to include minimal, experimental Shiny example.
* Updated documentation `browseURL()` calls to include filenames only.

# RopenLayers 0.0.2

* Updated HTML tooltip class name to 'ol-tooltip' to prevent clashing when embedding in other pages.

# ROpenLayers 0.0.1

* Initial Commit
