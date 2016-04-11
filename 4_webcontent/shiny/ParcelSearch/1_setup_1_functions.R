# CAC BASELINE CONDITIONS REPORT

# SETUP: LOAD PACKAGES AND PROJECT SETTINGS --------------------------------------------------------------

require(scales)         # for ggplot2 label formatting (e.g., 'dollar', 'percent', ect.)
require(gplots)         # for converting colors to HEX strings
require(grDevices)      # for color palettes
require(rgdal)          # for readOGR and others
require(sp)             # for spatial objects
require(leaflet)        # for interactive maps (NOT leafletR here)
require(plyr)           # for rounding (the `round_any` function)
require(dplyr)          # for working with data frames
require(ggplot2)        # for plotting
require(tigris)
require(acs)
require(stringr)        # to pad fips codes
require(purrr)
require(magrittr)
require(downloader)
require(tmap)
require(rgeos)
require(operator.tools) # for the `notin` function
require(tidyr)          # for the `spread` function
require(acs)            # for loading US Census data
require(readr)
require(rapport)        # for creating a camel case string
require(VGAM)           # for creating estimate median values for neighborhoods
require(htmlwidgets)
require(classInt)       # for setting breaks in graphs (http://bit.ly/1QexSEP)
require(spdep)          # for identifying spatial neighbors
require(maptools)       # for combining SpatialPolygonsDataFrames
require(grid)
require(gridExtra)
require(useful)         # for "$150K labeling 
require(readxl)         # for reading Excel documents
require(stringr)        # for string extraction



options(scipen=999,stringsAsFactors = FALSE)

crs_proj <- CRS("+init=epsg:4326") # This project will use WGS 84 projected coordinate system
crs_geog <- CRS("+init=epsg:2285") # Washington State plane CRS

# SETUP: MY FUNCTIONS --------------------------------------------------------------------


myLfltShiny <- function(){
        leaflet() %>% 
                addTiles(
                        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                ) 
}


# myMini
# A quick function to add a (well-formatted) mini map to a leaflet map

myMini <- function(map){
        addMiniMap(map = map,
                   position = "topleft",
                   width = 150, height = 450,zoomLevelFixed = 10,toggleDisplay = TRUE,collapsedWidth = 25,collapsedHeight = 25)
}


mySptlPntsDF <- function(shp){
        
        if(is.null(shp)){
                return(message("The `shp` object is null"))
        }
        
        data <- shp@data %>% as.data.frame()
        
        shp_cnts <- gCentroid(shp, byid = TRUE)
        
        shp_rn <- row.names(shp_cnts)
        
        shp_len <- shp_cnts@coords %>% nrow()
        
        if(length(shp_rn) != shp_len){
                return(message("The `shp` object does not have the same number of row names as the list of polygons"))
        }
        
        else{
                nodata <- rep(NA, times = shp_len) %>% as.data.frame()
                
                rownames(nodata) <- shp_rn
                
                shp_cnts %<>% 
                        SpatialPointsDataFrame(data = nodata)
                
                shp_cnts@data <- data
                
                return(shp_cnts)
        }
        
        
}

