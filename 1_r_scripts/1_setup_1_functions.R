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
require(maptools)       # for combine SpatialPolygonsDataFrames
require(grid)
require(gridExtra)
require(useful)         # for "$150K labeling 
require(readxl)         # for reading Excel documents
require(stringr)        # for string extraction



options(scipen=999,stringsAsFactors = FALSE)

crs_proj <- CRS("+init=epsg:4326") # This project will use WGS 84 projected coordinate system
crs_geog <- CRS("+init=epsg:2285") # Washington State plane CRS

# SETUP: MY FUNCTIONS --------------------------------------------------------------------

# myMini
# A quick function to add a (well-formatted) mini map to a leaflet map

myMini <- function(map){
        addMiniMap(map = map,
                   position = "topleft",
                   width = 150, height = 450,zoomLevelFixed = 10)
}
        
# norm0to1
# A function to convert numeric values to a 0 to 1 scale (rounds up to the nearest 100th)

norm0to1 <- function(x) {plyr::round_any(x = (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - 
                                                                               min(x, na.rm=TRUE)),accuracy = .01, f = ceiling)} 

# wtr_clip
# A function for clipping census geometries (or any other areal data) with waterbody polygons
# Note: the waterbody shapefile should be flattened (`gUnaryUnion`) before being passed to this function

wtr_clip <- function(orig, wtr, path){
        
        new <- 
                gDifference(spgeom1 = orig, spgeom2 = wtr, 
                            byid = TRUE) %>% 
                mySptlPolyDF()
        
        new@data <- 
                gCentroid(new,byid = T) %>% 
                over(., orig) %>% 
                as.data.frame()
        
        new
        
}


# Quick conversion of objects from class=SpatialPolygons to class=SpatialPolygonsDataFrame
# (this is necessary for saving spatial data in the ESRI Shapefile format)

mySptlPolyDF <- function(shp){
        
        shp_rn <- row.names(shp)
        
        shp_len <- shp@polygons %>% length()
        
        if(length(shp_rn) != shp_len){
                return(message("The `shp` object does not have the same number of row names as the list of polygons"))
        }
        
        else{
                nodata <- rep(NA, times = shp_len) %>% as.data.frame()
                
                rownames(nodata) <- shp_rn
                
                shp %<>% 
                        SpatialPolygonsDataFrame(data = nodata)
                
                return(shp)
        }
        

}


# Yellow-Green-Blue color palette

myYlGnBu <- colorRampPalette(colors = RColorBrewer::brewer.pal(n = 9,
                                                               name = "YlGnBu"), 
                             space = "Lab",
                             bias = 1.5)

# getObjName: a function to convert an object name into a string

getObjName <- function(obj) {
        deparse(substitute(obj))
}

# myGeoJoin: a function to bind dataframes to a shapefile's attribute table.

myGeoJoin <- function(spatial_data,data_frame,by_sp = "GEOID", by_df = "GEOID"){
        
        require(tigris)
        require(dplyr)
        
        new <- geo_join(spatial_data = spatial_data,data_frame = data_frame,by_sp = by_sp, by_df = by_df)
        
        if(by_sp == by_df){
                str <- paste0(by_sp,"\\.{1}")
                
                new@data %<>% select(-matches(str))
        }
        
        return(new)
}

# myLeaflet: A quick leaflet map for viewing one set of polygons

myLeaflet <- function(data){
        leaflet() %>%
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = data,
                            color = col2hex("white"),weight = 2, opacity = .33,
                            fillColor = col2hex("blue"), fillOpacity = .33) %>% 
                addPolygons(data = myCACbound,
                            fillOpacity = 0,
                            weight = 3,
                            color = "#ff9900",
                            dashArray = "5, 5",
                            opacity = 1)
}

# bgSelect: A function to expedite the process of merging, buffering, and overlap selection of block groups

bgSelect <- function(shapes, multi = FALSE, buffer = -500, layer){
        
        if(multi == TRUE){
                shapes <-  rgeos::gUnaryUnion(spgeom = shapes)
                
                shp_buf <- spTransform(shapes, CRSobj = crs_geog) %>% 
                        gBuffer(spgeom = .,width = buffer) %>%
                        spTransform(., CRSobj = crs_proj)
                
                overlap <- gIntersects(spgeom1 = bg_sea,
                                       spgeom2 = shp_buf,
                                       byid = T) %>% 
                        which(.==TRUE) %>% 
                        
                
                bg_sea[overlap,] %>%
                        writeOGR(dsn = "./2_inputs/",
                                 layer = layer,
                                 driver = "ESRI Shapefile")
                
        }
        
        if(multi == FALSE){
                shp_buf <- spTransform(shapes, CRSobj = crs_geog) %>% 
                        gBuffer(spgeom = .,width = buffer) %>%
                        spTransform(., CRSobj = crs_proj)
                
                overlap <- gIntersects(spgeom1 = bg_sea,
                                       spgeom2 = shp_buf,
                                       byid = T) %>% 
                        which(.==TRUE)
                
                bg_sea[overlap,] %>%
                        writeOGR(dsn = "./2_inputs/",
                                 layer = layer,
                                 driver = "ESRI Shapefile")
        }
        
}

# bgSelectCntrd: A similar function to `bgSelect` except it returns block groups whose centroids are within
# the buffer polygon

bgSelectCntrd <- function(overShape,layer){
        
        cnts <- gCentroid(spgeom = bg_sea, byid = TRUE) %>% 
                spTransform(CRSobj = crs_proj)
        
        overShape %<>% spTransform(CRSobj = crs_proj)
        
        cnts[overShape,] %>% 
                bg_sea[.,] %>% 
                writeOGR(dsn = "./2_inputs/",
                         layer = layer,
                         driver = "ESRI Shapefile")
        

        
        
}
