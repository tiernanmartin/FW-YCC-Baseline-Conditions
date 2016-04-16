# CAC BASELINE CONDITIONS REPORT

# SETUP: LOAD PACKAGES AND PROJECT SETTINGS --------------------------------------------------------------
options(scipen=999,stringsAsFactors = FALSE)

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
# install.packages("rgeos", type = "mac.binary")
# library(rgeos)
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
require(rgeos)
require(grid)
require(gridExtra)
require(useful)         # for "$150K labeling 
require(readxl)         # for reading Excel documents
require(stringr)        # for string extraction


crs_proj <- CRS("+init=epsg:4326") # This project will use WGS 84 projected coordinate system
crs_geog <- CRS("+init=epsg:2285") # Washington State plane CRS

# SETUP: MY FUNCTIONS --------------------------------------------------------------------

# myLfltSmpl
# Creates a simple leaflet map with an appealing basemap and an argument for polygon data

myLfltSmpl <- function(data){
        leaflet() %>% 
                addTiles(
                        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                ) %>% 
                addPolygons(data = data)
}

# myLflt
# Creates a simple leaflet map with an appealing basemap (no other layers)

myLflt <- function(data){
        leaflet() %>% 
                addTiles(
                        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                )
}



# mySptlPntsDF
# Converts 'SpatialPoints' objects to 'SpatialPointsDataFrames',
# allowing them to be saved as shapefiles

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




# UV2Census
# A function that attributes Urban Village identities to Census geometries (e.g., block groups, tracts)

UV2Census_shiny <- function(tract = TRUE, unit = "hu"){
        
        # tract = TRUE
        # unit = "hu"
        geo <- if(tract == TRUE){tract_sea} else(bg_sea)
        blk <- blk_sea
        by_sp <- if(tract == TRUE){"TRACTCE"} else("GEOID")
        pop <- read_csv(file = "pop.csv",col_types = "cccn")
        hu <- read_csv(file = "hu.csv",col_types = "cccn")
        count <- if(unit == "hu"){hu} else(pop)
        
        
        # Read in the Housing Units data from ACS 2010 (block scale)
        
        # Join urban village df and housing units df to the blocks shp,
        # then filter out blocks with 0 housing units, as well as those in manufacturing/industrial
        # and outside villages
        
        blk <- 
                blk_sea %>% 
                geo_join(data_frame = seaAcsUvs,
                         by_sp = "GEOID10",
                         by_df = "GEOID10") %>% 
                geo_join(data_frame = count,
                         by_sp = "GEOID10",
                         by_df = "GEO.id2") %>% 
                .[!is.na(.@data$D001),] %>%  
                .[!is.na(.@data$URBAN_VILLAGE_NAME),]
        
        
        
        # Create different `ID` column depending on whether this is a tract-level or block-group-level operation
        if(tract == TRUE){
                blk@data %<>%
                        mutate(ID = substr(GEOID10,6,11)) %>% 
                        select(ID,D001,UV = URBAN_VILLAGE_NAME)
        } else {
                blk@data %<>%
                        mutate(ID = substr(GEOID10,1,12)) %>% 
                        select(ID,D001,UV = URBAN_VILLAGE_NAME)
        }
        
        geos <-  sort(unique(blk@data$ID)) %>% 
                as.data.frame()
        
        
        geos <- blk@data$ID %>%
                sort() %>% 
                unique() %>% 
                as.data.frame()
        
        colnames(geos) <- "ID"
        
        tractUV <-
                blk@data %>% 
                group_by(ID,UV) %>%  
                summarise(HU_COUNT = sum(D001)) %>% 
                spread(key = UV, value = HU_COUNT) %>% 
                ungroup() %>% 
                select(-ID)  %>% 
                replace(is.na(.),0) %>% 
                mutate(UV = max.col(m= .,ties.method = "first")) %>% 
                mutate(UV = colnames(.)[as.numeric(UV)]) %>% 
                mutate(UV = ifelse(rowSums(x = .[,colnames(.) %!in% "UV"]) == 0,
                                   "Not an Urban Village",
                                   UV)) %>%
                mutate(SUM = rowSums(.[,sapply(.,is.numeric)])) %>% 
                mutate(ID = as.character(row_number()))
        
        df1 <-
                tractUV %>% 
                filter(UV %in% "Outside Villages") %>% 
                select(-starts_with("Outside")) %>% 
                select(-starts_with("SUM")) %>% 
                mutate(SUM2 = rowSums(.[,sapply(.,is.numeric)])) %>% 
                filter(SUM2 > 0) %>% 
                mutate(SUM2 = as.character(SUM2)) %>% 
                mutate(UV2 = max.col(m= .[,sapply(.,is.numeric)],ties.method = "first")) %>% 
                mutate(UV2 = colnames(.)[UV2]) %>%  
                group_by(UV2) %>% 
                tidyr::nest() %>% 
                mutate(HU = purrr::map(.x = data, .f = function(x){
                        
                        # x <- df4$data[[1]] %>% as.data.frame()
                        
                        x <- as.data.frame(x)
                        
                        high <- 
                                colSums(x[,sapply(x,is.numeric)]) %>% which.max() %>% names()
                        
                        ID <- "ID"
                        SUM2 <- "SUM2"
                        
                        row <- which.max(x[,high])
                        
                        new <- 
                                x[row,c(high,ID,SUM2)] %>% 
                                as.data.frame() %>% 
                                mutate(UV = colnames(.)[1]) %>% 
                                .[,2:4]
                        
                        return(new)
                        
                })) %>% 
                .["HU"] %>%
                unnest() %>% 
                rename(UV2 = UV) %>% 
                filter(UV2 %!in% tractUV$UV) %>% 
                mutate(ID = as.numeric(ID),
                       SUM2 = as.numeric(SUM2))
        
        
        df2 <- 
                tractUV %>% 
                select(UV,SUM) %>% 
                bind_cols(geos) %>% 
                select(GEOID = ID,UV,SUM) %>% 
                mutate(ID = row_number()) %>% 
                left_join(df1, by = "ID") %>% 
                mutate(UV3 = ifelse(is.na(UV2),
                                    UV,
                                    UV2)) %>% 
                mutate(SUM3 = ifelse(is.na(SUM2),
                                     SUM,
                                     SUM2)) %>% 
                select(-ID) %>% select(GEOID,UV,SUM,UV2,SUM2,UV3,SUM3)
        
        
        shp <- 
                geo_join(spatial_data = geo,
                         data_frame = df2,
                         by_sp = by_sp,
                         by_df = "GEOID")
        
        # Remove the "Outside Villages"
        shp %<>%
                .[shp@data$UV3 %!in% c("Outside Villages",
                                       "Not an Urban Village") & !is.na(shp@data$UV3),]
        
        return(shp)
        
}

# UV2Census
# A function that attributes Urban Village identities to Census geometries (e.g., block groups, tracts)

UV2Census <- function(tract = TRUE, unit = "hu"){
        
        # tract = TRUE
        # unit = "hu"
        geo <- if(tract == TRUE){tract_sea} else(bg_sea)
        blk <- blk_sea
        by_sp <- if(tract == TRUE){"TRACTCE"} else("GEOID")
        pop <- read_csv(file = "./2_inputs/wa_kc_blocks_pop/DEC_10_SF1_P1_with_ann.csv",col_types = "cccn")
        hu <- read_csv(file = "./2_inputs/wa_kc_blocks_hu/DEC_10_SF1_H1.csv",col_types = "cccn")
        count <- if(unit == "hu"){hu} else(pop)
        
        
        # Read in the Housing Units data from ACS 2010 (block scale)
        
        # Join urban village df and housing units df to the blocks shp,
        # then filter out blocks with 0 housing units, as well as those in manufacturing/industrial
        # and outside villages
        
        blk <- 
                blk_sea %>% 
                geo_join(data_frame = seaAcsUvs,
                         by_sp = "GEOID10",
                         by_df = "GEOID10") %>% 
                geo_join(data_frame = count,
                         by_sp = "GEOID10",
                         by_df = "GEO.id2") %>% 
                .[!is.na(.@data$D001),] %>%  
                .[!is.na(.@data$URBAN_VILLAGE_NAME),]
        
        
        
        # Create different `ID` column depending on whether this is a tract-level or block-group-level operation
        if(tract == TRUE){
                blk@data %<>%
                        mutate(ID = substr(GEOID10,6,11)) %>% 
                        select(ID,D001,UV = URBAN_VILLAGE_NAME)
        } else {
                blk@data %<>%
                        mutate(ID = substr(GEOID10,1,12)) %>% 
                        select(ID,D001,UV = URBAN_VILLAGE_NAME)
        }
        
        geos <-  sort(unique(blk@data$ID)) %>% 
                as.data.frame()
        
        
        geos <- blk@data$ID %>%
                sort() %>% 
                unique() %>% 
                as.data.frame()
        
        colnames(geos) <- "ID"
        
        tractUV <-
                blk@data %>% 
                group_by(ID,UV) %>%  
                summarise(HU_COUNT = sum(D001)) %>% 
                spread(key = UV, value = HU_COUNT) %>% 
                ungroup() %>% 
                select(-ID)  %>% 
                replace(is.na(.),0) %>% 
                mutate(UV = max.col(m= .,ties.method = "first")) %>% 
                mutate(UV = colnames(.)[as.numeric(UV)]) %>% 
                mutate(UV = ifelse(rowSums(x = .[,colnames(.) %!in% "UV"]) == 0,
                                   "Not an Urban Village",
                                   UV)) %>%
                mutate(SUM = rowSums(.[,sapply(.,is.numeric)])) %>% 
                mutate(ID = as.character(row_number()))
        
        df1 <-
                tractUV %>% 
                filter(UV %in% "Outside Villages") %>% 
                select(-starts_with("Outside")) %>% 
                select(-starts_with("SUM")) %>% 
                mutate(SUM2 = rowSums(.[,sapply(.,is.numeric)])) %>% 
                filter(SUM2 > 0) %>% 
                mutate(SUM2 = as.character(SUM2)) %>% 
                mutate(UV2 = max.col(m= .[,sapply(.,is.numeric)],ties.method = "first")) %>% 
                mutate(UV2 = colnames(.)[UV2]) %>%  
                group_by(UV2) %>% 
                tidyr::nest() %>% 
                mutate(HU = purrr::map(.x = data, .f = function(x){
                        
                        # x <- df4$data[[1]] %>% as.data.frame()
                        
                        x <- as.data.frame(x)
                        
                        high <- 
                                colSums(x[,sapply(x,is.numeric)]) %>% which.max() %>% names()
                        
                        ID <- "ID"
                        SUM2 <- "SUM2"
                        
                        row <- which.max(x[,high])
                        
                        new <- 
                                x[row,c(high,ID,SUM2)] %>% 
                                as.data.frame() %>% 
                                mutate(UV = colnames(.)[1]) %>% 
                                .[,2:4]
                        
                        return(new)
                        
                })) %>% 
                .["HU"] %>%
                unnest() %>% 
                rename(UV2 = UV) %>% 
                filter(UV2 %!in% tractUV$UV) %>% 
                mutate(ID = as.numeric(ID),
                       SUM2 = as.numeric(SUM2))
        
        
        df2 <- 
                tractUV %>% 
                select(UV,SUM) %>% 
                bind_cols(geos) %>% 
                select(GEOID = ID,UV,SUM) %>% 
                mutate(ID = row_number()) %>% 
                left_join(df1, by = "ID") %>% 
                mutate(UV3 = ifelse(is.na(UV2),
                                    UV,
                                    UV2)) %>% 
                mutate(SUM3 = ifelse(is.na(SUM2),
                                     SUM,
                                     SUM2)) %>% 
                select(-ID) %>% select(GEOID,UV,SUM,UV2,SUM2,UV3,SUM3)
        
        
        shp <- 
                geo_join(spatial_data = geo,
                         data_frame = df2,
                         by_sp = by_sp,
                         by_df = "GEOID")
        
        # Remove the "Outside Villages"
        shp %<>%
                .[shp@data$UV3 %!in% c("Outside Villages",
                                       "Not an Urban Village") & !is.na(shp@data$UV3),]
        
        return(shp)
        
}

# myLfltShiny
# The presets for Leaflet maps created for the Shiny website

myLfltShiny <- function(){
        leaflet() %>% 
                addTiles(
                        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                ) %>% 
                myMini()
}


# myMini
# A quick function to add a (well-formatted) mini map to a leaflet map

myMini <- function(map){
        addMiniMap(map = map,
                   position = "topleft",
                   width = 150, height = 450,zoomLevelFixed = 10,toggleDisplay = TRUE,collapsedWidth = 25,collapsedHeight = 25)
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

mySptlLinesDF <- function(shp){
        
        shp_rn <- row.names(shp)
        
        shp_len <- shp@lines %>% length()
        
        if(length(shp_rn) != shp_len){
                return(message("The `shp` object does not have the same number of row names as the list of polygons"))
        }
        
        else{
                nodata <- rep(NA, times = shp_len) %>% as.data.frame()
                
                rownames(nodata) <- shp_rn
                
                shp %<>% 
                        SpatialLinesDataFrame(data = nodata)
                
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
                shapes <-  gUnaryUnion(spgeom = shapes)
                
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
