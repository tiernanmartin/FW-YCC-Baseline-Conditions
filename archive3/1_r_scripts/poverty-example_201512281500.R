# CAC BASELINE CONDITIONS REPORT

# LOAD PACKAGES AND PROJECT SETTINGS --------------------------------------------------------------

require(gplots)         # for converting colors to HEX strings
require(rgdal)          # for readOGR and others
require(sp)             # for spatial objects
require(leaflet)        # for interactive maps (NOT leafletR here)
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

options(scipen=999,stringsAsFactors = FALSE)

crs_proj <- CRS("+init=epsg:4326") # This project will use WGS 84 projected coordinate system
crs_geog <- CRS("+init=epsg:2285") # Washington State plane CRS

# MY FUNCTIONS --------------------------------------------------------------------

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

# SPATIAL DATA: CENSUS BLOCK GROUPS (BG'S) --------------------------------------------------------

# Note: to expedite the processing time, the following `if()` scripts are run once and the outputs
# are saved and accessed directly in all subsequent uses

# All Census block groups overlapping the CAC neighborhoods (excluding slivers)

if(!file.exists("./2_inputs/blockgroups_seattle.shp")){
        tigris::block_groups(state = "WA",county = "King") %>%  # Download the census block groups for King County
                spTransform(., CRSobj = crs_proj) %>% 
                writeOGR(dsn = "./2_inputs/","blockgroups_seattle",driver = "ESRI Shapefile")
}

bg_sea <- readOGR(dsn = "./2_inputs/",
                  layer = "blockgroups_seattle", 
                  stringsAsFactors = FALSE) %>% 
        spTransform(.,CRSobj = crs_proj)

# SPATIAL DATA: BG'S IN CAC NEIGHBORHOODS ---------------------------------------------------------

# All Census block groups overlapping the CAC neighborhoods (excluding slivers)

if(!file.exists("./2_inputs/blockgroups_CAC_nhoods.shp")){
        
        url <- "https://data.seattle.gov/download/2mbt-aqqx/application/zip" # save the URL for the neighborhood boundaries
        
        temp <- tempfile() # create a temporary file to hold the compressed download
        
        download(url, dest = temp, mode="wb") # download the file
        
        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
        
        nhoods <- readOGR(dsn = "./2_inputs/Neighborhoods/WGS84/",  # select YCC and adjacent neigborhood boundaries
                          layer = "Neighborhoods") %>% 
                spTransform(.,CRSobj = crs_proj)
        
        # select the Centreal Area Crescent neighborhoods from the small list
        CAC_sm <- c("Atlantic", "First Hill", "International District", "Minor", "Pioneer Square", "Yesler Terrace") 
        
        nhoods_CAC <<- nhoods[nhoods$S_HOOD %in% CAC_sm,]
        
        bgSelect(shapes = nhoods_CAC,multi = TRUE,layer = "blockgroups_CAC_nhoods")
}

bg_nhoods <- readOGR(dsn = "./2_inputs/",   
                  layer = "blockgroups_CAC_nhoods",
                  stringsAsFactors = FALSE) %>% 
        spTransform(CRSobj = crs_proj)

# SPATIAL DATA: BG'S IN BAILEY-GATZERT BOUNDARY ---------------------------------------------------

# All Census block groups in the Bailey-Gatzert attendance boundary

if(!file.exists("./2_inputs/blockgroups_bgatz.shp")){
        
        url <- "https://www.seattleschools.org/UserFiles/Servers/Server_543/File/District/Departments/Enrollment%20Planning/Maps/gisdata/SPS_AttendanceAreasAndSchools_Shapefiles_2015_2016.zip" # save the URL for the neighborhood boundaries
        
        temp <- tempfile() # create a temporary file to hold the compressed download
        
        download(url, dest = temp, mode="wb") # download the file
        
        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
        
        bgatz <- readOGR(dsn = "./2_inputs/SPS_AttendanceAreasAndSchools_Shapefiles_2015_2016_v2/",
                         layer = "sps_attendance_area_ES_2015_2016") %>% 
                .[.@data$ES_ZONE == "Gatzert",] %>%
                spTransform(., CRSobj = crs_proj)
        
        bgatz_buf <- spTransform(bgatz, CRSobj = crs_geog) %>% 
                gBuffer(spgeom = .,width = -500) %>%
                spTransform(., CRSobj = crs_proj)
        
        overlap <- gIntersects(spgeom1 = bg_sea,
                               spgeom2 = bgatz_buf,
                               byid = T) %>% 
                which(.==TRUE)
        
        bg_sea[overlap,] %>%
                writeOGR(dsn = "./2_inputs/",
                         layer = "blockgroups_bgatz",
                         driver = "ESRI Shapefile")
}

bg_bgatz <- readOGR(dsn = "./2_inputs/",
                    layer = "blockgroups_bgatz",
                    stringsAsFactors = FALSE) %>% 
        spTransform(CRSobj = crs_proj)

# SPATIAL DATA: BG'S IN MY CAC BOUNDARY -----------------------------------------------------------

# All Census block groups within the boundary of the printed map (provided by Amy Gore, 12-14-2015)

myCACbound <- readOGR(dsn = "./2_inputs/myCACbound/",layer = "myCACbound") %>% 
        spTransform(CRSobj = crs_proj)

if(!file.exists("./2_inputs/blockgroups_myCACbound.shp")){
        bgSelect(shapes = myCACbound,layer = "blockgroups_myCACbound")
}

bg_myCAC <- readOGR(dsn = "./2_inputs/", layer = "blockgroups_myCACbound") %>% 
        spTransform(CRSobj = crs_proj)

if(!file.exists("./2_inputs/bg_cntrs_myCAC.shp")){
        bg_cntrs_myCAC <- gCentroid(spgeom = bg_myCAC,byid = TRUE) %>% # create centroids for the bg's
                spTransform(CRSobj = crs_proj) %>% 
                SpatialPointsDataFrame(data = as.data.frame(bg_myCAC@data)) %>% 
                writeOGR(dsn = "./2_inputs/",layer = "bg_cntrs_myCAC",driver = "ESRI Shapefile")
        
        
}

bg_cntrs_myCAC <- readOGR(dsn = "./2_inputs/",layer = "bg_cntrs_myCAC") %>% # create centroids for the bg's
        spTransform(CRSobj = crs_proj)

# SPATIAL DATA: BG'S WITH CENTROID IN MY CAC BOUNDARY ---------------------------------------------

# All Census blocks whose centroids are within the boundary of the printed map

if(!file.exists("./2_inputs/blockgroups_myCACbound_cntrs.shp")){
        bgSelectCntrd(overShape = myCACbound,layer = "blockgroups_myCACbound_cntrs")        
}

bg_myCAC_cntrs <- readOGR(dsn = "./2_inputs/",layer = "blockgroups_myCACbound_cntrs") %>% 
        spTransform(CRSobj = crs_proj)

# SPATIAL DATA: BG'S W/ MAJORITY OF POP. IN MY CAC BOUND ------------------------------------------

# All Census block groups where the majority of the population live within the boundary of the printed map
# (note: this uses the centroid selection method)

make_bg_pop_myCAC_rev <- function() {
        
        if(!file.exists("./2_inputs/blks_CAC.shp")){
                blks <- 
                        readOGR(dsn = "2_inputs/blocks10/", layer = "blocks10") %>% # read in census block shapefiles
                        spTransform(CRSobj = crs_proj)
                
                bg_geoids <- bg_myCAC@data$GEOID # block group GEOID's
                
                blks_CAC <-
                        blks[blks@data$GEO_ID_GRP %in% bg_geoids, ] %>%   # subset the Census block spatial polygons df
                        writeOGR(dsn = "./2_inputs/",layer = "blks_CAC",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                
        }
        
        blks_CAC <- readOGR(dsn = "./2_inputs/",layer = "blks_CAC") %>% 
                spTransform(CRSobj = crs_proj)
        
        blk_df <<-
                readr::read_csv(file = "./2_inputs/wa_kc_blocks_pop/DEC_10_SF1_P1_with_ann.csv",
                                # load population data
                                skip = 1,
                                col_types = "cccn")
        
        blks_CAC <-
                geo_join(
                        spatial_data = blks_CAC,
                        # join spatial and demographic data
                        data_frame = blk_df,
                        by_sp = "GEO_ID_BLK",
                        by_df = "Id2"
                ) %>%
                spTransform(CRSobj = crs_proj)
        
        overlap <- gCentroid(spgeom = blks_CAC, byid = TRUE) %>%
                .[myCACbound,] %>%
                blks_CAC[.,]
        
        overlap <- overlap@data$GEO_ID_BLK
        
        blks_CAC@data %<>% select(GEO_ID_GRP, GEO_ID_BLK, Geography, Total) %>% # drop irrelevant variables
                mutate(Location = as.factor(ifelse(
                        GEO_ID_BLK %in% overlap,
                        "inside",
                        "outside"
                )))
        
        blks_CAC <<- blks_CAC
        
        include <-
                blks_CAC@data %>%
                group_by(GEO_ID_GRP, Location) %>%
                summarise(Count = sum(Total)) %>%
                spread(Location, Count) %>%
                replace(is.na(.), 0) %>%
                filter(inside > outside) %>%
                select(GEO_ID_GRP) %>%
                unlist()
        
        bg_pop_myCAC <<- bg_sea[bg_sea@data$GEOID %in% include, ]
        
        bg_pop_notMyCAC <<- bg_sea[bg_sea@data$GEOID %!in% include, ]
        
        SODO <-
                "530330093002" # Using 'myLflt_bgTest' I found that this is the SODO bg GEOID
        
        bg_pop_myCAC_rev <-
                bg_pop_myCAC[bg_pop_myCAC@data$GEOID %!in% SODO, ]
        
        return(bg_pop_myCAC_rev)
}

if(!file.exists("./2_inputs/bg_pop_myCAC_rev.shp")){
        make_bg_pop_myCAC_rev() %>% 
                writeOGR(dsn = "./2_inputs/",layer = "bg_pop_myCAC_rev",driver = "ESRI Shapefile")
}

bg_pop_myCAC_rev <- readOGR(dsn = "./2_inputs/",layer = "bg_pop_myCAC_rev") %>% 
        spTransform(CRSobj = crs_proj)

# This function saves a Leaflet map of the census blocks whose centroids are within
# the CAC boundary.

myLflt_bkTest <- function(){
        make_bg_pop_myCAC_rev()
        
        leaflet() %>%
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = blks_CAC,
                                  fillColor = ~pal(Location),
                                  stroke = TRUE, color = "White", weight = .5, opacity = 1) %>% 
                addPolygons(data = myCACbound,
                            fillOpacity = 0,
                            weight = 3,
                            color = "#ff9900",
                            dashArray = "5, 3",
                            opacity = 1)
}

# This function saves a Leaflet map. This particular map shows which block groups are included (color),
# the pop. density (hue), and the boundary of the CAC boundary. It shows that the Stadium District/SODO
# block group should probably not be included but all other inclusions and exclusions are appropriate.

myLflt_bgTest <- function(){
        
        make_bg_pop_myCAC_rev()
        
        pal_r <- colorNumeric(palette = "Reds",
                              domain = 0:sd(blks_CAC$Total))
        
        pal_b <- colorNumeric(palette = "Blues",
                              domain = 0:sd(blks_CAC$Total))
        
        popup_text <- paste0("GEOID: ",bg_myCAC@data$GEOID)
        
        blks_CAC_in <- blks_CAC[blks_CAC@data$GEO_ID_GRP %in% bg_pop_myCAC@data$GEOID,]
        
        blks_CAC_out <- blks_CAC[blks_CAC@data$GEO_ID_GRP %!in% bg_pop_myCAC@data$GEOID,]
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = blks_CAC_in,
                            fillColor = ~pal_r(Total), fillOpacity = .75,
                            stroke = FALSE) %>% 
                addPolygons(data = blks_CAC_out,
                            fillColor = ~pal_b(Total), fillOpacity = .75,
                            stroke = FALSE) %>% 
                addPolygons(data = bg_myCAC,
                            fillOpacity = 0,
                            weight = 1, color = "black", opacity = 1,
                            popup = popup_text) %>% 
                addPolygons(data = myCACbound,
                            fillOpacity = 0,
                            weight = 3,
                            color = "#ff9900",
                            dashArray = "5, 5",
                            opacity = 1)
}

# This function saves a Leaflet map of the population density (block) within the CAC boundary
# census blocks. Note: it reflects my revised selection, which excludes the SODO block group.

myLflt_bgCAC <- function(){
        
        make_bg_pop_myCAC_rev()
        
        pal_r <- colorNumeric(palette = "Reds",
                              domain = 0:sd(blks_CAC$Total))
        
        pal_b <- colorNumeric(palette = "Blues",
                              domain = 0:sd(blks_CAC$Total))
        
        popup_text <- paste0("GEOID: ",bg_myCAC@data$GEOID)
        
        blks_CAC_in <- blks_CAC[blks_CAC@data$GEO_ID_GRP %in% bg_pop_myCAC_rev@data$GEOID,]
        
        blks_CAC_out <- blks_CAC[blks_CAC@data$GEO_ID_GRP %!in% bg_pop_myCAC_rev@data$GEOID,]
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = blks_CAC_in,
                            fillColor = ~pal_r(Total), fillOpacity = .75,
                            stroke = FALSE) %>% 
                addPolygons(data = blks_CAC_out,
                            fillColor = ~pal_b(Total), fillOpacity = .75,
                            stroke = FALSE) %>% 
                addPolygons(data = bg_myCAC,
                            fillOpacity = 0,
                            weight = 1, color = "black", opacity = 1,
                            popup = popup_text) %>% 
                addPolygons(data = myCACbound,
                            fillOpacity = 0,
                            weight = 3,
                            color = "#ff9900",
                            dashArray = "5, 5",
                            opacity = 1)
}

myLflt_bkTest()

myLflt_bgTest()

myLflt_bgCAC()

# SPATIAL DATA: TRACTS ----------------------------------------------------------------------------

tract_CAC <- {
        if(!file.exists("./2_inputs/tracts.shp")){
                tigris::tracts(state = "WA", county = "King") %>% 
                        spTransform(CRSobj = crs_proj) %>% 
                        writeOGR(dsn = "./2_inputs/",layer = "tracts", driver = "ESRI Shapefile")
        }
        
        if(!file.exists("./2_inputs/tract_CAC.shp")){
                tract_CAC <- readOGR(dsn = "./2_inputs/", layer = "tracts") %>%
                        .[tracts@data$GEOID %in% paste0(bg_pop_myCAC_rev@data$STATEFP,
                                                        bg_pop_myCAC_rev@data$COUNTYFP,
                                                        bg_pop_myCAC_rev@data$TRACTCE), ] %>% 
                        spTransform(CRSobj = crs_proj) %>% 
                        writeOGR(dsn = "./2_inputs/", layer = "tract_CAC", driver = "ESRI Shapefile")
                
                
        }
        
        readOGR(dsn = "./2_inputs/",layer = "tract_CAC") %>% 
                spTransform(CRSobj = crs_proj)
        
} # creates a selection of Census tracts using the bg's found in `bg_pop_myCAC-rev'

# This function allows me to easily find the GEOID of tracts that I'm considering excluding 
# (e.g. Tract 93, the majority of which is in SODO)

myLflt_tractTest <- function(){
        popup_text <- paste0(tract_CAC@data$NAMELSAD,"<br>","GEOID: ",tract_CAC@data$GEOID)
        
        myLeaflet(tract_CAC) %>% 
                addPolygons(data = tract_CAC,
                            stroke = F,
                            fillOpacity = 0,
                            popup = popup_text)
        
}

myLflt_tractTest()

# Select tracts where the majority of the population lives within the CAC bound and map the result

if(!file.exists("./2_inputs/tract_pop.shp")){
        make_tract_pop <<- function() {
                
                if(!file.exists("./2_inputs/tract_blks_CAC.shp")){
                        overlap <- gIntersects(spgeom1 = tracts,spgeom2 = myCACbound,byid = T)%>% 
                                which(.==TRUE)
                        
                        tract_CAC_over <- tracts[overlap,] %>% 
                                spTransform(CRSobj = crs_proj)
                        
                        blks <- 
                                readOGR(dsn = "2_inputs/blocks10/", layer = "blocks10") %>% # read in census block shapefiles
                                spTransform(CRSobj = crs_proj)
                        
                        tract_geoids <- tract_CAC_over@data$TRACTCE # tract numbers
                        
                        blks[blks@data$TRACT_STR %in% tract_geoids, ] %>% # subset the Census block spatial polygons df
                                geo_join(data_frame = blk_df,
                                         by_sp = "GEO_ID_BLK",
                                         by_df = "Id2") %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "tract_blks_CAC",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                        
                }
                
                tract_blks_CAC <- readOGR(dsn = "./2_inputs/",layer = "tract_blks_CAC") %>% 
                        spTransform(CRSobj = crs_proj)
                
                overlap <- gCentroid(spgeom = tract_blks_CAC, byid = TRUE) %>%
                        .[myCACbound,] %>%
                        tract_blks_CAC[.,]
                
                overlap <- overlap@data$GEO_ID_BLK
                
                tract_blks_CAC@data %<>% select(TRACT_STR, GEO_ID_BLK, Geography, Total) %>% # drop irrelevant variables
                        mutate(Location = as.factor(ifelse(
                                GEO_ID_BLK %in% overlap,
                                "inside",
                                "outside"
                        )))
                
                tract_blks_CAC <<- tract_blks_CAC
                
                include <-
                        tract_blks_CAC@data %>%
                        group_by(TRACT_STR, Location) %>%
                        summarise(Count = sum(Total)) %>%
                        spread(Location, Count) %>%
                        replace(is.na(.), 0) %>%
                        filter(inside > outside) %>%
                        select(TRACT_STR) %>%
                        unlist()
                
                tract_pop <- tracts[tracts@data$TRACTCE %in% include, ] %>% 
                        writeOGR(dsn = "./2_inputs/",layer = "tract_pop",driver = "ESRI Shapefile")
                
        }
        make_tract_pop()
}

tract_pop <- readOGR(dsn = "./2_inputs",layer = "tract_pop") %>% 
        spTransform(CRSobj = crs_proj)

myLflt_TractPopTest <- function(){
        
        legend_layers <- factor(c("CAC boundary","blocks","tracts"),
                                levels = c("CAC boundary","blocks","tracts"), ordered = T)
        
        pal_r <- colorNumeric(palette = "Reds",
                              domain = 0:sd(tract_blks_CAC$Total))
        
        pal_b <- colorNumeric(palette = "Blues",
                              domain = 0:sd(tract_blks_CAC$Total))
        
        popup_text <- paste0(tract_CAC_over@data$NAMELSAD,"<br>","GEOID: ",tract_CAC@data$GEOID)
        
        tract_blks_in <- tract_blks_CAC[tract_blks_CAC@data$TRACT_STR %in% tract_pop@data$TRACTCE,]
        
        tract_blks_out <- tract_blks_CAC[tract_blks_CAC@data$TRACT_STR %!in% tract_pop@data$TRACTCE,]
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = tract_blks_in,
                            fillColor = ~pal_r(Total), fillOpacity = .75,
                            stroke = FALSE,
                            group = legend_layers[2]) %>% 
                addPolygons(data = tract_blks_out,
                            fillColor = ~pal_b(Total), fillOpacity = .75,
                            stroke = FALSE,
                            group = legend_layers[2]) %>% 
                addPolygons(data = myCACbound,
                            fillOpacity = 0,
                            weight = 3,
                            color = "#ff9900",
                            dashArray = "5, 5",
                            opacity = 1,
                            group = legend_layers[1]) %>% 
                addPolygons(data = tract_CAC_over,
                            fillOpacity = 0,
                            weight = 1, color = "black", opacity = 1,
                            popup = popup_text,
                            group = legend_layers[3]) %>% 
                addLayersControl(overlayGroups = c("CAC boundary","blocks","tracts"),
                                 options = layersControlOptions(collapsed = FALSE))
        
}

myLflt_TractPopTest()

# Revise the tract selection to include Tract 79 (Capitol Hill)

if(!file.exists("./2_inputs/tract_pop_rev.shp")){
        
        if(!exists("tract_pop")){
                tract_pop <- readOGR(dsn = "./2_inputs",layer = "tract_pop") %>% 
                        spTransform(CRSobj = crs_proj)
        }
        
        CapHill <- "007900" # save the Tract ID for Tract 79
        
        new_include <- c(tract_pop@data$TRACTCE,CapHill) 
        
        tracts[tracts@data$TRACTCE %in% new_include,] %>% 
                writeOGR(dsn = "./2_inputs/",layer = "tract_pop_rev",driver = "ESRI Shapefile")
        
}

tract_pop_rev <- readOGR(dsn = "./2_inputs/",layer = "tract_pop_rev") %>% 
        spTransform(CRSobj = crs_proj)



# DEMOGRAPHIC DATA: HOUSEHOLD INCOME --------------------------------------------------------------

# SAVED OLD CODE ----------------------------------------------------------------------------------

# [OLD] SPATIAL DATA: BG'S IN CAC NEIGHBORHOODS ---------------------------------------------------------

# All Census block groups overlapping the CAC neighborhoods (excluding slivers)

if(!file.exists("./2_inputs/blockgroups_CAC_nhoods.shp")){
        
        url <- "https://data.seattle.gov/download/2mbt-aqqx/application/zip" # save the URL for the neighborhood boundaries
        
        temp <- tempfile() # create a temporary file to hold the compressed download
        
        download(url, dest = temp, mode="wb") # download the file
        
        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
        
        nhoods <- readOGR(dsn = "./2_inputs/Neighborhoods/WGS84/",  # select YCC and adjacent neigborhood boundaries
                          layer = "Neighborhoods") %>% 
                spTransform(.,CRSobj = crs_proj)
        
        # select the Centreal Area Crescent neighborhoods from the small list
        CAC_sm <- c("Atlantic", "First Hill", "International District", "Minor", "Pioneer Square", "Yesler Terrace") 
        
        nhoods_CAC <<- nhoods[nhoods$S_HOOD %in% CAC_sm,]
        
        bgSelect(shapes = nhoods_CAC,multi = TRUE,layer = "blockgroups_CAC_nhoods")
}

bg_nhoods <- readOGR(dsn = "./2_inputs/",   
                     layer = "blockgroups_CAC_nhoods",
                     stringsAsFactors = FALSE) %>% 
        spTransform(CRSobj = crs_proj)

# [OLD] SPATIAL DATA: BG'S IN BAILEY-GATZERT BOUNDARY ---------------------------------------------------

# All Census block groups in the Bailey-Gatzert attendance boundary

if(!file.exists("./2_inputs/blockgroups_bgatz.shp")){
        
        url <- "https://www.seattleschools.org/UserFiles/Servers/Server_543/File/District/Departments/Enrollment%20Planning/Maps/gisdata/SPS_AttendanceAreasAndSchools_Shapefiles_2015_2016.zip" # save the URL for the neighborhood boundaries
        
        temp <- tempfile() # create a temporary file to hold the compressed download
        
        download(url, dest = temp, mode="wb") # download the file
        
        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
        
        bgatz <- readOGR(dsn = "./2_inputs/SPS_AttendanceAreasAndSchools_Shapefiles_2015_2016_v2/",
                         layer = "sps_attendance_area_ES_2015_2016") %>% 
                .[.@data$ES_ZONE == "Gatzert",] %>%
                spTransform(., CRSobj = crs_proj)
        
        bgatz_buf <- spTransform(bgatz, CRSobj = crs_geog) %>% 
                gBuffer(spgeom = .,width = -500) %>%
                spTransform(., CRSobj = crs_proj)
        
        overlap <- gIntersects(spgeom1 = bg_sea,
                               spgeom2 = bgatz_buf,
                               byid = T) %>% 
                which(.==TRUE)
        
        bg_sea[overlap,] %>%
                writeOGR(dsn = "./2_inputs/",
                         layer = "blockgroups_bgatz",
                         driver = "ESRI Shapefile")
}

bg_bgatz <- readOGR(dsn = "./2_inputs/",
                    layer = "blockgroups_bgatz",
                    stringsAsFactors = FALSE) %>% 
        spTransform(CRSobj = crs_proj)

# [OLD] SPATIAL DATA: BG'S IN MY CAC BOUNDARY -----------------------------------------------------------

# All Census block groups within the boundary of the printed map (provided by Amy Gore, 12-14-2015)

myCACbound <- readOGR(dsn = "./2_inputs/myCACbound/",layer = "myCACbound") %>% 
        spTransform(CRSobj = crs_proj)

if(!file.exists("./2_inputs/blockgroups_myCACbound.shp")){
        bgSelect(shapes = myCACbound,layer = "blockgroups_myCACbound")
}

bg_myCAC <- readOGR(dsn = "./2_inputs/", layer = "blockgroups_myCACbound") %>% 
        spTransform(CRSobj = crs_proj)

if(!file.exists("./2_inputs/bg_cntrs_myCAC.shp")){
        bg_cntrs_myCAC <- gCentroid(spgeom = bg_myCAC,byid = TRUE) %>% # create centroids for the bg's
                spTransform(CRSobj = crs_proj) %>% 
                SpatialPointsDataFrame(data = as.data.frame(bg_myCAC@data)) %>% 
                writeOGR(dsn = "./2_inputs/",layer = "bg_cntrs_myCAC",driver = "ESRI Shapefile")
        
        
}

bg_cntrs_myCAC <- readOGR(dsn = "./2_inputs/",layer = "bg_cntrs_myCAC") %>% # create centroids for the bg's
        spTransform(CRSobj = crs_proj)

# [OLD] SPATIAL DATA: BG'S WITH CENTROID IN MY CAC BOUNDARY ---------------------------------------------

# All Census blocks whose centroids are within the boundary of the printed map

if(!file.exists("./2_inputs/blockgroups_myCACbound_cntrs.shp")){
        bgSelectCntrd(overShape = myCACbound,layer = "blockgroups_myCACbound_cntrs")        
}

bg_myCAC_cntrs <- readOGR(dsn = "./2_inputs/",layer = "blockgroups_myCACbound_cntrs") %>% 
        spTransform(CRSobj = crs_proj)

# [OLD] SPATIAL DATA: BG'S W/ MAJORITY OF POP. IN MY CAC BOUND ------------------------------------------

# All Census block groups where the majority of the population live within the boundary of the printed map
# (note: this uses the centroid selection method)

make_bg_pop_myCAC_rev <- function() {
        
        if(!file.exists("./2_inputs/blks_CAC.shp")){
                blks <- 
                        readOGR(dsn = "2_inputs/blocks10/", layer = "blocks10") %>% # read in census block shapefiles
                        spTransform(CRSobj = crs_proj)
                
                bg_geoids <- bg_myCAC@data$GEOID # block group GEOID's
                
                blks_CAC <-
                        blks[blks@data$GEO_ID_GRP %in% bg_geoids, ] %>%   # subset the Census block spatial polygons df
                        writeOGR(dsn = "./2_inputs/",layer = "blks_CAC",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                
        }
        
        blks_CAC <- readOGR(dsn = "./2_inputs/",layer = "blks_CAC") %>% 
                spTransform(CRSobj = crs_proj)
        
        blk_df <<-
                readr::read_csv(file = "./2_inputs/wa_kc_blocks_pop/DEC_10_SF1_P1_with_ann.csv",
                                # load population data
                                skip = 1,
                                col_types = "cccn")
        
        blks_CAC <-
                geo_join(
                        spatial_data = blks_CAC,
                        # join spatial and demographic data
                        data_frame = blk_df,
                        by_sp = "GEO_ID_BLK",
                        by_df = "Id2"
                ) %>%
                spTransform(CRSobj = crs_proj)
        
        overlap <- gCentroid(spgeom = blks_CAC, byid = TRUE) %>%
                .[myCACbound,] %>%
                blks_CAC[.,]
        
        overlap <- overlap@data$GEO_ID_BLK
        
        blks_CAC@data %<>% select(GEO_ID_GRP, GEO_ID_BLK, Geography, Total) %>% # drop irrelevant variables
                mutate(Location = as.factor(ifelse(
                        GEO_ID_BLK %in% overlap,
                        "inside",
                        "outside"
                )))
        
        blks_CAC <<- blks_CAC
        
        include <-
                blks_CAC@data %>%
                group_by(GEO_ID_GRP, Location) %>%
                summarise(Count = sum(Total)) %>%
                spread(Location, Count) %>%
                replace(is.na(.), 0) %>%
                filter(inside > outside) %>%
                select(GEO_ID_GRP) %>%
                unlist()
        
        bg_pop_myCAC <<- bg_sea[bg_sea@data$GEOID %in% include, ]
        
        bg_pop_notMyCAC <<- bg_sea[bg_sea@data$GEOID %!in% include, ]
        
        SODO <-
                "530330093002" # Using 'myLflt_bgTest' I found that this is the SODO bg GEOID
        
        bg_pop_myCAC_rev <-
                bg_pop_myCAC[bg_pop_myCAC@data$GEOID %!in% SODO, ]
        
        return(bg_pop_myCAC_rev)
}

if(!file.exists("./2_inputs/bg_pop_myCAC_rev.shp")){
        make_bg_pop_myCAC_rev() %>% 
                writeOGR(dsn = "./2_inputs/",layer = "bg_pop_myCAC_rev",driver = "ESRI Shapefile")
}

bg_pop_myCAC_rev <- readOGR(dsn = "./2_inputs/",layer = "bg_pop_myCAC_rev") %>% 
        spTransform(CRSobj = crs_proj)

# This function saves a Leaflet map of the census blocks whose centroids are within
# the CAC boundary.

myLflt_bkTest <- function(){
        make_bg_pop_myCAC_rev()
        
        leaflet() %>%
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = blks_CAC,
                            fillColor = ~pal(Location),
                            stroke = TRUE, color = "White", weight = .5, opacity = 1) %>% 
                addPolygons(data = myCACbound,
                            fillOpacity = 0,
                            weight = 3,
                            color = "#ff9900",
                            dashArray = "5, 3",
                            opacity = 1)
}

# This function saves a Leaflet map. This particular map shows which block groups are included (color),
# the pop. density (hue), and the boundary of the CAC boundary. It shows that the Stadium District/SODO
# block group should probably not be included but all other inclusions and exclusions are appropriate.

myLflt_bgTest <- function(){
        
        make_bg_pop_myCAC_rev()
        
        pal_r <- colorNumeric(palette = "Reds",
                              domain = 0:sd(blks_CAC$Total))
        
        pal_b <- colorNumeric(palette = "Blues",
                              domain = 0:sd(blks_CAC$Total))
        
        popup_text <- paste0("GEOID: ",bg_myCAC@data$GEOID)
        
        blks_CAC_in <- blks_CAC[blks_CAC@data$GEO_ID_GRP %in% bg_pop_myCAC@data$GEOID,]
        
        blks_CAC_out <- blks_CAC[blks_CAC@data$GEO_ID_GRP %!in% bg_pop_myCAC@data$GEOID,]
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = blks_CAC_in,
                            fillColor = ~pal_r(Total), fillOpacity = .75,
                            stroke = FALSE) %>% 
                addPolygons(data = blks_CAC_out,
                            fillColor = ~pal_b(Total), fillOpacity = .75,
                            stroke = FALSE) %>% 
                addPolygons(data = bg_myCAC,
                            fillOpacity = 0,
                            weight = 1, color = "black", opacity = 1,
                            popup = popup_text) %>% 
                addPolygons(data = myCACbound,
                            fillOpacity = 0,
                            weight = 3,
                            color = "#ff9900",
                            dashArray = "5, 5",
                            opacity = 1)
}

# This function saves a Leaflet map of the population density (block) within the CAC boundary
# census blocks. Note: it reflects my revised selection, which excludes the SODO block group.

myLflt_bgCAC <- function(){
        
        make_bg_pop_myCAC_rev()
        
        pal_r <- colorNumeric(palette = "Reds",
                              domain = 0:sd(blks_CAC$Total))
        
        pal_b <- colorNumeric(palette = "Blues",
                              domain = 0:sd(blks_CAC$Total))
        
        popup_text <- paste0("GEOID: ",bg_myCAC@data$GEOID)
        
        blks_CAC_in <- blks_CAC[blks_CAC@data$GEO_ID_GRP %in% bg_pop_myCAC_rev@data$GEOID,]
        
        blks_CAC_out <- blks_CAC[blks_CAC@data$GEO_ID_GRP %!in% bg_pop_myCAC_rev@data$GEOID,]
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = blks_CAC_in,
                            fillColor = ~pal_r(Total), fillOpacity = .75,
                            stroke = FALSE) %>% 
                addPolygons(data = blks_CAC_out,
                            fillColor = ~pal_b(Total), fillOpacity = .75,
                            stroke = FALSE) %>% 
                addPolygons(data = bg_myCAC,
                            fillOpacity = 0,
                            weight = 1, color = "black", opacity = 1,
                            popup = popup_text) %>% 
                addPolygons(data = myCACbound,
                            fillOpacity = 0,
                            weight = 3,
                            color = "#ff9900",
                            dashArray = "5, 5",
                            opacity = 1)
}

myLflt_bkTest()

myLflt_bgTest()

myLflt_bgCAC()



# select the Centreal Area Crescent neighborhoods from the small list
CAC_sm <- c("Atlantic", "First Hill", "International District", "Minor", "Pioneer Square", "Yesler Terrace") 

nhoods_CAC <- nhoods[nhoods$S_HOOD %in% CAC_sm,]

# create a centroid object for neighborhood labels 
nhoods_CAC_cntr <- gCentroid(spgeom = nhoods_CAC,byid = TRUE) %>%     
        SpatialPointsDataFrame(.,data = as.data.frame(nhoods_CAC@data))

red <- "#ff0000" # leaflet uses HEX code colors
white <- "#ffffff"
black <- "#000000"
blue <- "#0000FF"



leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = nhoods_CAC,
                    fillColor = red, 
                    fillOpacity = .5,
                    color = white,
                    opacity = .5,
                    popup = ~S_HOOD) 


leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = nhoods_CAC,
                    fillColor = red, 
                    fillOpacity = .5,
                    color = white,
                    opacity = .5,
                    popup = ~S_HOOD) %>% 
        addPolygons(data = bg_CAC,
                    fillColor = blue,
                    opacity = 0) %>% 
        addPolygons(data = bgatz,
                    fillOpacity = 0,
                    color = black)