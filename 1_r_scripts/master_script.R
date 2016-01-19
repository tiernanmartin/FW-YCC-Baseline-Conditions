# CAC BASELINE CONDITIONS REPORT

# SETUP: LOAD PACKAGES AND PROJECT SETTINGS --------------------------------------------------------------

require(scales)         # for ggplot2 label formatting (e.g., 'dollar', 'percent', ect.)
require(gplots)         # for converting colors to HEX strings
require(grDevices)      # for color palettes
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


options(scipen=999,stringsAsFactors = FALSE)

crs_proj <- CRS("+init=epsg:4326") # This project will use WGS 84 projected coordinate system
crs_geog <- CRS("+init=epsg:2285") # Washington State plane CRS

# SETUP: MY FUNCTIONS --------------------------------------------------------------------

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

# -------------------------------------------------------------------------------------------------

# SPATIAL DATA: CAC NEIGHBORHOODS, BAILEY-GATZERT BOUNDARY, MY CAC BOUNDARY -----------------------

seaNhoods_CAC <- {
        if(!file.exists("./2_inputs/seaNhoods_CAC.shp")){
                
                url <- "https://data.seattle.gov/download/2mbt-aqqx/application/zip" # save the URL for the neighborhood boundaries
                
                temp <- tempfile() # create a temporary file to hold the compressed download
                
                download(url, dest = temp, mode="wb") # download the file
                
                unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
                
                seaNhoods <<- readOGR(dsn = "./2_inputs/Neighborhoods/WGS84/",  # select YCC and adjacent neigborhood boundaries
                                   layer = "Neighborhoods") %>% 
                        spTransform(.,CRSobj = crs_proj)
                
                # select the Centreal Area Crescent neighborhoods from the small list
                CAC_sm <- c("Atlantic", "First Hill", "International District", "Minor", "Pioneer Square", "Yesler Terrace") 
                
                seaNhoods[seaNhoods$S_HOOD %in% CAC_sm,] %>% 
                        writeOGR(dsn = "./2_inputs/",layer = "seaNhoods_CAC",driver = "ESRI Shapefile")
                
        }
        
        seaNhoods_CAC <- readOGR(dsn = "./2_inputs/",layer = "seaNhoods_CAC") %>% 
                spTransform(CRSobj = crs_proj)
}

bgatz <- {
        if(!file.exists("./2_inputs/bgatz.shp")){
                
                url <- "https://www.seattleschools.org/UserFiles/Servers/Server_543/File/District/Departments/Enrollment%20Planning/Maps/gisdata/SPS_AttendanceAreasAndSchools_Shapefiles_2015_2016.zip" # save the URL for the neighborhood boundaries
                
                temp <- tempfile() # create a temporary file to hold the compressed download
                
                download(url, dest = temp, mode="wb") # download the file
                
                unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
                
                readOGR(dsn = "./2_inputs/SPS_AttendanceAreasAndSchools_Shapefiles_2015_2016_v2/",
                        layer = "sps_attendance_area_ES_2015_2016") %>% 
                        .[.@data$ES_ZONE == "Gatzert",] %>%
                        spTransform(., CRSobj = crs_proj) %>% 
                        writeOGR(dsn = "./2_inputs/",layer = "bgatz",driver = "ESRI Shapefile")
        }
        
        readOGR(dsn = "./2_inputs/",
                layer = "bgatz") %>% 
                spTransform(CRSobj = crs_proj)
}

myCACbound <- {
        
        # The boundary of the printed map (provided by Amy Gore, 12-14-2015)
        
        readOGR(dsn = "./2_inputs/myCACbound/",layer = "myCACbound") %>% 
        spTransform(CRSobj = crs_proj)
        
        }

myCACbound_cntr <- {
        gCentroid(myCACbound) %>% 
                spTransform(CRSobj = crs_proj)
}

# SPATIAL DATA: CENSUS TRACTS, BLOCK GROUPS, BLOCKS -----------------------------------------------

# 'tract_CAC' and 'bg_CAC' are TIGER shapefiles that are clipped to remove waterbodies (for visual clarity)
tract_CAC <- {
        if(!file.exists("./2_inputs/tracts.shp")){
                tracts_orig <- 
                        tigris::tracts(state = "WA", county = "King") %>% 
                        spTransform(CRSobj = crs_proj)
                
                if(!file.exists("./2_inputs/NHDMajor.gdb")){  # check if the file already exists, if not then download it
                        url <- "ftp://www.ecy.wa.gov/gis_a/inlandWaters/NHD/NHDmajor.gdb.zip" # save the URL for the waterbodies data
                        
                        temp <- tempfile() # create a temporary file to hold the compressed download
                        
                        download(url, dest = temp, mode="wb") # download the file
                        
                        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
                        
                        dateDownloaded <- date()
                }
                
                path_gdb <- "./2_inputs/NHDMajor.gdb/" # path to the geodatabase folder
                
                waterbodies.shp <- 
                        readOGR(dsn = path_gdb,      # create a waterbodies shape
                                           layer = "NHD_MajorWaterbodies") %>%
                        gBuffer(byid=TRUE, width=0) %>% # clean up self-intersecting polygons
                        spTransform(CRSobj = crs_proj) # transform the projection to match the project projection
                
                tracts_big <- gUnaryUnion(tracts_orig) # simplify the tract polygons by merging them into one polygon
                
                waterbodies_cntr <- gCentroid(spgeom = waterbodies.shp,byid = TRUE) # create a set of center points for the waterbodies shapes
                
                intersect <- over(x = waterbodies_cntr,y = tracts_big,returnList = TRUE) %>%  # find the indices of all waterbodies whose center point overlaps the merged tracts shape
                        .[which(. == 1)] %>% 
                        names()
                
                # some of the Puget Sound polygon centroids aren't overlapped by tracts, so we'll added them manually
                ps <- 
                        waterbodies.shp@data %>% 
                        mutate(RN = rownames(.)) %>% 
                        as.data.frame %>% 
                        filter(GNIS_Name %in% "Puget Sound" & AreaSqKm > 100) %>% 
                        select(RN) %>% 
                        unlist()
                
                waterbodies_sel.shp <- waterbodies.shp[c(ps,intersect),] %>%  # refine the subset of the spatial data
                        spTransform(CRSobj = crs_proj) %>%  # change the CRS from geographic to projected
                        gUnaryUnion()
                
                tracts <- gDifference(spgeom1 = tracts_orig, spgeom2 = waterbodies_sel.shp, 
                                      byid = TRUE) # Remove the waterbodies from the tract shapes
                
                # tracts_all <- gUnaryUnion(tracts)
                # 
                # trt_cent <- gCentroid(tracts_orig, byid = TRUE)
                # 
                # test <- over(x = trt_cent,y = tracts_orig,returnList = TRUE)
                # 
                # 53033990100
                # test[which(test == 0)]
                # 
                # 
                # myLeaflet(tracts_orig) %>%
                #         addPolygons(data = tracts_orig,popup = tracts_orig$GEOID, stroke = F, fillOpacity = 0) %>% 
                #         addCircles(data = trt_cent,color = "orange", radius = 2) %>% 
                #         addPolygons(data = tracts,smoothFactor = 0,stroke = FALSE, fillColor = "black",fillOpacity = 1)
                # 
                # 
                # 
                # GEOIDS <- over(x = trt_cent,y = tracts_orig,returnList = TRUE)
                # 
                # 
                # myLeaflet(tracts) %>% 
                #         addCircleMarkers(data = trt_cent)
                
                # This (annoying) tract was completely removed during the waterbody clip, 
                # so it must also be removed from the original SpPolyDF
                
                PugetSoundTract <- "53033990100" 
                
                df <- tracts_orig@data %>% filter(GEOID %!in% PugetSoundTract)
                
                rn <- rownames(df)
                
                tracts <- spChFIDs(obj = tracts,x = rn) %>% # change the row IDs to match those in 'tracts_orig'
                        SpatialPolygonsDataFrame(Sr = .,data = df) %>% 
                        spTransform(.,CRSobj = crs_proj) %>% 
                        writeOGR(dsn = "./2_inputs/",layer = "tracts_noWtrbds", driver = "ESRI Shapefile")
        }
        
        
        
        if(!file.exists("./2_inputs/tract_CAC.shp")){
                tract_wa <- readOGR(dsn = "./2_inputs/", layer = "tracts_noWtrbds") %>% 
                        spTransform(CRSobj = crs_proj)
                
                overlap <- gIntersects(spgeom1 = tract_wa,spgeom2 = myCACbound, byid = T) %>%
                        which(. == T)
                
                tract_CAC <- tract_wa[overlap,] %>% 
                        writeOGR(dsn = "./2_inputs/", layer = "tract_CAC", driver = "ESRI Shapefile",overwrite_layer = TRUE)
                
        }
        
        readOGR(dsn = "./2_inputs/",layer = "tract_CAC") %>% 
                spTransform(CRSobj = crs_proj)
} 

bg_CAC <- {
        if(!file.exists("./2_inputs/bg.shp")){
                bg_orig <- tigris::block_groups(state = "WA", county = "King") %>% 
                        spTransform(CRSobj = crs_proj)
                
                if(!file.exists("./2_inputs/NHDMajor.gdb")){  # check if the file already exists, if not then download it
                        url <- "ftp://www.ecy.wa.gov/gis_a/inlandWaters/NHD/NHDmajor.gdb.zip" # save the URL for the waterbodies data
                        
                        temp <- tempfile() # create a temporary file to hold the compressed download
                        
                        download(url, dest = temp, mode="wb") # download the file
                        
                        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
                        
                        dateDownloaded <- date()
                }
                
                path_gdb <- "./2_inputs/NHDMajor.gdb/" # path to the geodatabase folder
                
                waterbodies.shp <- 
                        readOGR(dsn = path_gdb,      # create a waterbodies shape
                                layer = "NHD_MajorWaterbodies") %>%
                        gBuffer(byid=TRUE, width=0) %>% # clean up self-intersecting polygons
                        spTransform(CRSobj = crs_proj) # transform the projection to match the project projection
                
                bg_big <- gUnaryUnion(bg_orig) # simplify the tract polygons by merging them into one polygon
                
                waterbodies_cntr <- gCentroid(spgeom = waterbodies.shp,byid = TRUE) # create a set of center points for the waterbodies shapes
                
                intersect <- over(x = waterbodies_cntr,y = bg_big,returnList = TRUE) %>%  # find the indices of all waterbodies whose center point overlaps the merged bg shape
                        .[which(. == 1)] %>% 
                        names()
                
                # some of the Puget Sound polygon centroids aren't overlapped by bg, so we'll added them manually
                ps <- 
                        waterbodies.shp@data %>% 
                        mutate(RN = rownames(.)) %>% 
                        as.data.frame %>% 
                        filter(GNIS_Name %in% "Puget Sound" & AreaSqKm > 100) %>% 
                        select(RN) %>% 
                        unlist()
                
                waterbodies_sel.shp <- waterbodies.shp[c(ps,intersect),] %>%  # refine the subset of the spatial data
                        spTransform(CRSobj = crs_proj) %>%  # change the CRS from geographic to projected
                        gUnaryUnion()
                
                bg <- gDifference(spgeom1 = bg_orig, spgeom2 = waterbodies_sel.shp, 
                                      byid = TRUE) # Remove the waterbodies from the tract shapes
                
                PugetSoundTract <- "990100" 
                
                df <- bg_orig@data %>% filter(TRACTCE %!in% PugetSoundTract)
                
                rn <- rownames(df)
                
                bg <- spChFIDs(obj = bg,x = rn) %>% # change the row IDs to match those in 'bg_orig'
                        SpatialPolygonsDataFrame(Sr = .,data = df) %>% 
                        spTransform(.,CRSobj = crs_proj) %>% 
                        writeOGR(dsn = "./2_inputs/",layer = "bg_noWtrbds", driver = "ESRI Shapefile")
                
        }
        
        if(!file.exists("./2_inputs/bg_CAC.shp")){
                
                sel <- tract_CAC@data$TRACTCE
                
                bg_CAC <- readOGR(dsn = "./2_inputs/", layer = "bg_noWtrbds") %>% 
                        spTransform(CRSobj = crs_proj) %>% 
                        .[.@data$TRACTCE %in% sel,] %>% 
                        writeOGR(dsn = "./2_inputs/", layer = "bg_CAC", driver = "ESRI Shapefile",overwrite_layer = TRUE)
                
        }
        
        readOGR(dsn = "./2_inputs/",layer = "bg_CAC") %>% 
                spTransform(CRSobj = crs_proj)
}

blk_CAC <- {
        
        if(!file.exists("./2_inputs/blk_CAC.shp")){
                
                if(!file.exists("./2_inputs/blk.shp")){
                        tigris::blocks(state = "WA", county = "King") %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "blk", driver = "ESRI Shapefile")
                }
                
                sel <- tract_CAC@data$TRACTCE
                
                blk_CAC <- readOGR(dsn = "./2_inputs/", layer = "blk") %>% 
                        spTransform(CRSobj = crs_proj) %>% 
                        .[.@data$TRACTCE %in% sel,] 
                
                if(!file.exists("./2_inputs/NHDMajor.gdb")){  # check if the file already exists, if not then download it
                        url <- "ftp://www.ecy.wa.gov/gis_a/inlandWaters/NHD/NHDmajor.gdb.zip" # save the URL for the waterbodies data
                        
                        temp <- tempfile() # create a temporary file to hold the compressed download
                        
                        download(url, dest = temp, mode="wb") # download the file
                        
                        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
                        
                        dateDownloaded <- date()
                }
                
                path_gdb <- "./2_inputs/NHDMajor.gdb/" # path to the geodatabase folder
                
                waterbodies.shp <- 
                        readOGR(dsn = path_gdb,      # create a waterbodies shape
                                layer = "NHD_MajorWaterbodies") %>%
                        gBuffer(byid=TRUE, width=0) %>% # clean up self-intersecting polygons
                        spTransform(CRSobj = crs_proj) # transform the projection to match the project projection
                
                blk_big <- gUnaryUnion(blk_CAC) # simplify the tract polygons by merging them into one polygon
                
                waterbodies_cntr <- gCentroid(spgeom = waterbodies.shp,byid = TRUE) # create a set of center points for the waterbodies shapes
                
                intersect <- over(x = waterbodies_cntr,y = blk_big,returnList = TRUE) %>%  # find the indices of all waterbodies whose center point overlaps the merged bg shape
                        .[which(. == 1)] %>% 
                        names()
                ps <- 
                        waterbodies.shp@data %>% 
                        mutate(RN = rownames(.)) %>% 
                        as.data.frame %>% 
                        filter(GNIS_Name %in% "Puget Sound") %>% 
                        select(RN) %>% 
                        unlist()
                
                waterbodies_sel.shp <- waterbodies.shp[c(ps),] %>%  # refine the subset of the spatial data
                        spTransform(CRSobj = crs_proj) %>%  # change the CRS from geographic to projected
                        gUnaryUnion()
                
                blk_CAC_noWtrbds <- gDifference(spgeom1 = blk_CAC, spgeom2 = waterbodies_sel.shp, 
                                       byid = TRUE) 
                rn <- row.names(blk_CAC_noWtrbds)
                
                nodata <- rep(NA, times = 1262) %>% as.data.frame()
                
                rownames(nodata) <- rn
                
                blk_CAC_noWtrbds %>% 
                        SpatialPolygonsDataFrame(data = nodata) %>% 
                        writeOGR(dsn = "./2_inputs/",layer = "blk_CAC_noWtrbds",driver = "ESRI Shapefile") # Remove the waterbodies from the tract shapes
                
                # Must download the block-level data directly from American Commmunity Survery b/c 
                # the smallest scale of data provided by the Census API is the block-group level.
                    
                blk_df <- 
                        readr::read_csv(file = "./2_inputs/wa_kc_blocks_pop/DEC_10_SF1_P1_with_ann.csv",
                                        # load population data
                                        skip = 1,
                                        col_types = "cccn")
                
                
                
                blk_CAC <-
                        geo_join(
                                spatial_data = blk_CAC,
                                # join spatial and demographic data
                                data_frame = blk_df,
                                by_sp = "GEOID10",
                                by_df = "Id2"
                        ) %>%
                        spTransform(CRSobj = crs_proj) %>% 
                        writeOGR(dsn = "./2_inputs/", layer = "blk_CAC", driver = "ESRI Shapefile",overwrite_layer = T)
                
        }
        
        readOGR(dsn = "./2_inputs/",layer = "blk_CAC") %>% 
                spTransform(CRSobj = crs_proj)
}

blk_CAC_noWtrbds <- {
        readOGR(dsn = "./2_inputs/",layer = "blk_CAC_noWtrbds") %>% 
                spTransform(CRSobj = crs_proj)
}



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

# myLflt_tractTest()

# SPATIAL DATA: TRACT SELECTION, REVISION OF BLOCK GROUP + BLOCK SELECTION ------------------------

# Select tracts where the majority of the population lives within the CAC bound and map the result

tract_pop <- {
        if(!file.exists("./2_inputs/tract_pop.shp")){
                overlap <- gCentroid(spgeom = blk_CAC, byid = TRUE) %>%
                        .[myCACbound,] %>%
                        blk_CAC[.,]
                
                overlap <- overlap@data$GEOID10
                
                blk_CAC@data %<>% select(TRACTCE10, GEOID10, Geography, Total) %>% # drop irrelevant variables
                        mutate(Location = as.factor(ifelse(
                                GEOID10 %in% overlap,
                                "inside",
                                "outside"
                        )))
                
                include <-
                        blk_CAC@data %>%
                        group_by(TRACTCE10, Location) %>%
                        summarise(Count = sum(Total)) %>%
                        spread(Location, Count) %>%
                        replace(is.na(.), 0) %>%
                        filter(inside > outside) %>%
                        select(TRACTCE10) %>%
                        unlist()
                
                tract_pop <- tract_CAC[tract_CAC@data$TRACTCE %in% include, ] %>% 
                        writeOGR(dsn = "./2_inputs/",layer = "tract_pop",driver = "ESRI Shapefile",overwrite_layer = TRUE)
        }
        
        readOGR(dsn = "./2_inputs/",layer = "tract_pop") %>% 
                spTransform(CRSobj = crs_proj)
}

myLflt_TractPopTest <- function(){
        
        legend_layers <- factor(c("CAC boundary","blocks","tracts"),
                                levels = c("CAC boundary","blocks","tracts"), ordered = T)
        
        pal_r <- colorNumeric(palette = "Reds",
                              domain = 0:sd(blk_CAC$Total))
        
        pal_b <- colorNumeric(palette = "Blues",
                              domain = 0:sd(blk_CAC$Total))
        
        popup_text <- paste0(tract_CAC@data$NAMELSAD,"<br>","GEOID: ",tract_CAC@data$GEOID)
        
        tract_blk_in <- blk_CAC[blk_CAC@data$TRACTCE %in% tract_pop@data$TRACTCE,]
        
        tract_blk_out <- blk_CAC[blk_CAC@data$TRACTCE %!in% tract_pop@data$TRACTCE,]
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = tract_blk_in,
                            fillColor = ~pal_r(Total), fillOpacity = .75,
                            stroke = FALSE,
                            group = legend_layers[2]) %>% 
                addPolygons(data = tract_blk_out,
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
                addPolygons(data = tract_CAC,
                            fillOpacity = 0,
                            weight = 1, color = "black", opacity = 1,
                            popup = popup_text,
                            group = legend_layers[3]) %>% 
                addLayersControl(overlayGroups = c("CAC boundary","blocks","tracts"),
                                 options = layersControlOptions(collapsed = FALSE))
        
}

# myLflt_TractPopTest()

# Revise the tract selection to include Tract 79 (Capitol Hill)

tract_rev <- {
        if(!file.exists("./2_inputs/tract_rev.shp")){
                
                CapHill <- "007900" # save the Tract ID for Tract 79
                
                new_include <- c(tract_pop@data$TRACTCE,CapHill) 
                
                tract_CAC[tract_CAC@data$TRACTCE %in% new_include,] %>% 
                        writeOGR(dsn = "./2_inputs/",layer = "tract_rev",driver = "ESRI Shapefile")
        }
        
        readOGR(dsn = "./2_inputs/",layer = "tract_rev") %>% 
                spTransform(CRSobj = crs_proj)
}

bg_rev <- {
        if(!file.exists("./2_inputs/bg_rev.shp")){
                
                CapHill <- "007900" # save the Tract ID for Tract 79
                
                new_include <- c(tract_pop@data$TRACTCE,CapHill) 
                
                bg_rev <- bg_CAC[bg_CAC@data$TRACTCE %in% new_include,] 
                
                # bg_rev@polygons <- bg_rev %>%
                #         spTransform(CRSobj = crs_geog) %>%
                #         gBuffer(
                #                 width = 1,
                #                 byid = TRUE,
                #                 capStyle = "FLAT",
                #                 joinStyle = "MITRE"
                #         ) %>%
                #         spTransform(CRSobj = crs_proj) %>% 
                #         .@polygons
                
                writeOGR(obj = bg_rev,dsn = "./2_inputs/",layer = "bg_rev",driver = "ESRI Shapefile",overwrite_layer = TRUE)
        }
        
        readOGR(dsn = "./2_inputs/",layer = "bg_rev") %>% 
                spTransform(CRSobj = crs_proj)
}

blk_rev <- {
        if(!file.exists("./2_inputs/blk_rev.shp")){
                
                CapHill <- "007900" # save the Tract ID for Tract 79
                
                new_include <- c(tract_pop@data$TRACTCE,CapHill) 
                
                blk_CAC[blk_CAC@data$TRACTCE %in% new_include,] %>% 
                        writeOGR(dsn = "./2_inputs/",layer = "blk_rev",driver = "ESRI Shapefile")
        }
        
        readOGR(dsn = "./2_inputs/",layer = "blk_rev") %>% 
                spTransform(CRSobj = crs_proj)
}

# Shows the census spatial boundaries for the CAC study area

myLflt_censusSel <- function(){
        
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = blk_rev,
                            fillColor = col2hex("darkblue"),fillOpacity = .33,
                            color = col2hex("white"), weight = 1, opacity = .5,
                            group = "1. Blocks") %>% 
                addPolygons(data = bg_rev,
                            fill = FALSE,
                            color = col2hex("white"), weight = 3, opacity = .75,dashArray = "5, 10",
                            group = "2. Block Groups") %>% 
                addPolygons(data = tract_rev,
                            fill = FALSE,
                            color = col2hex("white"), weight = 5, opacity = .9,
                            group = "3. Tracts") %>% 
                addLayersControl(overlayGroups = c("1. Blocks","2. Block Groups","3. Tracts"),
                                 options = layersControlOptions(collapsed = FALSE))
}

# myLflt_censusSel()

# Shows the population density by block in the CAC study area

myLflt_blockPop <- function(){
        
        pal <- colorNumeric(palette = "Blues",domain = 0:sd(blk_rev@data$Total))
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = blk_rev,
                            fillColor = ~pal(Total),fillOpacity = .5,
                            color = col2hex("white"), weight = 1, opacity = .5,
                            group = "1. Blocks") %>% 
                addPolygons(data = bg_rev,
                            fill = FALSE,
                            color = col2hex("gray50"), weight = 3, opacity = .75,dashArray = "3, 6",
                            group = "2. Block Groups") %>% 
                addPolygons(data = tract_rev,
                            fill = FALSE,
                            color = col2hex("gray25"), weight = 5, opacity = .9,
                            group = "3. Tracts") %>% 
                addPolygons(data = myCACbound,
                            fillOpacity = 0,
                            weight = 3,
                            color = "#ff9900",
                            dashArray = "5, 5",
                            opacity = 1,
                            group = "4. CAC Boundary") %>% 
                addLayersControl(overlayGroups = c("1. Blocks","2. Block Groups","3. Tracts", "4. CAC Boundary"),
                                 options = layersControlOptions(collapsed = FALSE))
}

# myLflt_blockPop()

# SPATIAL DATA: SCALES OF ANALYSIS ----------------------------------------------------------------

# Neighborhood names at the different Census geographies

myNhoods_tract <- data.frame(
        "NHOOD.FULL" = c("Pioneer Square", 
                    "Chinatown-International District, Yesler Terrace & Little Saigon",
                    "Central District", "First Hill", "12 Ave & Capitol Hill"),
        "NHOOD.ABBR" = c("PS","CIDYTLS","CD","FH","12AV"))

myNhoods_bg <- data.frame(
        "NHOOD.FULL" = c("Pioneer Square", 
                        "Chinatown-International District","Yesler Terrace & Little Saigon",
                        "Central District", "First Hill", "12 Ave & Capitol Hill"),
        "NHOOD.ABBR" = c("PS","CID","YTLS","CD","FH","12AV"))

myNhoods_geo <- data.frame(
        "NHOOD.FULL" = c("Pioneer Square", "Chinatown-International District",
                         "Yesler Terrace", "Little Saigon", 
                         "Central District","First Hill", "12 Ave & Capitol Hill"),
        "NHOOD.ABBR" = c("PS","CID","YT","LS","CD","FH","12AV"))

tract_rev <- {
        tract_rev <-  
        tract_rev@data %>% 
                as.data.frame() %>%
                mutate(NHOOD.FULL = ifelse(
                        NAME %in% "92",
                        myNhoods_tract[1, 1],
                        ifelse(
                                NAME %in% "91",
                                myNhoods_tract[2, 1],
                                ifelse(
                                        NAME %in% c("90", "87", "79"),
                                        myNhoods_tract[3, 1],
                                        ifelse(
                                                NAME %in% "85",
                                                myNhoods_tract[4, 1],
                                                ifelse(NAME %in% "86",
                                                       myNhoods_tract[5, 1],
                                                       NA)
                                        )
                                )
                        )
                )) %>% 
                left_join(y = myNhoods_tract) %>% 
                select(GEOID,NHOOD.FULL,NHOOD.ABBR) %>% 
                myGeoJoin(spatial_data = tract_rev,data_frame = .,by_sp = "GEOID", by_df = "GEOID")
}

myLflt_tractRevTest <- function(){
        
        popup_text <- paste0(tract_rev@data$NHOOD.FULL)
        
        myLflt_blockPop() %>% 
                addPolygons(data = tract_rev,
                            fillOpacity = 0,
                            stroke = FALSE,
                            popup = popup_text)
                
} # Map with block pop. density, nhood boundaries, and popups 

# myLflt_tractRevTest()

myLflt_nhoodTract <- function(){
        
        pal <- colorFactor(palette = "Set3",
                           domain = tract_rev@data$NHOOD.FULL)
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = tract_rev,
                            fillColor = ~pal(NHOOD.FULL), fillOpacity = .8,
                            color = col2hex("white"), weight = 5, opacity = .9,
                            group = "3. Tracts") %>% 
                addPolygons(data = blk_rev,
                            fill = FALSE,
                            color = col2hex("white"), weight = 1, opacity = .5,
                            group = "1. Blocks") %>% 
                addPolygons(data = bg_rev,
                            fill = FALSE,
                            color = col2hex("white"), weight = 3, opacity = .75,dashArray = "5, 10",
                            group = "2. Block Groups") %>% 
                addLayersControl(overlayGroups = c("1. Blocks","2. Block Groups","3. Tracts"),
                                 options = layersControlOptions(collapsed = FALSE)) %>% 
                addLegend(pal = pal, 
                          values = tract_rev@data$NHOOD.FULL, 
                          position = "topright", 
                          title = "Study Area Neighborhoods: Census Tracts",
                          opacity = 1)
} # Map with tracts colored by neighborhood

# myLflt_nhoodTract()

bg_rev <- {
        bg_rev <-
                bg_rev@data %>%
                as.data.frame() %>%
                mutate(NHOOD.FULL = ifelse(
                        TRACTCE %in% "009200",
                        myNhoods_bg[1, 1],
                        ifelse(
                                GEOID %in% "530330091002",
                                myNhoods_bg[2, 1],
                                ifelse(
                                        GEOID %in% "530330091001",
                                        myNhoods_bg[3, 1],
                                        ifelse(
                                                TRACTCE %in% c("007900", "009000", "008700"),
                                                myNhoods_bg[4, 1],
                                                ifelse(
                                                        TRACTCE %in% "008500",
                                                        myNhoods_bg[5, 1],
                                                        ifelse(TRACTCE %in% "008600",
                                                               myNhoods_bg[6, 1],
                                                               NA)
                                                )
                                        )
                                )
                        )
                )) %>%
                left_join(y = myNhoods_bg) %>%
                select(GEOID, NHOOD.FULL, NHOOD.ABBR) %>%
                myGeoJoin(
                        spatial_data = bg_rev,
                        data_frame = .,
                        by_sp = "GEOID",
                        by_df = "GEOID"
                )
        # Eliminate interpolygonal slivers
        
}

myLflt_bgRevTest <- function(){
        
        popup_text <- paste0("GEOID: ",bg_rev@data$GEOID,
                             "<br>",bg_rev@data$NHOOD.FULL)
        
        myLflt_censusSel() %>% 
                addPolygons(data = bg_rev,
                            fillOpacity = 0,
                            stroke = F,
                            popup = popup_text)
        
}

# myLflt_bgRevTest()

myLflt_nhoodBg <- function(){
        
        pal <- colorFactor(palette = "Set2",
                           domain = bg_rev@data$NHOOD.FULL)
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = bg_rev,
                            fillColor = ~pal(NHOOD.FULL), fillOpacity = .5,
                            color = col2hex("white"), weight = 3, opacity = .75,dashArray = "5, 10",
                            group = "2. Block Groups") %>% 
                addPolygons(data = tract_rev,
                            fill = FALSE,
                            color = col2hex("white"), weight = 5, opacity = .9,
                            group = "3. Tracts") %>% 
                addPolygons(data = blk_rev,
                            fill = FALSE,
                            color = col2hex("white"), weight = 1, opacity = .5,
                            group = "1. Blocks") %>% 
                
                addLayersControl(overlayGroups = c("1. Blocks","2. Block Groups","3. Tracts"),
                                 options = layersControlOptions(collapsed = FALSE)) %>% 
                addLegend(pal = pal, 
                          values = bg_rev@data$NHOOD.FULL, 
                          position = "topright", 
                          title = "Study Area Neighborhoods:<br> Census Block Groups",
                          opacity = 1)
} # Map with block groups colored by neighborhood

# myLflt_nhoodBg()

# An outline of the study area neighborhoods 
# (as defined by the census tracts associated with them in this project)

nhoods_census_outline <- 
        bg_rev %>% 
        gUnaryUnion(id = as.factor(.@data$NHOOD.ABBR)) %>% 
        as('SpatialLines') 
        

myLflt_nhoods <- function(){
        pal <- colorFactor(palette = "Set2",
                           domain = seaNhoods_CAC@data$S_HOOD)
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = seaNhoods_CAC, 
                            fillColor = ~pal(seaNhoods_CAC@data$S_HOOD), fillOpacity = .5,
                            stroke = F,
                            popup = paste0(seaNhoods_CAC@data$S_HOOD)) %>% 
                addPolygons(data = blk_rev,
                            fill = FALSE,
                            color = col2hex("white"), weight = 1, opacity = .5,
                            group = "1. Blocks") %>% 
                addPolygons(data = bg_rev,
                            fill = FALSE,
                            color = col2hex("white"), weight = 3, opacity = .75,dashArray = "3, 6",
                            group = "2. Block Groups") %>% 
                addPolygons(data = tract_rev,
                            fill = FALSE,
                            color = col2hex("white"), weight = 5, opacity = .9,
                            group = "3. Tracts") %>% 
                addLayersControl(overlayGroups = c("1. Blocks","2. Block Groups","3. Tracts"),
                                 options = layersControlOptions(collapsed = FALSE))
        
        
}

# myLflt_nhoods()

# -------------------------------------------------------------------------------------------------        

# DEMOGRAPHIC DATA: SET UP ------------------------------------------------------------------------

# A function to create combined geographies of block.groups

make_nhood_acs <- function(nhood.abbr){
        
        tracts <- bg_rev@data %>% 
                filter(NHOOD.ABBR == nhood.abbr) %>% 
                select(TRACTCE) %>% 
                unlist() %>% 
                as.numeric()
        
        bgs <-  bg_rev@data %>% 
                filter(NHOOD.ABBR == nhood.abbr) %>% 
                select(BLKGRPCE) %>% 
                unlist() %>% 
                as.numeric()
        
        acs <- geo.make(state = "WA",
                 county = "King",
                 tract = tracts,
                 block.group = bgs,
                 combine = T,
                 combine.term = nhood.abbr)
        
        if(length(bgs) == 1){
                acs <- geo.make(state = "WA",
                                county = "King",
                                tract = tracts,
                                block.group = bgs)

        }
        return(acs)
}

# Use `make_nhood_acs` to create geographies for the study area neighborhoods

acs_bg_PS <- make_nhood_acs(nhood.abbr = "PS")

acs_bg_CID <- make_nhood_acs(nhood.abbr = "CID")

acs_bg_YTLS <- make_nhood_acs(nhood.abbr = "YTLS")

acs_bg_CD <- make_nhood_acs(nhood.abbr = "CD")

acs_bg_FH <- make_nhood_acs(nhood.abbr = "FH")

acs_bg_12AV <- make_nhood_acs(nhood.abbr = "12AV")

acs_sea <- geo.make(state = "WA", county = "King", place = "Seattle")

acs_KC <- geo.make(state = "WA", county = "King")

# Create a collection of all the neighborhood geographies

geo_bg <- c(acs_bg_CD,
         acs_bg_FH,
         acs_bg_12AV,
         acs_bg_PS,
         acs_bg_CID,
         acs_bg_YTLS)

geo_bg_plus <- c(acs_bg_CD,
                 acs_bg_FH,
                 acs_bg_12AV,
                 acs_bg_PS,
                 acs_bg_CID,
                 acs_bg_YTLS,
                 acs_sea,
                 acs_KC)

# DEMOGRAPHIC DATA: COMMUNITY CHARACTERISTICS -----------------------------------------------------

# Median Income

FH_medianIncome2014 <- {
        Sys.sleep(1)

        medInc <- acs.fetch(
                endyear = 2014,
                span = 5,
                geography = geo.make(
                        state = "WA",
                        county = "King",
                        tract = "008500"
                ),
                table.number = "B19013",
                col.names = "pretty"
        )
        
        medInc@estimate %>% as.numeric()
} # Save the Median income for First Hill (Tract 85) for reference

medianIncome2014 <- {
        if(!file.exists("./2_inputs/medianIncome2014.csv")){
                Sys.sleep(1)
                acs <- acs.fetch(endyear = 2014, span = 5,
                                 geography = geo_bg,
                                 table.number = "B19001", col.names = "pretty")
                
                rnames <- acs@estimate %>% t()  %>% as.data.frame() %>% rownames() %>% as.data.frame()
                
                medInc <- 
                        acs@estimate %>% 
                        t() %>% 
                        as.data.frame() %>% 
                        bind_cols(rnames) %>% 
                        gather(geo,count,-.) 
                
                colnames(medInc) <- c("Range","Nhood","HH_Count")
                
                medInc <- 
                        medInc %>% 
                        mutate(Range = gsub("Household\\sIncome:\\s","",x = Range),
                               Nhood = as.character(Nhood),
                               Nhood = ifelse(grepl("\\s2",x = Nhood),
                                              "CID",
                                              ifelse(grepl("\\s1",x = Nhood),
                                                     "YTLS",
                                                     Nhood))) %>% 
                        filter(!grepl("Total",Range)) %>% 
                        mutate(RangeNum = as.numeric(gsub("[^\\d]+", "", Range, perl=TRUE))) %>% 
                        mutate(RangeStr = as.character(RangeNum),
                               RangeStrLen = round(str_length(RangeStr)/2)) %>% 
                        mutate(RangeLower = ifelse(str_length(RangeStr) == 6,
                                                   RangeNum,
                                                   ifelse(str_length(RangeStr) < 6,
                                                          1,
                                                          str_extract(RangeStr,paste0("^\\d{",RangeStrLen,"}"))))) %>% 
                        mutate(RangeUpper = ifelse(str_length(RangeStr) == 6,
                                                   500000,
                                                   ifelse(str_length(RangeStr) < 6,
                                                          RangeNum,
                                                          str_extract(RangeStr,paste0("\\d{",RangeStrLen,"}$"))))) %>% 
                        select(RangeDesc = Range,
                               RangeLower,
                               RangeUpper,
                               Nhood,
                               HH_Count) %>% 
                        group_by(Nhood) %>% 
                        mutate(RangeLower = as.numeric(RangeLower),
                               RangeUpper = as.numeric(RangeUpper),
                               CumSum = cumsum(HH_Count)) %>% 
                        do({
                                cs <- .["CumSum"] %>% unlist() %>% as.numeric() %>% as.vector()
                                midRow <- findInterval(max(cs)/2, cs) + 1
                                
                                a <- .[midRow,"RangeLower"]
                                b <- .[midRow,"RangeUpper"]
                                Pa <- .[midRow-1,"CumSum"] / .[nrow(.),"CumSum"]
                                Pb <- .[midRow,"CumSum"] / .[nrow(.),"CumSum"]
                                
                                thedaNum <- log(1-Pa)-log(1-Pb)
                                thedaDen <- log(b)-log(a)
                                theda <- thedaNum/thedaDen
                                
                                kNum <- Pb - Pa
                                kDen <- 1 / (a^theda) - 1 / (b^theda)
                                k <- (kNum / kDen) ^ (1 / theda) 
                                
                                medianEst <- k * (2^(1 / theda))
                                
                                medianEst <- as.vector(medianEst) %>% round(digits = -1)
                                
                                data.frame(.,medianEst)
                        }) %>% 
                        summarise(first(medianEst))
                
                colnames(medInc) <- c("NHOOD.ABBR","MEDIAN")
                
                readr::write_csv(x = medInc,path = "./2_inputs/medianIncome2014.csv")
        }
        
        medianIncome2014 <- readr::read_csv("./2_inputs/medianIncome2014.csv")
        
            
} # Note: this uses Pareto interpolation to estimate the median income value

medianIncome2014_plus <- {
        
        if(!file.exists("./2_inputs/medianIncome2014_plus.csv")){
                Sys.sleep(1)
                acs <- acs.fetch(endyear = 2014, span = 5,
                                 geography = geo_bg_plus,
                                 table.number = "B19001", col.names = "pretty")
                
                rnames <- acs@estimate %>% t()  %>% as.data.frame() %>% rownames() %>% as.data.frame()
                
                medInc <- 
                        acs@estimate %>% 
                        t() %>% 
                        as.data.frame() %>% 
                        bind_cols(rnames) %>% 
                        gather(geo,count,-.) 
                
                colnames(medInc) <- c("Range","Geo","HH_Count")
                
                medInc <- 
                        medInc %>% 
                        mutate(NewGeo = Geo) %>% 
                        mutate(NewGeo = replace(x = NewGeo, grep("\\s2",NewGeo, perl = TRUE),values = "CID")) %>% 
                        mutate(NewGeo = replace(x = NewGeo, grep("\\s1",NewGeo, perl = TRUE),values = "YTLS")) %>% 
                        mutate(NewGeo = replace(x = NewGeo, grep("^Seattle",NewGeo, perl = TRUE),values = "SEA")) %>% 
                        mutate(NewGeo = replace(x = NewGeo, grep("^King",NewGeo, perl = TRUE),values = "KC")) %>% 
                        select(-Geo) %>% 
                        select(Range,Geo = NewGeo,HH_Count) %>% 
                        mutate(Range = gsub("Household\\sIncome:\\s","",x = Range),
                               Geo = as.character(Geo)) %>% 
                        filter(!grepl("Total",Range)) %>% 
                        mutate(RangeNum = as.numeric(gsub("[^\\d]+", "", Range, perl=TRUE))) %>% 
                        mutate(RangeStr = as.character(RangeNum),
                               RangeStrLen = round(str_length(RangeStr)/2)) %>% 
                        mutate(RangeLower = ifelse(str_length(RangeStr) == 6,
                                                   RangeNum,
                                                   ifelse(str_length(RangeStr) < 6,
                                                          1,
                                                          str_extract(RangeStr,paste0("^\\d{",RangeStrLen,"}"))))) %>% 
                        mutate(RangeUpper = ifelse(str_length(RangeStr) == 6,
                                                   500000,
                                                   ifelse(str_length(RangeStr) < 6,
                                                          RangeNum,
                                                          str_extract(RangeStr,paste0("\\d{",RangeStrLen,"}$"))))) %>% 
                        select(RangeDesc = Range,
                               RangeLower,
                               RangeUpper,
                               Geo,
                               HH_Count) %>% 
                        group_by(Geo) %>% 
                        mutate(RangeLower = as.numeric(RangeLower),
                               RangeUpper = as.numeric(RangeUpper),
                               CumSum = cumsum(HH_Count)) %>% 
                        do({
                                cs <- .["CumSum"] %>% unlist() %>% as.numeric() %>% as.vector()
                                midRow <- findInterval(max(cs)/2, cs) + 1
                                
                                a <- .[midRow,"RangeLower"]
                                b <- .[midRow,"RangeUpper"]
                                Pa <- .[midRow-1,"CumSum"] / .[nrow(.),"CumSum"]
                                Pb <- .[midRow,"CumSum"] / .[nrow(.),"CumSum"]
                                
                                thedaNum <- log(1-Pa)-log(1-Pb)
                                thedaDen <- log(b)-log(a)
                                theda <- thedaNum/thedaDen
                                
                                kNum <- Pb - Pa
                                kDen <- 1 / (a^theda) - 1 / (b^theda)
                                k <- (kNum / kDen) ^ (1 / theda) 
                                
                                medianEst <- k * (2^(1 / theda))
                                
                                medianEst <- as.vector(medianEst) %>% round(digits = -1)
                                
                                data.frame(.,medianEst)
                        }) %>% 
                        summarise(first(medianEst))
                
                colnames(medInc) <- c("GEO","MEDIAN")
                
                readr::write_csv(x = medInc,path = "./2_inputs/medianIncome2014_plus.csv")
        }
        
        medianIncome2014_plus <- readr::read_csv(file = "./2_inputs/medianIncome2014_plus.csv")
        
} # Note: this uses Pareto interpolation to estimate the median income value

medHhInc_bar <- function(){
        
        blues <- RColorBrewer::brewer.pal(n = 9, name = "Blues")
        
        pal <- colorNumeric(palette = blues,
                            domain = c(round(min(medianIncome2014_plus$MEDIAN),-4),round(max(medianIncome2014_plus$MEDIAN),-3)))
        
        mypal <- medianIncome2014_plus$MEDIAN %>% 
                sort() %>% 
                pal()
       
        g <- ggplot(medianIncome2014_plus, aes(x=reorder(GEO, MEDIAN), y=MEDIAN)) +
                geom_bar(stat='identity',fill = mypal,alpha = .5) +
                geom_text(data = medianIncome2014_plus,label = paste0(medianIncome2014_plus$GEO,": $",medianIncome2014_plus$MEDIAN), hjust = 1.1) +
                scale_y_continuous(labels = scales::dollar) +
                coord_flip() +
                theme(
                        plot.margin = unit(c(-10,0,0,0), "points"),
                        panel.background = element_blank(),
                        panel.grid = element_blank(),
                        panel.grid.minor = element_blank(), 
                        panel.grid.major = element_blank(),
                        plot.background = element_blank(),
                        axis.title.y = element_blank(),
                        axis.title.x = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.y = element_blank(),
                        axis.text.x = element_text(),
                        strip.background = element_blank()
                        )
        
        gg <- grid.arrange(g, ncol=1, bottom = textGrob("Source: U.S. Census Bureau; American Community Survey,\n2014 American Community Survey 5-Year Estimates, Table B19001",x = 0.05,
                                                        hjust = 0, vjust = .5,
                                                        gp = gpar(fontface = "italic",
                                                                  fontsize = 10)))
        
        
}

medHhInc_bar_labels <- function(){
        
        blues <- RColorBrewer::brewer.pal(n = 9, name = "Blues") %>% .[3:9]
        
        pal <- colorNumeric(palette = blues,
                            domain = c(round(min(medianIncome2014_plus$MEDIAN),-4),round(max(medianIncome2014_plus$MEDIAN),-3)))
        
        mypal <- medianIncome2014_plus$MEDIAN %>% 
                sort() %>% 
                pal()
        
        ggplot(medianIncome2014_plus, aes(x=reorder(GEO, MEDIAN), y=MEDIAN)) +
                geom_bar(stat='identity',fill = mypal, alpha = 0) +
                geom_text(data = medianIncome2014_plus,label = medianIncome2014_plus$GEO, hjust = 1.5) +
                scale_y_continuous(labels = scales::dollar) +
                coord_flip() +
                theme(
                        panel.background = element_blank(),
                        panel.grid.minor = element_blank(), 
                        panel.grid.major = element_blank(),
                        plot.background = element_blank(),
                        axis.title.y = element_blank(),
                        axis.title.x = element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.y = element_blank(),
                        axis.text.x = element_text())
        
        
}

{

# set the plot result back: dev.set(which = 1)
        
# Print Bar Plot       
       
png('./4_webcontent/images/medHhInc_bar.png',width=400,height=400,res = 72,units="px",bg = "transparent")
medHhInc_bar()
dev.off()

# Print Bar Plot labels      

png('~/Pictures/medHhInc_bar_labels.png',width=170,height=170,res = 72,units="px",bg = "transparent")
medHhInc_bar_labels()
dev.off()

}

myLflt_medInc <- function(){
        
        shp_df <- geo_join(spatial_data = bg_rev,
                           data_frame = medianIncome2014,
                           by_sp = "NHOOD.ABBR",by_df = "NHOOD.ABBR")
        
        blues <- RColorBrewer::brewer.pal(n = 9, name = "Blues")
        
        pal <- colorNumeric(palette = blues,
                            domain = c(round(min(medianIncome2014_plus$MEDIAN),-4),round(max(medianIncome2014_plus$MEDIAN),-3)))
        
        pal <- colorNumeric(palette = blues,
                            domain = c(0,round(max(medianIncome2014_plus$MEDIAN),-3)))
        
        
        leaflet() %>% 
                addProviderTiles("CartoDB.PositronNoLabels") %>% 
                setView(lng = myCACbound_cntr@coords[[1]],lat = myCACbound_cntr@coords[[2]],zoom = 13) %>% 
                addPolygons(data = shp_df,
                            smoothFactor = 0,
                            stroke = F,
                            fillColor = ~pal(shp_df@data[["MEDIAN"]]), fillOpacity = .5,
                            popup = paste0("Median Household Income",": $",shp_df@data[["MEDIAN"]],"<br>",
                                           shp_df@data[["NHOOD.FULL"]])) %>% 
                addPolylines(data = nhoods_census_outline,
                             color = col2hex("white"), weight = 3, opacity = .5,stroke = T,
                             fill = F)  %>%
                addLegend(pal = pal, 
                          values = range(c(0,round(max(medianIncome2014_plus$MEDIAN),-3))),
                          labFormat = labelFormat(prefix = "$"),
                          position = "topright", 
                          title = "Median<br>Household<br>Income",
                          opacity = .5) 
}

myLflt_medInc() %>% saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/lflt_medHhInc.html")


# Census data functions

getBgData <- function(df,tableNumber) {
        
        tracts <- tract_rev@data$TRACTCE %>% as.numeric()
        
        geo <-
                geo.make(
                        state = "WA",
                        county = "King",
                        tract = tracts,
                        block.group = "*"
                )
        
        df <-
                acs.fetch(
                        endyear = 2014,
                        span = 5,
                        geography = geo,
                        # fetch the corresponding census data
                        table.number = tableNumber,
                        col.names = "pretty"
                )
        
        dim <- dimnames(df@estimate)[[2]]
        
        df <- {
                data.frame(
                        paste0(
                                str_pad(
                                        string = df@geography$state,
                                        width = 2,
                                        side = "left",
                                        pad = "0"
                                ),
                                str_pad(
                                        string = df@geography$county,
                                        width = 3,
                                        side = "left",
                                        pad = "0"
                                ),
                                str_pad(
                                        string = df@geography$tract,
                                        width = 6,
                                        side = "left",
                                        pad = "0"
                                ),
                                df@geography$blockgroup
                        ),
                        df@estimate[, dim]
                )
        }
        
        rownames(df) <- c()
        
        return(df)
}

cleanBgData <- function(df, colNames){
        
        colnames(df) <- colNames
        
        readr::write_csv(df,path = path)
}

# Poverty
        
below200PctPov <- {
 
        dfName <- "below200PctPov"
        
        path <- paste0("./2_inputs/",dfName,".csv")
        
        if(!file.exists(path)){
                
                # getBgData(tableNumber = "C17002") %>% glimpse()
                
                getBgData(tableNumber = "C17002") %>% 
                        cleanBgData(colNames = c("GEOID",
                                                 "Total",
                                                 "under50",
                                                 "x50to99",
                                                 "x100to124",
                                                 "x125to149",
                                                 "x150to184",
                                                 "x185to199",
                                                 "x200andOver"))   
        }
        
        readr::read_csv(file = path,col_types = "cnnnnnnnn")
}

below200PctPov%<>%
        mutate(PctBelow200 = 1 - (x200andOver/Total)) %>% 
        mutate(PctBelow200 = 100*PctBelow200) %>% 
        mutate(PctBelow200 = round(PctBelow200,0)) %>% 
        select(GEOID,PctBelow200)

# Income

hhIncomeMedian_bg <- {
        
        dfName <- "hhIncomeMedian_bg"
        
        path <- paste0("./2_inputs/",dfName,".csv")
        
        if(!file.exists(path)){
                
                # getBgData(tableNumber = "B19013") %>% glimpse()
                
                getBgData(tableNumber = "B19013") %>% 
                        cleanBgData(colNames = c("GEOID",
                                                 "MedianIncome"))   
        }
        
        readr::read_csv(file = path,col_types = "cn")
}

# DEMOGRAPHIC TEST --------------------------------------------------------------------------------

myLflt_test <- function(sp = bg_rev,df,colName,popupHeading,legTitle){
        
        test <- myGeoJoin(spatial_data = sp,data_frame = df)
        
        pal <- colorNumeric(palette = "Blues",domain = test@data[,colName])
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = blk_rev,
                            fill = FALSE,
                            color = col2hex("white"), weight = 1, opacity = .5,
                            group = "1. Blocks") %>% 
                addPolygons(data = test,
                            fillColor = ~pal(test@data[,colName]), fillOpacity = .5,
                            color = col2hex("white"), weight = 3, opacity = .75,dashArray = "5, 10",
                            group = "2. Block Groups",
                            popup = paste0("GEOID: ",test@data$GEOID,"<br>",
                                           popupHeading,": ",test@data[,colName],"<br>",
                                           test@data[,"NHOOD.FULL"])) %>% 
                addPolygons(data = tract_rev,
                            fill = FALSE,
                            color = col2hex("white"), weight = 5, opacity = .9,
                            group = "3. Tracts") %>% 
                addPolygons(data = myCACbound,
                            fill = FALSE,
                            weight = 3,
                            color = "#ff9900",
                            dashArray = "5, 5",
                            opacity = 1,
                            group = "4. CAC Boundary") %>%
                addLayersControl(overlayGroups = c("1. Blocks","2. Block Groups","3. Tracts","4. CAC Boundary"),
                                 options = layersControlOptions(collapsed = FALSE)) %>% 
                addLegend(pal = pal, 
                          values = range(test@data[,colName]), 
                          position = "topright", 
                          title = legTitle,
                          opacity = 1)
        
        
}


# Example:
myLflt_test(df = hhIncomeMedian_bg,
            colName = "MedianIncome",
            popupHeading = "Median HH Income",
            legTitle = "Median Household Income (2014)")

# ------------------------------------------------------------------------------------------------- 
#  

