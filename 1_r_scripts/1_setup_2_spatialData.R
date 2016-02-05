# CAC BASELINE CONDITIONS REPORT

# SETUP: SOURCE R SCRIPTS -------------------------------------------------------------------------

source("./1_r_scripts/1_setup_1_functions.R") # load the project settings, packages, and user-defined functions

sessionInfo()

# -------------------------------------------------------------------------------------------------

# SPATIAL DATA: CAC NEIGHBORHOODS, BAILEY-GATZERT BOUNDARY, MY CAC BOUNDARY, WATERBODIES, UVS -----

seaNhoods_CAC <- {
        make_seaNhoods_CAC <- function(){
                if(!file.exists("./2_inputs/seaNhoods_CAC.shp")){
                        
                        url <- "https://data.seattle.gov/download/2mbt-aqqx/application/zip" # save the URL for the neighborhood boundaries
                        
                        temp <- tempfile() # create a temporary file to hold the compressed download
                        
                        download(url, dest = temp, mode="wb") # download the file
                        
                        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
                        
                        seaNhoods <- readOGR(dsn = "./2_inputs/Neighborhoods/WGS84/",  # select YCC and adjacent neigborhood boundaries
                                              layer = "Neighborhoods") %>% 
                                spTransform(.,CRSobj = crs_proj)
                        
                        # select the Centreal Area Crescent neighborhoods from the small list
                        CAC_sm <- c("Atlantic", "First Hill", "International District", "Minor", "Pioneer Square", "Yesler Terrace") 
                        
                        seaNhoods[seaNhoods$S_HOOD %in% CAC_sm,] %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "seaNhoods_CAC",driver = "ESRI Shapefile")
                        
                }
                
                seaNhoods <<- readOGR(dsn = "./2_inputs/Neighborhoods/WGS84/",  # select YCC and adjacent neigborhood boundaries
                                     layer = "Neighborhoods") %>% 
                        spTransform(.,CRSobj = crs_proj)
                
                seaNhoods_outline <<- 
                        seaNhoods %>% 
                        as('SpatialLines') 
                
                seaNhoods_CAC <- readOGR(dsn = "./2_inputs/",layer = "seaNhoods_CAC") %>% 
                        spTransform(CRSobj = crs_proj)
                
                
        }
        
        seaNhoods_CAC <- make_seaNhoods_CAC()
        rm(make_seaNhoods_CAC)
        seaNhoods_CAC
        
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

waterbodies <- {
        
        make_waterbodies <- function(){
                
                if(!file.exists("./2_inputs/waterbodies.shp")){
                        # Tracts
                        
                        tracts_orig <- tigris::tracts(state = "WA", county = "King") %>% 
                                spTransform(CRSobj = crs_proj)
                        
                        tracts_big <- gUnaryUnion(tracts_orig) # simplify the tract polygons by merging them into one polygon
                        
                        # Waterbodies
                        
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
                        
                        waterbodies.shp[c(ps,intersect),] %>%  # refine the subset of the spatial data
                                spTransform(CRSobj = crs_proj) %>%  # change the CRS from geographic to projected
                                gUnaryUnion() %>%
                                mySptlPolyDF() %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "waterbodies",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }
                
                readOGR(dsn = "./2_inputs/",layer = "waterbodies") %>% 
                        spTransform(CRSobj = crs_proj)
                
        }
        
        waterbodies <- make_waterbodies()
        
        rm(make_waterbodies)
        
        waterbodies
}

seaUvs <- {
        make_seaUvs <- function(){
                if(!file.exists("./2_inputs/Urban_Villages/StatePlane/DPD_uvmfg_polygon.shp")){
                        url <- "https://data.seattle.gov/download/ugw3-tp9e/application/zip" # save the URL for the neighborhood boundaries
                        
                        temp <- tempfile() # create a temporary file to hold the compressed download
                        
                        download(url, dest = temp, mode="wb") # download the file
                        
                        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
                        
                }
                
                
                readOGR(dsn = "2_inputs/Urban_Villages/StatePlane/",layer = "DPD_uvmfg_polygon") %>% 
                        spTransform(CRSobj = crs_proj) %>% 
                        wtr_clip(wtr = waterbodies) %>% 
                        writeOGR(dsn = "./2_inputs/",layer = "seaUvs",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                
                
        }
        
        make_seaUvs()

        rm(make_seaUvs)
        
        seaUvs <- readOGR(dsn = "./2_inputs/",layer = "seaUvs") %>% 
                spTransform(CRSobj = crs_proj)
        
        seaUvs_outline <<- 
                seaUvs %>% 
                as('SpatialLines') 
        
        seaUvs
        
        
}

seaUvs_ycc <- {
        
        
        make_seaUvs_ycc <- function(){
                
                seaUvs_ycc <- 
                        seaUvs[grepl("China*|Pioneer*|First*|12th*|23rd*|Pike|Capitol|Madison",seaUvs@data$UV_NAME),]
                
                if(file.exists("./2_inputs/seaUvs_ycc.shp")){
                        writeOGR(obj = seaUvs_ycc,
                                 dsn = "./2_inputs/",
                                 layer = "seaUvs_ycc",
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)
                }  
                
                return(seaUvs_ycc)
                
        }
        
        seaUvs_ycc <- make_seaUvs_ycc()
        
        rm(make_seaUvs_ycc)
        
        seaUvs_ycc
        
        
}

seaUvs_ycc_rev <- {
        
        make_seaUvs_ycc_rev <- function(){
                
                
                seaUvs_ycc_rev <-
                        readOGR(dsn = "./2_inputs/",layer = "seaUvs_ycc_rev") %>% 
                        spTransform(CRSobj = crs_proj) %>% 
                        .[,1:13]
                
                if(!file.exists("./2_inputs/seaUvs_ycc_rev.shp")){
                        writeOGR(obj = seaUvs_ycc_rev,dsn = "./2_inputs/",layer = "seaUvs_ycc_rev",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }
                
                return(seaUvs_ycc_rev)
        }
        
        seaUvs_ycc_rev <- make_seaUvs_ycc_rev()
        
        rm(make_seaUvs_ycc_rev)
        
        seaUvs_ycc_rev
        
        
}

seaAcsUvs <- {
        readxl::read_excel(path = "./2_inputs/dpdd017073.xlsx") %>% 
                mutate(TRACT_10 = str_pad(TRACT_10,width = 6, pad = "0"))
        }

# Note: these boundaries are editted versions of the Seattle Urban Village bounaries,
# which can be downloaded here: https://data.seattle.gov/download/ugw3-tp9e/application/zip

seaUVs_CAC <- {
        
        make_seaUVs_CAC <- function(){
                if(!file.exists("./2_inputs/seaUVs_CAC.shp")){
                        shp <- readOGR(dsn = "./2_inputs/",layer = "seaUVs_CAC") %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                .[order(.@data$UV_NAME),]
                        
                        shp@data %<>%
                                cbind(myNhoods_geo[order(myNhoods_geo$NHOOD.FULL),]) %>% 
                                select(NHOOD.FULL,NHOOD.ABBR,everything())
                        
                        writeOGR(obj = shp,dsn = "./2_inputs/",
                                 layer = "seaUVs_CAC",
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)
                        
                }
                
                readOGR(dsn = "./2_inputs/",layer = "seaUVs_CAC") %>% 
                        spTransform(CRSobj = crs_proj)
        }
        
        seaUVs_CAC <- make_seaUVs_CAC()
        
        rm(make_seaUVs_CAC)
        
        seaUVs_CAC
        
        
        
}

seaUVs_CAC_outline <- 
        seaUVs_CAC %>% 
        as('SpatialLines') 



# SPATIAL DATA: CENSUS TRACTS, BLOCK GROUPS, BLOCKS -----------------------------------------------

# all Census geometries are TIGER shapefiles that are clipped to remove waterbodies (for visual clarity)

tract_sea <- {
        
        make_tract_sea <- function(){
                
                
                if(!file.exists("./2_inputs/tracts.shp")){
                        tracts_orig <- 
                                tigris::tracts(state = "WA", county = "King") %>% 
                                spTransform(CRSobj = crs_proj)
                        
                        wtr_clip(orig = tracts_orig,wtr = waterbodies) %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "tracts",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }

                if(!file.exists("./2_inputs/tract_sea.shp")){
                        seaTrts <- 
                                readxl::read_excel(path = "./2_inputs/dpdd017073.xlsx") %>% 
                                mutate(TRACT_10 = str_pad(TRACT_10,width = 6, pad = "0")) %>% 
                                select(TRACT_10) %>% 
                                unique() %>% 
                                unlist()
                        
                        readOGR(dsn = "./2_inputs/",layer = "tracts") %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                .[.[["TRACTCE"]] %in% seaTrts,] %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "tract_sea",driver = "ESRI Shapefile", overwrite_layer = TRUE)
                        
                }
                
                readOGR(dsn = "./2_inputs/",layer = "tract_sea") %>% 
                        spTransform(CRSobj = crs_proj)
                
        }
        
        tract_sea <- make_tract_sea()
        
        rm(make_tract_sea)
        
        tract_sea_outline <<- 
                tract_sea %>% 
                as('SpatialLines')
        
        tract_sea
}

tract_CAC <- {
        
        make_tract_CAC <- function(){
                
                if(!file.exists("./2_inputs/tracts.shp")){
                        tracts_orig <- 
                                tigris::tracts(state = "WA", county = "King") %>% 
                                spTransform(CRSobj = crs_proj)
                        
                        wtr_clip(orig = tracts_orig,wtr = waterbodies) %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "tracts",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }
                
                
                if(!file.exists("./2_inputs/tracts_CAC.shp")){
                        
                        seaTrts <- 
                                readxl::read_excel(path = "./2_inputs/dpdd017073.xlsx") %>% 
                                mutate(TRACT_10new = str_pad(TRACT_10,width = 6, pad = "0")) %>% 
                                select(TRACT_10 = TRACT_10new) %>% 
                                unique() %>% 
                                unlist()
                        
                        
                        tracts <- 
                                readOGR(dsn = "./2_inputs/",layer = "tracts") %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                .[.[["TRACTCE"]] %in% seaTrts,]
                        
                                gContains(myCACbound,gCentroid(tracts, byid = TRUE),byid = TRUE) %>%
                                which(.==1) %>% 
                                tracts[.,] %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "tract_CAC",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }
                
                readOGR(dsn = "./2_inputs/",layer = "tract_CAC") %>% 
                        spTransform(CRSobj = crs_proj)
                
                
        }
        
        tract_CAC <- make_tract_CAC()
        
        rm(make_tract_CAC)
        
        tract_CAC
        
} 


bg_sea <- {
        
        make_bg_sea <- function(){
                
                
                if(!file.exists("./2_inputs/bg.shp")){
                        bg_orig <- 
                                tigris::block_groups(state = "WA", county = "King") %>% 
                                spTransform(CRSobj = crs_proj)
                        
                        wtr_clip(orig = bg_orig,wtr = waterbodies) %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "bg",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }
                
                if(!file.exists("./2_inputs/bg_sea.shp")){
                        seaTrts <- 
                                readxl::read_excel(path = "./2_inputs/dpdd017073.xlsx") %>% 
                                mutate(TRACT_10 = str_pad(TRACT_10,width = 6, pad = "0")) %>% 
                                select(TRACT_10) %>% 
                                unique() %>% 
                                unlist()
                        
                        readOGR(dsn = "./2_inputs/",layer = "bg") %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                .[.[["TRACTCE"]] %in% seaTrts,] %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "bg_sea",driver = "ESRI Shapefile", overwrite_layer = TRUE)
                        
                }
                
                bg_sea <- 
                        readOGR(dsn = "./2_inputs/",layer = "bg_sea") %>% 
                        spTransform(CRSobj = crs_proj)
                
                bg_sea
                
                
        }
        
        bg_sea <- make_bg_sea()
        
        rm(make_bg_sea)
        
        bg_sea_outline <<- 
                bg_sea %>% 
                as('SpatialLines')
        
        bg_sea

}

bg_CAC <- {
        
        make_bg_CAC <- function(){
                
                if(!file.exists("./2_inputs/bg.shp")){
                        bg_orig <- 
                                tigris::block_groups(state = "WA", county = "King") %>% 
                                spTransform(CRSobj = crs_proj)
                        
                        wtr_clip(orig = bg_orig,wtr = waterbodies) %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "bg",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }
                
                
                if(!file.exists("./2_inputs/bg_CAC.shp")){
                        bg <- 
                                readOGR(dsn = "./2_inputs/",layer = "bg") %>% 
                                spTransform(CRSobj = crs_proj)
                        
                                bg[bg@data$TRACTCE %in% c(tract_CAC@data$TRACTCE),] %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "bg_CAC",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }
                
                readOGR(dsn = "./2_inputs/",layer = "bg_CAC") %>% 
                        spTransform(CRSobj = crs_proj)
                
                
        }
        
        bg_CAC <- make_bg_CAC()
        
        rm(make_bg_CAC)
        
        bg_CAC
        
} 



blk_sea <- {
        
        make_blk_sea <- function(){
                
                if(!file.exists("./2_inputs/blk_sea.shp")){
                       
                        seaTrts <- 
                                readxl::read_excel(path = "./2_inputs/dpdd017073.xlsx") %>% 
                                mutate(TRACT_10 = str_pad(TRACT_10,width = 6, pad = "0")) %>% 
                                select(TRACT_10) %>% 
                                unique() %>% 
                                unlist()
                        
                        blk_sea <- 
                                tigris::blocks(state = "WA", county = "King") %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                .[.@data$TRACTCE10 %in% seaTrts,]
                        
                        # NOTE: this step takes a long time!
                        wtr_clip(orig = blk_sea,wtr = waterbodies) %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "blk_sea",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }
                
                blk_sea <- 
                        readOGR(dsn = "./2_inputs/",layer = "blk_sea") %>% 
                        spTransform(CRSobj = crs_proj)
                
                blk_sea
                
                
        }
        
        blk_sea <- make_blk_sea()
        
        rm(make_blk_sea)
        
        blk_sea
        
}

blk_CAC <- {
        
        make_blk_CAC <- function(){
                
                if(!file.exists("./2_inputs/blk.shp")){
                        
                        
                        blk_sea <- 
                                tigris::blocks(state = "WA", county = "King") %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                .[.@data$TRACTCE10 %in% seaAcsUvs$TRACT_10,]
                        
                        # NOTE: this step takes a long time!
                        wtr_clip(orig = blk_sea,wtr = waterbodies) %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "blk_sea",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }
                
                
                if(!file.exists("./2_inputs/blk_CAC.shp")){
                        blk <- 
                                readOGR(dsn = "./2_inputs/",layer = "blk") %>% 
                                spTransform(CRSobj = crs_proj)
                        
                        blk[blk@data$TRACTCE %in% c(tract_CAC@data$TRACTCE),] %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "blk_CAC",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }
                
                readOGR(dsn = "./2_inputs/",layer = "blk_CAC") %>% 
                        spTransform(CRSobj = crs_proj)
                
                
        }
        
        blk_CAC <- make_blk_CAC()
        
        rm(make_blk_CAC)
        
        blk_CAC
        
} 

# -------------------------------------------------------------------------------------------------

# SPATIAL DATA: TRACT SELECTION, REVISION OF BLOCK GROUP + BLOCK SELECTION ------------------------

# I decided to connect the urban village identities to cesus geographies using the housing units count
# from the 2010 Census. The function I wrote to do this also has the ability to do this calculation
# using tracts (as the census geometry), or using population as the determining variable.

# For the comparison between using housing units and population, run the R script below
source("./1_r_scripts/1_setup_uv2CensusDiff.R",echo = TRUE)

# Attribute Urban Village IDs to all block groups
# NOTE: this function can be used for either tract or block-group level attribution,
# and either housing units or population can be used as the determining variable.
# see '1_setup_1_functions.R' for the details of this function. 

bg_uvs <- {
        
        if(!file.exists("./2_inputs/bg_uvs.shp")){
                make_bg_uvs <- function(){
                        bg_uvs <- UV2Census(tract = FALSE)
                        writeOGR(obj = bg_uvs,
                                 dsn = "./2_inputs/",
                                 layer = "bg_uvs",
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)
                }
                
                make_bg_uvs()
                
                rm(make_bg_uvs)
                        
        }
        
        bg_uvs <- readOGR(dsn = "./2_inputs/",layer = "bg_uvs") %>% 
                spTransform(CRSobj = crs_proj)
}

# Join the Urban Village data with the census block polygons

blk_uvs <- {
        
        if(!file.exists("./2_inputs/blk_uvs.shp")){
                make_blk_uvs <- function(){
                        blk_uvs <-
                                blk_sea %>% 
                                geo_join(data_frame = seaAcsUvs,
                                         by_sp = "GEOID10",
                                         by_df = "GEOID10") %>% 
                                geo_join(data_frame = hu,
                                         by_sp = "GEOID10",
                                         by_df = "GEO.id2") %>% 
                                .[!is.na(.@data$D001),] %>% 
                                .[.@data$D001 > 0,] %>% 
                                .[!is.na(.@data$URBAN_VILLAGE_NAME),] %>% 
                                .[.@data$URBAN_VILLAGE_TYPE %!in% c("Manufacturing Industrial","Outside Villages"),]
                        
                        # Normalize the Housing Units count (0 to 1 scale)
                        blk_uvs@data %<>% 
                                mutate(RANGE = D001) %>% 
                                mutate_each_(funs(norm0to1), vars = "RANGE")
                        
                        writeOGR(obj = blk_uvs,dsn = "./2_inputs/",layer = "blk_uvs",
                                 driver = "ESRI Shapefile",overwrite_layer = TRUE)
                        
                }
                
                make_blk_uvs()
                
                rm(make_blk_uvs)
        }
        
        blk_uvs <- 
                readOGR(dsn = "./2_inputs/",layer = "blk_uvs") %>% 
                spTransform(CRSobj = crs_proj)
        
        
        
        
        
        
}

tract_ycc <- {
        make_tract_ycc <- function(){

                if(!file.exists("./2_inputs/tract_ycc.shp")){
                        
                        TRACTCE_ycc <- 
                                c("007900", "008600", "007401", "007500", "008300", "008400", 
                                  "008500", "008700", "008800", "009000", "009100", "007600", 
                                  "007402", "009200")
                        
                        tract_ycc <- 
                                tract_sea[tract_sea@data$TRACTCE %in% TRACTCE_ycc,]
                        
                        writeOGR(obj = tract_ycc,
                                 dsn = "./2_inputs/",
                                 layer = "tract_ycc",
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)
                }
                
                readOGR(dsn = "./2_inputs/",layer = "tract_ycc") %>% 
                        spTransform(CRSobj = crs_proj)
        }
        
        tract_ycc <- make_tract_ycc()
        
        rm(make_tract_ycc)
        
        tract_ycc
        
}
        
bg_ycc <- {
        
        make_bg_ycc <- function(){
                
                bg_ids <- 
                        c("530330079005","530330074014","530330084001","530330084003",
                          "530330079001","530330079004","530330074021","530330088001",
                          "530330075001","530330074012","530330065003","530330065002",
                          "530330088003","530330085002","530330085003","530330086003",
                          "530330087003","530330088002","530330085001","530330089003",
                          "530330090001","530330091001","530330091002","530330092001",
                          "530330092002","530330082002","530330082003","530330083001",
                          "530330083002","530330074011","530330074013","530330075002",
                          "530330075003","530330075004","530330075005","530330076001",
                          "530330076002","530330076003","530330077003","530330077004",
                          "530330086001","530330090002","530330086002","530330074023",
                          "530330074022","530330084002","530330079002","530330087002",
                          "530330087001","530330079003")
                
                df <- 
                        bg_uvs@data[bg_uvs@data$GEOID %in% bg_ids,c("GEOID","UV")]
                
                bg_ycc <- 
                        bg_sea[bg_sea@data$GEOID %in% bg_ids,]
                
                bg_ycc <- 
                        geo_join(spatial_data = bg_ycc,
                                 data_frame = df,
                                 by_sp = "GEOID",
                                 by_df = "GEOID")
                
                # Change the UV for the block groups who housing unit count UV was "unintuitive"
                
                bg_ycc@data[bg_ycc@data$GEOID %in% c("530330079003",
                                                     "530330079002",
                                                     "530330079001"),"UV"] <- "23rd & Union-Jackson"
                
               
                
                bg_ycc
          
        }
        
        bg_ycc <- make_bg_ycc()
        
        rm(make_bg_ycc)
        
        view_bg_ycc <- function(){
                
                popup <- paste0("GEOID: ",bg_ycc@data$GEOID, "<br>",
                                "Urban Village: ", bg_ycc@data$UV)
                pal <- colorFactor(palette = "Set2",domain = bg_ycc@data$UV)
                
                myLfltShiny() %>% 
                        addPolygons(data = bg_ycc,
                                    popup = popup,
                                    color = "white", opacity = 1, weight = 1.5,
                                    fillColor = ~pal(bg_ycc@data$UV), fillOpacity = .75) %>% 
                        addLegend(title = "Block Groups (by Urban Village)",
                                  position = c("topright"),pal = pal, values = unique(bg_ycc@data$UV))
        }
        
        
        view_bg_ycc() %>% 
        htmlwidgets::saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/lflt_bg_ycc.html")
        
        
        bg_ycc

}



# -------------------------------------------------------------------------------------------------

# ARCHIVE -----------------------------------------------------------------------------------------

# Select tracts where the majority of the population lives within the CAC bound and map the result

tract_pop <- {
        
        make_tract_pop <- function(){
                
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
        
        tract_pop <- make_tract_pop()
        
        rm(make_tract_pop)
        
        tract_pop
        
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
        
        make_tract_rev <- function(){
                if(!file.exists("./2_inputs/tract_rev.shp")){
                        
                        CapHill <- "007900" # save the Tract ID for Tract 79
                        
                        new_include <- c(tract_pop@data$TRACTCE,CapHill) 
                        
                        tract_CAC[tract_CAC@data$TRACTCE %in% new_include,] %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "tract_rev",driver = "ESRI Shapefile")
                }
                
                readOGR(dsn = "./2_inputs/",layer = "tract_rev") %>% 
                        spTransform(CRSobj = crs_proj)
        }
        
        tract_rev <- make_tract_rev()
        
        rm(make_tract_rev)
        
        tract_rev
        
        
}

bg_rev <- {
        
        make_bg_rev <- function(){
                
                if(!file.exists("./2_inputs/bg_rev.shp")){
                        
                        CapHill <- "007900" # save the Tract ID for Tract 79
                        
                        new_include <- c(tract_pop@data$TRACTCE,CapHill) 
                        
                        bg_rev <- bg_CAC[bg_CAC@data$TRACTCE %in% new_include,] 
                        
                        writeOGR(obj = bg_rev,dsn = "./2_inputs/",layer = "bg_rev",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }
                
                readOGR(dsn = "./2_inputs/",layer = "bg_rev") %>% 
                        spTransform(CRSobj = crs_proj)
        }
        
        bg_rev <- make_bg_rev()
        
        rm(make_bg_rev)
        
        bg_rev
        
}

blk_rev <- {
        
        make_blk_rev <- function(){
                if(!file.exists("./2_inputs/blk_rev.shp")){
                        
                        CapHill <- "007900" # save the Tract ID for Tract 79
                        
                        new_include <- c(tract_pop@data$TRACTCE,CapHill) 
                        
                        blk_CAC_noWtrbds[blk_CAC_noWtrbds@data$TRACTCE %in% new_include,] %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "blk_rev",driver = "ESRI Shapefile",
                                         overwrite_layer = TRUE)
                }
                
                readOGR(dsn = "./2_inputs/",layer = "blk_rev") %>% 
                        spTransform(CRSobj = crs_proj)
        }
        
        
        blk_rev <- make_blk_rev()
        
        rm(make_blk_rev)
        
        blk_rev
        
        
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

# SPATIAL DATA: SCALES OF ANALYSIS 

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
        
        make_tract_rev <- function(){
                
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
        
        tract_rev <- make_tract_rev()
        
        rm(make_tract_rev)
        
        tract_rev
        
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
        
        make_bg_rev <- function(){
                
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
        } # Eliminate interpolygonal slivers
        
        bg_rev <- make_bg_rev()
        
        rm(make_bg_rev)
        
        bg_rev
        
        
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


bg_hood_cntrs <- {
        
        make_bg_hood_cntrs <- function(){
                
                cntr <- bg_rev %>% 
                        raster::aggregate(by = "NHOOD.ABBR") %>% 
                        gCentroid(byid = TRUE)
                
                rn <- row.names(cntr)
                
                df <- bg_rev@data %>% 
                        mutate(NHOOD.ABBR = as.factor(NHOOD.ABBR)) %>% 
                        group_by(NHOOD.ABBR) %>% 
                        summarise(NHOOD = first(NHOOD.ABBR)) %>% 
                        dplyr::select(NHOOD) %>% 
                        as.data.frame()
                
                rownames(df) <- rn
                
                bg_hood_cntrs <-
                        SpatialPointsDataFrame(coords = cntr,data = df) %>% 
                        spTransform(CRSobj = crs_proj)
                
        }
        
        bg_hood_cntrs <- make_bg_hood_cntrs()
        
        rm(make_bg_hood_cntrs)
        
        bg_hood_cntrs
    
} # For labeling the neighborhoods


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


myLflt_uvs <- function(){
        pal <- colorFactor(palette = "Set2",
                           domain = seaUVs_CAC@data$NHOOD_ABBR)
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(group = "Neighborhoods",
                            data = seaUVs_CAC, 
                            fillColor = ~pal(seaUVs_CAC@data$NHOOD_ABBR), fillOpacity = .5,
                            stroke = F,
                            popup = paste0(seaUVs_CAC@data$NHOOD_ABBR)) %>% 
                addPolylines(group = "Neighborhoods",
                             data = seaUVs_CAC_outline,
                             color = "white", weight = 1, opacity = .75) %>% 
                addPolygons(group = "Census Geometry",
                            data = blk_rev,
                            fill = FALSE,
                            color = col2hex("dodgerblue"), weight = 1, opacity = .5) %>% 
                addPolygons(group = "Census Geometry",
                            data = bg_rev,
                            fill = FALSE,
                            color = col2hex("dodgerblue"), weight = 3, opacity = .75) %>% 
                addPolygons(group = "Census Geometry",
                            data = tract_rev,
                            fill = FALSE,
                            color = col2hex("dodgerblue"), weight = 5, opacity = .9) %>% 
                addLayersControl(overlayGroups = c("Neighborhoods","Census Geometry"),
                                 options = layersControlOptions(collapsed = FALSE))
        
        
}

# myLflt_uvs()
      
