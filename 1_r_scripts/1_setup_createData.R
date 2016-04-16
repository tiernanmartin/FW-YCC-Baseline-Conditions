# CAC BASELINE CONDITIONS REPORT

# SETUP: SOURCE R SCRIPTS -------------------------------------------------------------------------

source("./1_r_scripts/1_setup_1_functions.R") # load the project settings, packages, and user-defined functions


# -------------------------------------------------------------------------------------------------

# SPATIAL DATA: CAC NEIGHBORHOODS, BAILEY-GATZERT BOUNDARY, MY CAC BOUNDARY, WATERBODIES, UVS -----

seaNhoods_CAC <- {
  if (!file.exists("./2_inputs/seaNhoods_CAC.shp")) {
    make_seaNhoods_CAC <- function() {
            url <- "https://data.seattle.gov/download/2mbt-aqqx/application/zip" # save the URL for the neighborhood boundaries
            
            temp <- tempfile() # create a temporary file to hold the compressed download
            
            download(url, dest = temp, mode="wb") # download the file
            
            unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
            
            # select the Centreal Area Crescent neighborhoods from the small list
            CAC_sm <- c("Atlantic", "First Hill", "International District", "Minor", "Pioneer Square", "Yesler Terrace") 
            
            seaNhoods_CAC <- readOGR(dsn = "./2_inputs/Neighborhoods/WGS84/",  # select YCC and adjacent neigborhood boundaries
                                 layer = "Neighborhoods") %>% 
                    spTransform(.,CRSobj = crs_proj) %>% 
                    .[.@data$S_HOOD %in% CAC_sm,]
            
      writeOGR(obj = seaNhoods_CAC, dsn = "./2_inputs/", 
        layer = "seaNhoods_CAC", driver = "ESRI Shapefile")
      
      colnames(seaNhoods_CAC@data) %>% data_frame() %>% 
        write_csv(path = "./2_inputs/seaNhoods_CAC_cn.csv")
      
      view_seaNhoods_CAC <<- function() {
        
              
        myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
        
        pal <- colorFactor(palette = "Set2", domain = as.factor(seaNhoods_CAC@data$S_HOOD))
        
        myLflt() %>% addPolygons(data = seaNhoods_CAC, 
          smoothFactor = 0, color = col2hex("white"), 
          weight = 1.5, opacity = 0.5, fillColor = pal(as.factor(seaNhoods_CAC@data$S_HOOD)), 
          fillOpacity = 0.75) %>% addLegend(position = "topright", 
          title = "CHANGE_THIS", pal = pal, values = as.factor(seaNhoods_CAC@data$S_HOOD), 
          opacity = 0.75, labFormat = labelFormat())
      }
      seaNhoods_CAC
      
    }
    
    seaNhoods_CAC <- make_seaNhoods_CAC()
    rm(make_seaNhoods_CAC)
    seaNhoods_CAC
  } else {
    make_seaNhoods_CAC <- function() {
      seaNhoods_CAC <- readOGR(dsn = "./2_inputs/", layer = "seaNhoods_CAC") %>% 
        spTransform(CRSobj = crs_proj)
      cn <- read_csv("./2_inputs/seaNhoods_CAC_cn.csv") %>% 
        unlist(use.names = FALSE)
      
      colnames(seaNhoods_CAC@data) <- cn
      view_seaNhoods_CAC <<- function() {
              
              
              myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
              
              pal <- colorFactor(palette = "Set2", domain = as.factor(seaNhoods_CAC@data$S_HOOD))
              
              myLflt() %>% addPolygons(data = seaNhoods_CAC, 
                                       smoothFactor = 0, color = col2hex("white"), 
                                       weight = 1.5, opacity = 0.5, fillColor = pal(as.factor(seaNhoods_CAC@data$S_HOOD)), 
                                       fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                         title = "CHANGE_THIS", pal = pal, values = as.factor(seaNhoods_CAC@data$S_HOOD), 
                                                                         opacity = 0.75, labFormat = labelFormat())
      }
      seaNhoods_CAC
    }
    seaNhoods_CAC <- make_seaNhoods_CAC()
    rm(make_seaNhoods_CAC)
    seaNhoods_CAC
  }
}

bgatz <- {
  if (!file.exists("./2_inputs/bgatz.shp")) {
    make_bgatz <- function() {
            url <- "https://www.seattleschools.org/UserFiles/Servers/Server_543/File/District/Departments/Enrollment%20Planning/Maps/gisdata/SPS_AttendanceAreasAndSchools_Shapefiles_2015_2016.zip" # save the URL for the neighborhood boundaries
            
            temp <- tempfile() # create a temporary file to hold the compressed download
            
            download(url, dest = temp, mode="wb") # download the file
            
            unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
            
            readOGR(dsn = "./2_inputs/SPS_AttendanceAreasAndSchools_Shapefiles_2015_2016_v2/",
                    layer = "sps_attendance_area_ES_2015_2016") %>% 
                    .[.@data$ES_ZONE == "Gatzert",] %>%
                    spTransform(., CRSobj = crs_proj) %>% 
                    writeOGR(dsn = "./2_inputs/",layer = "bgatz",driver = "ESRI Shapefile")
      bgatz <- readOGR(dsn = "./2_inputs/",layer = "bgatz") %>% 
              spTransform(CRSobj = crs_proj)
      
      colnames(bgatz@data) %>% data_frame() %>% write_csv(path = "./2_inputs/bgatz_cn.csv")
      
      view_bgatz <<- function() {
        
        myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
        
        # pal <- colorNumeric(palette = myYlOrRd, domain = range(CHANGE_THIS))
        pal <- colorFactor(palette = "Set2", domain = as.factor(bgatz@data$ES_ZONE))
        
        myLflt() %>% addPolygons(data = bgatz, 
          smoothFactor = 0, color = col2hex("white"), 
          weight = 1.5, opacity = 0.5, fillColor = pal(bgatz@data$ES_ZONE), 
          fillOpacity = 0.75) %>% addLegend(position = "topright", 
          title = "CHANGE_THIS", pal = pal, values = as.factor(bgatz@data$ES_ZONE), 
          opacity = 0.75, labFormat = labelFormat())
      }
      bgatz
      
    }
    
    bgatz <- make_bgatz()
    rm(make_bgatz)
    bgatz
  } else {
    make_bgatz <- function() {
      bgatz <- readOGR(dsn = "./2_inputs/", layer = "bgatz") %>% 
        spTransform(CRSobj = crs_proj)
      cn <- read_csv("./2_inputs/bgatz_cn.csv") %>% unlist(use.names = FALSE)
      
      colnames(bgatz@data) <- cn
      view_bgatz <<- function() {
              
              myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
              
              # pal <- colorNumeric(palette = myYlOrRd, domain = range(CHANGE_THIS))
              pal <- colorFactor(palette = "Set2", domain = as.factor(bgatz@data$ES_ZONE))
              
              myLflt() %>% addPolygons(data = bgatz, 
                                       smoothFactor = 0, color = col2hex("white"), 
                                       weight = 1.5, opacity = 0.5, fillColor = pal(bgatz@data$ES_ZONE), 
                                       fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                         title = "CHANGE_THIS", pal = pal, values = as.factor(bgatz@data$ES_ZONE), 
                                                                         opacity = 0.75, labFormat = labelFormat())
      }
      bgatz
    }
    bgatz <- make_bgatz()
    rm(make_bgatz)
    bgatz
  }
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
  if (!file.exists("./2_inputs/seaUvs.shp")) {
    make_seaUvs <- function() {
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
            
      seaUvs <- readOGR(dsn = "./2_inputs/",layer = "seaUvs") %>% spTransform(CRSobj = crs_proj)
      
      colnames(seaUvs@data) %>% data_frame() %>% write_csv(path = "./2_inputs/seaUvs_cn.csv")
      
      view_seaUvs <<- function() {
        
        myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
        
        # pal <- colorNumeric(palette = myYlOrRd, domain = range(CHANGE_THIS))
        pal <- colorFactor(palette = 'Set2', domain = as.factor(seaUvs@data$UV_NAME))
        
        myLflt() %>% addPolygons(data = seaUvs, 
          smoothFactor = 0, color = col2hex("white"), 
          weight = 1.5, opacity = 0.5, fillColor = pal(seaUvs@data$UV_NAME), 
          fillOpacity = 0.75) %>% addLegend(position = "topright", 
          title = "CHANGE_THIS", pal = pal, values = range(seaUvs@data$UV_NAME), 
          opacity = 0.75, labFormat = labelFormat())
        
        # myLflt() %>% addPolygons(data = CHANGE_THIS, smoothFactor =
        # 0, color = col2hex('white'),weight = 1.5,opacity = .5,
        # fillColor = pal(CHANGE_THIS),fillOpacity = .75) %>%
        # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
        # = pal, values = as.factor(CHANGE_THIS), opacity = .75,
        # labFormat = labelFormat())
        
      }
      seaUvs
      
    }
    
    seaUvs <- make_seaUvs()
    rm(make_seaUvs)
    seaUvs
  } else {
    make_seaUvs <- function() {
      seaUvs <- readOGR(dsn = "./2_inputs/", layer = "seaUvs") %>% 
        spTransform(CRSobj = crs_proj)
      cn <- read_csv("./2_inputs/seaUvs_cn.csv") %>% unlist(use.names = FALSE)
      
      colnames(seaUvs@data) <- cn
      view_seaUvs <<- function() {
              
              myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
              
              # pal <- colorNumeric(palette = myYlOrRd, domain = range(CHANGE_THIS))
              pal <- colorFactor(palette = 'Set2', domain = as.factor(seaUvs@data$UV_NAME))
              
              myLflt() %>% addPolygons(data = seaUvs, 
                                       smoothFactor = 0, color = col2hex("white"), 
                                       weight = 1.5, opacity = 0.5, fillColor = pal(seaUvs@data$UV_NAME), 
                                       fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                         title = "CHANGE_THIS", pal = pal, values = range(seaUvs@data$UV_NAME), 
                                                                         opacity = 0.75, labFormat = labelFormat())
              
              # myLflt() %>% addPolygons(data = CHANGE_THIS, smoothFactor =
              # 0, color = col2hex('white'),weight = 1.5,opacity = .5,
              # fillColor = pal(CHANGE_THIS),fillOpacity = .75) %>%
              # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
              # = pal, values = as.factor(CHANGE_THIS), opacity = .75,
              # labFormat = labelFormat())
              
      }
      seaUvs
    }
    seaUvs <- make_seaUvs()
    rm(make_seaUvs)
    seaUvs
  }
}

seaUvs_ycc <- {
        if (!file.exists("./2_inputs/seaUvs_ycc.shp")) {
                make_seaUvs_ycc <- function() {
                        if(!file.exists("./2_inputs/Urban_Villages/StatePlane/DPD_uvmfg_polygon.shp")){
                                url <- "https://data.seattle.gov/download/ugw3-tp9e/application/zip" # save the URL for the neighborhood boundaries
                                
                                temp <- tempfile() # create a temporary file to hold the compressed download
                                
                                download(url, dest = temp, mode="wb") # download the file
                                
                                unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
                                
                        }
                        
                        
                        readOGR(dsn = "2_inputs/Urban_Villages/StatePlane/",layer = "DPD_uvmfg_polygon") %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                wtr_clip(wtr = waterbodies) %>% 
                                .[grepl("China*|Pioneer*|First*|12th*|23rd*|Pike|Capitol|Madison",.@data$UV_NAME),] %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "seaUvs_ycc",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                        
                        seaUvs_ycc <- readOGR(dsn = "./2_inputs/",layer = "seaUvs_ycc") %>% spTransform(CRSobj = crs_proj)
                        
                        colnames(seaUvs_ycc@data) %>% data_frame() %>% write_csv(path = "./2_inputs/seaUvs_ycc_cn.csv")
                        
                        view_seaUvs_ycc <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                # pal <- colorNumeric(palette = myYlOrRd, domain = range(CHANGE_THIS))
                                pal <- colorFactor(palette = 'Set2', domain = as.factor(seaUvs_ycc@data$UV_NAME))
                                
                                # myLflt() %>% addPolygons(data = seaUvs_ycc, 
                                #                          smoothFactor = 0, color = col2hex("white"), 
                                #                          weight = 1.5, opacity = 0.5, fillColor = pal(seaUvs_ycc@data$UV_NAME), 
                                #                          fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                #                                                            title = "CHANGE_THIS", pal = pal, values = range(seaUvs_ycc@data$UV_NAME), 
                                #                                                            opacity = 0.75, labFormat = labelFormat())
                                
                                myLflt() %>% addPolygons(data = seaUvs_ycc, smoothFactor =
                                                                 0, color = col2hex('white'),weight = 1.5,opacity = .5,
                                                         fillColor = pal(seaUvs_ycc@data$UV_NAME),fillOpacity = .75) %>%
                                        addLegend(position = 'topright', title = 'YCC Urban Villages', pal
                                                  = pal, values = as.factor(seaUvs_ycc@data$UV_NAME), opacity = .75,
                                                  labFormat = labelFormat())
                                
                        }
                        seaUvs_ycc
                        
                }
                
                seaUvs_ycc <- make_seaUvs_ycc()
                rm(make_seaUvs_ycc)
                seaUvs_ycc
        } else {
                make_seaUvs_ycc <- function() {
                        seaUvs_ycc <- readOGR(dsn = "./2_inputs/", layer = "seaUvs_ycc") %>% 
                                spTransform(CRSobj = crs_proj)
                        cn <- read_csv("./2_inputs/seaUvs_ycc_cn.csv") %>% unlist(use.names = FALSE)
                        
                        colnames(seaUvs_ycc@data) <- cn
                        view_seaUvs_ycc <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                # pal <- colorNumeric(palette = myYlOrRd, domain = range(CHANGE_THIS))
                                pal <- colorFactor(palette = 'Set2', domain = as.factor(seaUvs_ycc@data$UV_NAME))
                                
                                # myLflt() %>% addPolygons(data = seaUvs_ycc, 
                                #                          smoothFactor = 0, color = col2hex("white"), 
                                #                          weight = 1.5, opacity = 0.5, fillColor = pal(seaUvs_ycc@data$UV_NAME), 
                                #                          fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                #                                                            title = "CHANGE_THIS", pal = pal, values = range(seaUvs_ycc@data$UV_NAME), 
                                #                                                            opacity = 0.75, labFormat = labelFormat())
                                
                                myLflt() %>% addPolygons(data = seaUvs_ycc, smoothFactor =
                                0, color = col2hex('white'),weight = 1.5,opacity = .5,
                                fillColor = pal(seaUvs_ycc@data$UV_NAME),fillOpacity = .75) %>%
                                addLegend(position = 'topright', title = 'YCC Urban Villages', pal
                                = pal, values = as.factor(seaUvs_ycc@data$UV_NAME), opacity = .75,
                                labFormat = labelFormat())
                                
                        }
                        seaUvs_ycc
                }
                seaUvs_ycc <- make_seaUvs_ycc()
                rm(make_seaUvs_ycc)
                seaUvs_ycc
        }
}

seaUvs_ycc_rev <- {
  
    make_seaUvs_ycc_rev <- function() {
      
            seaUvs_ycc_rev <- readOGR(dsn = "./2_inputs/", layer = "seaUvs_ycc_rev") %>% 
                    spTransform(CRSobj = crs_proj) %>% 
                    .[,1:13]
            
            view_seaUvs_ycc_rev <<- function() {
                    
                    myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                    
                    # pal <- colorNumeric(palette = myYlOrRd, domain = range(CHANGE_THIS))
                    pal <- colorFactor(palette = 'Set2', domain = as.factor(seaUvs_ycc_rev@data$UV_NAME))
                    
                    # myLflt() %>% addPolygons(data = seaUvs_ycc_rev, 
                    #                          smoothFactor = 0, color = col2hex("white"), 
                    #                          weight = 1.5, opacity = 0.5, fillColor = pal(seaUvs_ycc_rev@data$UV_NAME), 
                    #                          fillOpacity = 0.75) %>% addLegend(position = "topright", 
                    #                                                            title = "CHANGE_THIS", pal = pal, values = range(seaUvs_ycc_rev@data$UV_NAME), 
                    #                                                            opacity = 0.75, labFormat = labelFormat())
                    
                    myLflt() %>% addPolygons(data = seaUvs_ycc_rev, smoothFactor =
                                                     0, color = col2hex('white'),weight = 1.5,opacity = .5,
                                             fillColor = pal(seaUvs_ycc_rev@data$UV_NAME),fillOpacity = .75) %>%
                            addLegend(position = 'topright', title = 'YCC Urban Villages', pal
                                      = pal, values = as.factor(seaUvs_ycc_rev@data$UV_NAME), opacity = .75,
                                      labFormat = labelFormat())
                    
            }
      seaUvs_ycc_rev
    }
    seaUvs_ycc_rev <- make_seaUvs_ycc_rev()
    rm(make_seaUvs_ycc_rev)
    seaUvs_ycc_rev
}

seaAcsUvs <- {
        readxl::read_excel(path = "./2_inputs/dpdd017073.xlsx") %>% 
                mutate(TRACT_10 = str_pad(TRACT_10,width = 6, pad = "0"))
        }

# SPATIAL DATA: CENSUS TRACTS, BLOCK GROUPS, BLOCKS, PARCELS --------------------------------------

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

bounds_sea <- {
        
        tract_sea %>% gUnaryUnion() %>% .@bbox
        
       
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

# -------------------------------------------------------------------------------------------------

# SPATIAL DATA: TRACT SELECTION, REVISION OF BLOCK GROUP + BLOCK SELECTION ------------------------

# I decided to connect the urban village identities to cesus geographies using the housing units count
# from the 2010 Census. The function I wrote to do this also has the ability to do this calculation
# housing units or using population as the determining variable - at both the tract- and block group-level.

# For the comparison between using housing units and population, run the R script below
# source("./1_r_scripts/1_setup_uv2CensusDiff.R",echo = TRUE)

# Attribute Urban Village IDs to all block groups
# NOTE: this function can be used for either tract or block-group level attribution,
# and either housing units or population can be used as the determining variable.
# see '1_setup_1_functions.R' for the details of this function. 

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

# Determine Urban Villages for Census Tracts and Block Groups (based on housing unit count)

tract_uvs <- {
        
        if(!file.exists("./2_inputs/tract_uvs.shp")){
                make_tract_uvs <- function(){
                        tract_uvs <- UV2Census()
                        writeOGR(obj = tract_uvs,
                                 dsn = "./2_inputs/",
                                 layer = "tract_uvs",
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)
                }
                
                make_tract_uvs()
                
                rm(make_tract_uvs)
                
        }
        
        tract_uvs <- readOGR(dsn = "./2_inputs/",layer = "tract_uvs") %>% 
                spTransform(CRSobj = crs_proj)
        
        view_tract_uvs <- function(){
                popup <- paste0("TRACT: ",tract_uvs@data$TRACTCE,"<br>",
                                "Urban Village: ", tract_uvs@data$UV3)
                
                pal <- colorFactor(palette = "Set2",domain = tract_uvs@data$UV3)
                
                myLfltShiny() %>% 
                        addPolygons(data = tract_uvs,
                                    popup = popup,
                                    color = "white", opacity = 1, weight = 1.5,
                                    fillColor = ~pal(tract_uvs@data$UV3), fillOpacity = .75) %>% 
                        addLegend(title = "Tracts (by Urban Village)",
                                  position = c("topright"),pal = pal, values = unique(tract_uvs@data$UV3))
                
        }
        
        tract_uvs
        
        
        
}

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
        
        view_bg_uvs <- function(){
                popup <- paste0("GEOID: ",bg_uvs@data$GEOID,"<br>",
                                "Urban Village: ", bg_uvs@data$UV3)
                
                pal <- colorFactor(palette = "Set2",domain = bg_uvs@data$UV3)
                
                myLfltShiny() %>% 
                        addPolygons(data = bg_uvs,
                                    popup = popup,
                                    color = "white", opacity = 1, weight = 1.5,
                                    fillColor = ~pal(bg_uvs@data$UV3), fillOpacity = .75) %>% 
                        addLegend(title = "Tracts (by Urban Village)",
                                  position = c("topright"),pal = pal, values = unique(bg_uvs@data$UV3))
                
        }
        
        bg_uvs
}

# YCC: Tracts and Block Groups with UV by HU count

tract_ycc_hu <- {
        make_tract_ycc_hu <- function(){
                
                if(!file.exists("./2_inputs/tract_ycc_hu.shp")){
                        
                        tract_ycc_hu <-
                                tract_uvs %>% 
                                .[.@data$UV %in% seaUvs_ycc@data$UV_NAME, ]
                        
                        writeOGR(obj = tract_ycc_hu,
                                 dsn = "./2_inputs/",
                                 layer = "tract_ycc_hu",
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)
                }
                
                readOGR(dsn = "./2_inputs/",layer = "tract_ycc_hu") %>% 
                        spTransform(CRSobj = crs_proj)
        }
        
        tract_ycc_hu <- make_tract_ycc_hu()
        
        rm(make_tract_ycc_hu)
        
        view_tract_ycc_hu <- function(){
                
                popup <- paste0("TRACT: ",tract_ycc_hu@data$TRACTCE, "<br>",
                                "Urban Village: ", tract_ycc_hu@data$UV)
                pal <- colorFactor(palette = "Set2",domain = tract_ycc_hu@data$UV)
                
                myLfltShiny() %>% 
                        addPolygons(data = tract_ycc_hu,
                                    popup = popup,
                                    color = "white", opacity = 1, weight = 1.5,
                                    fillColor = ~pal(tract_ycc_hu@data$UV), fillOpacity = .75) %>% 
                        addLegend(title = "OPTION A: HOUSING UNIT COUNT<br>YCC Tracts (by Urban Village)",
                                  position = c("topright"),pal = pal, values = unique(tract_ycc_hu@data$UV))
        }

        view_tract_ycc_hu() %>%
                saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/lflt_tract_ycc_hu.html")
        
        
        tract_ycc_hu
        
}

bg_ycc_hu <- {
        
        make_bg_ycc_hu <- function(){
                
                if(!file.exists("./2_inputs/bg_ycc_hu.shp")){
                        bg_ycc_hu <- 
                                bg_uvs[bg_uvs@data$UV %in% seaUvs_ycc@data$UV_NAME,]
                        
                        writeOGR(obj = bg_ycc_hu,
                                 dsn = "./2_inputs/",
                                 layer = "bg_ycc_hu",
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)
                        
                }
                
                readOGR(dsn = "./2_inputs/",layer = "bg_ycc_hu") %>% 
                        spTransform(CRSobj = crs_proj)
                
                
                
        }
        
        bg_ycc_hu <- make_bg_ycc_hu()
        
        rm(make_bg_ycc_hu)
        
        view_bg_ycc_hu <- function(){
                
                popup <- paste0("GEOID: ",bg_ycc_hu@data$GEOID, "<br>",
                                "Urban Village: ", bg_ycc_hu@data$UV)
                pal <- colorFactor(palette = "Set2",domain = bg_ycc_hu@data$UV)
                
                myLfltShiny() %>% 
                        addPolygons(data = bg_ycc_hu,
                                    popup = popup,
                                    color = "white", opacity = 1, weight = 1.5,
                                    fillColor = ~pal(bg_ycc_hu@data$UV), fillOpacity = .75) %>% 
                        addLegend(title = "OPTION A: HOUSING UNIT COUNT<br>YCC Block Groups (by Urban Village)",
                                  position = c("topright"),pal = pal, values = unique(bg_ycc_hu@data$UV))
        }
        
        view_bg_ycc_hu() %>%
                saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/lflt_bg_ycc_hu.html")
        
        bg_ycc_hu
        
}


# YCC: Tracts and Block Groups by (somewhat) arbitrary attribution

tract_ycc_arb <- {
        make_tract_ycc_arb <- function(){

                if(!file.exists("./2_inputs/tract_ycc_arb.shp")){
                        
                        tract_ids <- 
                                c("007900", "008600", "007401", "007500", "008300", "008400", 
                                  "008500", "008700", "008800", "009000", "009100", "007600", 
                                  "007402", "009200")
                        
                        df <- 
                                tract_uvs@data[tract_uvs@data$TRACTCE %in% tract_ids,c("TRACTCE","UV")]
                        
                        tract_ycc_arb <- 
                                tract_sea[tract_sea@data$TRACTCE %in% tract_ids,] %>% 
                                geo_join(data_frame = df,
                                         by_sp = "TRACTCE",
                                         by_df = "TRACTCE")
                        
                        
                        tract_ycc_arb@data[tract_ycc_arb@data$TRACTCE %in% c("007900"),"UV"] <- "23rd & Union-Jackson"
                        
                        
                        writeOGR(obj = tract_ycc_arb,
                                 dsn = "./2_inputs/",
                                 layer = "tract_ycc_arb",
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)
                }
                
                readOGR(dsn = "./2_inputs/",layer = "tract_ycc_arb") %>% 
                        spTransform(CRSobj = crs_proj)
        }
        
        tract_ycc_arb <- make_tract_ycc_arb()
        
        rm(make_tract_ycc_arb)
        
        view_tract_ycc_arb <- function(){
                
                popup <- paste0("TRACT: ",tract_ycc_arb@data$TRACTCE, "<br>",
                                "Urban Village: ", tract_ycc_arb@data$UV)
                pal <- colorFactor(palette = "Set2",domain = tract_ycc_arb@data$UV)
                
                myLfltShiny() %>% 
                        addPolygons(data = tract_ycc_arb,
                                    popup = popup,
                                    color = "white", opacity = 1, weight = 1.5,
                                    fillColor = ~pal(tract_ycc_arb@data$UV), fillOpacity = .75) %>% 
                        addLegend(title = "OPTION B: ARBITRARY<br>YCC Tracts (by Urban Village)",
                                  position = c("topright"),pal = pal, values = unique(tract_ycc_arb@data$UV))
        }
        
        
        # view_tract_ycc_arb() %>%
        #         saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/lflt_tract_ycc_arb.html")

        
        tract_ycc_arb
        
}
        
bg_ycc_arb <- {
        
        make_bg_ycc_arb <- function(){
                
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
                
                bg_ycc_arb <- 
                        bg_sea[bg_sea@data$GEOID %in% bg_ids,]
                
                bg_ycc_arb <- 
                        geo_join(spatial_data = bg_ycc_arb,
                                 data_frame = df,
                                 by_sp = "GEOID",
                                 by_df = "GEOID")
                
                # Change the UV for the block groups who housing unit count UV was "unintuitive"
                
                bg_ycc_arb@data[bg_ycc_arb@data$GEOID %in% c("530330079003",
                                                     "530330079002",
                                                     "530330079001"),"UV"] <- "23rd & Union-Jackson"
                
               
                
                bg_ycc_arb
          
        }
        
        bg_ycc_arb <- make_bg_ycc_arb()
        
        rm(make_bg_ycc_arb)
        
        view_bg_ycc_arb <- function(){
                
                popup <- paste0("GEOID: ",bg_ycc_arb@data$GEOID, "<br>",
                                "Urban Village: ", bg_ycc_arb@data$UV)
                pal <- colorFactor(palette = "Set2",domain = bg_ycc_arb@data$UV)
                
                myLfltShiny() %>% 
                        addPolygons(data = bg_ycc_arb,
                                    popup = popup,
                                    color = "white", opacity = 1, weight = 1.5,
                                    fillColor = ~pal(bg_ycc_arb@data$UV), fillOpacity = .75) %>% 
                        addLegend(title = "OPTION B: ARBITRARY<br>YCC Block Groups (by Urban Village)",
                                  position = c("topright"),pal = pal, values = unique(bg_ycc_arb@data$UV))
        }
        
        # view_bg_ycc_arb() %>%
        #         saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/lflt_bg_ycc_arb.html")
        
        bg_ycc_arb

}


# Parcels (within a 1000 foot buffer of the Arbitrary YCC boundary)
# Note: this output uses the 'arbitrary' urban village-to-census mapping
# Note 2: the object is used by the YCC ParcelSearch tool

parcel_ycc <- {
        
        make_parcel_ycc <- function(){
                
                df <- read_csv(file = "./2_inputs/Capacity_For_All_Parcel_2015.csv")
                
                # Make `parcel_sea`
                if(!file.exists("./2_inputs/parcel_sea.shp")){
                        url <- "ftp://ftp.kingcounty.gov/gis/Web/GISData/parcel_SHP.zip" # save the URL for the waterbodies data
                        
                        temp <- tempfile() # create a temporary file to hold the compressed download
                        
                        download(url, dest = temp, mode="wb") # download the file
                        
                        unzip (temp, exdir = "./2_inputs/") # extract the ESRI geodatabase file to a project folder
                        
                        parcel_sea <- readOGR(dsn = "./2_inputs/parcel/",layer = "parcel") %>% 
                                .[parcel_sea@data$PIN %in% unique(df$PIN),] %>% 
                                spTransform(CRSobj = crs_proj)
                        
                        writeOGR(obj = parcel_sea,
                                 dsn = "./2_inputs/",
                                 layer = "parcel_sea",
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)
                }
                
                # Load `parcel_sea`
                if(!exists("parcel_sea")){
                        parcel_sea <<- readOGR(dsn = "./2_inputs/",layer = "parcel_sea") %>% 
                                spTransform(CRSobj = crs_proj)
                }
                
                # Join `GEOID` to `parcel_sea`
                
                join_geoid <- function(){
                        
                        t_parcel <- 
                                geo_join(spatial_data = parcel_sea,
                                         data_frame = df,
                                         by_sp = "PIN",
                                         by_df = "PIN")
                        
                        t_parcel@data %<>% 
                                mutate(TRBL10CHAR = as.character(TRBL10)) %>% 
                                mutate(TRBL10CHAR = gsub(pattern = "\\.",replacement = "",x = TRBL10CHAR)) %>% 
                                mutate(TRBL10CHAR = str_pad(TRBL10CHAR,width = 8, pad = "0",side = "right")) %>% 
                                mutate(GEOID_TRT = paste0("5303300",substr(TRBL10CHAR,1,4))) %>% 
                                mutate(GEOID_BG = paste0(GEOID_TRT,substr(TRBL10CHAR,5,5))) %>% 
                                mutate(GEOID_BLK = paste0(GEOID_BG, substr(TRBL10CHAR,6,8))) %>% 
                                select(-PIN.1,-Location.1,-TRBL10CHAR) %>% 
                                select(GEOID_TRT,GEOID_BG,GEOID_BLK,everything())
                        
                        t_parcel
                        
                }
                
                parcel_sea_geoid <- join_geoid()
                
                
                # Join `UV` to `parcel_sea`

                make_parcel_sea_geoid_uv <- function(){
                        
                        uv_tr <- tract_ycc_arb@data %>% select(GEOID,UV)
                        uv_bg <- bg_ycc_arb@data %>% select(GEOID,UV)
                        
                        parcel_sea_geoid_uv <- 
                                geo_join(spatial_data = parcel_sea_geoid,
                                          data_frame = uv_tr,
                                          by_sp = "GEOID_TRT",
                                          by_df = "GEOID") %>% 
                                geo_join(spatial_data = .,
                                          data_frame = uv_bg,
                                          by_sp = "GEOID_BG",
                                          by_df = "GEOID")
                        
                        parcel_sea_geoid_uv@data  %<>% 
                                select(-GEOID,-GEOID.1) %>% 
                                select(UV_TR = UV, UV_BG = UV.1,everything()) %>% 
                                replace_na(list(UV_TR = "Outside YCC")) %>% 
                                replace_na(list(UV_BG = "Outside YCC"))
                        
                        parcel_sea_geoid_uv
                        
                }
                        
                parcel_sea_geoid_uv <- make_parcel_sea_geoid_uv()
                
                # Make `parcel_ycc'
                
                if(!file.exists("./2_inputs/parcel_ycc.shp")){
                        
                        parcel_ycc <- 
                                seaUvs_ycc_rev %>% 
                                spTransform(CRSobj = crs_geog) %>% 
                                gBuffer(byid = FALSE,width = 1000) %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                gWithin(spgeom1 = parcel_sea_geoid,spgeom2 = .,byid = TRUE) %>% 
                                .[1,] %>% 
                                unlist(use.names = F) %>% 
                                parcel_sea_geoid_uv[.,]
                        
                        cn <- colnames(parcel_ycc@data) %>% as.data.frame()
                        
                        writeOGR(obj = parcel_ycc,
                                 dsn = "./2_inputs/",
                                 layer = "parcel_ycc",
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)
                        
                        # writeOGR(obj = parcel_ycc,
                        #          dsn = "./4_webcontent/shiny/ParcelSearch/",
                        #          layer = "parcel_ycc",
                        #          driver = "ESRI Shapefile",
                        #          overwrite_layer = TRUE)
                        
                        write_csv(cn,"./2_inputs/parcel_ycc_cn.csv")
                        # write_csv(cn,"./4_webcontent/shiny/ParcelSearch/parcel_ycc_cn.csv")
                        
                
                }
                
                # Load `parcel_ycc`
                if(!exists("parcel_ycc")){
                        parcel_ycc <- readOGR(dsn = "./2_inputs/",layer = "parcel_ycc") %>% 
                                spTransform(CRSobj = crs_proj)
                        
                        cn <- read_csv(file = "./2_inputs/parcel_ycc_cn.csv") %>% unlist(use.names = F)
                        
                        colnames(parcel_ycc@data) <- cn
                }
                
                parcel_ycc
                
        }
        
        parcel_ycc <- make_parcel_ycc()
        
        rm(make_parcel_ycc)
        
        parcel_ycc
}


