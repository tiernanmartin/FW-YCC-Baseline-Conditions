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

sea <- {
        if (!file.exists("./2_inputs/sea.shp")) {
                make_sea <- function() {
                       
                        sea <- tigris::places(state = "WA") %>% 
                                subset(NAME == "Seattle") %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                wtr_clip(wtr = waterbodies)
                        
                        writeOGR(obj = sea, dsn = "./2_inputs/", layer = "sea", 
                                 driver = "ESRI Shapefile", overwrite_layer = TRUE)
                        
                        colnames(sea@data) %>% data_frame() %>% write_csv(path = "./2_inputs/sea_cn.csv")
                        
                        view_sea <<- function() {
                                
                                myLfltSmpl(sea)
                                # myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                # max <- max(sea@data$IND) %>% round_any(10, ceiling)
                                # pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                # # pal <- colorFactor(palette = 'Set2', domain =
                                # # as.factor(sea@data$IND))
                                # 
                                # myLflt() %>% addPolygons(data = sea, smoothFactor = 0, 
                                #                          color = col2hex("white"), weight = 1.5, opacity = 0.5, 
                                #                          fillColor = pal(sea@data$IND), fillOpacity = 0.75) %>% 
                                #         addLegend(position = "topright", title = "CHANGE_THIS", 
                                #                   pal = pal, values = range(0:max), opacity = 0.75, 
                                #                   labFormat = labelFormat())
                                
                                # myLflt() %>% addPolygons(data = sea, smoothFactor = 0,
                                # color = col2hex('white'),weight = 1.5,opacity = .5,
                                # fillColor = pal(sea@data$IND),fillOpacity = .75) %>%
                                # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                # = pal, values = as.factor(sea@data$IND), opacity = .75,
                                # labFormat = labelFormat())
                                
                        }
                        sea
                        
                }
                
                sea <- make_sea()
                rm(make_sea)
                sea
        } else {
                make_sea <- function() {
                        sea <- readOGR(dsn = "./2_inputs/", layer = "sea") %>% 
                                spTransform(CRSobj = crs_proj)
                        cn <- read_csv("./2_inputs/sea_cn.csv") %>% unlist(use.names = FALSE)
                        
                        colnames(sea@data) <- cn
                        view_sea <<- function() {
                                
                                myLfltSmpl(sea)
                                # myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                # max <- max(sea@data$IND) %>% round_any(10, ceiling)
                                # pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                # # pal <- colorFactor(palette = 'Set2', domain =
                                # # as.factor(sea@data$IND))
                                # 
                                # myLflt() %>% addPolygons(data = sea, smoothFactor = 0, 
                                #                          color = col2hex("white"), weight = 1.5, opacity = 0.5, 
                                #                          fillColor = pal(sea@data$IND), fillOpacity = 0.75) %>% 
                                #         addLegend(position = "topright", title = "CHANGE_THIS", 
                                #                   pal = pal, values = range(0:max), opacity = 0.75, 
                                #                   labFormat = labelFormat())
                                
                                # myLflt() %>% addPolygons(data = sea, smoothFactor = 0,
                                # color = col2hex('white'),weight = 1.5,opacity = .5,
                                # fillColor = pal(sea@data$IND),fillOpacity = .75) %>%
                                # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                # = pal, values = as.factor(sea@data$IND), opacity = .75,
                                # labFormat = labelFormat())
                                
                        }
                        sea
                }
                sea <- make_sea()
                rm(make_sea)
                sea
        }
}

sea_ua <- {
        if (!file.exists("./2_inputs/sea_ua.shp")) {
                make_sea_ua <- function() {
                        
                        sea_ua <- tigris::urban_areas(cb = TRUE) %>% 
                                subset(NAME10 == "Seattle, WA") %>% 
                                spTransform(CRSobj = crs_proj)
                        
                        writeOGR(obj = sea_ua, dsn = "./2_inputs/", layer = "sea_ua", 
                                 driver = "ESRI Shapefile", overwrite_layer = TRUE)
                        
                        colnames(sea_ua@data) %>% data_frame() %>% write_csv(path = "./2_inputs/sea_ua_cn.csv")
                        
                        view_sea_ua <<- function() {
                                myLfltSmpl(sea_ua)
                                
                        }
                        sea_ua
                        
                }
                
                sea_ua <- make_sea_ua()
                rm(make_sea_ua)
                sea_ua
        } else {
                make_sea_ua <- function() {
                        sea_ua <- readOGR(dsn = "./2_inputs/", layer = "sea_ua") %>% 
                                spTransform(CRSobj = crs_proj)
                        cn <- read_csv("./2_inputs/sea_ua_cn.csv") %>% unlist(use.names = FALSE)
                        
                        colnames(sea_ua@data) <- cn
                        view_sea_ua <<- function() {
                                myLfltSmpl(sea_ua)
                                
                        }
                        sea_ua
                }
                sea_ua <- make_sea_ua()
                rm(make_sea_ua)
                sea_ua
        }
}

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

# ACS/UV Hybrid Neighborhood Boundaries

uv_ycc_arb <- {
        if (!file.exists("./2_inputs/uv_ycc_arb.shp")) {
                make_uv_ycc_arb <- function() {
                        
                        # Merge tracts by UV
                        
                        uv1 <- maptools::unionSpatialPolygons(SpP = tract_ycc_arb,tract_ycc_arb@data$UV)
                        uv2 <- tract_ycc_arb@data %>% group_by(UV) %>% summarise() %>% as.data.frame()
                        uv3 <- SpatialPolygonsDataFrame(Sr = uv1,data = uv2,match.ID = FALSE)
                        
                        uv_ycc_arb <- uv3
                        writeOGR(obj = uv_ycc_arb, dsn = "./2_inputs/", layer = "uv_ycc_arb", 
                                 driver = "ESRI Shapefile", overwrite_layer = TRUE)
                        
                        colnames(uv_ycc_arb@data) %>% data_frame() %>% write_csv(path = "./2_inputs/uv_ycc_arb_cn.csv")
                        
                        view_uv_ycc_arb <<- function() {
                                
                                # myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                # pal <- colorNumeric(palette = myYlOrRd, domain = range(CHANGE_THIS))
                                pal <- colorFactor(palette = 'Set2', domain = as.factor(uv3@data$UV))
                                
                                # myLflt() %>% addPolygons(data = uv3, 
                                #                          smoothFactor = 0, color = col2hex("white"), 
                                #                          weight = 1.5, opacity = 0.5, fillColor = pal(uv3@data$UV), 
                                #                          fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                #                                                            title = "CHANGE_THIS", pal = pal, values = range(CHANGE_THIS), 
                                #                                                            opacity = 0.75, labFormat = labelFormat())
                                
                                myLflt() %>% addPolygons(data = uv3, smoothFactor =
                                                                 0, color = col2hex('white'),weight = 1.5,opacity = .5,
                                                         fillColor = pal(uv3@data$UV),fillOpacity = .75) %>%
                                        addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                                  = pal, values = as.factor(uv3@data$UV), opacity = .75,
                                                  labFormat = labelFormat())
                                
                        }
                        uv_ycc_arb
                        
                }
                
                uv_ycc_arb <- make_uv_ycc_arb()
                rm(make_uv_ycc_arb)
                uv_ycc_arb
        } else {
                make_uv_ycc_arb <- function() {
                        uv_ycc_arb <- readOGR(dsn = "./2_inputs/", layer = "uv_ycc_arb") %>% 
                                spTransform(CRSobj = crs_proj)
                        cn <- read_csv("./2_inputs/uv_ycc_arb_cn.csv") %>% 
                                unlist(use.names = FALSE)
                        
                        colnames(uv_ycc_arb@data) <- cn
                        view_uv_ycc_arb <<- function() {
                                
                                # myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                # pal <- colorNumeric(palette = myYlOrRd, domain = range(CHANGE_THIS))
                                pal <- colorFactor(palette = 'Set2', domain = as.factor(uv_ycc_arb@data$UV))
                                
                                # myLflt() %>% addPolygons(data = uv3, 
                                #                          smoothFactor = 0, color = col2hex("white"), 
                                #                          weight = 1.5, opacity = 0.5, fillColor = pal(uv3@data$UV), 
                                #                          fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                #                                                            title = "CHANGE_THIS", pal = pal, values = range(CHANGE_THIS), 
                                #                                                            opacity = 0.75, labFormat = labelFormat())
                                
                                myLflt() %>% addPolygons(data = uv_ycc_arb, smoothFactor =
                                                                 0, color = col2hex('white'),weight = 1.5,opacity = .5,
                                                         fillColor = pal(uv_ycc_arb@data$UV),fillOpacity = .75) %>%
                                        addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                                  = pal, values = as.factor(uv_ycc_arb@data$UV), opacity = .75,
                                                  labFormat = labelFormat())
                                
                        }
                        uv_ycc_arb
                }
                uv_ycc_arb <- make_uv_ycc_arb()
                rm(make_uv_ycc_arb)
                uv_ycc_arb
        }
}


# Parcels (within a 1000 foot buffer of the Arbitrary YCC boundary)
# Note: this output uses the 'arbitrary' urban village-to-census mapping
# Note 2: the object is used by the YCC ParcelSearch tool

# source('./1_r_scripts/1_setup_createData_parcel_ycc.R')

# -------------------------------------------------------------------------------------------------

# DEMOGRAPHIC DATA: AMERICAN COMMUNITY SURVEY (ACS), COMPREHENSIVE HOUSING AFFORDABILITY STRATEGY (CHAS) -----

# Race & Ethnicity

pctRace_seattle <- {
        if (!file.exists("./2_inputs/pctRace_seattle.csv")) {
                make_pctRace_seattle <- function() {
                        
                        sea <- geo.make(state = "WA",place = "Seattle")
                        
                        race1 <- acs.fetch(endyear = 2014,geography = sea,table.number = "B03002")
                        
                        race2 <- race1@estimate %>% 
                                as.data.frame() %>% 
                                mutate_each(funs(as.numeric),everything()) %>% 
                                rename(TOTAL = B03002_001,
                                       WHITE = B03002_003,
                                       BLACK = B03002_004,
                                       AM_INDIAN = B03002_005,
                                       ASIAN = B03002_006,
                                       PACIFIC = B03002_007,
                                       HISPANIC = B03002_012) %>% 
                                mutate(OTHER = B03002_008 + B03002_009) %>% 
                                mutate(POC = TOTAL - WHITE) %>% 
                                select(TOTAL,
                                       WHITE,
                                       POC,
                                       BLACK,
                                       AM_INDIAN,
                                       ASIAN,
                                       PACIFIC,
                                       HISPANIC,
                                       OTHER) %>% 
                                mutate(PCT_WHITE = myPctRound(WHITE/TOTAL),
                                       PCT_POC = myPctRound(POC/TOTAL),
                                       PCT_BLACK = myPctRound(BLACK/TOTAL),
                                       PCT_AM_INDIAN = myPctRound(AM_INDIAN/TOTAL),
                                       PCT_ASIAN = myPctRound(ASIAN/TOTAL),
                                       PCT_PACIFIC = myPctRound(PACIFIC/TOTAL),
                                       PCT_HISPANIC = myPctRound(HISPANIC/TOTAL),
                                       PCT_OTHER = myPctRound(OTHER/TOTAL))
                        
                        pctRace_seattle <- race2
                        
                        pctRace_seattle %>% 
                                write_csv(path = "./2_inputs/pctRace_seattle.csv")
                        
                        
                        pctRace_seattle
                        
                }
                
                pctRace_seattle <- make_pctRace_seattle()
                rm(make_pctRace_seattle)
                pctRace_seattle
        } else {
                make_pctRace_seattle <- function() {
                        pctRace_seattle <- read_csv("./2_inputs/pctRace_seattle.csv")
                        
                }
                pctRace_seattle <- make_pctRace_seattle()
                rm(make_pctRace_seattle)
                pctRace_seattle
        }
}

pctRace_sea_tr <- {
        if (!file.exists("./2_inputs/pctRace_sea_tr.shp")) {
                make_pctRace_sea_tr <- function() {
                        
                        tr1 <- tract_sea
                        
                        tr_wa <- geo.make(state = "WA",county = "King", tract = "*")
                        
                        race1 <- acs.fetch(endyear = 2014,geography = tr_wa,table.number = "B03002")
                        colCode <- race1@acs.colnames
                        colPretty <- acs.fetch(endyear = 2014,geography = tr_wa,table.number = "B03002",col.names = "pretty") %>%
                                .@acs.colnames
                        colGuide <- rbind(colCode,colPretty) %>% as.data.frame()
                        
                        race2 <- race1[race1@geography$tract %in% tr1$TRACTCE,] %>%
                                .@estimate %>% 
                                cbind(race1[race1@geography$tract %in% tr1$TRACTCE,]@geography$tract) %>%
                                as.data.frame() %>% 
                                rename(TRACTCE = V22) %>% 
                                mutate_each(funs(as.numeric),-contains("TRACTCE")) %>% 
                                rename(TOTAL = B03002_001,
                                       WHITE = B03002_003,
                                       BLACK = B03002_004,
                                       AM_INDIAN = B03002_005,
                                       ASIAN = B03002_006,
                                       PACIFIC = B03002_007,
                                       HISPANIC = B03002_012) %>% 
                                mutate(OTHER = B03002_008 + B03002_009) %>% 
                                mutate(POC = TOTAL - WHITE) %>% 
                                select(TRACTCE,
                                       TOTAL,
                                       WHITE,
                                       POC,
                                       BLACK,
                                       AM_INDIAN,
                                       ASIAN,
                                       PACIFIC,
                                       HISPANIC,
                                       OTHER) %>% 
                                mutate(PCT_WHITE = myPctRound(WHITE/TOTAL),
                                       PCT_POC = myPctRound(POC/TOTAL),
                                       PCT_BLACK = myPctRound(BLACK/TOTAL),
                                       PCT_AM_INDIAN = myPctRound(AM_INDIAN/TOTAL),
                                       PCT_ASIAN = myPctRound(ASIAN/TOTAL),
                                       PCT_PACIFIC = myPctRound(PACIFIC/TOTAL),
                                       PCT_HISPANIC = myPctRound(HISPANIC/TOTAL),
                                       PCT_OTHER = myPctRound(OTHER/TOTAL))
                        
                        tr2 <- myGeoJoin(tr1,race2,"TRACTCE","TRACTCE")
                        
                        pctRace_sea_tr <- tr2
                        writeOGR(obj = pctRace_sea_tr, dsn = "./2_inputs/", layer = "pctRace_sea_tr", 
                                 driver = "ESRI Shapefile", overwrite_layer = TRUE)
                        
                        colnames(pctRace_sea_tr@data) %>% data_frame() %>% write_csv(path = "./2_inputs/pctRace_sea_tr_cn.csv")
                        
                        view_pctRace_sea_tr_pctPOC <<- function() {
                                tr2 <- pctRace_sea_tr
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                max <- max(tr2@data$PCT_POC) %>% round_any(10, ceiling)
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                # pal <- colorFactor(palette = 'Set2', domain =
                                # as.factor(tr2@data$PCT_POC))
                                
                                myLflt() %>% addPolygons(data = tr2, smoothFactor = 0, 
                                                         color = col2hex("white"), weight = 1.5, opacity = 0.5, 
                                                         fillColor = pal(tr2@data$PCT_POC), fillOpacity = 0.75) %>% 
                                        addLegend(position = "topright", title = "CHANGE_THIS", 
                                                  pal = pal, values = range(0:max), opacity = 0.75, 
                                                  labFormat = labelFormat(suffix = "%"))
                                
                                # myLflt() %>% addPolygons(data = tr2, smoothFactor = 0,
                                # color = col2hex('white'),weight = 1.5,opacity = .5,
                                # fillColor = pal(tr2@data$PCT_POC),fillOpacity = .75) %>%
                                # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                # = pal, values = as.factor(tr2@data$PCT_POC), opacity = .75,
                                # labFormat = labelFormat())
                                
                        }
                        pctRace_sea_tr
                        
                }
                
                pctRace_sea_tr <- make_pctRace_sea_tr()
                rm(make_pctRace_sea_tr)
                pctRace_sea_tr
        } else {
                make_pctRace_sea_tr <- function() {
                        pctRace_sea_tr <- readOGR(dsn = "./2_inputs/", layer = "pctRace_sea_tr") %>% 
                                spTransform(CRSobj = crs_proj)
                        cn <- read_csv("./2_inputs/pctRace_sea_tr_cn.csv") %>% unlist(use.names = FALSE)
                        
                        colnames(pctRace_sea_tr@data) <- cn
                        view_pctRace_sea_tr_pctPOC <<- function() {
                                tr2 <- pctRace_sea_tr
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                max <- max(tr2@data$PCT_POC) %>% round_any(10, ceiling)
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                # pal <- colorFactor(palette = 'Set2', domain =
                                # as.factor(tr2@data$PCT_POC))
                                
                                myLflt() %>% addPolygons(data = tr2, smoothFactor = 0, 
                                                         color = col2hex("white"), weight = 1.5, opacity = 0.5, 
                                                         fillColor = pal(tr2@data$PCT_POC), fillOpacity = 0.75) %>% 
                                        addLegend(position = "topright", title = "CHANGE_THIS", 
                                                  pal = pal, values = range(0:max), opacity = 0.75, 
                                                  labFormat = labelFormat(suffix = "%"))
                                
                                # myLflt() %>% addPolygons(data = tr2, smoothFactor = 0,
                                # color = col2hex('white'),weight = 1.5,opacity = .5,
                                # fillColor = pal(tr2@data$PCT_POC),fillOpacity = .75) %>%
                                # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                # = pal, values = as.factor(tr2@data$PCT_POC), opacity = .75,
                                # labFormat = labelFormat())
                                
                        }
                        pctRace_sea_tr
                }
                pctRace_sea_tr <- make_pctRace_sea_tr()
                rm(make_pctRace_sea_tr)
                pctRace_sea_tr
        }
}

pctRace_ycc_tr <- {
        if (!file.exists("./2_inputs/pctRace_ycc_tr.shp")) {
                make_pctRace_ycc_tr <- function() {
                        
                        
                        # Subset the Seattle data to include only YCC tracts
                        tr1 <- pctRace_sea_tr %>% subset(TRACTCE %in% tract_ycc_arb@data$TRACTCE)
                        
                        
                        # Join the UV names
                        UVs <- tract_ycc_arb@data %>% select(TRACTCE,UV)
                        
                        tr2 <- tr1 
                        
                        tr2@data %<>%
                                left_join(UVs) 
                        
                        # Group by UV (and calculate the percentages, if applicable)
                        uv1 <- tr2@data %>% 
                                as.data.frame() %>% 
                                group_by(UV) %>% 
                                summarise(TOTAL = sum(TOTAL),
                                          WHITE = sum(WHITE),
                                          POC = sum(POC),
                                          BLACK = sum(BLACK),
                                          AM_INDIAN = sum(AM_INDIAN),
                                          ASIAN = sum(ASIAN),
                                          PACIFIC = sum(PACIFIC),
                                          HISPANIC = sum(HISPANIC),
                                          OTHER = sum(OTHER),
                                          PCT_WHITE = myPctRound(sum(WHITE)/sum(TOTAL)),
                                          PCT_POC = myPctRound(sum(POC)/sum(TOTAL)),
                                          PCT_BLACK = myPctRound(sum(BLACK)/sum(TOTAL)),
                                          PCT_AM_INDIAN = myPctRound(sum(AM_INDIAN)/sum(TOTAL)),
                                          PCT_ASIAN = myPctRound(sum(ASIAN)/sum(TOTAL)),
                                          PCT_PACIFIC = myPctRound(sum(PACIFIC)/sum(TOTAL)),
                                          PCT_HISPANIC = myPctRound(sum(HISPANIC)/sum(TOTAL)),
                                          PCT_OTHER = myPctRound(sum(OTHER)/sum(TOTAL))
                                ) 
                        
                        # Join the summary CB/CB50 values to the grouped ACS/UV polygons
                        
                        uv2 <- myGeoJoin(spatial_data = uv_ycc_arb,data_frame = uv1,by_sp = 'UV',by_df = 'UV') 
                        
                        
                        
                        pctRace_ycc_tr <- uv2
                        writeOGR(obj = pctRace_ycc_tr, dsn = "./2_inputs/", 
                                 layer = "pctRace_ycc_tr", driver = "ESRI Shapefile", 
                                 overwrite_layer = TRUE)
                        
                        colnames(pctRace_ycc_tr@data) %>% data_frame() %>% 
                                write_csv(path = "./2_inputs/pctRace_ycc_tr_cn.csv")
                        
                        view_pctRace_sea_ycc_pctPOC <<- function() {
                                uv2 <- pctRace_ycc_tr
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                max <- max(uv2@data$PCT_POC) %>% round_any(10, ceiling)
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                # pal <- colorFactor(palette = 'Set2', domain =
                                # as.factor(uv2@data$PCT_POC))
                                
                                myLflt() %>% addPolygons(data = uv2, smoothFactor = 0, 
                                                         color = col2hex("white"), weight = 1.5, opacity = 0.5, 
                                                         fillColor = pal(uv2@data$PCT_POC), fillOpacity = 0.75) %>% 
                                        addLegend(position = "topright", title = "CHANGE_THIS", 
                                                  pal = pal, values = range(0:max), opacity = 0.75, 
                                                  labFormat = labelFormat(suffix = "%"))
                                
                                # myLflt() %>% addPolygons(data = uv2, smoothFactor = 0,
                                # color = col2hex('white'),weight = 1.5,opacity = .5,
                                # fillColor = pal(uv2@data$PCT_POC),fillOpacity = .75) %>%
                                # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                # = pal, values = as.factor(uv2@data$PCT_POC), opacity = .75,
                                # labFormat = labelFormat())
                                
                        }
                        pctRace_ycc_tr
                        
                }
                
                pctRace_ycc_tr <- make_pctRace_ycc_tr()
                rm(make_pctRace_ycc_tr)
                pctRace_ycc_tr
        } else {
                make_pctRace_ycc_tr <- function() {
                        pctRace_ycc_tr <- readOGR(dsn = "./2_inputs/", layer = "pctRace_ycc_tr") %>% 
                                spTransform(CRSobj = crs_proj)
                        cn <- read_csv("./2_inputs/pctRace_ycc_tr_cn.csv") %>% 
                                unlist(use.names = FALSE)
                        
                        colnames(pctRace_ycc_tr@data) <- cn
                        view_pctRace_sea_ycc_pctPOC <<- function() {
                                uv2 <- pctRace_ycc_tr
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                max <- max(uv2@data$PCT_POC) %>% round_any(10, ceiling)
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                # pal <- colorFactor(palette = 'Set2', domain =
                                # as.factor(uv2@data$PCT_POC))
                                
                                myLflt() %>% addPolygons(data = uv2, smoothFactor = 0, 
                                                         color = col2hex("white"), weight = 1.5, opacity = 0.5, 
                                                         fillColor = pal(uv2@data$PCT_POC), fillOpacity = 0.75) %>% 
                                        addLegend(position = "topright", title = "CHANGE_THIS", 
                                                  pal = pal, values = range(0:max), opacity = 0.75, 
                                                  labFormat = labelFormat(suffix = "%"))
                                
                                # myLflt() %>% addPolygons(data = uv2, smoothFactor = 0,
                                # color = col2hex('white'),weight = 1.5,opacity = .5,
                                # fillColor = pal(uv2@data$PCT_POC),fillOpacity = .75) %>%
                                # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                # = pal, values = as.factor(uv2@data$PCT_POC), opacity = .75,
                                # labFormat = labelFormat())
                                
                        }
                        pctRace_ycc_tr
                }
                pctRace_ycc_tr <- make_pctRace_ycc_tr()
                rm(make_pctRace_ycc_tr)
                pctRace_ycc_tr
        }
}

# Housing

pctHsCstBrdn_seattle <- {
        if (!file.exists("./2_inputs/pctHsCstBrdn_seattle.csv")) {
                make_pctHsCstBrdn_seattle <- function() {
                        tr1 <- tract_sea
                        
                        if (!file.exists("./2_inputs/ACS__5_Year_CHAS_Data_by_Summary_Level_080_as_of_2012.csv")){
                                
                                url <- 'http://egis.hud.opendata.arcgis.com/datasets/6c5f5f39d40c470586fa040063ff8d1d_0.csv' # direct URL to the file download
                                
                                download(url, dest = './2_inputs/ACS__5_Year_CHAS_Data_by_Summary_Level_080_as_of_2012.csv', mode='wb') # download the file
                        }
                        
                        hcb1 <- read_csv("./2_inputs/ACS__5_Year_CHAS_Data_by_Summary_Level_080_as_of_2012.csv") %>% 
                                select(TRACT,OWNER = T8_EST2,RENTER = T8_EST68,T8_CB,T8_CB_PCT,T8_CB50,T8_CB50_PCT) %>%
                                mutate(ALL = OWNER + RENTER) %>% 
                                select(TRACT,ALL,everything()) %>% 
                                filter(TRACT %in% tr1@data$GEOID) 
                        
                        
                        sea <- hcb1 %>% 
                                select(ALL,OWNER,RENTER,T8_CB,T8_CB50) %>% 
                                summarise_each(funs(sum)) %>% 
                                mutate(T8_CB_PCT = myPctRound(T8_CB/ALL),
                                       T8_CB50_PCT = myPctRound(T8_CB50/ALL)) %>% 
                                select(ALL,OWNER,RENTER,T8_CB,T8_CB_PCT,T8_CB50,T8_CB50_PCT)
                        
                        pctHsCstBrdn_seattle <- sea
                        pctHsCstBrdn_seattle %>% 
                                write_csv(path = "./2_inputs/pctHsCstBrdn_seattle.csv")
                        
                        pctHsCstBrdn_seattle
                        
                }
                
                pctHsCstBrdn_seattle <- make_pctHsCstBrdn_seattle()
                rm(make_pctHsCstBrdn_seattle)
                pctHsCstBrdn_seattle
        } else {
                make_pctHsCstBrdn_seattle <- function() {
                        read_csv("./2_inputs/pctHsCstBrdn_seattle.csv")
                }
                pctHsCstBrdn_seattle <- make_pctHsCstBrdn_seattle()
                rm(make_pctHsCstBrdn_seattle)
                pctHsCstBrdn_seattle
        }
}

pctHsCstBrdn_sea_tr <- {
        if (!file.exists("./2_inputs/pctHsCstBrdn_sea_tr.shp")) {
                make_pctHsCstBrdn_sea_tr <- function() {
                        tr1 <- tract_sea
                        
                        if (!file.exists("./2_inputs/ACS__5_Year_CHAS_Data_by_Summary_Level_080_as_of_2012.csv")){
                                
                                url <- 'http://egis.hud.opendata.arcgis.com/datasets/6c5f5f39d40c470586fa040063ff8d1d_0.csv' # direct URL to the file download
                                
                                download(url, dest = './2_inputs/ACS__5_Year_CHAS_Data_by_Summary_Level_080_as_of_2012.csv', mode='wb') # download the file
                        }
                        
                        hcb1 <- read_csv("./2_inputs/ACS__5_Year_CHAS_Data_by_Summary_Level_080_as_of_2012.csv") %>% 
                                select(TRACT,OWNER = T8_EST2,RENTER = T8_EST68,T8_CB,T8_CB_PCT,T8_CB50,T8_CB50_PCT) %>%
                                mutate(ALL = OWNER + RENTER) %>% 
                                select(TRACT,ALL,everything()) %>% 
                                filter(TRACT %in% tr1@data$GEOID) %>% 
                                filter(!is.na(T8_CB_PCT))
                        
                        tr2 <- tr1 %>% 
                                myGeoJoin(data_frame = hcb1,
                                          by_sp = "GEOID",
                                          by_df = "TRACT")
                        
                        pctHsCstBrdn_sea_tr <- tr2
                        writeOGR(obj = pctHsCstBrdn_sea_tr, dsn = "./2_inputs/", 
                                 layer = "pctHsCstBrdn_sea_tr", driver = "ESRI Shapefile",overwrite_layer = TRUE)
                        
                        colnames(pctHsCstBrdn_sea_tr@data) %>% data_frame() %>% 
                                write_csv(path = "./2_inputs/pctHsCstBrdn_sea_tr_cn.csv")
                        
                        view_pctHsCstBrdn_sea_tr <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:100))
                                
                                myLflt() %>% addPolygons(data = pctHsCstBrdn_sea_tr, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(pctHsCstBrdn_sea_tr@data$T8_CB_PCT), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           # title = "Percent of Families Housing-Cost Burdened", 
                                                                                           pal = pal, values = range(0:100), 
                                                                                           opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        view_pctHsCstBrdn_sea_tr50 <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:50))
                                
                                myLflt() %>% addPolygons(data = pctHsCstBrdn_sea_tr, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(pctHsCstBrdn_sea_tr@data$T8_CB50_PCT), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           # title = "Percent of Families Severely Housing-Cost Burdened", 
                                                                                           pal = pal, values = range(0:50), 
                                                                                           opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        
                        pctHsCstBrdn_sea_tr
                        
                }
                
                pctHsCstBrdn_sea_tr <- make_pctHsCstBrdn_sea_tr()
                rm(make_pctHsCstBrdn_sea_tr)
                pctHsCstBrdn_sea_tr
        } else {
                make_pctHsCstBrdn_sea_tr <- function() {
                        pctHsCstBrdn_sea_tr <- readOGR(dsn = "./2_inputs/", layer = "pctHsCstBrdn_sea_tr") %>% 
                                spTransform(CRSobj = crs_proj)
                        cn <- read_csv("./2_inputs/pctHsCstBrdn_sea_tr_cn.csv") %>% 
                                unlist(use.names = FALSE)
                        
                        colnames(pctHsCstBrdn_sea_tr@data) <- cn
                        view_pctHsCstBrdn_sea_tr <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:100))
                                
                                myLflt() %>% addPolygons(data = pctHsCstBrdn_sea_tr, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(pctHsCstBrdn_sea_tr@data$T8_CB_PCT), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           # title = "Percent of Families Housing-Cost Burdened", 
                                                                                           pal = pal, values = range(0:100), 
                                                                                           opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        view_pctHsCstBrdn_sea_tr50 <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:50))
                                
                                myLflt() %>% addPolygons(data = pctHsCstBrdn_sea_tr, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(pctHsCstBrdn_sea_tr@data$T8_CB50_PCT), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           # title = "Percent of Families Severely Housing-Cost Burdened", 
                                                                                           pal = pal, values = range(0:50), 
                                                                                           opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        pctHsCstBrdn_sea_tr
                }
                pctHsCstBrdn_sea_tr <- make_pctHsCstBrdn_sea_tr()
                rm(make_pctHsCstBrdn_sea_tr)
                pctHsCstBrdn_sea_tr
        }
}

pctHsCstBrdn_ycc_tr <- {
        if (!file.exists("./2_inputs/pctHsCstBrdn_ycc_tr.shp")) {
                make_pctHsCstBrdn_ycc_tr <- function() {
                        
                        # Subset the Seattle HCB to include only YCC tracts
                        tr1 <- pctHsCstBrdn_sea_tr %>% subset(TRACTCE %in% tract_ycc_arb@data$TRACTCE)
                        
                        # Join the UV names
                        UVs <- tract_ycc_arb@data %>% 
                                select(TRACTCE,UV) 
                        
                        tr2 <- tr1 
                        
                        tr2@data %<>%
                                left_join(UVs) 
                        
                        # Group by UV and calculate the CB and CB50 percentages
                        
                        uv1 <- tr2@data %>% 
                                as.data.frame() %>% 
                                group_by(UV) %>% 
                                summarise(CB_PCT = myPctRound(sum(T8_CB)/sum(ALL)),
                                          CB_PCT50 = myPctRound(sum(T8_CB50)/sum(ALL))) 
                        
                        
                        # Join the summary CB/CB50 values to the grouped ACS/UV polygons
                        
                        uv2 <- myGeoJoin(spatial_data = uv_ycc_arb,data_frame = uv1,by_sp = "UV",by_df = "UV") 
                        
                        pctHsCstBrdn_ycc_tr <- uv2
                        writeOGR(obj = pctHsCstBrdn_ycc_tr, dsn = "./2_inputs/", 
                                 layer = "pctHsCstBrdn_ycc_tr", driver = "ESRI Shapefile",overwrite_layer = TRUE)
                        
                        colnames(pctHsCstBrdn_ycc_tr@data) %>% data_frame() %>% 
                                write_csv(path = "./2_inputs/pctHsCstBrdn_ycc_tr_cn.csv")
                        
                        view_pctHsCstBrdn_ycc_tr <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                max <- max(pctHsCstBrdn_ycc_tr@data$CB_PCT) %>% round_any(.,10,ceiling)
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                
                                myLflt() %>% 
                                        addPolygons(data = pctHsCstBrdn_ycc_tr, 
                                                    smoothFactor = 0, color = col2hex("white"), 
                                                    weight = 1.5, opacity = 0.5, fillColor = pal(pctHsCstBrdn_ycc_tr@data$CB_PCT), 
                                                    fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                      # title = "Percent of Families Housing-Cost Burdened", 
                                                                                      pal = pal, values = range(0:max), 
                                                                                      opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        view_pctHsCstBrdn50_ycc_tr <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                max <- max(pctHsCstBrdn_ycc_tr@data$CB_PCT50) %>% round_any(.,10,ceiling)
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                
                                myLflt() %>% addPolygons(data = pctHsCstBrdn_ycc_tr, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(pctHsCstBrdn_ycc_tr@data$CB_PCT50), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           # title = "Percent of Families Severely Housing-Cost Burdened", 
                                                                                           pal = pal, values = range(0:max), 
                                                                                           opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        
                        pctHsCstBrdn_ycc_tr
                        
                }
                
                pctHsCstBrdn_ycc_tr <- make_pctHsCstBrdn_ycc_tr()
                rm(make_pctHsCstBrdn_ycc_tr)
                pctHsCstBrdn_ycc_tr
        } else {
                make_pctHsCstBrdn_ycc_tr <- function() {
                        pctHsCstBrdn_ycc_tr <- readOGR(dsn = "./2_inputs/", layer = "pctHsCstBrdn_ycc_tr") %>% 
                                spTransform(CRSobj = crs_proj)
                        cn <- read_csv("./2_inputs/pctHsCstBrdn_ycc_tr_cn.csv") %>% 
                                unlist(use.names = FALSE)
                        
                        colnames(pctHsCstBrdn_ycc_tr@data) <- cn
                        view_pctHsCstBrdn_ycc_tr <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                max <- max(pctHsCstBrdn_ycc_tr@data$CB_PCT) %>% round_any(.,10,ceiling)
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                
                                myLflt() %>% 
                                        addPolygons(data = pctHsCstBrdn_ycc_tr, 
                                                    smoothFactor = 0, color = col2hex("white"), 
                                                    weight = 1.5, opacity = 0.5, fillColor = pal(pctHsCstBrdn_ycc_tr@data$CB_PCT), 
                                                    fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                      # title = "Percent of Families Housing-Cost Burdened", 
                                                                                      pal = pal, values = range(0:max), 
                                                                                      opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        view_pctHsCstBrdn_ycc_tr50 <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                max <- max(pctHsCstBrdn_ycc_tr@data$CB_PCT50) %>% round_any(.,10,ceiling)
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                
                                myLflt() %>% addPolygons(data = pctHsCstBrdn_ycc_tr, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(pctHsCstBrdn_ycc_tr@data$CB_PCT50), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           # title = "Percent of Families Severely Housing-Cost Burdened", 
                                                                                           pal = pal, values = range(0:max), 
                                                                                           opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        pctHsCstBrdn_ycc_tr
                }
                pctHsCstBrdn_ycc_tr <- make_pctHsCstBrdn_ycc_tr()
                rm(make_pctHsCstBrdn_ycc_tr)
                pctHsCstBrdn_ycc_tr
        }
}


# Income

pctBelowPvty_seattle <- {
        if (!file.exists("./2_inputs/pctBelowPvty_seattle.csv")) {
                make_pctBelowPvty_seattle <- function() {
                        
                        tr1 <- tract_sea
                        
                        tr_wa <- geo.make(state = "WA",county = "King", tract = "*")
                        
                        pbp1 <- acs.fetch(endyear = 2014,geography = tr_wa,table.number = "C17002",col.names = "pretty")
                        
                        pbp2 <- pbp1[pbp1@geography$tract %in% tr1$TRACTCE,] %>%
                                .@estimate %>%
                                cbind(pbp1[pbp1@geography$tract %in% tr1$TRACTCE,]@geography$tract) %>%
                                as.data.frame()
                        
                        colnames(pbp2) <- c("TOTAL",
                                            "UNDER50PCT",
                                            "BTWN50TO99PCT",
                                            "BTWN100TO124PCT",
                                            "BTWN125TO149PCT",
                                            "BTWN150TO184PCT",
                                            "BTWN185TO199PCT",
                                            "OVER200PCT",
                                            "TRACTCE")
                        
                        tr2 <- myGeoJoin(tr1,pbp2,"TRACTCE","TRACTCE")
                        
                        sea <- tr2 %>% 
                                as.data.frame() %>% 
                                mutate_each(funs(as.numeric),TOTAL:OVER200PCT) %>% 
                                mutate(UNDER200PCT = TOTAL - OVER200PCT) %>% 
                                select(TOTAL:UNDER200PCT) %>% 
                                summarise_each(funs(sum)) %>% 
                                mutate(PCT_UNDER200PCT = 1 - OVER200PCT/TOTAL) %>% 
                                mutate(PCT_UNDER200PCT = myPctRound(PCT_UNDER200PCT))
                        
                        pctBelowPvty_seattle <- sea
                        
                        pctBelowPvty_seattle %>% write_csv(path = "./2_inputs/pctBelowPvty_seattle.csv")
                        
                        pctBelowPvty_seattle
                        
                }
                
                pctBelowPvty_seattle <- make_pctBelowPvty_seattle()
                rm(make_pctBelowPvty_seattle)
                pctBelowPvty_seattle
        } else {
                make_pctBelowPvty_seattle <- function() {
                        pctBelowPvty_seattle <- 
                                read_csv("./2_inputs/pctBelowPvty_seattle.csv")
                }
                pctBelowPvty_seattle
        }
        pctBelowPvty_seattle <- make_pctBelowPvty_seattle()
        rm(make_pctBelowPvty_seattle)
        pctBelowPvty_seattle
}

pctBelowPvty_sea <- {
        if (!file.exists("./2_inputs/pctBelowPvty_sea.shp")) {
                make_pctBelowPvty_sea <- function() {
                        
                        tr1 <- tract_sea
                        
                        tr_wa <- geo.make(state = "WA",county = "King", tract = "*")
                        
                        pbp1 <- acs.fetch(endyear = 2014,geography = tr_wa,table.number = "C17002",col.names = "pretty")
                        
                        pbp2 <- pbp1[pbp1@geography$tract %in% tr1$TRACTCE,] %>%
                                .@estimate %>%
                                cbind(pbp1[pbp1@geography$tract %in% tr1$TRACTCE,]@geography$tract) %>%
                                as.data.frame()
                        
                        colnames(pbp2) <- c("TOTAL",
                                            "UNDER50PCT",
                                            "BTWN50TO99PCT",
                                            "BTWN100TO124PCT",
                                            "BTWN125TO149PCT",
                                            "BTWN150TO184PCT",
                                            "BTWN185TO199PCT",
                                            "OVER200PCT",
                                            "TRACTCE")
                        
                        tr2 <- myGeoJoin(tr1,pbp2,"TRACTCE","TRACTCE")
                        
                        tr2@data %<>%
                                mutate_each(funs(as.numeric),TOTAL:OVER200PCT) %>% 
                                mutate(UNDER200PCT = TOTAL - OVER200PCT) %>% 
                                mutate(PCT_UNDER200PCT = (1 - OVER200PCT/TOTAL) * 100) %>% 
                                mutate(PCT_UNDER200PCT = round_any(PCT_UNDER200PCT,1))
                        
                        pctBelowPvty_sea <- tr2
                        
                        writeOGR(obj = pctBelowPvty_sea, dsn = "./2_inputs/", 
                                 layer = "pctBelowPvty_sea", driver = "ESRI Shapefile", overwrite_layer = TRUE)
                        
                        colnames(pctBelowPvty_sea@data) %>% data_frame() %>% 
                                write_csv(path = "./2_inputs/pctBelowPvty_sea_cn.csv")
                        
                        view_pctBelowPvty_sea <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                max <- max(tr2@data$PCT_UNDER200PCT) %>% round_any(10, ceiling)
                                
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                
                                myLflt() %>% addPolygons(data = tr2, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(tr2@data$PCT_UNDER200PCT), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           # title = "Percent of Population Below 200% Poverty", 
                                                                                           pal = pal, values = range(0:max), 
                                                                                           opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        pctBelowPvty_sea
                        
                }
                
                pctBelowPvty_sea <- make_pctBelowPvty_sea()
                rm(make_pctBelowPvty_sea)
                pctBelowPvty_sea
        } else {
                make_pctBelowPvty_sea <- function() {
                        pctBelowPvty_sea <- 
                                readOGR(dsn = "./2_inputs/", layer = "pctBelowPvty_sea") %>% 
                                spTransform(CRSobj = crs_proj)
                        cn <- read_csv("./2_inputs/pctBelowPvty_sea_cn.csv") %>% 
                                unlist(use.names = FALSE)
                        
                        colnames(pctBelowPvty_sea@data) <- cn
                        
                        view_pctBelowPvty_sea <<- function() {
                                
                                tr2 <- pctBelowPvty_sea
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                max <- max(tr2@data$PCT_UNDER200PCT) %>% round_any(10, ceiling)
                                
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                
                                myLflt() %>% addPolygons(data = tr2, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(tr2@data$PCT_UNDER200PCT), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           # title = "Percent of Population Below 200% Poverty", 
                                                                                           pal = pal, values = range(0:max), 
                                                                                           opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        pctBelowPvty_sea
                }
                pctBelowPvty_sea <- make_pctBelowPvty_sea()
                rm(make_pctBelowPvty_sea)
                pctBelowPvty_sea
        }
}

pctBelowPvty_ycc <- {
        if (!file.exists("./2_inputs/pctBelowPvty_ycc.shp")) {
                make_pctBelowPvty_ycc <- function() {
                        
                        
                        # Subset the Seattle data to include only YCC tracts
                        tr1 <- pctBelowPvty_sea %>% subset(TRACTCE %in% tract_ycc_arb@data$TRACTCE)

                           
                           # Join the UV names
                           UVs <- tract_ycc_arb@data %>% select(TRACTCE,UV)
                           
                           tr2 <- tr1 
                           
                           tr2@data %<>%
                           left_join(UVs) 
                           
                           # Group by UV (and calculate the percentages, if applicable)
                           
                           uv1 <- tr2@data %>% 
                                   as.data.frame() %>% 
                                   group_by(UV) %>% 
                                   summarise(PCT_UNDER200PCT = sum(UNDER200PCT)/sum(TOTAL)) 
                           
                           # Join the summary CB/CB50 values to the grouped ACS/UV polygons
                           
                           uv2 <- myGeoJoin(spatial_data = uv_ycc_arb,data_frame = uv1,by_sp = 'UV',by_df = 'UV') 
                           
                           # Convert the decimals into 1^e2 (better for mapping)
                           
                           uv2@data %<>% 
                           mutate(PCT_UNDER200PCT = round_any(PCT_UNDER200PCT * 100,.01, round))


                        
                        pctBelowPvty_ycc <- uv2
                        writeOGR(obj = pctBelowPvty_ycc, dsn = "./2_inputs/", 
                                 layer = "pctBelowPvty_ycc", driver = "ESRI Shapefile")
                        
                        colnames(pctBelowPvty_ycc@data) %>% data_frame() %>% 
                                write_csv(path = "./2_inputs/pctBelowPvty_ycc_cn.csv")
                        
                        view_pctBelowPvty_ycc <<- function() {
                                
                                uv2 <- pctBelowPvty_ycc
                                        
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                max <- max(uv2@data$PCT_UNDER200PCT) %>% round_any(10,ceiling)
                                
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                # pal <- colorFactor(palette = 'Set2', domain =
                                # as.factor(CHANGE_THIS))
                                
                                myLflt() %>% addPolygons(data = uv2, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(uv2@data$PCT_UNDER200PCT), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           title = "CHANGE_THIS", pal = pal, values = range(0:max), 
                                                                                           opacity = 0.75, labFormat = labelFormat())
                                
                                # myLflt() %>% addPolygons(data = CHANGE_THIS, smoothFactor =
                                # 0, color = col2hex('white'),weight = 1.5,opacity = .5,
                                # fillColor = pal(CHANGE_THIS),fillOpacity = .75) %>%
                                # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                # = pal, values = as.factor(CHANGE_THIS), opacity = .75,
                                # labFormat = labelFormat())
                                
                        }
                        pctBelowPvty_ycc
                        
                }
                
                pctBelowPvty_ycc <- make_pctBelowPvty_ycc()
                rm(make_pctBelowPvty_ycc)
                pctBelowPvty_ycc
        } else {
                make_pctBelowPvty_ycc <- function() {
                        pctBelowPvty_ycc <- readOGR(dsn = "./2_inputs/", 
                                                    layer = "pctBelowPvty_ycc") %>% spTransform(CRSobj = crs_proj)
                        cn <- read_csv("./2_inputs/pctBelowPvty_ycc_cn.csv") %>% 
                                unlist(use.names = FALSE)
                        
                        colnames(pctBelowPvty_ycc@data) <- cn
                        view_pctBelowPvty_ycc <<- function() {
                                
                                uv2 <- pctBelowPvty_ycc
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                max <- max(uv2@data$PCT_UNDER200PCT) %>% round_any(10,ceiling)
                                
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                # pal <- colorFactor(palette = 'Set2', domain =
                                # as.factor(CHANGE_THIS))
                                
                                myLflt() %>% addPolygons(data = uv2, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(uv2@data$PCT_UNDER200PCT), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           title = "CHANGE_THIS", pal = pal, values = range(0:max), 
                                                                                           opacity = 0.75, labFormat = labelFormat())
                                
                                # myLflt() %>% addPolygons(data = CHANGE_THIS, smoothFactor =
                                # 0, color = col2hex('white'),weight = 1.5,opacity = .5,
                                # fillColor = pal(CHANGE_THIS),fillOpacity = .75) %>%
                                # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                # = pal, values = as.factor(CHANGE_THIS), opacity = .75,
                                # labFormat = labelFormat())
                                
                        }
                        pctBelowPvty_ycc
                }
                pctBelowPvty_ycc <- make_pctBelowPvty_ycc()
                rm(make_pctBelowPvty_ycc)
                pctBelowPvty_ycc
        }
}


# Language

pctEngLTvwell_seattle <- {
        if (!file.exists("./2_inputs/pctEngLTvwell_seattle.csv")) {
                make_pctEngLTvwell_seattle <- function() {
                        
                        tr1 <- tract_sea
                        
                        tr_wa <- geo.make(state = "WA",county = "King", tract = "*")
                        
                        eng1 <- acs.fetch(endyear = 2014,geography = tr_wa,table.number = "B16001",col.names = "pretty")
                        
                        eng2 <- eng1[eng1@geography$tract %in% tr1$TRACTCE,] %>%
                                .@estimate %>% 
                                cbind(eng1[eng1@geography$tract %in% tr1$TRACTCE,]@geography$tract) %>%
                                as.data.frame()
                        eng3 <- 
                                eng2 %>% 
                                select(TRACTCE = contains("V120"),TOTAL = contains("Total"),contains("less")) %>% 
                                mutate_each(funs(as.numeric),-contains("TRACTCE")) %>% 
                                mutate(LTVW = select(.,-matches("TRACTCE|TOTAL")) %>% rowSums()) %>% 
                                select(TOTAL,LTVW) %>% 
                                summarise_each(funs(sum)) %>% 
                                mutate(PCT_LTVW = myPctRound(LTVW/TOTAL))
                        
                        write_csv(eng3,"./2_inputs/pctEngLTvwell_seattle.csv")
                        
                        pctEngLTvwell_seattle <- eng3
                        pctEngLTvwell_seattle
                        
                }
                
                pctEngLTvwell_seattle <- make_pctEngLTvwell_seattle()
                rm(make_pctEngLTvwell_seattle)
                pctEngLTvwell_seattle
        } else {
                make_pctEngLTvwell_seattle <- function() {
                        pctEngLTvwell_seattle <- read_csv("./2_inputs/pctEngLTvwell_seattle.csv")
                        
                }
                pctEngLTvwell_seattle <- make_pctEngLTvwell_seattle()
                rm(make_pctEngLTvwell_seattle)
                pctEngLTvwell_seattle
        }
        
}

pctEngLTvwell_sea <- {
        if (!file.exists("./2_inputs/pctEngLTvwell_sea.shp")) {
                make_pctEngLTvwell_sea <- function() {
                        
                        tr1 <- tract_sea
                        
                        tr_wa <- geo.make(state = "WA",county = "King", tract = "*")
                        
                        eng1 <- acs.fetch(endyear = 2014,geography = tr_wa,table.number = "B16001",col.names = "pretty")
                        
                        eng2 <- eng1[eng1@geography$tract %in% tr1$TRACTCE,] %>%
                                .@estimate %>% 
                                cbind(eng1[eng1@geography$tract %in% tr1$TRACTCE,]@geography$tract) %>%
                                as.data.frame()
                        eng3 <- 
                                eng2 %>% 
                                select(TRACTCE = contains("V120"),TOTAL = contains("Total"),contains("less")) %>% 
                                mutate_each(funs(as.numeric),-contains("TRACTCE")) %>% 
                                mutate(LTVW = select(.,-matches("TRACTCE|TOTAL")) %>% rowSums()) %>% 
                                mutate(PCT_LTVW = LTVW/TOTAL) %>% 
                                mutate(PCT_LTVW = round_any(PCT_LTVW *100, .01, round)) %>% 
                                select(TRACTCE,TOTAL,LTVW,PCT_LTVW)
                        
                        tr2 <- myGeoJoin(tr1,eng3,"TRACTCE","TRACTCE")
                        
                        pctEngLTvwell_sea <- tr2
                        writeOGR(obj = pctEngLTvwell_sea, dsn = "./2_inputs/", 
                                 layer = "pctEngLTvwell_sea", driver = "ESRI Shapefile", overwrite_layer = TRUE)
                        
                        colnames(pctEngLTvwell_sea@data) %>% data_frame() %>% 
                                write_csv(path = "./2_inputs/pctEngLTvwell_sea_cn.csv")
                        
                        view_pctEngLTvwell_sea <<- function() {
                                tr2 <- pctEngLTvwell_sea
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                max <- max(tr2@data$PCT_LTVW) %>% round_any(10, ceiling)
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                # pal <- colorFactor(palette = 'Set2', domain =
                                # as.factor(tr2@data$PCT_LTVW))
                                
                                myLflt() %>% addPolygons(data = tr2, smoothFactor = 0, 
                                                         color = col2hex("white"), weight = 1.5, opacity = 0.5, 
                                                         fillColor = pal(tr2@data$PCT_LTVW), fillOpacity = 0.75) %>% 
                                        addLegend(position = "topright", title = "CHANGE_THIS", 
                                                  pal = pal, values = range(0:max), opacity = 0.75, 
                                                  labFormat = labelFormat())
                                
                                # myLflt() %>% addPolygons(data = tr2, smoothFactor = 0,
                                # color = col2hex('white'),weight = 1.5,opacity = .5,
                                # fillColor = pal(tr2@data$IND),fillOpacity = .75) %>%
                                # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                # = pal, values = as.factor(tr2@data$IND), opacity = .75,
                                # labFormat = labelFormat())
                                
                        }
                        pctEngLTvwell_sea
                        
                }
                
                pctEngLTvwell_sea <- make_pctEngLTvwell_sea()
                rm(make_pctEngLTvwell_sea)
                pctEngLTvwell_sea
        } else {
                make_pctEngLTvwell_sea <- function() {
                        pctEngLTvwell_sea <- readOGR(dsn = "./2_inputs/", layer = "pctEngLTvwell_sea") %>% 
                                spTransform(CRSobj = crs_proj)
                        cn <- read_csv("./2_inputs/pctEngLTvwell_sea_cn.csv") %>% 
                                unlist(use.names = FALSE)
                        
                        colnames(pctEngLTvwell_sea@data) <- cn
                        view_pctEngLTvwell_sea <<- function() {
                                tr2 <- pctEngLTvwell_sea
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                max <- max(tr2@data$PCT_LTVW) %>% round_any(10, ceiling)
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                # pal <- colorFactor(palette = 'Set2', domain =
                                # as.factor(tr2@data$PCT_LTVW))
                                
                                myLflt() %>% addPolygons(data = tr2, smoothFactor = 0, 
                                                         color = col2hex("white"), weight = 1.5, opacity = 0.5, 
                                                         fillColor = pal(tr2@data$PCT_LTVW), fillOpacity = 0.75) %>% 
                                        addLegend(position = "topright", title = "CHANGE_THIS", 
                                                  pal = pal, values = range(0:max), opacity = 0.75, 
                                                  labFormat = labelFormat())
                                
                                # myLflt() %>% addPolygons(data = tr2, smoothFactor = 0,
                                # color = col2hex('white'),weight = 1.5,opacity = .5,
                                # fillColor = pal(tr2@data$IND),fillOpacity = .75) %>%
                                # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                # = pal, values = as.factor(tr2@data$IND), opacity = .75,
                                # labFormat = labelFormat())
                                
                        }
                        pctEngLTvwell_sea
                }
                pctEngLTvwell_sea <- make_pctEngLTvwell_sea()
                rm(make_pctEngLTvwell_sea)
                pctEngLTvwell_sea
        }
}

pctEngLTvwell_ycc <- {
        if (!file.exists("./2_inputs/pctEngLTvwell_ycc.shp")) {
                make_pctEngLTvwell_ycc <- function() {
                        
                        # Subset the Seattle data to include only YCC tracts
                        tr1 <- pctEngLTvwell_sea %>% subset(TRACTCE %in% tract_ycc_arb@data$TRACTCE)
                        
                        
                        # Join the UV names
                        UVs <- tract_ycc_arb@data %>% select(TRACTCE,UV)
                        
                        tr2 <- tr1 
                        
                        tr2@data %<>%
                                left_join(UVs) 
                        
                        # Group by UV (and calculate the percentages, if applicable)
                        uv1 <- tr2@data %>% 
                                as.data.frame() %>% 
                                group_by(UV) %>% 
                                summarise(ABOVE5_TOTAL = sum(TOTAL))
                        uv2 <- tr2@data %>% 
                                as.data.frame() %>% 
                                group_by(UV) %>% 
                                summarise(ABOVE5_LTVW = sum(LTVW))
                        uv3 <- tr2@data %>% 
                                as.data.frame() %>% 
                                group_by(UV) %>% 
                                summarise(PCT_LTVW = sum(LTVW)/sum(TOTAL))
                        uv4 <- 
                                left_join(uv1,uv2) %>% 
                                left_join(uv3)
                        
                        # Join the summary CB/CB50 values to the grouped ACS/UV polygons
                        
                        uv5 <- myGeoJoin(spatial_data = uv_ycc_arb,data_frame = uv4,by_sp = 'UV',by_df = 'UV') 
                        
                        # Convert the decimals into 1^e2 (better for mapping)
                        
                        uv5@data %<>% 
                                mutate(PCT_LTVW = round_any(PCT_LTVW * 100,.01, round))
                        
                        
                        pctEngLTvwell_ycc <- uv5
                        writeOGR(obj = pctEngLTvwell_ycc, dsn = "./2_inputs/", 
                                 layer = "pctEngLTvwell_ycc", driver = "ESRI Shapefile", 
                                 overwrite_layer = TRUE)
                        
                        colnames(pctEngLTvwell_ycc@data) %>% data_frame() %>% 
                                write_csv(path = "./2_inputs/pctEngLTvwell_ycc_cn.csv")
                        
                        view_pctEngLTvwell_ycc <<- function() {
                                uv5 <- pctEngLTvwell_ycc
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                max <- max(uv5@data$PCT_LTVW) %>% round_any(10, ceiling)
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                # pal <- colorFactor(palette = 'Set2', domain =
                                # as.factor(uv5@data$PCT_LTVW))
                                
                                myLflt() %>% addPolygons(data = uv5, smoothFactor = 0, 
                                                         color = col2hex("white"), weight = 1.5, opacity = 0.5, 
                                                         fillColor = pal(uv5@data$PCT_LTVW), fillOpacity = 0.75) %>% 
                                        addLegend(position = "topright", title = "CHANGE_THIS", 
                                                  pal = pal, values = range(0:max), opacity = 0.75, 
                                                  labFormat = labelFormat())
                                
                                # myLflt() %>% addPolygons(data = uv5, smoothFactor = 0,
                                # color = col2hex('white'),weight = 1.5,opacity = .5,
                                # fillColor = pal(uv5@data$PCT_LTVW),fillOpacity = .75) %>%
                                # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                # = pal, values = as.factor(uv5@data$PCT_LTVW), opacity = .75,
                                # labFormat = labelFormat())
                                
                        }
                        pctEngLTvwell_ycc
                        
                }
                
                pctEngLTvwell_ycc <- make_pctEngLTvwell_ycc()
                rm(make_pctEngLTvwell_ycc)
                pctEngLTvwell_ycc
        } else {
                make_pctEngLTvwell_ycc <- function() {
                        pctEngLTvwell_ycc <- readOGR(dsn = "./2_inputs/", 
                                                     layer = "pctEngLTvwell_ycc") %>% spTransform(CRSobj = crs_proj)
                        cn <- read_csv("./2_inputs/pctEngLTvwell_ycc_cn.csv") %>% 
                                unlist(use.names = FALSE)
                        
                        colnames(pctEngLTvwell_ycc@data) <- cn
                        view_pctEngLTvwell_ycc <<- function() {
                                uv5 <- pctEngLTvwell_ycc
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                max <- max(uv5@data$PCT_LTVW) %>% round_any(10, ceiling)
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:max))
                                # pal <- colorFactor(palette = 'Set2', domain =
                                # as.factor(uv5@data$PCT_LTVW))
                                
                                myLflt() %>% addPolygons(data = uv5, smoothFactor = 0, 
                                                         color = col2hex("white"), weight = 1.5, opacity = 0.5, 
                                                         fillColor = pal(uv5@data$PCT_LTVW), fillOpacity = 0.75) %>% 
                                        addLegend(position = "topright", title = "CHANGE_THIS", 
                                                  pal = pal, values = range(0:max), opacity = 0.75, 
                                                  labFormat = labelFormat())
                                
                                # myLflt() %>% addPolygons(data = uv5, smoothFactor = 0,
                                # color = col2hex('white'),weight = 1.5,opacity = .5,
                                # fillColor = pal(uv5@data$PCT_LTVW),fillOpacity = .75) %>%
                                # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                # = pal, values = as.factor(uv5@data$PCT_LTVW), opacity = .75,
                                # labFormat = labelFormat())
                                
                        }
                        pctEngLTvwell_ycc
                }
                pctEngLTvwell_ycc <- make_pctEngLTvwell_ycc()
                rm(make_pctEngLTvwell_ycc)
                pctEngLTvwell_ycc
        }
}



