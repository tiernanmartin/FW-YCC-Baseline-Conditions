# Note: this script depends on object created in the '/2_inputs/1_setup_createData.R' file
# Run that file prior to running this script

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
                        
                        # Create a abbreviated column names to prevent data loss
                        cn_new <- paste0(rep("V",ncol(parcel_ycc@data)),paste0(1:ncol(parcel_ycc@data)))
                        
                        colnames(parcel_ycc@data) <- cn_new
                        
                        writeOGR(obj = parcel_ycc,
                                 dsn = "./2_inputs/",
                                 layer = "parcel_ycc",
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)
                        
                        # Use this function to save the shapefile to the ParcelFinder tool repo
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