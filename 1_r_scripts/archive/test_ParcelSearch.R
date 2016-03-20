
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
                        parcel_sea <- readOGR(dsn = "./2_inputs/",layer = "parcel_sea") %>% 
                                spTransform(CRSobj = crs_proj)
                }
                
                # Join `GEOID` to `parcel_sea

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
                                parcel_sea_geoid[.,]
                        
                        cn <- colnames(parcel_ycc@data) %>% as.data.frame()
                
                        writeOGR(obj = parcel_ycc,
                                 dsn = "./2_inputs/",
                                 layer = "parcel_ycc",
                                 driver = "ESRI Shapefile",
                                 overwrite_layer = TRUE)
                        
                        write_csv(cn,"./2_inputs/parcel_ycc_cn.csv")
                        
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

t_par <- 
        parcel_ycc %>% 
        .[.@data$TAX_STATUS %!in% "T" & !is.na(.@data$TAX_STATUS),]

t_par2 <- 
        parcel_ycc %>% 
        .[.@data$PUB_OWN_TYPE %!in% "PRIVATE" & !is.na(.@data$PUB_OWN_TYPE) | .@data$TAX_STATUS %!in% "T" & !is.na(.@data$TAX_STATUS),]

t_par2 <- 
        parcel_ycc %>% 
        subset(subset = TAX_STATUS %!in% c("T","0") & PUB_OWN_TYPE == "PRIVATE" & PROP_NAME == "VACANT LAND") 

        myLeaflet(t_par2) %>% 
        addCircles(data = gCentroid(t_par2,byid = TRUE))
        .[TAX_STATUS %!in% c("T","0") & PUB_OWN_TYPE == "PRIVATE" & PROP_NAME == "VACANT LAND",]

        parcel_ycc@data %>% 
                mutate(TRBL10CHAR = as.character(TRBL10)) %>% 
                mutate(TRBL10CHAR = gsub(pattern = "\\.",replacement = "",x = TRBL10CHAR)) %>% 
                mutate(TRBL10CHARCNT = nchar(TRBL10CHAR)) %>% 
                mutate(TRBL10CHAR = str_pad(TRBL10CHAR,width = 8, pad = "0",side = "right")) %>% 
                mutate(GEOID_TRT = paste0("5303300",substr(TRBL10CHAR,1,4))) %>% 
                mutate(GEOID_BG = paste0(GEOID_TRT,substr(TRBL10CHAR,5,5))) %>% 
                mutate(GEOID_BLK = paste0(GEOID_BG, substr(TRBL10CHAR,6,8))) %>% 
                select(GEOID,GEOID_TRT,GEOID_BG,GEOID_BLK,TRBL10,TRBL10CHAR,TRBL10CHARCNT) %>% select(GEOID_BLK) %>% 
                sample_n(25) %>%  View()
                mutate(FIRST8 = str_extract(string = TRBL10CHAR, pattern = "[[:alnum:]]{1,8}")) %>%  View()
                .[1,1]
                View()
                
                nchar() %>% summary()
                sample_n(25) %>% glimpse()
        
        parcel_ycc@data %>% 
                select(GEOID,TRBL10) %>% 
                sample_n(25) %>% glimpse()
         
a <- "79001000"
substr(a,1,3)

t_df <- 
        parcel_ycc@data %>% 
        filter(TAX_STATUS == "T" & PUB_OWN_TYPE != "PRIVATE") %>% nrow()
        sample_n(size = 10) %>% View()
        
        
        parcel_ycc@data %>% 
                select(PUB_OWN_TYPE,TAX_STATUS,PROP_NAME,LAND_NO_SHORE_SQFT,PARCEL_DEV_SQFT,ZONELUT) %>% 
                filter(TAX_STATUS %!in% c("T","0") & PUB_OWN_TYPE == "PRIVATE" & PROP_NAME == "VACANT LAND") %>% View()
                sample_n(size = 10) %>% View()
                
        