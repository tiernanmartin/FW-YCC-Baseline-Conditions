tract_UV <- {
        
        make_tract_UV <- function(){
                
                seaAcsUvs <- readxl::read_excel(path = "./2_inputs/dpdd017073.xlsx") %>% 
                        mutate(TRACT_10 = str_pad(TRACT_10,width = 6, pad = "0"))
                
                if(!file.exists("./2_inputs/tracts.shp")){
                        tracts_orig <- 
                                tigris::tracts(state = "WA", county = "King") %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                .[.@data$TRACTCE %in% seaAcsUvs$TRACT_10,] # select only Seattle tracts
                        
                        wtr_clip(orig = tracts_orig,wtr = waterbodies) %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "tracts",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }
                
                tracts <- readOGR(dsn = "./2_inputs/",layer = "tracts") %>% 
                        spTransform(CRSobj = crs_proj)
                
                if(!file.exists("./2_inputs/blk.shp")){
                        blk_sea <- 
                                tigris::blocks(state = "WA", county = "King") %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                blk_orig[blk_orig@data$TRACTCE10 %in% seaAcsUvs$TRACT_10,]
                        
                        # NOTE: this step takes a long time!
                        wtr_clip(orig = blk_sea,wtr = waterbodies) %>% 
                                writeOGR(dsn = "./2_inputs/",layer = "blk",driver = "ESRI Shapefile",overwrite_layer = TRUE)
                }
                
                blk <- readOGR(dsn = "./2_inputs/",layer = "blk") %>% 
                        spTransform(CRSobj = crs_proj)
                
                # function goes here
                
                
                if(!file.exists("./2_inputs/tracts_UV.shp")){
                        
                        seaAcsUvs <- readxl::read_excel(path = "./2_inputs/dpdd017073.xlsx") %>% 
                                filter(grepl("China*|Pioneer*|First*|12th*|23rd*|Pike|Capitol|Madison|Commercial",URBAN_VILLAGE_NAME))
                        
                        seaAcsUvs_trt <- 
                                seaAcsUvs %>% 
                                mutate(TRACT_10 = str_pad(TRACT_10,width = 6, pad = "0")) %>% 
                                group_by(TRACT_10) %>% 
                                summarise(UV_NAME = first(URBAN_VILLAGE_NAME),
                                          UV_TYPE = first(URBAN_VILLAGE_TYPE))
                        
                        seaTrts <- 
                                seaAcsUvs %>% 
                                mutate(TRACT_10 = str_pad(TRACT_10,width = 6, pad = "0")) %>% 
                                select(TRACT_10) %>% 
                                unique() %>% 
                                unlist()
                        
                        tracts <- 
                                readOGR(dsn = "./2_inputs/",layer = "tracts") %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                .[.[["TRACTCE"]] %in% seaTrts,]
                        
                        test <- 
                                myGeoJoin(spatial_data = tracts,data_frame = seaAcsUvs_trt,
                                          by_sp = "TRACTCE" ,by_df = "TRACT_10")
                        
                        myLeaflet(test) %>% 
                                addPolygons(data = seaUVs,
                                            weight = 2, color = "black", opacity = 1,
                                            fill = F) %>% 
                                addPolygons(data = test,
                                            stroke = F,
                                            fillOpacity = 0,
                                            popup = ~UV_NAME) 
                        
                        seaAcsUvs_bg <- 
                                seaAcsUvs %>% 
                                mutate(GEOID_BG = str_extract(GEOID10,"^\\d{12}")) %>% 
                                group_by(GEOID_BG) %>% 
                                summarise(UV_NAME = first(URBAN_VILLAGE_NAME),
                                          UV_TYPE = first(URBAN_VILLAGE_TYPE))
                        
                        bg <- 
                                readOGR(dsn = "./2_inputs/",layer = "bg") %>% 
                                spTransform(CRSobj = crs_proj) %>% 
                                .[.[["GEOID"]] %in% seaAcsUvs_bg$GEOID_BG,]
                        
                        test <- 
                                geo_join(spatial_data = bg,data_frame = seaAcsUvs_bg,
                                          by_sp = "GEOID" ,by_df = "GEOID_BG")
                        
                        pal <- colorFactor(palette = "Set2",
                                                    domain = test@data$UV_NAME)
                        
                        
                        
                        
                        leaflet() %>% 
                                addProviderTiles("CartoDB.Positron") %>% 
                                addPolygons(group = "Census Block Groups",
                                            data = test,
                                            color = "white",weight = 1.5, opacity = .5,
                                            fillColor = ~pal(test@data$UV_NAME), fillOpacity = .75,
                                            popup = ~UV_NAME) %>%
                                addPolylines(group = "Urban Villages",
                                             data = seaUVs_ycc_outline,
                                             color = "dodgerblue", weight = 2, opacity = 1)%>% 
                                addLegend(title = "Block Groups by Urban Village",
                                          position = "topright",
                                          pal = pal,
                                          values = test@data$UV_NAME) %>% 
                                addLayersControl(overlayGroups = c("Census Block Groups","Urban Villages"),options = layersControlOptions(collapsed = F))
                        
                        
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