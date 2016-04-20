hcb1 <- read_csv("./2_inputs/ACS__5_Year_CHAS_Data_by_Summary_Level_080_as_of_2012.csv")

hcb1 %>% select(starts_with("T8")) %>% colnames() %>% as.data.frame() %>% View()


pctHsCstBrdn_ycc <- {
        if (!file.exists("./2_inputs/pctHsCstBrdn_ycc.shp")) {
                make_pctHsCstBrdn_ycc <- function() {
                        
                        # Subset the Seattle HCB to include only YCC tracts
                        tr1 <- pctHsCstBrdn_sea %>% subset(TRACTCE %in% tract_ycc_arb@data$TRACTCE)
                        
                        # Select the columns related to cost burdening
                        hcb1 <- read_csv("./2_inputs/ACS__5_Year_CHAS_Data_by_Summary_Level_080_as_of_2012.csv") %>% 
                                select(TRACT,OWNER = T8_EST2,RENTER = T8_EST68,T8_CB,T8_CB_PCT,T8_CB50,T8_CB50_PCT) %>%
                                mutate(ALL = OWNER + RENTER) %>% 
                                select(TRACT,ALL,everything()) %>% 
                                filter(TRACT %in% tr1@data$GEOID)
                        
                        # Join the UV names and hcb data
                        UVs <- tract_ycc_hu@data %>% select(TRACTCE,UV)
                        
                        tr2 <- tr1 
                        
                        tr2@data %<>%
                                left_join(UVs) %>% 
                                left_join(hcb1, by = c("TRACTCE" = "TRACT"))
                        
                        # Join the UV data 
                        
                                
                        
                        # Group by UV and calculate the CB and CB50 percentages
                        
                                
                                        
                        pctHsCstBrdn_ycc <- tr2
                        writeOGR(obj = pctHsCstBrdn_ycc, dsn = "./2_inputs/", 
                                 layer = "pctHsCstBrdn_ycc", driver = "ESRI Shapefile")
                        
                        colnames(pctHsCstBrdn_ycc@data) %>% data_frame() %>% 
                                write_csv(path = "./2_inputs/pctHsCstBrdn_ycc_cn.csv")
                        
                        view_pctHsCstBrdn_ycc <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:100))
                                
                                myLflt() %>% addPolygons(data = pctHsCstBrdn_ycc, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(pctHsCstBrdn_ycc@data$T8_CB_PCT), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           # title = "Percent of Families Housing-Cost Burdened", 
                                                                                           pal = pal, values = range(0:100), 
                                                                                           opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        view_pctHsCstBrdn_ycc50 <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:50))
                                
                                myLflt() %>% addPolygons(data = pctHsCstBrdn_ycc, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(pctHsCstBrdn_ycc@data$T8_CB50_PCT), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           # title = "Percent of Families Severely Housing-Cost Burdened", 
                                                                                           pal = pal, values = range(0:50), 
                                                                                           opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        
                        pctHsCstBrdn_ycc
                        
                }
                
                pctHsCstBrdn_ycc <- make_pctHsCstBrdn_ycc()
                rm(make_pctHsCstBrdn_ycc)
                pctHsCstBrdn_ycc
        } else {
                make_pctHsCstBrdn_ycc <- function() {
                        pctHsCstBrdn_ycc <- readOGR(dsn = "./2_inputs/", layer = "pctHsCstBrdn_ycc") %>% 
                                spTransform(CRSobj = crs_proj)
                        cn <- read_csv("./2_inputs/pctHsCstBrdn_ycc_cn.csv") %>% 
                                unlist(use.names = FALSE)
                        
                        colnames(pctHsCstBrdn_ycc@data) <- cn
                        view_pctHsCstBrdn_ycc <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:100))
                                
                                myLflt() %>% addPolygons(data = pctHsCstBrdn_ycc, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(pctHsCstBrdn_ycc@data$T8_CB_PCT), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           # title = "Percent of Families Housing-Cost Burdened", 
                                                                                           pal = pal, values = range(0:100), 
                                                                                           opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        view_pctHsCstBrdn_ycc50 <<- function() {
                                
                                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                
                                pal <- colorNumeric(palette = myYlOrRd, domain = range(0:50))
                                
                                myLflt() %>% addPolygons(data = pctHsCstBrdn_ycc, 
                                                         smoothFactor = 0, color = col2hex("white"), 
                                                         weight = 1.5, opacity = 0.5, fillColor = pal(pctHsCstBrdn_ycc@data$T8_CB50_PCT), 
                                                         fillOpacity = 0.75) %>% addLegend(position = "topright", 
                                                                                           # title = "Percent of Families Severely Housing-Cost Burdened", 
                                                                                           pal = pal, values = range(0:50), 
                                                                                           opacity = 0.75, labFormat = labelFormat(suffix = "%"))
                        }
                        pctHsCstBrdn_ycc
                }
                pctHsCstBrdn_ycc <- make_pctHsCstBrdn_ycc()
                rm(make_pctHsCstBrdn_ycc)
                pctHsCstBrdn_ycc
        }
}