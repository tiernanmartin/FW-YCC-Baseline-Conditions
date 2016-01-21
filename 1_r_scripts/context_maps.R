# CAC BASELINE CONDITIONS REPORT

# SETUP: LOAD PACKAGES AND PROJECT SETTINGS --------------------------------------------------------------

source(file = "./1_r_scripts/setup_and_spatial_data.R")

# -------------------------------------------------------------------------------------------------        

# NEIGHBORHOODS ------------------------------------------------------------------------

mylflt_seaNhoods <- function(){
 
        pal <- colorFactor(palette = "Set2",
                           domain = seaNhoods@data$S_HOOD)
        
        leaflet() %>% 
                addProviderTiles("CartoDB.PositronNoLabels") %>% 
                addPolygons(data = seaNhoods,
                            smoothFactor = 0,
                            fillColor = ~pal(seaNhoods@data$S_HOOD), fillOpacity = .5,
                            stroke = F,
                            popup = paste0(seaNhoods@data$S_HOOD))
}

# mylflt_seaNhoods() %>%
#         saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/lflt_seaNhoods.html")

mylflt_seaUVs <- function(){
        
        pal <- colorFactor(palette = "Set2",
                           domain = seaUVs@data$UV_TYPE)
        
        palLeg <- colorFactor(palette = "Set2",
                           domain = seaUVs@data$TYPE_NAME)
        
        leaflet() %>% 
                addProviderTiles("CartoDB.PositronNoLabels") %>% 
                addPolygons(data = seaUVs,
                            smoothFactor = 0,
                            fillColor = ~pal(seaUVs@data$UV_TYPE), fillOpacity = .5,
                            stroke = F,
                            popup = paste0("<h3>",seaUVs@data$UV_NAME,"</h3>",
                                           seaUVs@data$TYPE_NAME)) %>% 
                addPolylines(data = seaUVs_outline,
                             color = col2hex("white"), weight = 2, opacity = .5,stroke = T,
                             fill = F) %>% 
                addLegend(pal = palLeg, 
                          values = seaUVs@data$TYPE_NAME,
                          position = "topright", 
                          title = "Urban Village Hierarchy",
                          opacity = .5)
}

# mylflt_seaUVs() %>%
#         saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/lflt_seaUVs.html")

mylflt_seaUVs() %>% addPolylines(data = seaNhoods_outline,
                                 group = "Neighborhoods",
                                 color = col2hex("dodgerblue"), weight = 1, opacity = 1,stroke = T,
                                 fill = F) %>% 
        addLayersControl(overlayGroups = c("Neighborhoods")) %>% 
        saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/lflt_seaUVs_nhoods.html")
        

mylflt_CAC_bounds <- function(){
        yt <- seaUVs_CAC[seaUVs_CAC$NHOOD_ABBR %in% c("YT"),]
        
        yt_choiceBound <- readOGR(dsn = "./2_inputs/",layer = "yt_choiceBound") %>% 
                spTransform(CRSobj = crs_proj)
        
        pal <- RColorBrewer::brewer.pal(6,"Set2")
        
        yellow <- pal[[6]]
        blue <- pal[[3]]
        orange <- pal[[2]]
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = yt,
                            group = "Yesler Terrace",
                            stroke = F,
                            fillColor = yellow, fillOpacity = .75) %>% 
                addPolygons(data = yt_choiceBound,
                            group = "YT Choice Boundary",
                            fill = FALSE,
                            color = yellow,weight = 3, opacity = .75, dashArray = "5, 5") %>% 
                addPolygons(data = bgatz,
                            group = "Bailey-Gatzert",
                            fill = FALSE,
                            color = blue, weight = 3, opacity = .75, dashArray = "2.5, 5, 10, 5") %>% 
                addPolygons(data = myCACbound,
                            group = "CAC Boundary",
                            fill = FALSE,
                            color = orange, weight = 4, opacity = 1, dashArray = "10, 5") %>% 
                addLayersControl(overlayGroups = c("Yesler Terrace",
                                                   "YT Choice Boundary",
                                                   "Bailey-Gatzert",
                                                   "CAC Boundary"))
                
        
}

mylflt_CAC_bounds() %>% 
        saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/lflt_CAC_bounds.html")
