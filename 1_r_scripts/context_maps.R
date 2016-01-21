# CAC BASELINE CONDITIONS REPORT

# SETUP: LOAD PACKAGES AND PROJECT SETTINGS --------------------------------------------------------------

source(file = "./1_r_scripts/setup_and_spatial_data.R")

# -------------------------------------------------------------------------------------------------        

# NEIGHBORHOODS ------------------------------------------------------------------------

mylflt_seaNhoods <- function(){
 
        pal <- colorFactor(palette = "Set2",
                           domain = seaNhoods@data$S_HOOD)
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
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
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = seaUVs,
                            smoothFactor = 0,
                            fillColor = ~pal(seaUVs@data$UV_TYPE), fillOpacity = .5,
                            stroke = F,
                            popup = paste0("<h3>",seaUVs@data$UV_NAME,"</h3>",
                                           seaUVs@data$TYPE_NAME)) %>% 
                addPolylines(data = seaUVs_outline,
                             color = col2hex("white"), weight = 1, opacity = .25,stroke = T,
                             fill = F) %>% 
                addLegend(pal = palLeg, 
                          values = seaUVs@data$TYPE_NAME,
                          position = "topright", 
                          title = "Urban Village Hierarchy",
                          opacity = .5)
}

mylflt_seaUVs() %>%
        saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/lflt_seaUVs.html")

mylflt_seaUVs() %>% addPolylines(data = seaNhoods_outline,
                                 color = col2hex("dodgerblue"), weight = 1, opacity = .75,stroke = T,
                                 fill = F) %>% 
        # saveWidget(file = "~/Documents/FW/YCC/FW-YCC-Baseline-Conditions/4_webcontent/html/lflt_seaUVs_nhoods.html")
        