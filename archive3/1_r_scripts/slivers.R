library(rgdal)      
library(sp)
library(rgeos)
library(tigris)
library(magrittr)
library(leaflet)
library(gplots)
library(tmap)

# This project will use WGS 84 projected coordinate system
crs_proj <- CRS("+init=epsg:4326")

# These are the FIPS codes of the specific block groups in my study area
sel <- c("530330079005", "530330079001", "530330079004", 
         "530330085002", "530330085003", "530330086003", 
         "530330087003", "530330085001", "530330090001", 
         "530330091001", "530330091002", "530330092001", 
         "530330092002", "530330086001", "530330090002", 
         "530330086002", "530330079003", "530330079002", 
         "530330087002", "530330087001")

# Create polygons
polygons <- tigris::block_groups(state = "WA",county = "King") %>% 
        .[.@data$GEOID %in% sel,] %>% 
        spTransform(CRSobj = crs_proj)

# Map1: Full Opacity
leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        addPolygons(data = polygons, smoothFactor = 0,
                    weight = .75, color = col2hex("red"), opacity = 1,
                    fillColor = col2hex("red"), fillOpacity = 1)


# Map1: Reduced Opacity
leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        addPolygons(data = polygons, smoothFactor = 0,
                    stroke = FALSE,
                    fillColor = col2hex("red"), fillOpacity = .5)

