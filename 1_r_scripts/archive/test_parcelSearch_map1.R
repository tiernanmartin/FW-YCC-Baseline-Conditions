mySet2 <- RColorBrewer::brewer.pal(n = 8,name = "Set2")[c(3,6)]

pal <- colorFactor(palette = mySet2,domain = parcel_ycc@data$PUB_OWN_TYPE)

myLfltShiny() %>%
        fitBounds(lng1 = bounds_ycc["x","min"],lat1 = bounds_ycc["y","min"],
                  lng2 = bounds_ycc["x","max"],lat2 = bounds_ycc["y","max"]) %>% 
        addPolygons(data = parcel_ycc,
                    smoothFactor = 0,
                    stroke = F,
                    fillColor = ~pal(parcel_ycc@data$PUB_OWN_TYPE),fillOpacity = .75) %>% 
        addLegend(position = "topright",pal = pal, values = unique(parcel_ycc@data$PUB_OWN_TYPE))


myLfltShiny() %>%
        fitBounds(lng1 = bounds_ycc["x","min"],lat1 = bounds_ycc["y","min"],
                  lng2 = bounds_ycc["x","max"],lat2 = bounds_ycc["y","max"]) %>% 
        addPolygons(data = parcel_sea,smoothFactor = 0,stroke = F)

myLfltShiny() %>%
        addPolygons(data = parcel_sea,smoothFactor = 0,stroke = F)