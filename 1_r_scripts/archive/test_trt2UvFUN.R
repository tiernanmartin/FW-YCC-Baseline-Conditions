trt_test <- readOGR(dsn = "./2_inputs/",layer = "tract_CAC") %>% 
        spTransform(CRSobj = crs_proj)

blk_test <- readOGR(dsn = "./2_inputs/",layer = "blk_CAC") %>% 
        spTransform(CRSobj = crs_proj) %>% 
        .[!is.na(.@data$STATEFP10),] %>% 
        .[.@data$TRACTCE10 %in% trt_test@data$TRACTCE,]




{
test <- 
        blk_test 

test2 <- 
        test[sapply(slot(test, 'polygons'), function(i) slot(i, 'area') >= .00000025),] 

test2cent <- test[sapply(slot(test, 'polygons'), function(i) slot(i, 'area') < .00000025),] %>% 
        gCentroid(byid = TRUE)

myLeaflet(test2) %>% 
        addCircles(data = test2cent)
}

hu <- read_csv(file = "./2_inputs/wa_kc_blocks_hu/DEC_10_SF1_H1.csv",col_types = "cccn")

blk_test <- 
        blk_test %>% 
        myGeoJoin(data_frame = hu,by_sp = "GEOID10",by_df = "GEO.id2") %>% 
        .[.@data$D001 > 0,]

blk_test@data %<>%
        select(TRACTCE10,GEOID10,D001)




{
pal <- colorNumeric(palette = "Spectral",domain = blk_test$D001)


leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        addPolygons(data = blk_test,
                    stroke = F,
                    smoothFactor = 0,
                    fillColor = ~pal(blk_test$D001), fillOpacity = .75) %>% 
        addLegend(position = "topright",pal = pal, values = range(0,max(blk_test$D001)))
}


whichUV <- function(){
        
        cents <- gCentroid(spgeom = blk_test,byid = TRUE)
        
        UVcount <- seaUVs_CAC$NHOOD_ABBR %>% unique %>% length
        
        map <- setNames(c("none",seaUVs_CAC$NHOOD_ABBR), 0:UVcount)
        
        a <- gWithin(spgeom1 = cents,spgeom2 = seaUVs_CAC,byid = TRUE, returnDense = FALSE) %>%
                sapply(function(x) ifelse(is.null(x), 0, x)) %>%
                sapply(function(x) x+1) %>% 
                map[.] %>% 
                as.data.frame()
        
        
        
        blk_test@data %<>% 
                as.data.frame() %>% 
                bind_cols(a) %>% 
                as.data.frame()
        
        colnames(blk_test@data)[4] <- "UV"
        
        trts <-  sort(unique(blk_test@data$TRACTCE10)) %>% 
                as.data.frame()
        
        colnames(trts) <- "TRACTCE10"
        
        tractUV <- 
                blk_test@data %>% 
                group_by(TRACTCE10,UV) %>% 
                summarise(HU_COUNT = sum(D001)) %>% 
                spread(key = UV, value = HU_COUNT) %>% 
                ungroup() %>% 
                select(-TRACTCE10)  %>% 
                replace(is.na(.),0) %>% 
                mutate(UV = max.col(m= .,ties.method = "first")) %>% 
                mutate(UV = colnames(.)[as.numeric(UV)]) %>% 
                select(UV) %>% 
                bind_cols(trts) %>% 
                select(TRACTCE10,UV)
                
        test <- 
                geo_join(spatial_data = trt_test,
                 data_frame = tractUV,
                 by_sp = "TRACTCE",
                 by_df = "TRACTCE10")
        
        test@data %<>%
                as.data.frame %>% 
                mutate(UV = factor(UV))
        
        pal = colorFactor(palette = "Set2",domain = test@data$UV)
        
        pal <- colorFactor(palette = "Set2",
                           domain = seaNhoods@data$S_HOOD)
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = test,
                            smoothFactor = 0,
                            weight = 1, color = col2hex("white"), opacity = 1,
                            fillColor = ~pal(test@data$UV), fillOpacity = .75) %>% 
                addLegend(position = "topright",pal = pal, values = unique(test@data$UV))
        
        
}