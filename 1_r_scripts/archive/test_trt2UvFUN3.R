fun <- function(tract = TRUE,blk,colName){
        
        # REMOVE THESE AFTER TESTING

        geo <- if(tract == TRUE){tract_sea} else {bg_sea}
        blk <- blk_sea
        colName <- "TRACTCE10"
        print(colName)

        
        # Read in the Housing Units data from ACS 2010 (block scale)
        hu <- read_csv(file = "./2_inputs/wa_kc_blocks_hu/DEC_10_SF1_H1.csv",col_types = "cccn")
        
        # Join it with the block spatial polygon df
        blk %<>% 
                myGeoJoin(data_frame = hu,by_sp = "GEOID10",by_df = "GEO.id2") %>% 
                .[.@data$D001 > 0 & !is.na(.@data$D001),]
        
        blk <- geo_join(spatial_data = blk,
                        data_frame = seaAcsUvs,
                        by_sp = "GEOID10", 
                        by_df = "GEOID10")
        blk@data %<>%
                select(TRACTCE10,GEOID10,D001,UV = URBAN_VILLAGE_NAME)
        
        # Remove the blocks whose Urban Village is 'NA'
        blk <- blk[!is.na(blk@data$UV),]
        
        geos <-  sort(unique(blk@data[[colName]])) %>% 
                as.data.frame()
        
        colnames(geos) <- colName
        
        # This is a necessary step for the 'group_by_' function in the following chain
        grp_cols <- 
                blk@data %>% 
                select(UV,contains(colName)) %>% 
                names()
        
        drp_col <- 
                blk@data %>% 
                select(contains(colName)) %>% 
                names()
        
        
        tractUV <- 
                blk@data %>% 
                group_by_(.dots = grp_cols) %>%  
                summarise(HU_COUNT = sum(D001)) %>% 
                spread(key = UV, value = HU_COUNT) %>% 
                ungroup() %>% 
                .[,2:ncol(.)]  %>% 
                replace(is.na(.),0) %>% 
                mutate(UV = max.col(m= .,ties.method = "first")) %>% 
                mutate(UV = colnames(.)[as.numeric(UV)]) %>% 
                select(UV) %>% 
                bind_cols(geos) %>% 
                select_(.dots = grp_cols)
        
        test <- 
                geo_join(spatial_data = geo,
                         data_frame = tractUV,
                         by_sp = "TRACTCE",
                         by_df = colName)
        
        
        
        
        # # Select the relevant variables
        # blk@data %<>%
        #         select(TRACTCE10,GEOID10,D001,URBAN_VILLAGE_NAME)
        # 
        # # Find the centroid of the blocks
        # cents <- gCentroid(spgeom = blk,byid = TRUE)
        # 
        # # How many urban villages are there?
        # UVcount <- seaUVs$UV_NAME %>% unique() %>% length()
        # 
        # # Create a cross-tabulating "map"
        # map <- setNames(c("none",seaUVs$UV_NAME), 0:UVcount)
        # 
        # # Apply the map to the 
        # a <- gWithin(spgeom1 = cents,spgeom2 = seaUVs,byid = TRUE, returnDense = FALSE) %>%
        #         sapply(function(x) ifelse(is.null(x), 0, x)) %>%
        #         sapply(function(x) x+1) %>% 
        #         map[.] %>% 
        #         as.data.frame()
        # 
        # blk@data %<>% 
        #         as.data.frame() %>% 
        #         bind_cols(a) %>% 
        #         as.data.frame()
        # 
        # colnames(blk@data)[4] <- "UV"
        
        
        
        
        test@data %<>%
                as.data.frame %>% 
                mutate(UV = factor(UV))
        
        
        
}




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
        
        
        
        pal <- colorFactor(palette = "Set2",domain = test@data$URBAN_VILLAGE_NAME)
        
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(data = test,
                            popup = ~URBAN_VILLAGE_NAME,
                            smoothFactor = 0,
                            weight = 1, color = col2hex("white"), opacity = 1,
                            fillColor = ~pal(test@data$URBAN_VILLAGE_NAME), fillOpacity = .75) %>% 
                addLegend(position = "topright",pal = pal, values = unique(test@data$URBAN_VILLAGE_NAME))
        
        
}