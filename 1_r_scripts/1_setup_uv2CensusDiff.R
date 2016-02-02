fun <- function(tract = TRUE, unit = "hu"){
        
        geo <- if(tract == TRUE){tract_sea} else(bg_sea)
        blk <- blk_sea
        by_sp <- if(tract == TRUE){"TRACTCE"} else{"GEOID"}
        pop <- read_csv(file = "./2_inputs/wa_kc_blocks_pop/DEC_10_SF1_P1_with_ann.csv",col_types = "cccn")
        hu <- read_csv(file = "./2_inputs/wa_kc_blocks_hu/DEC_10_SF1_H1.csv",col_types = "cccn")
        count <- if(unit == "hu"){hu} else(pop)
                
        
        # Read in the Housing Units data from ACS 2010 (block scale)
        
        # Join it with the block spatial polygon df
        blk %<>% 
                myGeoJoin(data_frame = count,by_sp = "GEOID10",by_df = "GEO.id2") %>% 
                .[.@data$D001 > 0 & !is.na(.@data$D001),]
        
        blk <- geo_join(spatial_data = blk,
                        data_frame = seaAcsUvs,
                        by_sp = "GEOID10", 
                        by_df = "GEOID10")
        
        # Create different `ID` column depending on whether this is a tract-level or block-group-level operation
        if(tract == TRUE){
                blk@data %<>%
                        mutate(ID = substr(GEOID10,6,11)) %>% 
                        select(ID,D001,UV = URBAN_VILLAGE_NAME)
        } else {
                blk@data %<>%
                        mutate(ID = substr(GEOID10,1,12)) %>% 
                        select(ID,D001,UV = URBAN_VILLAGE_NAME)
        }

        # Remove the blocks whose Urban Village is 'NA'
        blk <- blk[!is.na(blk@data$UV),]
        
        geos <-  sort(unique(blk@data$ID)) %>% 
                as.data.frame()
        
        colnames(geos) <- "ID"

        tractUV <- 
                blk@data %>% 
                group_by(UV,ID) %>%  
                summarise(HU_COUNT = sum(D001)) %>% 
                spread(key = UV, value = HU_COUNT) %>% 
                ungroup() %>% 
                select(-ID)  %>% 
                replace(is.na(.),0) %>% 
                mutate(UV = max.col(m= .,ties.method = "first")) %>% 
                mutate(UV = colnames(.)[as.numeric(UV)]) %>% 
                select(UV) %>% 
                bind_cols(geos) %>% 
                select(ID,UV)
        
        shp <- 
                geo_join(spatial_data = geo,
                         data_frame = tractUV,
                         by_sp = by_sp,
                         by_df = "ID")
        
        # Remove the "Outside Villages"
        shp %<>%
                .[shp@data$UV %!in% "Outside Villages" & !is.na(shp@data$UV),]
        
        return(shp)
        
        
        
        
}

test <- fun(tract = FALSE, unit = "hu")

test2 <- fun(tract = FALSE, unit = "pop")

x <- as.data.frame(test@data)
y <- as.data.frame(test2@data)

diff <- full_join(x = x,
                   y = y,
                   by =  "ID") %>%
        filter(UV.x != UV.y) %>% 
        select(ID) %>% 
        unlist()

differs <- 
        test[test@data$ID %in% diff,] %>% 
        gCentroid(byid = TRUE) %>% 
        spTransform(CRSobj = crs_proj)

        
        pal <- colorFactor(palette = "Set2",domain = test@data$UV)
        pal2 <- colorFactor(palette = "Set2",domain = test2@data$UV)
        
        leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(group = "Housing Units",
                            data = test,
                            popup = ~UV,
                            smoothFactor = 0,
                            stroke = F,
                            fillColor = ~pal(test@data$UV), fillOpacity = .75) %>% 
                addPolygons(group = "Population",
                            data = test2,
                            popup = ~UV,
                            smoothFactor = 0,
                            stroke = F,
                            fillColor = ~pal2(test2@data$UV), fillOpacity = .75) %>% 
                addPolylines(data = bg_sea_outline,
                             color = col2hex("white"),opacity = .5, weight = 2) %>% 
                addCircles(group = "Difference",
                           data = differs) %>% 
                addLegend(position = "topright",pal = pal, values = unique(test@data$UV)) %>% 
                addLayersControl(overlayGroups = c("Housing Units","Population","Difference"), position = "topright")
  