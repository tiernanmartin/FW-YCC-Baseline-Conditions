fun <- function(tract = TRUE, unit = "hu"){
        
        tract = FALSE
        unit = "hu"
        
        geo <- if(tract == TRUE){tract_sea} else(bg_sea)
        blk <- blk_sea
        by_sp <- if(tract == TRUE){"TRACTCE"} else("GEOID")
        pop <- read_csv(file = "./2_inputs/wa_kc_blocks_pop/DEC_10_SF1_P1_with_ann.csv",col_types = "cccn")
        hu <- read_csv(file = "./2_inputs/wa_kc_blocks_hu/DEC_10_SF1_H1.csv",col_types = "cccn")
        count <- if(unit == "hu"){hu} else(pop)
                
        
        # Read in the Housing Units data from ACS 2010 (block scale)
        
        # Join urban village df and housing units df to the blocks shp,
        # then filter out blocks with 0 housing units, as well as those in manufacturing/industrial
        # and outside villages
        
        blk <- 
                blk_sea %>% 
                geo_join(data_frame = seaAcsUvs,
                         by_sp = "GEOID10",
                         by_df = "GEOID10") %>% 
                geo_join(data_frame = hu,
                         by_sp = "GEOID10",
                         by_df = "GEO.id2") %>% 
                .[!is.na(.@data$D001),] %>% 
                .[.@data$D001 > 0,] %>% 
                .[!is.na(.@data$URBAN_VILLAGE_NAME),] %>% 
                .[.@data$URBAN_VILLAGE_TYPE %!in% c("Manufacturing Industrial","Outside Villages"),]
        
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
        
        geos <-  sort(unique(blk@data$ID)) %>% 
                as.data.frame()
        
        colnames(geos) <- "ID"

        
        test <- 
                blk@data %>% 
                group_by(ID,UV) %>%  
                summarise(HU_COUNT = sum(D001)) %>% 
                spread(key = UV, value = HU_COUNT) 
        
        
        tractUV <- 
                blk@data %>% 
                group_by(ID,UV) %>%  
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

outline <- fun(tract = FALSE, unit = "hu") %>% 
        as('SpatialLines')

seaUVs_outline  <-  
        seaUVs[seaUVs@data$UV_TYPE %!in% "MIC",] %>% 
        as('SpatialLines')

blk_uvs <- 
        blk_sea %>% 
        geo_join(data_frame = seaAcsUvs,
                 by_sp = "GEOID10",
                 by_df = "GEOID10") %>% 
        geo_join(data_frame = hu,
                 by_sp = "GEOID10",
                 by_df = "GEO.id2") %>% 
        .[!is.na(.@data$D001),] %>% 
        .[.@data$D001 > 0,] %>% 
        .[!is.na(.@data$URBAN_VILLAGE_NAME),] %>% 
        .[.@data$URBAN_VILLAGE_TYPE %!in% c("Manufacturing Industrial","Outside Villages"),]

# Normalize the Housing Units count (0 to 1 scale)
blk_uvs@data %<>% 
        mutate(RANGE = D001) %>% 
        mutate_each_(funs(norm0to1), vars = "RANGE")

pal <- colorFactor(palette = "Set2",domain = test@data$UV)
pal2 <- colorFactor(palette = "Set2",domain = blk_uvs@data$URBAN_VILLAGE_NAME)
        
        
       
        
leaflet() %>% 
                addProviderTiles("CartoDB.Positron") %>% 
                addPolygons(group = "blocks",
                            data = blk_uvs,
                            smoothFactor = 0,
                            color = ~pal2(blk_uvs@data$URBAN_VILLAGE_NAME),opacity = .25, weight = 1,
                            fillColor = ~pal2(blk_uvs@data$URBAN_VILLAGE_NAME), fillOpacity = ~RANGE) %>%
                
                addPolylines(group = "block group geos",
                            data = bg_sea_outline,
                            color = col2hex("grey25"),opacity = .8, weight = 2) %>% 
                addPolygons(group = "block groups",
                            data = test,
                            popup = ~UV,
                            smoothFactor = 0,
                            stroke = F,
                            fillColor = ~pal(test@data$UV), fillOpacity = .75) %>% 
                addPolygons(group = "block groups",
                            data = bg_sea,
                            stroke = F,
                            fillOpacity = 0,
                            popup = ~GEOID) %>% 
                addPolylines(group = "block groups",
                             data = outline,
                             color = col2hex("white"),opacity = .75, weight = 2) %>%
                addPolylines(group = "urban villages",
                             data = seaUVs_outline,
                             color = col2hex("grey30"),opacity = .5, weight = 2) %>%
                
                addLegend(position = "topright",pal = pal, values = unique(test@data$UV)) %>% 
                addLayersControl(overlayGroups = c("blocks","block groups","urban villages"), position = "topright")
  