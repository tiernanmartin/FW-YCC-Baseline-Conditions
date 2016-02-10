
UV2Census <- function(tract = TRUE, unit = "hu"){
        
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
                .[!is.na(.@data$URBAN_VILLAGE_NAME),]
        
        
        
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
