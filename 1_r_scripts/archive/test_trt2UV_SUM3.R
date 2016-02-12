
UV2Census <- function(tract = TRUE, unit = "hu"){
        
        tract = TRUE
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
                geo_join(data_frame = count,
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
        
        
        geos <- blk@data$ID %>%
                sort() %>% 
                unique() %>% 
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
                mutate(UV = ifelse(rowSums(x = .[,colnames(.) %!in% "UV"]) == 0,
                                   "Not an Urban Village",
                                   UV)) %>%
                mutate(SUM = rowSums(.[,sapply(.,is.numeric)])) %>% 
                mutate(ID = as.character(row_number()))
        
        df1 <-
                tractUV %>% 
                filter(UV %in% "Outside Villages") %>% 
                select(-starts_with("Outside")) %>% 
                select(-starts_with("SUM")) %>% 
                mutate(SUM2 = rowSums(.[,sapply(.,is.numeric)])) %>% 
                filter(SUM2 > 0) %>% 
                mutate(SUM2 = as.character(SUM2)) %>% 
                mutate(UV2 = max.col(m= .[,sapply(.,is.numeric)],ties.method = "first")) %>% 
                mutate(UV2 = colnames(.)[UV2]) %>%  
                group_by(UV2) %>% 
                tidyr::nest() %>% 
                mutate(HU = purrr::map(.x = data, .f = function(x){
                        
                        # x <- df4$data[[1]] %>% as.data.frame()
                        
                        x <- as.data.frame(x)
                
                        high <- 
                                colSums(x[,sapply(x,is.numeric)]) %>% which.max() %>% names()
                        
                        ID <- "ID"
                        SUM2 <- "SUM2"
                        
                        row <- which.max(x[,high])
                        
                        new <- 
                                x[row,c(high,ID,SUM2)] %>% 
                                as.data.frame() %>% 
                                mutate(UV = colnames(.)[1]) %>% 
                                .[,2:4]
                        
                        return(new)
                        
                })) %>% 
                .["HU"] %>%
                unnest() %>% 
                rename(UV2 = UV) %>% 
                filter(UV2 %!in% tractUV$UV) %>% 
                mutate(ID = as.numeric(ID),
                       SUM2 = as.numeric(SUM2))
        
        
        df2 <- 
                tractUV %>% 
                select(UV,SUM) %>% 
                bind_cols(geos) %>% 
                select(GEOID = ID,UV,SUM) %>% 
                mutate(ID = row_number()) %>% 
                left_join(df1, by = "ID") %>% 
                mutate(UV3 = ifelse(is.na(UV2),
                                    UV,
                                    UV2)) %>% 
                mutate(SUM3 = ifelse(is.na(SUM2),
                                     SUM,
                                     SUM2)) %>% 
                select(-ID) %>% select(GEOID,UV,SUM,UV2,SUM2,UV3,SUM3)
        
        
        shp <- 
                geo_join(spatial_data = geo,
                         data_frame = df2,
                         by_sp = by_sp,
                         by_df = "GEOID")
        
        # Remove the "Outside Villages"
        shp %<>%
                .[shp@data$UV3 %!in% c("Outside Villages",
                                       "Not an Urban Village") & !is.na(shp@data$UV3),]
        
        return(shp)
        
}
