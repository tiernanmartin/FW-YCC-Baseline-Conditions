testFUN <- function(tract = TRUE, unit = "hu"){
        
        # tract = TRUE
        # unit = "hu"
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
                mutate(ID = row_number())
        
        df1 <-
                tractUV %>% 
                filter(UV %in% "Outside Villages")
                
        df2 <- 
                df1 %>% 
                select(-starts_with("Outside")) %>%
                select(-UV) %>% 
                select(-ID) %>%
                mutate(SUM = rowSums(x = .)) %>% 
                cbind(df1$ID) %>%
                filter(SUM > 0) 
        
        colnames(df2)[grep(pattern = "ID",x = colnames(df2))] <- "ID"
        
        
        IDSUM <- 
                df2 %>% 
                select(ID, SUM)
        
        df3 <- 
                df2 %>% 
                select(-ID) %>% select(-SUM) %>% 
                mutate(UV2 = max.col(m= .,ties.method = "first")) %>% 
                mutate(UV2 = colnames(.)[UV2]) %>% 
                cbind(IDSUM)
        
        df4 <- 
                df3 %>% 
                group_by(UV2) %>% 
                tidyr::nest() %>% 
                mutate(HU = purrr::map(.x = data, .f = function(x){
                       
                        
                        
                        x <- x %>% as.data.frame()
                        
                        IDSUM <- 
                                x %>% 
                                select(ID,SUM)
                        
                        x %<>% select(-ID) %>% select(-SUM)
                        
                        high <- 
                                x %>% colSums() %>% which.max() %>% names()
                        
                        ID <- "ID"
                        row <- which.max(x[,high])
                        
                        x %<>%
                                cbind(IDSUM)
                        
                        new <- 
                                x[row,c(high,ID)] %>% 
                                as.data.frame() %>% 
                                mutate(UV = colnames(.)[1])
                        
                        
                        colnames(new)[1] <- "HU"
                        
                       return(new)
                                
                })) %>% 
                .["HU"] %>% 
                unnest() %>% 
                rename(UV2 = UV) %>% 
                filter(UV2 %!in% tractUV$UV)
        
        
        df5 <- 
                tractUV %>% 
                select(UV) %>% 
                bind_cols(geos) %>% 
                select(GEOID = ID,UV) %>% 
                mutate(ID = row_number()) %>% 
                left_join(df4, by = "ID") %>% 
                mutate(UV3 = ifelse(is.na(UV2),
                                    UV,
                                    UV2)) %>% 
                select(-ID)
        
        
        shp <- 
                geo_join(spatial_data = geo,
                         data_frame = df5,
                         by_sp = by_sp,
                         by_df = "GEOID")
        
        # Remove the "Outside Villages"
        shp %<>%
                .[shp@data$UV3 %!in% c("Outside Villages",
                                       "Not an Urban Village") & !is.na(shp@data$UV3),]
        
        return(shp)
        
}

test <- testFUN(tract = T)

test2 <-
        test %>% 
        .[.@data$UV %!in% "Outside Villages",]


pal <- colorFactor(palette = "Set2",domain = test@data$UV3)
pal2 <- colorFactor(palette = "Set2",domain = test2@data$UV)

myLfltShiny() %>% 
        addPolygons(group = "Some",
                    data = test2,
                    popup = ~UV,
                    smoothFactor = 0,
                    color = "white", weight = 2, opacity = .5,
                    fillColor = ~pal2(test2@data$UV), fillOpacity = .75) %>% 
        addPolygons(group = "All",
                    data = test,
                    popup = ~UV3,
                    smoothFactor = 0,
                    color = "white", weight = 2, opacity = .5,
                    fillColor = ~pal(test@data$UV3), fillOpacity = .75) %>% 
        addLayersControl(overlayGroups = c("Some", "All"),options = layersControlOptions(collapsed = F))

        