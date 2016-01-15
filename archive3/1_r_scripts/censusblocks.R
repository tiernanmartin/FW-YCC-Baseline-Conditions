# get blocks with block group FIPS and population
# over[blocks, myCACbound] -> 'in' and 'out' strings
# group by bg and sum both 'in' and 'out'
# keep the bg if() 'in' > 'out'

# SPATIAL DATA -----
blks <- readOGR(dsn = "2_inputs/blocks10/",layer = "blocks10") %>% 
        spTransform(CRSobj = crs_proj)

myCACbound %<>% spTransform(CRSobj = crs_proj)

bg_geoids <- bg_myCAC@data$GEOID

blks_CAC <- blks[blks@data$GEO_ID_GRP %in% bg_geoids,]
        

# JOIN THE DEMOGRAPHIC DATA ------

blk_df <- readr::read_csv(file = "./2_inputs/wa_kc_blocks_pop/DEC_10_SF1_P1_with_ann.csv",
                          skip = 1,col_types = "cccn")

blks_CAC <-  geo_join(spatial_data = blks_CAC,
                      data_frame = blk_df,
                      by_sp = "GEO_ID_BLK", by_df = "Id2") %>% 
        spTransform(CRSobj = crs_proj)

blks_CAC@data %<>% select(GEO_ID_GRP,GEO_ID_BLK,Geography,Total)

overlap <- gIntersects(spgeom1 = blks_CAC,
                       spgeom2 = myCACbound,
                       byid = TRUE) %>% 
        t() %>% 
        data.frame() %>% 
        rename(CAC = X0)

test <- blks_CAC

test@data <-  bind_cols(as.data.frame(test@data),
                            overlap) %>% 
        as.data.frame() 

sum_df <- as.data.frame(test@data) %>% 
        group_by(GEO_ID_GRP,CAC) %>% 
        mutate(Count = sum(Total)) %>% 
        ungroup() %>% 
        group_by(GEO_ID_GRP) %>% 
        arrange(Count) %>% 
        mutate(INCLUDE = if(first(CAC) == TRUE) FALSE else TRUE) %>% 
        ungroup() %>% 
        select(GEO_ID_BLK, INCLUDE)

blks_CAC <-  geo_join(spatial_data = blks_CAC,
                      data_frame = sum_df,
                      by_sp = "GEO_ID_BLK", by_df = "GEO_ID_BLK") %>% 
        spTransform(CRSobj = crs_proj)

