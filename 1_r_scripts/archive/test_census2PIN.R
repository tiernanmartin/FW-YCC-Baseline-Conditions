
df <- read_csv(file = "./2_inputs/Capacity_For_All_Parcel_2015.csv")

# chntwn <- seaUvs_ycc@data %>% filter(UV_NAME == "Chinatown-International District") %>% select(VILLNUMB) %>% unique() %>% unlist(use.names = FALSE)

t_parcel <- 
        geo_join(spatial_data = parcel_sea,
                 data_frame = df,
                 by_sp = "PIN",
                 by_df = "PIN")


cnts <- gCentroid(spgeom = t_parcel,byid = TRUE)

bg <- bg_uvs

# myLeaflet(bg) %>% 
#         addCircles(data = cnts)

t_df <- 
        sp::over(x = t_parcel,y = bg) %>% select(GEOID)

t_df2 <- 
        t_parcel@data %>% 
        select(PIN) %>% 
        bind_cols(t_df)

t_parcel2 <- 
        geo_join(spatial_data = t_parcel,data_frame = t_df2, by_sp = "PIN", by_df = "PIN")


library(microbenchmark)

microbenchmark(t_FUN)




pal <- colorFactor(palette = "Set2",domain = t_parcel2@data$GEOID)

myLfltShiny() %>% 
        addPolygons(data = t_parcel2,
                    smoothFactor = 0,
                    stroke = F,
                    fillColor = ~pal(t_parcel2@data$GEOID), fillOpacity = .75)

