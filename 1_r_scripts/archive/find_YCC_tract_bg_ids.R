tract_ycc <- {
        
        names <- seaUvs_ycc@data$UV_NAME
        
        trts <- bg_uvs@data[bg_uvs@data$UV %in% names,"TRACTCE"] %>% 
                unlist %>% unique() %>% sort()
        
        test <- 
        tract_sea[tract_sea@data$TRACTCE %in% trts,]
        
        myLfltShiny() %>% 
                addPolygons(data = test,
                            popup = ~TRACTCE,
                            color = col2hex("white"), opacity = .75, weight = 3,
                            fillColor = col2hex("darkorange"), fillOpacity = .75)
        
        trts2 <- trts[trts %!in% c("006500","007700","008900","009400","009300","008200")]
        
        test2 <- 
                tract_sea[tract_sea@data$TRACTCE %in% trts2,]
        
        myLfltShiny() %>% 
                addPolygons(data = test,
                            popup = ~TRACTCE,
                            color = col2hex("white"), opacity = .75, weight = 3,
                            fillColor = col2hex("darkorange"), fillOpacity = .75) %>% 
                addPolygons(data = test2,
                            popup = ~TRACTCE,
                            color = col2hex("white"), opacity = .75, weight = 3,
                            fillColor = col2hex("tomato"), fillOpacity = .75)
                
        TRACTCE_ycc <- 
                c("007900", "008600", "007401", "007500", "008300", "008400", 
                  "008500", "008700", "008800", "009000", "009100", "007600", 
                  "007402", "009200")
        
        
        
}

bg_ycc <- {
        
        names <- seaUvs_ycc@data$UV_NAME
        
        test_bg <- 
        bg_uvs[bg_uvs@data$UV %in% names,] 
        
        
        mySet2 <- RColorBrewer::brewer.pal(n = 7, "Set2")
        pal <- colorFactor(palette = mySet2,domain = test_bg@data$UV)
        
        outline <- bg_uvs %>% 
                as('SpatialLines')
 
        tr_ids <- bg_sea@data$GEOID[bg_sea@data$TRACTCE %in% tract_ycc@data$TRACTCE]
        
        test <- 
                bg_sea[bg_sea@data$GEOID %in% tr_ids,]
        
        
        myLfltShiny() %>%
                addPolygons(group = "Block Groups",
                            data = test_bg,
                            smoothFactor = 0,
                            popup = ~UV,
                            stroke = FALSE,
                            fillColor = ~pal(test_bg@data$UV), fillOpacity = .75) %>% 
                addPolygons(data = bg_sea,
                            popup = ~GEOID,
                            color = col2hex("black"), opacity = .75, weight = 3,
                            fillOpacity = 0)
                
        rm <- c("530330093002","530330094002","530330089004","530330089002")
        
        test_bg %<>% .[.@data$GEOID %!in% rm,]
        
        paste(test_bg@data$GEOID,collapse = ",")
        
        # be sure to add "530330079003"
        
        c("530330079005","530330074014","530330084001","530330084003",
          "530330079001","530330079004","530330074021","530330088001",
          "530330075001","530330074012","530330065003","530330065002",
          "530330088003","530330085002","530330085003","530330086003",
          "530330087003","530330088002","530330085001","530330089003",
          "530330090001","530330091001","530330091002","530330092001",
          "530330092002","530330082002","530330082003","530330083001",
          "530330083002","530330074011","530330074013","530330075002",
          "530330075003","530330075004","530330075005","530330076001",
          "530330076002","530330076003","530330077003","530330077004",
          "530330086001","530330090002","530330086002","530330074023",
          "530330074022","530330084002","530330079002","530330087002",
          "530330087001","530330079003")
        
        bg_ycc <- 
                bg_sea[]
        
        
        
        
        
        
}