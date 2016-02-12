
# SETUP -------------------------------------------------------------------------------------------


source("./1_setup_1_functions.R")
source("./1_setup_1_data.R")
library(shiny)


# UI ----------------------------------------------------------------------------------------------
ui <-
        shinyUI(navbarPage(title = "Seattle Spatial Dilemma", id = "nav",
                           tabPanel(title = "Urban Villages",
                                    div(class="outer",
                                        tags$head(includeCSS("style.css")),
                                        leafletOutput("map1",height = "100%",width = "100%"),
                                        tags$div(id="cite",
                                                 tags$i("Source: City of Seattle"))
                                        )
                                    ),
                           tabPanel(title = "Housing Units",
                                    div(class="outer",
                                        tags$head(includeCSS("style.css")),
                                        leafletOutput("map2",height = "100%",width = "100%"),
                                        tags$div(id="cite",
                                                 tags$i("Source: City of Seattle"))
                                    )
                           ),tabPanel(title = "Blocks",
                                      div(class="outer",
                                          tags$head(includeCSS("style.css")),
                                          leafletOutput("map3",height = "100%",width = "100%"),
                                          tags$div(id="cite",
                                                   tags$i("Source: City of Seattle"))
                                      )
                           ),
                           tabPanel(title = "Blocks + Block Groups",
                                    div(class="outer",
                                        tags$head(includeCSS("style.css")),
                                        leafletOutput("map4",height = "100%",width = "100%"),
                                        tags$div(id="cite",
                                                 tags$i("Source: City of Seattle"))
                                    )
                           ),tabPanel(title = "Block Groups by UV",
                                      div(class="outer",
                                          tags$head(includeCSS("style.css")),
                                          leafletOutput("map5",height = "100%",width = "100%"),
                                          tags$div(id="cite",
                                                   tags$i("Source: City of Seattle"))
                                      )
                           ),
                           tabPanel(title = "Interactive Map",
                                    div(class="outer",
                                        tags$head(includeCSS("style.css")),
                                        leafletOutput("map6",height = "100%",width = "100%"),
                                        tags$div(id="cite",
                                                 tags$i("Source: City of Seattle"))
                                    )
                           )
                               ))
                           
                           
                           

# SERVER ------------------------------------------------------------------------------------------
server <- function(input, output){
        
        
        map1 <- {
                
                seaUvs@data  %<>% 
                        mutate(TYPE_NAME = factor(TYPE_NAME,levels = c("Urban Center Village",
                                                                       "Urban Center",
                                                                       "Hub Urban Village",
                                                                       "Residential Urban Village",
                                                                       "Manufacturing Industrial"
                        ),ordered = TRUE))
                
                seaUvs %<>%
                        .[!is.na(seaUvs@data$TYPE_NAME),]
                
                mySet2 <- RColorBrewer::brewer.pal(n = 7, "Set2")
                pal <- colorFactor(palette = mySet2,domain = sort(seaUvs@data$TYPE_NAME))
                
                myLfltShiny() %>% 
                        addPolygons(data = seaUvs,
                                    smoothFactor = 0,
                                    color = col2hex("white"),opacity = .33, weight = 1.5,
                                    fillColor = ~pal(seaUvs@data$TYPE_NAME), fillOpacity = .75) %>%
                        addLegend(title = "Urban Centers & Villages",position = "topright",pal = pal, values = unique(seaUvs@data$TYPE_NAME))
        }
        
        map2 <- {

                dat <-
                        blk_sea %>%
                        geo_join(data_frame = hu,
                                 by_sp = "GEOID10",
                                 by_df = "GEO.id2") %>%
                        .[!is.na(.@data$D001),] %>%
                        .[.@data$D001 > 0,]

                x <- RColorBrewer::brewer.pal(n = 9,name = "Oranges")[1:7]

                # pal <- colorNumeric(palette = x,domain = dat@data$D001)

                pal <- colorQuantile(palette = x, domain = dat@data$D001,probs = seq(0,1, by=0.20))

                bins <- cut(dat@data$D001, breaks=c(quantile(dat@data$D001,  probs = seq(0,1, by=0.20),
                                                                na.rm=FALSE, names=TRUE, include.lowest=FALSE, right = TRUE,
                                                                labels=c("1","2","3","4","5")))) %>%
                        unique() %>% sort()
                cols <- pal(dat@data$D001) %>% unique() %>% sort(decreasing = TRUE)


                # myLfltShiny() %>%
                #         addPolygons(data = dat,
                #                     smoothFactor = 0,
                #                     stroke = FALSE,
                #                     # color = col2hex("white"),opacity = .33, weight = 1.5,
                #                     fillColor = ~pal(dat@data$D001), fillOpacity = .75) %>%
                #         addLegend(title = "Housing Units",
                #                   position = "topright",pal = pal, values = range(1,max(dat@data$D001)))

                myLfltShiny() %>%
                        addPolygons(data = dat,
                                    smoothFactor = 0,
                                    stroke = FALSE,
                                    # color = col2hex("white"),opacity = .33, weight = 1.5,
                                    fillColor = ~pal(dat@data$D001), fillOpacity = .75) %>%
                        addLegend(title = "Housing Units",
                                  position = "topright",
                                  colors = cols,
                                  labels = bins)
        }

        map3 <- {

                blk_uvs <-
                        blk_sea %>%
                        geo_join(data_frame = seaAcsUvs,
                                 by_sp = "GEOID10",
                                 by_df = "GEOID10") %>%
                        .[!is.na(.@data$URBAN_VILLAGE_NAME),] %>%
                        .[.@data$URBAN_VILLAGE_NAME %!in% c("Outside Villages"),]

                mySet2 <- RColorBrewer::brewer.pal(n = 7, "Set2")
                pal <- colorFactor(palette = mySet2,domain = blk_uvs@data$URBAN_VILLAGE_NAME)

                myLfltShiny() %>%
                        addPolygons(group = "Blocks",
                                    data = blk_uvs,
                                    smoothFactor = 0,
                                    color = col2hex("white"),opacity = .33, weight = 1.5,
                                    fillColor = ~pal(blk_uvs@data$URBAN_VILLAGE_NAME), fillOpacity = .75) %>%
                        addPolygons(group = "Urban Village Boundaries",
                                    data = seaUvs,
                            color = col2hex("orange"), weight = 2, opacity =1,
                            fillOpacity = 0) %>%
                        addLegend(title = "Blocks by Urban Village",position = "topright",pal = pal, values = unique(blk_uvs@data$URBAN_VILLAGE_NAME)) %>%
                        addLayersControl(overlayGroups = c("Blocks","Urban Village Boundaries"),position = "topright",options = layersControlOptions(collapsed = FALSE))
        }

        map4 <- {

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


                mySet2 <- RColorBrewer::brewer.pal(n = 7, "Set2")
                pal <- colorFactor(palette = mySet2,domain = blk_uvs@data$URBAN_VILLAGE_NAME)


                myLfltShiny() %>%
                        addPolygons(group = "Blocks",
                                    data = blk_uvs,
                                    smoothFactor = 0,
                                    stroke = F,
                                    fillColor = ~pal(blk_uvs@data$URBAN_VILLAGE_NAME), fillOpacity = .75) %>%
                        addPolygons(group = "Blocks by HU Density",
                                    data = blk_uvs,
                                    smoothFactor = 0,
                                    color = ~pal(blk_uvs@data$URBAN_VILLAGE_NAME),opacity = .5, weight = 1.5,
                                    fillColor = ~pal(blk_uvs@data$URBAN_VILLAGE_NAME), fillOpacity = ~RANGE) %>%
                        # addPolygons(group = "Urban Village Boundaries",
                        #             data = seaUvs,
                        #             color = col2hex("orange"), weight = 2, opacity =.5,
                        #             fillOpacity = 0) %>%
                        addPolygons(group = "Block Groups",
                                    smoothFactor = 0,
                                    data = bg_sea,
                                    color = col2hex("grey25"), weight = 2.5, opacity = .25,
                                    fillOpacity = 0)%>%
                        addLegend(title = "Blocks by Urban Village",position = "topright",pal = pal, values = unique(blk_uvs@data$URBAN_VILLAGE_NAME)) %>%
                        addLayersControl(overlayGroups = c("Blocks","Blocks by HU Density","Block Groups"),position = "topright",options = layersControlOptions(collapsed = FALSE))
        }

        map5 <- {

                mySet2 <- RColorBrewer::brewer.pal(n = 7, "Set2")
                pal <- colorFactor(palette = mySet2,domain = bg_uvs@data$UV)
                
                outline <- bg_uvs %>% 
                        as('SpatialLines')

                myLfltShiny() %>%
                        addPolygons(group = "Block Groups",
                                    data = bg_uvs,
                                    smoothFactor = 0,
                                    popup = ~UV,
                                    stroke = FALSE,
                                    fillColor = ~pal(bg_uvs@data$UV), fillOpacity = .75) %>%
                        addPolylines(group = "Block Group Boundaries",
                                    data = bg_uvs,
                                    color = col2hex("white"), weight = 2.5, opacity = .75) %>% 
                        addLegend(title = "Block Groups by Urban Village",position = "topright",pal = pal, values = unique(bg_uvs@data$UV)) %>% 
                        addLayersControl(overlayGroups = c("Block Groups","Block Group Boundaries"),options = layersControlOptions(collapsed = FALSE))
        }

        # map6 <- {
        # 
        #         # pal <- colorFactor(palette = "Set2",domain = sort(seaUvs@data$TYPE_NAME))
        #         #
        #         # myLfltShiny() %>%
        #         #         addLegend(title = "Urban Centers & Villages",position = "topright",pal = pal, values = unique(seaUvs@data$TYPE_NAME))
        # }
                
        output$map1 <- renderLeaflet({map1})
        output$map2 <- renderLeaflet({map2})
        output$map3 <- renderLeaflet({map3})
        output$map4 <- renderLeaflet({map4})
        output$map5 <- renderLeaflet({map5})
        # output$map6 <- renderLeaflet({map6})
        
        
}

# RUN ---------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

