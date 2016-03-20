
# SETUP -------------------------------------------------------------------------------------------


source("./1_setup_1_functions.R")
# source("./1_setup_1_create_data.R") # Note: this script changes the starting files used by the Shiny App. It should be run from the project's working directory -- not within the app itself.
source("./1_setup_1_load_data.R")
library(shiny)
library(markdown)
library(rmarkdown)


# UI ----------------------------------------------------------------------------------------------
ui <-
        shinyUI(navbarPage(title = "ParcelSearch v1", id = "nav",
                           tabPanel(title = "Map",
                                    fluidRow(
                                            column(width = 8,
                                                   div(class="outer",
                                                       tags$head(includeCSS("style.css")),
                                                       leafletOutput("map1",height = "100%",width = "66%"),
                                                       tags$div(id="cite",
                                                                tags$p(tags$i("Sources: City of Seattle")))
                                                   )),
                                            column(width = 4,
                                                   fluidRow(column(width = 5,
                                                                   h4("Filters"),
                                                                   selectInput(inputId = "maptype",
                                                                               label = "Map Layers",
                                                                               choices = c("PUBLIC","PRIVATE - TAX EXEMPT"),
                                                                               selected = ("PUBLIC"))
                                                                   ),
                                                            column(width = 1,
                                                                   ""),
                                                            column(width = 6,
                                                                   h4(HTML("<br>")),
                                                                   checkboxGroupInput(inputId = "uv",
                                                                                      label = "Urban Villages",
                                                                                      choices = unique(parcel_ycc_reduc@data$UV_TR),
                                                                                      selected = unique(parcel_ycc_reduc@data$UV_TR))
                                                                   )
                                                   ),
                                                   fluidRow(
                                                           column(width = 5,
                                                                  submitButton(text = "Apply Changes",icon = icon(name = "refresh"))),
                                                           column(width = 1,
                                                                  ""),
                                                           column(width = 6,
                                                                  downloadButton(outputId = "dl_m1",label = "Download: CSV"))
                                                            ),
                                                   fluidRow(column(width = 11,
                                                                   h4("Data Preview"),
                                                                   verbatimTextOutput(outputId = "dt1glimpse"))
                                                   )))
                                    ),
                           tabPanel(title = "Map + Filters",
                                    tags$head(
                                            tags$style(HTML("
                                                        
                                                        .shiny-output-error-validation {
                                                            color: red;
                                                            
                                                            
                                                            }
                                                            "))
                                            
                                            
                                            ),
                                    fluidRow(column(width = 8,
                                                    
# +-- Map -----------------------------------------
                                                    div(class="outer",
                                                        tags$head(includeCSS("style.css")),
                                                        leafletOutput("map2",height = "100%",width = "66%"),
                                                        tags$div(id="cite",
                                                                 tags$p(tags$i("Sources: City of Seattle")))
                                                    )),
                                             column(width = 4,
                                                    
# +-- Controls ------------------------------------
                                                    fluidRow(
                                                            column(width = 3,
                                                                   h4("Filters"),
                                                                   # checkboxGroupInput(inputId = "uv",
                                                                   #                    label = "Urban Villages",
                                                                   #                    choices = unique(parcel_ycc_reduc_reduc@data$UV_TR),
                                                                   #                    selected = unique(parcel_ycc_reduc@data$UV_TR)),
                                                                   checkboxGroupInput(inputId = "type",
                                                                                      label = "Tax-Exempt Status",
                                                                                      choices = c("PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK"),
                                                                                      selected = c("PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK")
                                                                                      )
                                                                   ),
                                                            column(width = 1,
                                                                   ""),
                                                            column(width = 8,
                                                                   h4(HTML("<br>")),
                                                                   # fluidRow(selectInput(inputId = "")),
                                                                   fluidRow(
                                                                           column(width = 6,
                                                                                  numericInput(inputId = "capfla_low",label = "",value = 0,min = 0,max = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX))),
                                                                           # column(width = 1),
                                                                           column(width = 6,
                                                                                  numericInput(inputId = "capfla_hi",label = "",value = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),min = 0,max = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX)))
                                                                   ),
                                                                   fluidRow(
                                                                           column(width = 6,
                                                                                  numericInput(inputId = "landval_low",label = "",value = 0,min = 0,max = max(parcel_ycc_reduc@data$LAND_AV))),
                                                                           # column(width = 1),
                                                                           column(width = 6,
                                                                                  numericInput(inputId = "landval_hi",label = "",value = max(parcel_ycc_reduc@data$LAND_AV),min = 0,max = max(parcel_ycc_reduc@data$LAND_AV)))
                                                                   )
                                                                   )),
                                                        fluidRow(column(width = 4,
                                                                        submitButton(text = "Apply Changes",icon = icon(name = "refresh"))),
                                                                 column(width = 2,
                                                                        ""),
                                                                 column(width = 5,
                                                                        # htmlOutput("error_icon"),
                                                                        # textOutput("error"),
                                                                        downloadButton(outputId = "dl",label = "Download: CSV"))),
                                                    
# +-- Data Table ----------------------------------
                                                    fluidRow(column(width = 12,
                                                                    h4("Data Preview"),
                                                                    dataTableOutput(outputId = "dt"))
                                                    )
                                                
                                    )
                           ))))
                           # tabPanel(title = "Notes + Sources",
                           #          fluidRow(column(width = 3),
                           #                   column(width = 6,
                           #                          htmlOutput("notes")),
                           #                   column(width = 3))))
        # )

# SERVER ------------------------------------------------------------------------------------------
server <- function(input, output){

# +-- Map ----
        
        PuYl <- RColorBrewer::brewer.pal(n = 8,name = "Set2")[c(3,6)]
        RdGn <- RColorBrewer::brewer.pal(n = 8,name = "Set2")[c(1,2)]
        
        output$map1 <- renderLeaflet({
                myLfltShiny() %>%
                        fitBounds(lng1 = bounds_ycc["min","x"],lat1 = bounds_ycc["min","y"],
                                  lng2 = bounds_ycc["max","x"],lat2 = bounds_ycc["max","y"])
        })
        
        rv1 <- reactiveValues(shp = parcel_ycc_reduc,
                              maptype = "SMPL_PUBOWN",
                              palcol1 = PuYl[1],
                              palcol2 = PuYl[2],
                              df = parcel_ycc_reduc@data
                              )
        
        filter_shp1 <<- function(){
                
                rv1$shp <- parcel_ycc_reduc %>% subset(subset = UV_TR %in% input$uv)
                
                return(rv1$shp)
                
        }
        
        observe({
                
                leafletProxy(mapId = "map1") %>%
                        clearMarkers() %>%
                        clearMarkerClusters() %>% 
                        clearControls() %>% 
                        clearShapes()
                
                shp_test <- try(filter_shp1(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","")
                )
                
                rv1$shp <- filter_shp1()
                
                rv1$df <- rv1$shp %>% .@data %>% as.data.frame()
                
                rv1$maptype <- ifelse(input$maptype == "PUBLIC",
                                      "SMPL_PUBOWN",
                                      "TAX_EXEMPT")
                rv1$palcol1 <- ifelse(input$maptype == "PUBLIC",
                                  PuYl[1],
                                  RdGn[1])
                rv1$palcol2 <- ifelse(input$maptype == "PUBLIC",
                                      PuYl[2],
                                      RdGn[2])
                
                # rv1$pal <- if(input$maptype != "PUBLIC")({colorFactor(palette = RdGn,domain = parcel_ycc_reduc@data$TAX_EXEMPT)})
                
                # pal <- rv1$pal
                
                pal <- colorFactor(palette = c(rv1$palcol1,rv1$palcol2),domain = rv1$df[[rv1$maptype]])
                
                


                # pal <- colorFactor(palette = ifelse(rv1$maptype == "PUBLIC",
                #                                     PuYl,
                #                                     RdGn),domain = ifelse(rv1$maptype == "PUBLIC",
                #                                                     parcel_ycc_reduc@data$SMPL_PUBOWN,
                #                                                     parcel_ycc_reduc@data$TAX_EXEMPT))
                
                leafletProxy(mapId = "map1") %>%
                        clearShapes() %>% 
                        addPolygons(data = rv1$shp,
                                    smoothFactor = 0,
                                    color = col2hex("white"), weight = .5, opacity = .5,
                                    fillColor = ~pal(rv1$df[[rv1$maptype]]),fillOpacity = .75)%>% 
                        clearControls() %>% 
                        addLegend(position = "topright",pal = pal,values = unique(rv1$df[[rv1$maptype]]))
                        
                
        })
        
        
        output$map2 <- renderLeaflet({
                
                # mySet2 <- RColorBrewer::brewer.pal(n = 8,name = "Set2")[c(3,6)]
                # 
                # pal <- colorFactor(palette = mySet2,domain = parcel_ycc_reduc@data$SMPL_PUBOWN)
                
                myLfltShiny() %>%
                        fitBounds(lng1 = bounds_ycc["min","x"],lat1 = bounds_ycc["min","y"],
                                  lng2 = bounds_ycc["max","x"],lat2 = bounds_ycc["max","y"])
                
                # myLfltShiny() %>%
                #         fitBounds(lng1 = bounds_ycc["x","min"],lat1 = bounds_ycc["y","min"],
                #                   lng2 = bounds_ycc["x","max"],lat2 = bounds_ycc["y","max"]) %>% 
                #         addPolygons(data = parcel_ycc_reduc,
                #                     smoothFactor = 0,
                #                     color = col2hex("white"),weight = 1,opacity = .75,
                #                     fillColor = ~pal(parcel_ycc_reduc@data$SMPL_PUBOWN),fillOpacity = .75)
        })
        
        rv <- reactiveValues(shp_orig = parcel_ycc_reduc,
                             shp = parcel_ycc_reduc,
                             cnts = parcel_ycc_reduc %>% gCentroid(byid = TRUE),
                             df = parcel_ycc_reduc@data %>% as.data.frame())
        # Filter Function
        
        observe({
                
                filter_shp <<- function(){
                        
                        df1 <- rv$shp_orig@data %>% .[input$type]
                        
                        rv$shp_orig@data$FILTER <- df1 %>% rowwise() %>% do(i = ifelse(any(. == TRUE),TRUE,FALSE)) %>% unlist()
                        
                        rv$shp  <- 
                                rv$shp_orig %>% 
                                subset(subset = FILTER & 
                                               dplyr::between(ADJRCAP_FL_AREA_MAX,input$capfla_low,input$capfla_hi) &
                                               dplyr::between(LAND_AV,input$landval_low,input$landval_hi))
                        
                        return(rv$shp)
                        
                }     
        })
        
        # Error message

        # output$error <- renderText({
        #         shp_test <- try(filter_shp(),
        #                         silent = TRUE)
        # 
        #         validate(
        #                 need(class(shp_test) != "try-error","The current combination of filters excludes all parcels - please revise filter choices.")
        #         )
        # 
        # })
        
        
        # Reactive Values
        observe({
                
                shp_test <- try(filter_shp(),
                                silent = TRUE)

                validate(
                        need(class(shp_test) != "try-error","")
                )
                
                rv$shp <- filter_shp()
                
                rv$df <- rv$shp %>% .@data %>% as.data.frame()
                
                rv$cnts <- rv$shp %>% 
                        mySptlPntsDF()
                
                
        })
        
        # Updated Map
        observe({
                
                leafletProxy(mapId = "map2") %>%
                        clearMarkers() %>%
                        clearMarkerClusters()
                
                shp_test <- try(filter_shp(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","")
                )
                
                
                popup <- paste0(strong("Property Name: "),rv$cnts@data$PROP_NAME,br(),
                                strong("PIN: "), rv$cnts@data$PIN,br(),
                                paste0("<a href=\"", rv$cnts@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>"))
                
                leafletProxy(mapId = "map2") %>%
                        clearMarkers() %>%
                        clearMarkerClusters() %>% 
                        addMarkers(data = rv$cnts,popup = popup,clusterOptions = markerClusterOptions())
                
        })
        
# +-- Data Table -------
        
        
        output$dt <- renderDataTable({
                
                shp_test <- try(filter_shp(),
                                silent = TRUE)

                validate(
                        need(class(shp_test) != "try-error","The current combination of filters excludes all parcels - please revise filter choices.")
                )
                
                rv$df %>% 
                        as.data.frame() %>% 
                        select(PROP_NAME,PUB_OWN_TYPE,RESSTAT)
                
        })

# +-- Data Glimpse -------
        
        output$dt1glimpse <- renderPrint({
                shp_test <- try(filter_shp1(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","The current combination of filters excludes all parcels - please revise filter choices.")
                )
                rv1$df %>% glimpse(width = 50)
                # parcel_ycc_reduc@data[[rv1$maptype]] %>% head()
                # parcel_ycc_reduc@data[rv1$maptype] %>% as.data.frame() %>% sample_n(10)
        })
        
        
        
# +-- Download Buttons ----

        observe({
                
                shp_test <- try(filter_shp1(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","")
                )
                
                output$dl_m1 <-
                        downloadHandler(filename = function(){
                                make_csv_filename <- function(){
                                        paste("ParcelSearch",format(Sys.time(), "%Y%m%d%H%M%S"),sep = "_") %>%
                                                paste0(".csv")
                                }
                                make_csv_filename()
                        },
                        content = function(file){
                                write.csv(rv1$df,file, row.names = FALSE)
                        })
        })
        
        observe({
                
                shp_test <- try(filter_shp(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","")
                )
                
                output$dl <-
                        downloadHandler(filename = function(){
                                make_csv_filename <- function(){
                                        paste("ParcelSearch",format(Sys.time(), "%Y%m%d%H%M%S"),sep = "_") %>%
                                                paste0(".csv")
                                }
                                make_csv_filename()
                        },
                        content = function(file){
                                write.csv(rv$df,file, row.names = FALSE)
                        })
        })
        
        # 
        # 
        # output$SHP <- downloadHandler(
        #         filename = function(){
        #                 make_shp_filename <- function(){
        #                         paste("Census2UV",geo_label(),var_label(),Sys.Date(),sep = "_") %>% 
        #                                 paste0(".zip")
        #                 }
        #                 make_shp_filename()
        #         },
        #         content = function(file) {
        #                 
        #                 shp <- rv$shp
        #                 
        #                 make_shp_layer <- function(){
        #                         paste("shp",Sys.Date(),sep = "_")
        #                 }
        #                 
        #                 shp_layer <- make_shp_layer()
        #                 
        #                 make_shp_filepath <- function(){
        #                         paste0("./shp_",Sys.Date(),".*")
        #                 }
        #                 
        #                 shp_filepath <- make_shp_filepath()
        #                 
        #                 make_shp_zipfile <- function(){
        #                         paste0("shp_export_",Sys.Date(),".zip")
        #                 }
        #                 
        #                 shp_zipfile <- make_shp_zipfile()
        #                 
        #                 
        #                 writeOGR(shp, dsn=".", layer= shp_layer, driver="ESRI Shapefile",overwrite_layer = TRUE)
        #                 
        #                 zip(zipfile = shp_zipfile, files= Sys.glob(shp_filepath))
        #                 
        #                 file.copy(shp_zipfile, file)
        #                 
        #         }
        # )
        
# +-- Notes + Sources ------
        # getPage<-function() {
        #         return(includeHTML("notes.html"))
        # }
        # output$notes <- renderUI({getPage()})
}

# RUN ---------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)