
# SETUP -------------------------------------------------------------------------------------------


source("./1_setup_1_functions.R")
# source("./1_setup_1_create_data.R") # Note: this script changes the starting files used by the Shiny App. It should be run from the project's working directory -- not within the app itself.
source("./1_setup_1_load_data.R")
library(shiny)
library(markdown)
library(rmarkdown)
library(shinythemes)


# UI ----------------------------------------------------------------------------------------------
ui <-
        shinyUI(navbarPage(title = "ParcelSearch v1", id = "nav",
                           theme = shinytheme("spacelab"),
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
                                                                   checkboxGroupInput(inputId = "uv",
                                                                                      label = "Urban Villages",
                                                                                      choices = unique(parcel_ycc_reduc@data$UV_TR),
                                                                                      selected = unique(parcel_ycc_reduc@data$UV_TR))
                                                                   
                                                                   
                                                                   ),
                                                            column(width = 6,
                                                                   h4("Controls"),
                                                                   wellPanel(
                                                                           fluidRow(
                                                                                   column(width = 12,
                                                                                           selectInput(inputId = "maptype",
                                                                                                       label = "Map Layers",
                                                                                                       choices = c("PUBLIC","PRIVATE - TAX EXEMPT"),
                                                                                                       selected = ("PUBLIC")))),
                                                                           fluidRow(
                                                                                   column(width = 6,
                                                                                          actionButton(inputId = "apply_m1",label = "Refresh",icon = icon(name = "refresh"))),
                                                                                   column(width = 6,
                                                                                          downloadButton(outputId = "dl_m1",label = "CSV"))
                                                                           )
                                                                           
                                                                   )
                                                                   )
                                                   ),
                                                   fluidRow(column(width = 12,
                                                                   tags$hr(),
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
                                                        absolutePanel(top = 10,right = 10,draggable = TRUE,
                                                                      uiOutput(outputId = "clustOpts")),
                                                        tags$div(id="cite",
                                                                 tags$p(tags$i("Sources: City of Seattle")))
                                                    )),
                                             column(width = 4,
                                                    
# +-- Controls ------------------------------------
                                                    fluidRow(
                                                            column(width = 5,
                                                                   h4("Filters"),
                                                                   checkboxGroupInput(inputId = "type",
                                                                                      label = "Tax-Exempt Status",
                                                                                      choices = c("PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK"),
                                                                                      selected = c("PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK")
                                                                                      )
                                                                   ),
                                                            # column(width = 1,
                                                            #        ""),
                                                            column(width = 6,
                                                                   h4("Controls"),
                                                                   wellPanel(
                                                                           fluidRow(
                                                                                   column(width = 12,
                                                                                          selectInput(inputId = "input_type",label = "Toggle Filter Type",choices = c("slider","numeric")))
                                                                                           ),
                                                                           fluidRow(
                                                                                   column(width = 6,
                                                                                          actionButton(inputId = "apply",label = "Refresh",icon = icon(name = "refresh"))),
                                                                                   column(width = 6,
                                                                                          downloadButton(outputId = "dl",label = "CSV"))
                                                                           )
                                                                   )
                                                                   
                                                                   )),
                                                    fluidRow(column(width = 6,
                                                                    uiOutput("capfla_low"),
                                                                    uiOutput("landval_low")),
                                                             column(width = 6,
                                                                    uiOutput("capfla_hi"),
                                                                    uiOutput("landval_hi"))),
                                                                    
                                                    
# +-- Data Table ----------------------------------
                                                    fluidRow(column(width = 12,
                                                                    tags$hr(),
                                                                    h4("Data Preview"),
                                                                    # verbatimTextOutput(outputId = "dynamicPrint"),
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

# +-- Map1 ----
        
        
        PuYl <- RColorBrewer::brewer.pal(n = 8,name = "Set2")[c(3,6)]
        RdGn <- RColorBrewer::brewer.pal(n = 8,name = "Set2")[c(1,2)]
        
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
        
        
        output$map1 <- renderLeaflet({
                
                pal <- colorFactor(palette = PuYl,domain = parcel_ycc_reduc@data[["SMPL_PUBOWN"]])
                
                myLfltShiny() %>%
                        fitBounds(lng1 = bounds_ycc["min","x"],lat1 = bounds_ycc["min","y"],
                                  lng2 = bounds_ycc["max","x"],lat2 = bounds_ycc["max","y"]) %>% 
                        addPolygons(data = parcel_ycc_reduc,
                                    smoothFactor = 0,
                                    color = col2hex("white"), weight = .5, opacity = .5,
                                    fillColor = ~pal(parcel_ycc_reduc@data[["SMPL_PUBOWN"]]),fillOpacity = .75)%>%
                        addLegend(position = "topright",pal = pal,values = unique(parcel_ycc_reduc@data[["SMPL_PUBOWN"]]))
                        
        })

        observeEvent(input$apply_m1,{
                
                shp_test <- try(filter_shp1(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","")
                )
                
                rv1$shp <- filter_shp1()
                
                rv1$df <- rv1$shp %>% .@data %>% as.data.frame()
                
                rv1$maptype <- ifelse(input$maptype == "PUBLIC",
                                      "SMPL_PUBOWN",
                                      "SMPL_TAXE")
                rv1$palcol1 <- ifelse(input$maptype == "PUBLIC",
                                  PuYl[1],
                                  RdGn[1])
                rv1$palcol2 <- ifelse(input$maptype == "PUBLIC",
                                      PuYl[2],
                                      RdGn[2])
                
                pal <- colorFactor(palette = c(rv1$palcol1,rv1$palcol2),domain = rv1$df[[rv1$maptype]])
                
                
                leafletProxy(mapId = "map1") %>%
                        clearShapes() %>% 
                        addPolygons(data = rv1$shp,
                                    smoothFactor = 0,
                                    color = col2hex("white"), weight = .5, opacity = .5,
                                    fillColor = ~pal(rv1$df[[rv1$maptype]]),fillOpacity = .75)%>% 
                        clearControls() %>% 
                        addLegend(position = "topright",pal = pal,values = unique(rv1$df[[rv1$maptype]]))
                        
                
        })
        
       
# +-- UI Switch (Map2) ---------
        
        output$clustOpts <- renderUI({
                if (nrow(as.data.frame(rv$df)) > 500)
                        return()
                checkboxInput(inputId = "clustOpts",label = "Disable pin clusters",value = TRUE)
        })
        
        
        output$capfla_low <- renderUI({
                if (is.null(input$input_type))
                        return()
                
                switch(input$input_type,
                       "slider" = sliderInput("capfla", "Max. Fl. Area",
                                              value = c(0,max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX)),min = 0,max = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),step = 1000),
                       "numeric" =  numericInput("capfla_low", "Max. Fl. Area: Min.",
                                                 value = 0,min = 0,max = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX)))
        })
        
        output$capfla_hi <- renderUI({
                if (input$input_type == "slider")
                        return()
                
                switch(input$input_type,
                       "numeric" =  numericInput("capfla_hi", "Max. Fl. Area: Max.",
                                                 value = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX), min = 0, max = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX)))
        })
        
        output$landval_low <- renderUI({
                if (is.null(input$input_type))
                        return()
                
                switch(input$input_type,
                       "slider" = sliderInput("landval", "Appr. Land $",
                                              value = c(0,max(parcel_ycc_reduc@data$LAND_AV)),min = 0,max = max(parcel_ycc_reduc@data$LAND_AV),step = 1000),
                       "numeric" =  numericInput("landval_low", "Appr. Land $: Min.",
                                                 value = 0,min = 0,max = max(parcel_ycc_reduc@data$LAND_AV)))
        })
        
        output$landval_hi <- renderUI({
                if (input$input_type == "slider")
                        return()
                
                switch(input$input_type,
                       "numeric" =  numericInput("landval_hi", "Appr. Land $: Max.",
                                                 value = max(parcel_ycc_reduc@data$LAND_AV), min = 0, max = max(parcel_ycc_reduc@data$LAND_AV)))
        })

# +-- Map2 ----
        # Reactive Values
        
        rv <- reactiveValues(types = c("PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK"),
                             shp_orig = parcel_ycc_reduc,
                             shp = parcel_ycc_reduc,
                             cnts = parcel_ycc_reduc %>% mySptlPntsDF(),
                             df = parcel_ycc_reduc@data %>% as.data.frame(),
                             capfla_low = 0,
                             capfla_hi = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),
                             landval_low = 0,
                             landval_hi = max(parcel_ycc_reduc@data$LAND_AV))
        
        
        # Adjustthe slider/select variables
        
    
        observeEvent(input$apply,{

                if(input$input_type == "slider"){
                        rv$capfla_low  <- input$capfla[[1]]
                        rv$capfla_hi  <- input$capfla[[2]]
                        rv$landval_low <- input$landval[[1]]
                        rv$landval_hi <- input$landval[[2]]
                }
                if(input$input_type == "numeric"){
                        rv$capfla_low  <- input$capfla_low
                        rv$capfla_hi  <- input$capfla_hi
                        rv$landval_low <- input$landval_low
                        rv$landval_hi <- input$landval_hi
                }

        })
        
        # Adjust the 'types' reactive value
        
        observeEvent(input$apply,{
                rv$types <- input$type
        })
        
        # Filter Function
        
        filter_shp <- function(){
                
                df1 <- rv$shp_orig@data %>% .[rv$types]
                
                rv$shp_orig@data$FILTER <- df1 %>% rowwise() %>% do(i = ifelse(any(. == TRUE),TRUE,FALSE)) %>% unlist()
                
                rv$shp  <-
                        rv$shp_orig %>%
                        subset(subset = FILTER &
                                       dplyr::between(ADJRCAP_FL_AREA_MAX,rv$capfla_low,rv$capfla_hi) &
                                       dplyr::between(LAND_AV,rv$landval_low,rv$landval_hi))
                
                return(rv$shp)}
        
        observeEvent(input$apply,{
                
                filter_shp <<- function(){
                        
                        df1 <- rv$shp_orig@data %>% .[rv$types]

                        rv$shp_orig@data$FILTER <- df1 %>% rowwise() %>% do(i = ifelse(any(. == TRUE),TRUE,FALSE)) %>% unlist()
                        
                        rv$shp  <-
                                rv$shp_orig %>%
                                subset(subset = FILTER &
                                               dplyr::between(ADJRCAP_FL_AREA_MAX,rv$capfla_low,rv$capfla_hi) &
                                               dplyr::between(LAND_AV,rv$landval_low,rv$landval_hi))
                        
                        return(rv$shp)
                        
                }     
        })
        
        
        # Base map
        
        output$map2 <- renderLeaflet({
                cnts <- parcel_ycc_reduc %>% mySptlPntsDF()
                
                popup <- paste0(strong("Property Name: "),cnts@data$PROP_NAME,br(),
                                strong("PIN: "), cnts@data$PIN,br(),
                                paste0("<a href=\"", cnts@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>"))
                
                myLfltShiny() %>%
                        fitBounds(lng1 = bounds_ycc["min","x"],lat1 = bounds_ycc["min","y"],
                                  lng2 = bounds_ycc["max","x"],lat2 = bounds_ycc["max","y"]) %>% 
                        addMarkers(data = cnts,popup = popup,clusterOptions = markerClusterOptions())
                
        })
   
        # Reactive Values
        observeEvent(input$apply,{
                
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
        observeEvent(input$apply,{
                
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
        
# +-- Data Glimpse (Map1) -------
        
        output$dt1glimpse <- renderPrint({
                shp_test <- try(filter_shp1(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","The current combination of filters excludes all parcels - please revise filter choices.")
                )
                rv1$df %>% glimpse(width = 55)
        })
        
# +--- Dynamic Print ----------

        # This was used for debugging the dynamic inputs
         
        # output$dynamicPrint <- renderPrint({
        #         # vals <- c(rv$capfla_low,rv$capfla_hi,rv$landval_low,rv$landval_hi)
        #         vals <- c(input$input_type,
        #                   rv$capfla_low,
        #                   rv$capfla_hi,
        #                   rv$landval_low,
        #                   rv$landval_hi)
        #         # vals <- c(input$capfla[[1]],
        #         #           input$capfla[[2]],
        #         #           input$landval[[1]],
        #         #           input$landval[[2]])
        #         print(vals)
        # })
        
        
# +-- Data Table (Map2) -------
        
        
        output$dt <- renderDataTable({
                
                shp_test <- try(filter_shp(),
                                silent = TRUE)

                validate(
                        need(class(shp_test) != "try-error","The current combination of filters excludes all parcels - please revise filter choices.")
                )
                
                
                
                
                rv$df %>%
                        as.data.frame() %>%
                        select('Property Name' = PROP_NAME,'Max. Floor Area' = ADJRCAP_FL_AREA_MAX,'Appr. Land Value' = LAND_AV)
                
        })


        
        
        
# +-- Download Buttons (Map1) ----

        observeEvent(input$apply_m1,{
                
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
        
# +-- Download Buttons (Map2) ----
        observeEvent(input$apply,{
                
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