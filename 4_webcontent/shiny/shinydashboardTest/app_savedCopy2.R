
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
                           tabPanel(title = "Map + Data",
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
                                                        leafletOutput("map1",height = "100%",width = "66%"),
                                                        tags$div(id="cite",
                                                                 tags$p(tags$i("Sources: City of Seattle")))
                                                    )),
                                             column(width = 4,
                                                    
# +-- Controls ------------------------------------
                                                    fluidRow(
                                                            column(width = 5,
                                                                   h4("Filters"),
                                                                   # checkboxGroupInput(inputId = "uv",
                                                                   #                    label = "Urban Villages",
                                                                   #                    choices = unique(parcel_ycc_reduc_reduc@data$UV_TR),
                                                                   #                    selected = unique(parcel_ycc_reduc@data$UV_TR)),
                                                                   checkboxGroupInput(inputId = "pub",
                                                                                      label = "Public or Private",
                                                                                      choices = unique(parcel_ycc_reduc@data$PUB_OWN_TYPE)[!is.na(unique(parcel_ycc_reduc@data$PUB_OWN_TYPE))],
                                                                                      selected = unique(parcel_ycc_reduc@data$PUB_OWN_TYPE)[!is.na(unique(parcel_ycc_reduc@data$PUB_OWN_TYPE))]),
                                                                   checkboxGroupInput(inputId = "tax",
                                                                                      label = "Tax-Exempt Status",
                                                                                      choices = unique(parcel_ycc_reduc@data$TAX_STATUS),
                                                                                      selected = unique(parcel_ycc_reduc@data$TAX_STATUS)),
                                                                   sliderInput(inputId = "devsf",label = "Developable Lot Area",post = paste0(" ","ft",tags$sup(2)),step = 1000,min = 0,max = round_any(max(parcel_ycc_reduc@data$PARCEL_DEV_SQFT),f = ceiling, accuracy = 1000),value = c(0, round_any(max(parcel_ycc_reduc@data$PARCEL_DEV_SQFT),f = ceiling, accuracy = 1000))),
                                                                   sliderInput(inputId = "capfla",label = "Max. Floor Area Capacity",post = paste0(" ","ft",tags$sup(2)),step = 10000,min = 0,max = round_any(max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),f = ceiling, accuracy = 1000),value = c(0,round_any(max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),f = ceiling, accuracy = 1000)))),
                                                            column(width = 2,
                                                                   h4(HTML("<br>")),
                                                                   h5(HTML("<br>"))),
                                                            column(width = 4,
                                                                   h4(HTML("<br>")),
                                                                   # h5(HTML("<br>")),
                                                                   checkboxGroupInput(inputId = "resstat",
                                                                                      label = "Development Status",
                                                                                      choices = unique(parcel_ycc_reduc@data$RESSTAT),
                                                                                      selected = unique(parcel_ycc_reduc@data$RESSTAT)))),
                                                        fluidRow(column(width = 3,
                                                                        submitButton(text = "Apply Changes",icon = icon(name = "refresh"))),
                                                                 column(width = 2,
                                                                        ""),
                                                                 column(width = 6,
                                                                        # htmlOutput("error_icon"),
                                                                        textOutput("error"))),
                                                    
# +-- Data Table ----------------------------------
                                                    fluidRow(column(width = 12,
                                                                    h4("Data"),
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
        output$map1 <- renderLeaflet({
                
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
        
        rv <- reactiveValues(shp = parcel_ycc_reduc, 
                             cnts = parcel_ycc_reduc %>% gCentroid(byid = TRUE),
                             df = parcel_ycc_reduc@data %>% as.data.frame())
        
        # Error message
        
        
        # output$error_icon <- renderText({
        #         icon("exclamation-triangle")
        # })
        
        observe({
                
                filter_shp <- function(){
                        
                        df1 <- rv$shp@data %>% select(PROP_NAME:ADJRCAP_FL_AREA_MAX)
                        
                        cols <- c(input$PUBLIC,
                                     input$TAX_EXEMPT,
                                     input$REDEV,
                                     input$PARKING,
                                     input$CHURCH,
                                     input$HIST_LNDMRK) %>% 
                                paste(.,collapse = "|")
                        
                        df2 <- rv$shp@data %>% as.data.frame() %>% 
                                .[,grepl(cols,colnames(.))]
                        
                        df2 <- rv$shp@data %>%
                                rowwise() %>% 
                                mutate(FILTER = ifelse(any(REDEV:CHURCH == T),TRUE,FALSE)) %>% 
                                select(FILTER)
                        
                        df3 <- cbind(df1,df2)
                                
                        rv$shp@data <- df3
                        
                        rv$shp %<>%
                                subset(subset = FILTER)
                        
                        
                        return(rv$shp)
                }
                
                rv$shp <- filter_shp()
        })
        
        
        
        output$error <- renderText({
                shp_test <- try(subset(x = parcel_ycc_reduc,subset = PUBLIC | TAX_EXEMPT | REDEV | PARKING | CHURCH |HIST_LNDMRK
                                               dplyr::between(PARCEL_DEV_SQFT, input$devsf[1], input$devsf[2]) &
                                               dplyr::between(ADJRCAP_FL_AREA_MAX,input$capfla[1],input$capfla[2])),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","The current combination of filters excludes all parcels - please revise filter choices.")
                )
                ""
        })
        
        
        # Reactive Values
        observe({
                
                shp_test <- try(subset(x = parcel_ycc_reduc,
                                       TAX_STATUS %in% input$tax & 
                                               PUB_OWN_TYPE %in% input$pub & 
                                               RESSTAT %in% input$resstat &
                                               dplyr::between(PARCEL_DEV_SQFT, input$devsf[1], input$devsf[2]) &
                                               dplyr::between(ADJRCAP_FL_AREA_MAX,input$capfla[1],input$capfla[2])),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","")
                )
                
                
                rv$shp <- subset(x = parcel_ycc_reduc,
                                 TAX_STATUS %in% input$tax & 
                                         PUB_OWN_TYPE %in% input$pub & 
                                         RESSTAT %in% input$resstat &
                                         dplyr::between(PARCEL_DEV_SQFT, input$devsf[1], input$devsf[2]) &
                                         dplyr::between(ADJRCAP_FL_AREA_MAX,input$capfla[1],input$capfla[2]))
                
                
                
                rv$df <- rv$shp %>% .@data %>% as.data.frame()
                
                rv$cnts <- rv$shp %>% 
                        mySptlPntsDF()
                
                
        })
        
        # Updated Map
        observe({
                
                shp_test <- try(subset(x = parcel_ycc_reduc,
                                       TAX_STATUS %in% input$tax & 
                                               PUB_OWN_TYPE %in% input$pub & 
                                               RESSTAT %in% input$resstat &
                                               dplyr::between(PARCEL_DEV_SQFT, input$devsf[1], input$devsf[2]) &
                                               dplyr::between(ADJRCAP_FL_AREA_MAX,input$capfla[1],input$capfla[2])),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","")
                )
                
                
                popup <- paste0(strong("Property Name: "),rv$cnts@data$PROP_NAME,br(),
                                strong("PIN: "), rv$cnts@data$PIN,br(),
                                paste0("<a href=\"", rv$cnts@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>"))
                
                leafletProxy(mapId = "map1") %>%
                        clearMarkers() %>%
                        clearMarkerClusters() %>% 
                        addMarkers(data = rv$cnts,popup = popup,clusterOptions = markerClusterOptions())
                
        })
        
# +-- Data Table -------
        output$dt <- renderDataTable({
                
                shp_test <- try(subset(x = parcel_ycc_reduc,
                                       TAX_STATUS %in% input$tax & 
                                               PUB_OWN_TYPE %in% input$pub & 
                                               RESSTAT %in% input$resstat &
                                               dplyr::between(PARCEL_DEV_SQFT, input$devsf[1], input$devsf[2]) &
                                               dplyr::between(ADJRCAP_FL_AREA_MAX,input$capfla[1],input$capfla[2])),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","")
                )
                
                rv$df %>% 
                        as.data.frame() %>% 
                        select(PROP_NAME,PUB_OWN_TYPE,RESSTAT)
                
        })
        
# +-- Download Buttons ----
        # output$CSV <- 
        #         downloadHandler(filename = function(){
        #                 make_csv_filename <- function(){
        #                         paste("Census2UV",geo_label(),var_label(),Sys.Date(),sep = "_") %>% 
        #                                 paste0(".csv")
        #                 }
        #                 make_csv_filename()
        #         },
        #         content = function(file){
        #                 countName <- function(){input$var %>% as.character() %>% toupper()}
        #                 DT <-
        #                         rv$df %>%
        #                         select(GEOID,SUM3,"URBAN VILLAGE" = UV3)
        #                 colnames(DT)[colnames(DT) %in% "SUM3"] <- countName()
        #                 DT
        #                 
        #                 write.csv(DT,file, row.names = FALSE)
        #         })
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