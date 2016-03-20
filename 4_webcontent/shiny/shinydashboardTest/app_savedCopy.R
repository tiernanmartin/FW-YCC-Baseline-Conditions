
# SETUP -------------------------------------------------------------------------------------------


source("./1_setup_1_functions.R")
# source("./1_setup_1_create_data.R") # Note: this script changes the starting files used by the Shiny App. It should be run from the project's working directory -- not within the app itself.
source("./1_setup_1_load_data.R")
library(shiny)
library(markdown)
library(rmarkdown)
# options(shiny.error = browser) # REMOVE THIS!!

# UI ----------------------------------------------------------------------------------------------
ui <-
        shinyUI(navbarPage(title = "ParcelSearch v1", id = "nav",
                           tabPanel(title = "Map + Data",
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
                                                            column(width = 6,
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
                                                                   checkboxGroupInput(inputId = "resstat",
                                                                                      label = "Development Status",
                                                                                      choices = unique(parcel_ycc_reduc@data$RESSTAT),
                                                                                      selected = unique(parcel_ycc_reduc@data$RESSTAT))),
                                                            column(width = 6,
                                                                   h4(HTML("<br>")),
                                                                   h5(HTML("<br>")),
                                                                   sliderInput(inputId = "devsf",label = "Developable Lot Area",post = paste0(" ","ft",tags$sup(2)),step = 1000,min = 0,max = round_any(max(parcel_ycc_reduc@data$PARCEL_DEV_SQFT),f = ceiling, accuracy = 1000),value = c(0, round_any(max(parcel_ycc_reduc@data$PARCEL_DEV_SQFT),f = ceiling, accuracy = 1000))),
                                                                   sliderInput(inputId = "capfla",label = "Max. Floor Area Capacity",post = paste0(" ","ft",tags$sup(2)),step = 10000,min = 0,max = round_any(max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),f = ceiling, accuracy = 1000),value = c(0,round_any(max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),f = ceiling, accuracy = 1000))),
                                                                   submitButton(text = "Apply Changes",icon = icon(name = "refresh")))),
                                                    
# +-- Data Table ----------------------------------
                                                    fluidRow(
                                                            h4("Data"),
                                                            dataTableOutput(outputId = "dt") 
                                                    )
                                                
                                    )
                           )))
                           # tabPanel(title = "Notes + Sources",
                           #          fluidRow(column(width = 3),
                           #                   column(width = 6,
                           #                          htmlOutput("notes")),
                           #                   column(width = 3))))
        )

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
        
        # Reactive Values
        observe({
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