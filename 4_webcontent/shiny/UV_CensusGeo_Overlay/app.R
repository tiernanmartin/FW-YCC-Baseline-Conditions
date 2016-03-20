
# SETUP -------------------------------------------------------------------------------------------


source("./1_setup_1_functions.R")
source("./1_setup_1_data.R")
library(shiny)
library(markdown)
library(rmarkdown)


# UI ----------------------------------------------------------------------------------------------
ui <-
        shinyUI(navbarPage(title = "Census2UV", id = "nav",
                           tabPanel(title = "Map + Data",
                                    fluidRow(column(width = 8,
                                                    
# +-- Map -----------------------------------------
                                                    div(class="outer",
                                                        tags$head(includeCSS("style.css")),
                                                        leafletOutput("map1",height = "100%",width = "66%"),
                                                        tags$div(id="cite",
                                                                 tags$p(tags$i("Sources: U.S. Census Bureau, Census 2010; City of Seattle")))
                                                    )),
                                             column(width = 4,
                                                    
# +-- Controls ------------------------------------
                                                    fluidRow(
                                                            column(width = 6,
                                                                   h4("Map Controls"),
                                                                   selectInput(inputId = "geo",
                                                                               label = "Census Geography",
                                                                               choices = c("Tract","Block Group")),
                                                                   selectInput(inputId = "var",
                                                                               label = "Census Variable",
                                                                               choices = c("Housing Units","Population"))),
                                                            column(width = 3,
                                                                   h4("Downloads"),
                                                                   h5(HTML("<br>")),
                                                                   downloadButton(outputId = "SHP",label = "SHP")),
                                                            column(width = 3,
                                                                   h4(HTML("<br>")),
                                                                   h5(HTML("<br>")),
                                                                   downloadButton(outputId = "CSV",label = "CSV"))),
                                                    
                                                    
                                                    
# +-- Data Table ----------------------------------
                                                    h4("Data"),
                                                    dataTableOutput("table")
                                                    # textOutput(outputId = "print")
                                                    
                                                   
                                                    
                                             )
                                                    
                                                    
                                    
                                    )
                               ),
# +-- Notes + Sources ----------------------------------
                           tabPanel(title = "Notes + Sources",
                                    fluidRow(column(width = 3),
                                             column(width = 6,
                                                    htmlOutput("notes")),
                                             column(width = 3))))
        )
                           
                           
                           

# SERVER ------------------------------------------------------------------------------------------
server <- function(input, output){
# +-- Function arguments ----------------------------------------------------------------------
        geo <- reactive({
                geo = ifelse(input$geo == "Tract",
                             TRUE,
                             FALSE)
        })
        var <- reactive({
                var = ifelse(input$var == "Population",
                             "pop",
                             "hu")
        })
        
# +-- Reactive values for labeling saved files ----------------------------------------------------------------------
        geo_label <- reactive({
                geo = ifelse(input$geo == "Tract",
                             "trt",
                             "bg")
        })
        var_label <- reactive({
                var = ifelse(input$var == "Population",
                             "pop",
                             "hu")
        })
        
        
        
# +-- Reactive expression: shapefile and dataframe ----
        rv <- reactiveValues(shp = UV2Census_shiny(), df = UV2Census_shiny() %>% .@data)
        
# +-- Invalidators -------
        observeEvent(input$geo, {
                rv$shp <- UV2Census_shiny(tract = geo(),unit = var())
                rv$df <- UV2Census_shiny(tract = geo(),unit = var()) %>% .@data
                
        })
        
        observeEvent(input$var, {
                rv$shp <- UV2Census_shiny(tract = geo(),unit = var())
                rv$df <- UV2Census_shiny(tract = geo(),unit = var()) %>% .@data
                
        })
        
        
        
        
        
# +-- Map --------
        output$map1 <- renderLeaflet({
                myLfltShiny() %>%
                        fitBounds(lng1 = bounds_sea["x","min"],lat1 = bounds_sea["y","min"],
                                  lng2 = bounds_sea["x","max"],lat2 = bounds_sea["y","max"])
        })

        observe({
                pal <- colorFactor(palette = "Set2",domain = rv$df$UV3)
                
                outline <- rv$shp %>%
                        as('SpatialLines')
                
                leafletProxy(mapId = "map1") %>% 
                        clearShapes() %>%
                        addPolygons(group = "Block Groups",
                                    data = rv$shp,
                                    smoothFactor = 0,
                                    popup = ~UV3,
                                    stroke = FALSE,
                                    fillColor = ~pal(rv$df$UV3), fillOpacity = .75) %>%
                        addPolylines(group = "Block Group Boundaries",
                                     data = outline,
                                     color = col2hex("white"), weight = 2.5, opacity = .75)
                        
        })
       
# +-- Data Table --------
        output$table <- renderDataTable({
                
                countName <- function(){input$var %>% as.character() %>% toupper()}
                DT <-
                        rv$df %>%
                        select(GEOID,SUM3,"URBAN VILLAGE" = UV3)
                colnames(DT)[colnames(DT) %in% "SUM3"] <- countName()
                DT
        })
        
# +-- Download Button ------- 
        output$CSV <- 
                downloadHandler(filename = function(){
                        make_csv_filename <- function(){
                                paste("Census2UV",geo_label(),var_label(),Sys.Date(),sep = "_") %>% 
                                        paste0(".csv")
                                }
                        make_csv_filename()
                },
                content = function(file){
                        countName <- function(){input$var %>% as.character() %>% toupper()}
                        DT <-
                                rv$df %>%
                                select(GEOID,SUM3,"URBAN VILLAGE" = UV3)
                        colnames(DT)[colnames(DT) %in% "SUM3"] <- countName()
                        DT
                        
                        write.csv(DT,file, row.names = FALSE)
                })
        
        output$SHP <- downloadHandler(
                filename = function(){
                        make_shp_filename <- function(){
                                paste("Census2UV",geo_label(),var_label(),Sys.Date(),sep = "_") %>% 
                                        paste0(".zip")
                        }
                        make_shp_filename()
                },
                content = function(file) {
                        
                        shp <- rv$shp
                        
                        make_shp_layer <- function(){
                                paste("shp",Sys.Date(),sep = "_")
                        }
                        
                        shp_layer <- make_shp_layer()
                        
                        make_shp_filepath <- function(){
                                paste0("./shp_",Sys.Date(),".*")
                        }
                        
                        shp_filepath <- make_shp_filepath()
                        
                        make_shp_zipfile <- function(){
                                paste0("shp_export_",Sys.Date(),".zip")
                        }
                        
                        shp_zipfile <- make_shp_zipfile()
                        
                        
                        writeOGR(shp, dsn=".", layer= shp_layer, driver="ESRI Shapefile",overwrite_layer = TRUE)
                        
                        zip(zipfile = shp_zipfile, files= Sys.glob(shp_filepath))
                        
                        file.copy(shp_zipfile, file)
                        
                }
        )
        
# +-- Notes + Sources -------
        getPage<-function() {
                return(includeHTML("notes.html"))
        }
        output$notes <- renderUI({getPage()})
}

# RUN ---------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

