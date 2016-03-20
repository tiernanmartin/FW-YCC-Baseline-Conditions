## app.R ##
source("./1_setup_1_functions.R")
# source("./1_setup_1_create_data.R") # Note: this script changes the starting files used by the Shiny App. It should be run from the project's working directory -- not within the app itself.
source("./1_setup_1_load_data.R")
library(shiny)
library(markdown)
library(rmarkdown)
library(shinythemes)
library(DT)
library(shinydashboard)


# UI ---------
# +--- Header---------
header <- dashboardHeader(
        title = "YCC ParselSearch",
        titleWidth = 350
)
# +--- Siderbar ---------        
sidebar <- dashboardSidebar(width = 350,
                            tags$script(
                                    '$(".sidebar-toggle").on("click", function() { $(this).trigger("shown"); });'
                            ),
        sidebarMenu(id="tabs",
                    fluidRow(
                            column(2,offset = 1,
                                   div(actionButton(inputId = "apply_map1",label = "",icon=icon("times")),
                                       title = "Remove the pin")),
                            column(2,offset = 1,
                                   div(actionButton(inputId = "zoom", label="",icon = icon("search-plus")),
                                       title = "Zoom to the selected pin")),
                            column(2,offset = 1,
                                   div(actionButton(inputId = "apply_map1",label = "",icon=icon("map-o")),
                                       title = "Refresh the map")),
                            column(2,offset = 1,
                                   div(downloadButton(outputId = "dl_m1",label = ""),
                                       title = "Download the data as a .csv file"))
                            
                    ),
                    menuItem("About", tabName = "about", icon = icon("question"),selected=TRUE),
                    menuItem("Map I", tabName = "map1", icon=icon("map"),
                             fluidRow(
                                     column(1),
                                     column(4, 
                                            br(),
                                            menuSubItem("View",tabName = "Map1View")),
                                     column(7,
                                            checkboxInput(inputId = "map1_showDT",label = "Show Data Table",value = TRUE))
                             )
                             ),
                    menuItem("Map II", tabName = "map2", icon=icon("map-marker"),
                             menuSubItem("View",tabName = "Map2View"),
                             checkboxInput(inputId = "map2_showDT",label = "Show Data Table",value = TRUE),
                             h5(br())
                             ),
                    menuItem("Notes", tabName = "notes", icon = icon("file-text"))
                    
        ),
        
        conditionalPanel("input.tabs == 'Map1View'",
                         hr(),
                         fluidRow(
                                 column(1),
                                 column(6,
                                        HTML(paste(icon("sliders"),"&nbsp;",tags$strong("Map Filters")))),
                                 column(4,
                                        actionButton(inputId = "apply_map1",label = "Refresh",icon=icon("map-o")))
                         ),
                         fluidRow(
                                 column(1),
                                 column(10,
                                        selectInput(inputId = "maptype_map1",
                                                    label = "Map Layers",
                                                    choices = c("PUBLIC","PRIVATE - TAX EXEMPT"),
                                                    selected = ("PUBLIC")),
                                        checkboxGroupInput(inputId = "uv_map1",
                                                           label = "Urban Villages",
                                                           choices = unique(parcel_ycc_reduc@data$UV_TR),
                                                           selected = unique(parcel_ycc_reduc@data$UV_TR))
                                 )
                         )
        ),
        conditionalPanel("input.tabs == 'Map2View'",
                         hr(),
                         fluidRow(
                                 column(1),
                                 column(6,
                                        HTML(paste(icon("sliders"),"&nbsp;",tags$strong("Map Filters")))),
                                 column(4,
                                        actionButton(inputId = "apply_map1",label = "Refresh",icon=icon("map-o")))
                         ),
                         fluidRow(
                                 column(1),
                                 column(6,
                                        checkboxGroupInput(inputId = "type_map2",
                                                           label = "Parcel Categories",
                                                           choices = c("ALL","PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK"),
                                                           selected = c("ALL","PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK")
                                        )),
                                 column(3,
                                        radioButtons(inputId = "rangetype_map2",label = "",choices = c("slider","input")
                                        ))
                                        
                                 ),
                         fluidRow(
                                 column(1),
                                 column(10,
                                        sliderInput("capfla", "Maximum Floor Area",
                                                               value = c(0,max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX)),min = 0,max = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),step = 1000),
                                        sliderInput("capfla", "Maximum Floor Area",
                                                    value = c(0,max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX)),min = 0,max = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),step = 1000),
                                        sliderInput("capfla", "Maximum Floor Area",
                                                    value = c(0,max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX)),min = 0,max = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),step = 1000))
                         )
        )
        
        
        
)
# +--- Body---------
body <- dashboardBody(
        tabItems(
                tabItem(
                        tabName = "Map1View",
                        fluidPage(
                                uiOutput(outputId = "map1UI")
                                # fluidRow(
                                #         column(width = 4,
                                #                uiOutput(outputId = "map1UI")),
                                #         column(width = 8,
                                #                box(title = "Map",status = "warning",solidHeader = TRUE,width = 12,height = "100%",
                                #                    leafletOutput('map1b',height = "750px")))
                                # )
                                
                        )
                )
                
                
        )
        

)
                                        
                
# +--- UI function ------------
ui <- dashboardPage(header, sidebar, body, skin = "yellow")

# SERVER ---------
server <- function(input, output) {
        output$map1a <- renderLeaflet({
                myLfltShiny() %>%
                        fitBounds(lng1 = bounds_ycc["min","x"],lat1 = bounds_ycc["min","y"],
                                  lng2 = bounds_ycc["max","x"],lat2 = bounds_ycc["max","y"])
        })
        
        observe({
                leafletProxy(mapId = "map1a") %>% clearMarkers()
        })
        
        
        output$map1b <- renderLeaflet({
                myLfltShiny() %>%
                        fitBounds(lng1 = bounds_ycc["min","x"],lat1 = bounds_ycc["min","y"],
                                  lng2 = bounds_ycc["max","x"],lat2 = bounds_ycc["max","y"])
        })
        
        output$dt1 <- DT::renderDataTable({
                parcel_ycc_reduc@data %>%
                        as.data.frame() %>%
                        select('Property Name' = PROP_NAME,'Neighborhood' = UV_TR) %>% 
                        DT::datatable()
        })
        

        observe({
                output$map1UI <- renderUI({
                        
                        leafletProxy(mapId = "map1a") 

                        make_DTcol <- function(){
                                column(width = 4,
                                       box(title = "",collapsible = TRUE,solidHeader = TRUE,width = 12,height = "100%",
                                           DT::dataTableOutput(outputId = "dt1",height = "750px")))
                        }
                        
                        make_map8 <- function(){
                                column(width = 8,
                                       box(title = "",solidHeader = TRUE,width = 12,height = "100%",
                                           leafletOutput('map1a',height = "750px")))
                        }
                        
                        make_map12 <- function(){
                                column(width = 12,
                                       box(title = "",solidHeader = TRUE,width = 12,height = "100%",
                                           leafletOutput('map1a',height = "750px")))
                        }
                        
                        
                        fluidRow(
                                
                                if(input$map1_showDT == T)({make_DTcol()})else NULL,
                                if(input$map1_showDT == T)({make_map8()}) else ({make_map12()})
                                
                        )
                        
                })
        })
        

        
}

# RUN --------
shinyApp(ui, server)
