# SETUP -------------------------------------------------------------------------------------------

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
# +--- Header ---------
header <- dashboardHeader(
        title = "YCC ParcelSearch",
        titleWidth = "500px"
)
# +--- Sidebar ---------
sidebar <- dashboardSidebar(width = "500px",
# ---- +--- Custom CSS ---------
                            tags$style(HTML("
                                            
                                           .btn {
                                            opacity: .5; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                            }
                                            .btn-default {
                                            opacity: .5; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                            }
                                            .action-button {
                                            opacity: .5; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                            }
                                            .btn:hover{
                                            opacity: 1; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                            }
                                            .btn-default:hover{
                                            opacity: 1; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                            }
                                            .action-button:hover{
                                            opacity: 1; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                            }
                                            .dataTables_wrapper .dataTables_info {
                                            color: #FFFFFF; opacity: .75;
                                            }
                                            .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                                            color: #FFFFFF; opacity: .75;
                                            }
                                            .dataTables_wrapper .dataTables_paginate .paginate_button {
                                            color: #FFFFFF !important;
                                            }
                                            table.dataTable tbody tr {
                                            background-color: Transparent;
                                            }
                                            table.dataTable.no-footer {
                                            border-bottom: 0px;
                                            }
                                            input, optgroup, select, textarea {
                                            margin: 0;
                                            font: inherit;
                                            color: #222d32;
                                            }
                                            table.dataTable tbody .selected, table.dataTable .selected td.sorting_1, table.dataTable .selected td.sorting_2, table.dataTable .selected td.sorting_3, div.DTS tbody .even.selected, .table-striped tbody>.selected:nth-child(odd)>td, .table-striped tbody>.selected:nth-child(even)>td {
                                            background-color: Transparent !important;
                                            color: #3c8dbc;
                                            }
                                            table.dataTable tbody tr.even.active {
                                            background-color: Transparent !important;
                                            }
                                            .pagination>li>a {
                                            background: Transparent;
                                            color: #FFFFFF; opacity: .75;
                                            border-color: Transparent;
                                            border-radius: 0!important;
                                            }
                                            .pagination>.active>a, .pagination>.active>a:focus, .pagination>.active>a:hover, .pagination>.active>span, .pagination>.active>span:focus, .pagination>.active>span:hover {
                                            z-index: 2;
                                            font-weight: bold;
                                            color: #FFFFFF; opacity: 1;
                                            cursor: default;
                                            background-color: Transparent;
                                            border-color: Transparent;
                                            }

                                            .pagination>.disabled>a, .pagination>.disabled>a:focus, .pagination>.disabled>a:hover, .pagination>.disabled>span, .pagination>.disabled>span:focus, .pagination>.disabled>span:hover{
                                            color: #FFFFFF; opacity: .75;
                                            cursor: default;
                                            background-color: Transparent;
                                            border-color: Transparent;
                                            }
                                            .multicol {
                                            -webkit-column-count: 2; /* Chrome, Safari, Opera */
                                            -moz-column-count: 2; /* Firefox */
                                            column-count: 2;
                                            -webkit-column-gap: 0px; /* Chrome, Safari, Opera */
                                            -moz-column-gap: 0px; /* Firefox */                                      column-gap: 40px;
                                            -webkit-column-width: 50px; /* Chrome, Safari, Opera */
                                            column-width: 50px;
                                            }
                                            .control-label {
                                            display: none;
                                            margin-bottom: 0px; height: 0px;
                                            }
                                            .checkbox{
                                            margin-top: 2.5px;
                                            }
                                            ")),
# ---- +--- Sidebar Panel Content ---------
                            # h6(br()),
                            fluidRow(
                                    column(11,
                                           sidebarMenu(id = "menu",
                                                   menuItem("Data Table", tabName = "table", icon = icon("table")),
                                                   menuItem("Parcel Categories & Neighborhoods", tabName = "categories", icon = icon("list")),
                                                   menuItem("Development Conditions", tabName = "dev", icon = icon("sliders"))
                                                   
                                                   
                                           )),
                                    column(1,
                                           div(style = "font-size: 150%; position: absolute; left: -5px; top: 5px;",
                                               title = "Refresh the map",
                                               HTML("
                                                    <button id='refresh' type='button' class='action-button'><i class='fa fa-refresh'></i></button>
                                                    ")),
                                           div(style = "font-size: 150%;  position: absolute; left: -5px; top: 40px;",
                                               title = "Download the data as a .csv file",
                                               HTML("
                                                    <button id='download' type='button' class='action-button'><i class='fa fa-download'></i></button>
                                                    ")),
                                           div(style = "font-size: 150%; position: absolute; left: -5px; top: 75px;",
                                               title = "Zoom in to the pin",
                                               HTML("
                                                    <button id='zoom' type='button' class='action-button'><i class='fa fa-search-plus'></i></button>
                                                    ")),
                                           div(style = "font-size: 150%; position: absolute; left: -5px; top: 110px;",
                                               title = "Remove the pin",
                                               HTML("
                                                    <button id='remove' type='button' class='action-button'><i class='fa fa-times'></i></button>
                                                    "))
                                               )),
                            h6(br()),
                            conditionalPanel(
                                    condition = "input.menu == 'table'",
                                    fluidRow(
                                            column(10,offset = 1,
                                                   div(style = "float: right;",
                                                       DT::dataTableOutput("dt",width = "100%"))),
                                            column(1)
                                                     
                                             )
                            ),
                                conditionalPanel(
                                        condition = "input.menu == 'categories'",
                                        fluidRow(
                                                column(4,offset = 1,
                                                       checkboxGroupInput(inputId = "type",
                                                                          label = "Parcel Categories",
                                                                          choices = c("ALL","PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK"),
                                                                          selected = c("ALL","PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK")
                                                       )),
                                                column(6,
                                                       checkboxGroupInput(inputId = "uv",
                                                                          label = "Urban Villages",
                                                                          choices = unique(parcel_ycc_reduc@data$UV_BG),
                                                                          selected = unique(parcel_ycc_reduc@data$UV_BG))),
                                                column(1)
                                                
                                        )
                                ),
                                conditionalPanel(
                                        condition = "input.menu == 'dev'",
                                        fluidRow(
                                                column(5, offset = 1,
                                                       strong("Maximum Floor Area"),
                                                       sliderInput("capfla", "",
                                                                   value = c(0,max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX)),min = 0,max = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),step = 1000),
                                                       strong("Appraise Land Value"),
                                                       sliderInput("landval", "",
                                                                   value = c(0,max(parcel_ycc_reduc@data$LAND_AV)),min = 0,max = max(parcel_ycc_reduc@data$LAND_AV),step = 100000),
                                                       strong("Appraise Building Value"),
                                                       sliderInput("bldgval", "",
                                                                   value = c(0,max(parcel_ycc_reduc@data$BLDG_AV)),min = 0,max = max(parcel_ycc_reduc@data$BLDG_AV),step = 10000)
                                                       ),
                                                 column(5,
                                                        strong("Zoning"),
                                                        div(id = "zoning",class = "multicol",
                                                            checkboxGroupInput(inputId = "uv",
                                                                               label = "LABEL",
                                                                               choices = unique(parcel_ycc_reduc@data$ZONELUT),
                                                                               selected = unique(parcel_ycc_reduc@data$ZONELUT))
                                                            )
                                                       ),
                                                column(1)
                                        )
                                )



                            
                            
                            
                           
                            
                            
                            
                            
                            )
# +--- Body ---------
body <- dashboardBody(
        bootstrapPage(
                
                tags$script(
                        '$(".sidebar-toggle").on("click", function() { $(this).trigger("shown"); });'
                ),
                tags$head(tags$style(
                        HTML('
                             section.content{
                             padding:0px;
                             }
                             .outer {
                             height: calc(100vh - 50px);
                             padding: 0px;
                             margin: 0;
                             }
                             '))),
                
                tags$div(class = 'outer', leafletOutput("map",height = "100%", width = '100%'))
                )
                )
# +--- Create UI ---------
ui <- dashboardPage(header, sidebar, body)        

# SERVER ---------
server <- function(input, output) {
#  +--- Reactive Values -----
        
        rv <- reactiveValues(types = c("ALL","PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK"),
                             shp_orig = parcel_ycc_reduc,
                             shp = parcel_ycc_reduc,
                             cnts = parcel_ycc_reduc %>% mySptlPntsDF(),
                             df = parcel_ycc_reduc@data %>% as.data.frame(),
                             capfla_low = 0,
                             capfla_hi = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),
                             landval_low = 0,
                             landval_hi = max(parcel_ycc_reduc@data$LAND_AV),
                             bldgval_low = 0,
                             bldgval_hi = max(parcel_ycc_reduc@data$BLDG_AV),
                             pin = NULL,
                             uv = unique(parcel_ycc_reduc@data$UV_BG),
                             rowSel = NULL)
        
        # Adjust the rowSel (row selection) value
        
        observe({
                rv$rowSel <- input$dt_rows_selected
        })
        
        # Adjustthe slider/select variables
        
        observeEvent(input$apply,{
                
                if(input$input_type == "slider"){
                        rv$capfla_low  <- ifelse(is.null(input$capfla[[1]]),
                                                 rv$capfla_low,
                                                 input$capfla[[1]])
                        rv$capfla_hi  <- ifelse(is.null(input$capfla[[2]]),
                                                rv$capfla_hi,
                                                input$capfla[[2]])
                        rv$landval_low <- ifelse(is.null(input$landval[[1]]),
                                                 rv$landval_low,
                                                 input$landval[[1]])
                        rv$landval_hi <- ifelse(is.null(input$landval[[2]]),
                                                rv$landval_hi,
                                                input$landval[[2]])
                        rv$bldgval_low <- ifelse(is.null(input$bldgval[[1]]),
                                                 rv$bldgval_low,
                                                 input$bldgval[[1]])
                        rv$bldgval_hi <- ifelse(is.null(input$bldgval[[2]]),
                                                rv$bldgval_hi,
                                                input$bldgval[[2]])
                }
                if(input$input_type == "numeric"){
                        rv$capfla_low  <- ifelse(is.null(input$capfla_low),
                                                 rv$capfla_low,
                                                 input$capfla_low)
                        rv$capfla_hi  <- ifelse(is.null(input$capfla_hi),
                                                rv$capfla_hi,
                                                input$capfla_hi)
                        rv$landval_low <- ifelse(is.null(input$landval_low),
                                                 rv$landval_low,
                                                 input$landval_low)
                        rv$landval_hi <- ifelse(is.null(input$landval_hi),
                                                rv$landval_hi,
                                                input$landval_hi)
                        rv$bldgval_low <- ifelse(is.null(input$bldgval_low),
                                                 rv$bldgval_low,
                                                 input$bldgval_low)
                        rv$bldgval_hi <- ifelse(is.null(input$bldgval_hi),
                                                rv$bldgval_hi,
                                                input$bldgval_hi)
                }

                
        })
        
        # Adjust the 'types' reactive value
        
        observeEvent(input$apply,{
                rv$types <- input$type
                rv$uv <- input$uv_map
        })
        
#  +--- Filter Function -----
        
        filter_shp <- function(){
                
                df1 <- rv$shp_orig@data %>% .["ALL"]
                
                rv$shp_orig@data$FILTER <- df1 %>% rowwise() %>% do(i = ifelse(any(. == TRUE),TRUE,FALSE)) %>% unlist()
                
                rv$shp  <-
                        rv$shp_orig %>%
                        subset(subset = FILTER &
                                       dplyr::between(ADJRCAP_FL_AREA_MAX,rv$capfla_low,rv$capfla_hi) &
                                       dplyr::between(LAND_AV,rv$landval_low,rv$landval_hi) &
                                       dplyr::between(BLDG_AV,rv$bldgval_low,rv$bldgval_hi))
                
                return(rv$shp)}
        
        observeEvent(input$apply,{
                
                filter_shp <<- function(){
                        
                        df1 <- rv$shp_orig@data %>% .[rv$types]
                        
                        rv$shp_orig@data$FILTER <- df1 %>% rowwise() %>% do(i = ifelse(any(. == TRUE),TRUE,FALSE)) %>% unlist()
                        
                        rv$shp  <-
                                rv$shp_orig %>%
                                subset(subset = FILTER &
                                               dplyr::between(ADJRCAP_FL_AREA_MAX,rv$capfla_low,rv$capfla_hi) &
                                               dplyr::between(LAND_AV,rv$landval_low,rv$landval_hi) &
                                               dplyr::between(BLDG_AV,rv$bldgval_low,rv$bldgval_hi) &
                                               UV_BG %in% rv$uv)
                        # subset(subset = FILTER &
                        #                dplyr::between(ADJRCAP_FL_AREA_MAX,rv$capfla_low,rv$capfla_hi) &
                        #                UV_BG %in% rv$uv)
                        
                        return(rv$shp)
                        
                }
        })
#  +--- Map Content -----
        # Create Pins
        
        icon.blue <- makeAwesomeIcon(icon = 'circle', markerColor = 'blue', library='fa',
                                     iconColor = 'white')
        icon.red <- makeAwesomeIcon(icon = 'circle', markerColor = 'red', library='fa',
                                    iconColor = 'white')
        
        
        # Base map
        
        output$map <- renderLeaflet({
                cnts <- parcel_ycc_reduc %>% mySptlPntsDF()
                
                # popup <- paste0(strong("Property Name: "),cnts@data$PROP_NAME,br(),
                #                 strong("PIN: "), cnts@data$PIN,br(),
                #                 paste0("<a href=\"", cnts@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>"))
                popup <- paste0(strong("Property Name: "),cnts@data$PROP_NAME,br(),
                                strong("PIN: "), cnts@data$PIN,br(),
                                strong("Zoning: "),cnts@data$ZONING,br(),
                                strong("Developable Footprint: "),paste0(cnts@data$PARCEL_DEV_SQFT," ft",tags$sup(2)),br(),
                                strong("Maximum Developable Floor Area: "),paste0(cnts@data$ADJRCAP_FL_AREA_MAX," ft",tags$sup(2)),br(),
                                strong("Potential Units: "),cnts@data$POT_UNITS,br(),
                                paste0("<a href=\"", cnts@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>")
                )
                myLfltShiny() %>%
                        fitBounds(lng1 = bounds_ycc["min","x"],lat1 = bounds_ycc["min","y"],
                                  lng2 = bounds_ycc["max","x"],lat2 = bounds_ycc["max","y"]) %>% 
                        addAwesomeMarkers(data = cnts,icon = icon.blue,popup = popup,clusterOptions = markerClusterOptions())
                
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
                
                leafletProxy(mapId = "map") %>%
                        clearMarkers() %>%
                        clearMarkerClusters()
                
                shp_test <- try(filter_shp(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","")
                )
                
                # popup <- paste0(strong("Property Name: "),rv$cnts@data$PROP_NAME,br(),
                #                 strong("PIN: "), rv$cnts@data$PIN,br(),
                #                 paste0("<a href=\"", rv$cnts@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>"))
                
                popup <- paste0(strong("Property Name: "),rv$cnts@data$PROP_NAME,br(),
                                strong("PIN: "), rv$cnts@data$PIN,br(),
                                strong("Zoning: "),rv$cnts@data$ZONING,br(),
                                strong("Developable Footprint: "),paste0(rv$cnts@data$PARCEL_DEV_SQFT," ft",tags$sup(2)),br(),
                                strong("Maximum Developable Floor Area: "),paste0(rv$cnts@data$ADJRCAP_FL_AREA_MAX," ft",tags$sup(2)),br(),
                                strong("Potential Units: "),rv$cnts@data$POT_UNITS,br(),
                                paste0("<a href=\"", rv$cnts@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>")
                )
                
                leafletProxy(mapId = "map") %>%
                        clearMarkers() %>%
                        clearMarkerClusters() %>% 
                        addAwesomeMarkers(data = rv$cnts,icon = icon.blue,popup = popup,clusterOptions = markerClusterOptions())
                
        })
        
        # Toggle Clusters
        observeEvent(input$clustOpts,{
                
                # popup <- paste0(strong("Property Name: "),rv$cnts@data$PROP_NAME,br(),
                #                 strong("PIN: "), rv$cnts@data$PIN,br(),
                #                 paste0("<a href=\"", rv$cnts@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>"))
                
                popup <- paste0(strong("Property Name: "),rv$cnts@data$PROP_NAME,br(),
                                strong("PIN: "), rv$cnts@data$PIN,br(),
                                strong("Zoning: "),rv$cnts@data$ZONING,br(),
                                strong("Developable Footprint: "),paste0(rv$cnts@data$PARCEL_DEV_SQFT," ft",tags$sup(2)),br(),
                                strong("Maximum Developable Floor Area: "),paste0(rv$cnts@data$ADJRCAP_FL_AREA_MAX," ft",tags$sup(2)),br(),
                                strong("Potential Units: "),rv$cnts@data$POT_UNITS,br(),
                                paste0("<a href=\"", rv$cnts@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>")
                )
                
                if(input$clustOpts == T)
                        return(leafletProxy(mapId = "map") %>%
                                       clearMarkers() %>%
                                       clearMarkerClusters() %>% 
                                       addAwesomeMarkers(data = rv$cnts,popup = popup,icon = icon.blue,clusterOptions = markerClusterOptions()))
                else(leafletProxy(mapId = "map") %>%
                             clearMarkers() %>%
                             clearMarkerClusters() %>% 
                             addAwesomeMarkers(data = rv$cnts,icon = icon.blue,popup = popup))
        })
        
        
        
        # Zoom to Selected Pin
        
        observeEvent(input$zoom,{
                
                rv$pin <- rv$cnts[input$dt_rows_selected,]
                
                # popup <- paste0(strong("Property Name: "),rv$pin@data$PROP_NAME,br(),
                #                 strong("PIN: "), rv$pin@data$PIN,br(),
                #                 paste0("<a href=\"", rv$pin@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>"))
                
                popup <- paste0(strong("Property Name: "),rv$pin@data$PROP_NAME,br(),
                                strong("PIN: "), rv$pin@data$PIN,br(),
                                strong("Zoning: "),rv$pin@data$ZONING,br(),
                                strong("Developable Footprint: "),paste0(rv$pin@data$PARCEL_DEV_SQFT," ft",tags$sup(2)),br(),
                                strong("Maximum Developable Floor Area: "),paste0(rv$pin@data$ADJRCAP_FL_AREA_MAX," ft",tags$sup(2)),br(),
                                strong("Potential Units: "),rv$pin@data$POT_UNITS,br(),
                                paste0("<a href=\"", rv$pin@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>")
                )
                leafletProxy(mapId = "map") %>%
                        fitBounds(lng1 = rv$shp[input$dt_rows_selected,]@bbox["x","min"],lat1 = rv$shp[input$dt_rows_selected,]@bbox["y","min"],
                                  lng2 = rv$shp[input$dt_rows_selected,]@bbox["x","max"],lat2 = rv$shp[input$dt_rows_selected,]@bbox["y","max"]) 
                
                
        })
        
        # Highlight Selected Pin
        
        
        
        observeEvent(rv$rowSel,{
                rv$pin <- rv$cnts[rv$rowSel,]
                
                popup <- paste0(strong("Property Name: "),rv$pin@data$PROP_NAME,br(),
                                strong("PIN: "), rv$pin@data$PIN,br(),
                                strong("Zoning: "),rv$pin@data$ZONING,br(),
                                strong("Developable Footprint: "),paste0(rv$pin@data$PARCEL_DEV_SQFT," ft",tags$sup(2)),br(),
                                strong("Maximum Developable Floor Area: "),paste0(rv$pin@data$ADJRCAP_FL_AREA_MAX," ft",tags$sup(2)),br(),
                                strong("Potential Units: "),rv$pin@data$POT_UNITS,br(),
                                paste0("<a href=\"", rv$pin@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>")
                )
                
                clusters <- function(map){
                        if(input$clustOpts == T)return(map %>% clearMarkers() %>% 
                                                               clearMarkerClusters() %>% 
                                                               hideGroup("highlight") %>% 
                                                               addAwesomeMarkers(data = rv$cnts,icon = icon.blue,popup = popup,clusterOptions = markerClusterOptions())) 
                        else return(map %>% clearMarkers() %>% 
                                            clearMarkerClusters() %>% 
                                            hideGroup("highlight") %>% 
                                            addAwesomeMarkers(icon = icon.blue,data = rv$cnts,popup = popup))
                        
                }
                
                
                highlight <- function(map){
                        if(!is.null(rv$rowSel))addAwesomeMarkers(map,
                                                                 group = "highlight",
                                                                 data = rv$pin,
                                                                 popup = popup,
                                                                 icon = icon.red) %>% showGroup("highlight")
                        else(map %>% hideGroup("highlight"))
                        
                        
                }

                leafletProxy(mapId = "map") %>% 
                        clusters() %>%
                        highlight()
                
                
        })
        
        observeEvent(input$clear,{
                leafletProxy(mapId = "map") %>% hideGroup("highlight")
        })
#  +--- Data Table --------
        output$dt <- DT::renderDataTable(server = FALSE, selection = 'single',{
                
                shp_test <- try(filter_shp(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","The current combination of filters excludes all parcels - please revise filter choices.")
                )
                
                rv$df %>%
                        as.data.frame() %>%
                        select('Property Name' = PROP_NAME,'Zoning' = ZONING,'Max. Floor Area' = ADJRCAP_FL_AREA_MAX,'Appr. Land Value' = LAND_AV, 'Appr. Bldg. Value' = BLDG_AV) %>%
                        DT::datatable(selection = 'single',
                                      rownames = FALSE,
                                      extensions = 'Scroller',
                                      style = 'bootstrap',
                                      class = 'table table-condensed',
                                      options = list(
                                              pageLength = 100,
                                              dom = 'fti',
                                              deferRender = TRUE,
                                              scrollY = "500px",
                                              scrollCollapse = TRUE
                                                     ))
                
        })

        
}

# RUN --------
shinyApp(ui,server)