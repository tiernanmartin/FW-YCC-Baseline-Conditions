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

                                            .button {
                                            -webkit-appearance: none; opacity: .5; color: #FFFFFF !important; background-color: Transparent !important; background-repeat:no-repeat; padding: 0px 0px 0px !important;border: none !important; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                            }
                                            .button:hover {
                                            opacity: 1; color: #FFFFFF !important; background-color: Transparent !important; background-repeat:no-repeat; padding: 0px 0px 0px !important;border: none !important; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                            }
                                           .btn {
                                            color: inherit !important; opacity: .5; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                            }
                                            .btn-default {
                                            color: inherit !important; opacity: .5; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                            }
                                            .action-button {
                                            color: inherit !important; opacity: .5; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                            }
                                            .btn:hover{
                                            color: inherit !important; opacity: 1; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                            }
                                            .btn-default:hover{
                                            color: inherit !important; opacity: 1; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                            }
                                            .action-button:hover{
                                            color: inherit !important; opacity: 1; background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
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
                                            -webkit-column-count: 4; /* Chrome, Safari, Opera */
                                            -moz-column-count: 4; /* Firefox */
                                            column-count: 4;
                                            -webkit-column-gap: 0px; /* Chrome, Safari, Opera */
                                            -moz-column-gap: 0px; /* Firefox */                                      column-gap: 40px;
                                            -webkit-column-width: 50px; /* Chrome, Safari, Opera */
                                            column-width: 50px;
                                            }
                                            .control-label {
                                            display: none;
                                            margin-bottom: 0px; height: 0px;
                                            }
                                            #capfla_low div label:first-child {
                                            display: none;
                                            margin-bottom: 0px; height: 0px;
                                            }
                                            #capfla_hi div label:first-child {
                                            display: none;
                                            }
                                            #landval_low div label:first-child {
                                            display: none;
                                            margin-bottom: 0px; height: 0px;
                                            }
                                            #landval_hi div label:first-child {
                                            display: none;
                                            }
                                            #bldgval_low div label:first-child {
                                            display: none;
                                            margin-bottom: 0px; height: 0px;
                                            }
                                            #bldgval_hi div label:first-child {
                                            display: none;
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
                                           # div(class = "my-action-button1", title = "Refresh Map",
                                           #     actionButton(inputId = "refresh",label = "",icon = icon(name = "refresh")))
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
                                                    ")),
                                           div(style = "font-size: 75%; position: absolute; left: -10px; top: 150px;",
                                               title = "Turn on pin clusters",
                                               HTML("
                                                    <button id='clusters' type='button' class='action-button'><span class='fa-stack fa-lg'><i class='fa fa-circle-o fa-stack-2x'></i><i class='fa fa-map-marker fa-stack-1x'></i></span></button>
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
                                                       strong("Parcel Categories"),
                                                       checkboxGroupInput(inputId = "categories",
                                                                          label = "Parcel Categories",
                                                                          choices = c("ALL","PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK"),
                                                                          selected = c("ALL","PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK")
                                                       )),
                                                column(6,
                                                       strong("Neighborhoods"),
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
                                                # column(width = 3, offset = 1,
                                                #        uiOutput("capfla_low"),
                                                #        uiOutput("landval_low"),
                                                #        uiOutput("bldgval_low")),
                                                column(width = 7,offset = 1,
                                                       div(id = "capflaInput",
                                                           div(uiOutput("capfla")),
                                                           div(style = "margin-bottom: 0px;", uiOutput("capfla_label")),
                                                           div(style = "max-width: 100px; display: inline-block",uiOutput("capfla_low")),
                                                           div(style = "max-width: 100px; display: inline-block",uiOutput("capfla_hi")),
                                                           div(style="display: inline-block;",uiOutput("capfla_hi_max"))),
                                                       div(class="row-fluid",
                                                           div(uiOutput("landval")),
                                                           div(style = "margin-bottom: 0px;", uiOutput("landval_label")),
                                                           div(style = "max-width: 100px; display: inline-block",uiOutput("landval_low")),
                                                           div(style = "max-width: 100px; display: inline-block",uiOutput("landval_hi")),
                                                           div(style="display: inline-block;",uiOutput("landval_hi_max"))),
                                                       div(class="row-fluid",
                                                           div(uiOutput("bldgval")),
                                                           div(style = "margin-bottom: 0px;", uiOutput("bldgval_label")),
                                                           div(style = "max-width: 100px; display: inline-block",uiOutput("bldgval_low")),
                                                           div(style = "max-width: 100px; display: inline-block",uiOutput("bldgval_hi")),
                                                           div(style="display: inline-block;",uiOutput("bldgval_hi_max")))),
                                                # column(7, offset = 1,
                                                #        strong("Maximum Floor Area"),
                                                #        sliderInput("capfla", "",
                                                #                    value = c(0,max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX)),min = 0,max = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),step = 1000),
                                                #        strong("Appraise Land Value"),
                                                #        sliderInput("landval", "",
                                                #                    value = c(0,max(parcel_ycc_reduc@data$LAND_AV)),min = 0,max = max(parcel_ycc_reduc@data$LAND_AV),step = 100000),
                                                #        strong("Appraise Building Value"),
                                                #        sliderInput("bldgval", "",
                                                #                    value = c(0,max(parcel_ycc_reduc@data$BLDG_AV)),min = 0,max = max(parcel_ycc_reduc@data$BLDG_AV),step = 10000)
                                                #        ),
                                                column(3,
                                                       strong("Input Type"),
                                                       radioButtons(inputId = "input_type",label = "",choices = c("Numeric","Slider"),selected = "Numeric"))
                                        ),
                                        fluidRow(
                                                column(10, offset = 1,
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
ui <- dashboardPage(header, sidebar, body, skin = "yellow")        

# SERVER ---------
server <- function(input, output) {
#  +--- Reactive Values -----
        
        rv <- reactiveValues(categories = c("ALL","PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK"),
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
        
        observeEvent(input$refresh,{
                
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
        
        observeEvent(input$refresh,{
                rv$categories <- input$categories
                rv$uv <- input$uv
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
        
        observeEvent(input$refresh,{
                
                filter_shp <<- function(){
                        
                        df1 <- rv$shp_orig@data %>% .[rv$categories]
                        
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
#  +--- UI Switch (Map2) ---------
        
        # output$zoom <- renderUI({
        #         if (is.null(input$dt_rows_selected))
        #                 return()
        #         div(p(),
        #             actionButton(inputId = "zoom", label="Zoom to Pin",icon = icon("search-plus")),
        #             p())
        #         
        #         
        # })
        # output$clear <- renderUI({
        #         if (is.null(input$dt_rows_selected))
        #                 return()
        #         div(p(),
        #             actionButton(inputId = "clear", label="Remove Pin",icon = icon("times")),
        #             p())
        #         
        #         
        # })
        # 
        # output$clustOpts <- renderUI({
        #         if (nrow(as.data.frame(rv$df)) > 500)
        #                 return()
        #         checkboxInput(inputId = "clustOpts",label = HTML(paste("Clustered",icon("map-marker"))),value = TRUE)
        # })
        
        
        output$capfla <- renderUI({
                if (input$input_type == "Numeric")
                        return()
                else 
                        div(strong("Maximum Floor Area Range"),
                            sliderInput("capfla", "",
                                        value = c(0,max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX)),min = 0,max = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX),step = 1000))
                        
                
                
        })

        output$capfla_label <- renderUI({
                if (input$input_type == "Slider")
                        return()
                else strong("Maximum Floor Area Range")
                
        })
        
        output$capfla_low <- renderUI({
                if (input$input_type == "Slider")
                        return()
                else numericInput("capfla_low", "",
                                      value = 0,min = 0,max = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX))
                
        })
        
        output$capfla_hi <- renderUI({
                if (input$input_type == "Slider")
                        return()
                else div(
                         numericInput("capfla_hi", "",
                                      value = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX), min = 0, max = max(parcel_ycc_reduc@data$ADJRCAP_FL_AREA_MAX)))
                
        })
        
        output$landval_label <- renderUI({
                if (input$input_type == "Slider")
                        return()
                else strong("Appraised Land Value Range")
                
        })
        
        output$landval <- renderUI({
                if (input$input_type == "Numeric")
                        return()
                else div(strong("Appraised Land Value Range"),
                         sliderInput("landval", "Appraised Land Value Range",
                                     value = c(0,max(parcel_ycc_reduc@data$LAND_AV)),min = 0,max = max(parcel_ycc_reduc@data$LAND_AV),step = 1000))
                
        })
        
        output$landval_low <- renderUI({
                if (input$input_type == "Slider")
                        return()
                else div(strong(""),
                         numericInput("landval_low", "",
                                      value = 0,min = 0,max = max(parcel_ycc_reduc@data$LAND_AV)))
                
                
        })
        
        output$bldgval_label <- renderUI({
                if (input$input_type == "Slider")
                        return()
                else strong("Appraised Building Value Range")
                
        })
        
        output$bldgval <- renderUI({
                if (input$input_type == "Numeric")
                        return()
                else div(strong("Appraised Building Value Range"),
                         sliderInput("bldgval", "Appraised Building Value Range",
                                     value = c(0,max(parcel_ycc_reduc@data$BLDG_AV)),min = 0,max = max(parcel_ycc_reduc@data$BLDG_AV),step = 1000))
                
        })
        
        output$bldgval_low <- renderUI({
                if (input$input_type == "Slider")
                        return()
                else div(strong(""),
                         numericInput("bldgval_low", "",
                                      value = 0,min = 0,max = max(parcel_ycc_reduc@data$BLDG_AV)))
                
        })
        
        
        output$landval_hi <- renderUI({
                if (input$input_type == "Slider")
                        return()
                else div(strong(""),
                         numericInput("landval_hi", "",
                                      value = max(parcel_ycc_reduc@data$LAND_AV), min = 0, max = max(parcel_ycc_reduc@data$LAND_AV)))
                
        })
        
        output$bldgval_hi <- renderUI({
                if (input$input_type == "Slider")
                        return()
                else div(strong(""),
                         numericInput("bldgval_hi", "",
                                      value = max(parcel_ycc_reduc@data$BLDG_AV), min = 0, max = max(parcel_ycc_reduc@data$BLDG_AV)))
                
        })
        
        output$capfla_hi_max <- renderUI({
                if (input$input_type == "Slider")
                        return()
                div(HTML(paste("&le;","2.5M",paste0("ft",tags$sup(2)))),style = "color:LightGrey")
        })
        
        output$landval_hi_max <- renderUI({
                if (input$input_type == "Slider")
                        return()
                div(HTML(paste("&le;","$250M")),style = "color:LightGrey")
        })
        
        output$bldgval_hi_max <- renderUI({
                if (input$input_type == "Slider")
                        return()
                div(HTML(paste("&le;","$400M")),style = "color:LightGrey")
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
        observeEvent(input$refresh,{
                
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
        observeEvent(input$refresh,{
                
                leafletProxy(mapId = "map") %>%
                        clearMarkers() %>%
                        clearMarkerClusters()
                
                shp_test <- try(filter_shp(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","")
                )
                
                
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
        
        # Turn off clusters for less than 500 parcels
        observeEvent(input$refresh,{
                
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
                
                if(nrow(as.data.frame(rv$df)) > 500)
                        return(leafletProxy(mapId = "map") %>%
                                       clearMarkers() %>%
                                       clearMarkerClusters() %>%
                                       addAwesomeMarkers(data = rv$cnts,popup = popup,icon = icon.blue,clusterOptions = markerClusterOptions()))
                else(leafletProxy(mapId = "map") %>%
                             clearMarkers() %>%
                             clearMarkerClusters() %>%
                             addAwesomeMarkers(data = rv$cnts,icon = icon.blue,popup = popup))
        })
        
        # Toggle Clusters
        observeEvent(input$clusters,{
                
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
                
                return(leafletProxy(mapId = "map") %>%
                               clearMarkers() %>%
                               clearMarkerClusters() %>%
                               addAwesomeMarkers(data = rv$cnts,popup = popup,icon = icon.blue,clusterOptions = markerClusterOptions()))
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
                                  lng2 = rv$shp[input$dt_rows_selected,]@bbox["x","max"],lat2 = rv$shp[input$dt_rows_selected,]@bbox["y","max"]) %>% 
                        addPopups(data = rv$pin,
                                  popup = popup,options = popupOptions())


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
                        if(nrow(as.data.frame(rv$df)) > 500)
                                return(map %>%
                                               clearMarkers() %>%
                                               clearMarkerClusters() %>%
                                               addAwesomeMarkers(data = rv$cnts,popup = popup,icon = icon.blue,clusterOptions = markerClusterOptions()))
                        else(map %>%
                                     clearMarkers() %>%
                                     clearMarkerClusters() %>%
                                     addAwesomeMarkers(data = rv$cnts,icon = icon.blue,popup = popup))
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

        observeEvent(input$remove,{
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
#  +--- Download Button ----
        observeEvent(input$refresh,{
                
                shp_test <- try(filter_shp(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","")
                )
                
                output$download <-
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
}

# RUN --------
shinyApp(ui,server)