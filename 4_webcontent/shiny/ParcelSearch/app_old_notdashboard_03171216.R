
# SETUP -------------------------------------------------------------------------------------------


source("./1_setup_1_functions.R")
# source("./1_setup_1_create_data.R") # Note: this script changes the starting files used by the Shiny App. It should be run from the project's working directory -- not within the app itself.
source("./1_setup_1_load_data.R")
library(shiny)
library(markdown)
library(rmarkdown)
library(shinythemes)
library(DT)


# UI ----------------------------------------------------------------------------------------------
ui <-
        shinyUI(navbarPage(title = "ParcelSearch v1", id = "nav",
                           theme = shinytheme("spacelab"),
                           tabPanel(title = "Welcome",
                                    fluidRow(column(width = 3),
                                             column(width = 6,
                                                    # htmlOutput("welcome")
                                                    h1("Welcome"),
                                                    br(),
                                                    p("Welcome to the Yesler Community Collaborative",HTML(paste0("(",paste0("<a href=\"", "http://yescollab.org","\"", "\ target=\"_blank", "\">", "YCC", "</a>"),")"),"ParcelSearch tool.")),
                                                    p("The purpose of this tool is to empower YCC partners to identify sites where affordable housing could be developed."),
                                                    p("While using this tool, please be aware of the following points:",
                                                      br(),
                                                      tags$li("this tool is very slow -- allow 20-30 seconds to pass before seeing refreshed map results"),
                                                      tags$li("this is an unfinished, draft product and therefore may contain bugs"),
                                                      tags$li("the City of Seattle requests that all applications making use of their data provide a disclaimer (see the 'Notes' tab on the navigation bar above)")),
                                                    p("~ The Futurewise Team,",HTML(paste0("<a href=\"", "http://www.futurewise.org","\"", "\ target=\"_blank", "\">", "www.futurewise.org", "</a>"))),
                                                    tags$hr(),
                                                    p(
                                                            div(HTML(
                                                                    paste("Please direct any questions to",tags$a(href="mailto:tiernan@futurewise.org","tiernan@futurewise.org"))
                                                            ),
                                                                style = "color:LightGrey")
                                                    )
                                                    ),
                                             column(width = 3))),
                           tabPanel(title = "Map I",
                                    tags$head(
                                            tags$style(type = "text/css", ".button {background-color: Transparent; background-repeat:no-repeat; border: none; cursor:pointer; overflow: hidden; outline:none;}"),
                                            tags$style(HTML("
                                                        
                                                            .button_style {
                                                            background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                                            }
                                                            "))
                                    ),
                                    fluidRow(
                                            column(width = 8,
                                                   div(class="outer",
                                                       tags$head(includeCSS("style.css")),
                                                       leafletOutput("map1",height = "100%",width = "66%"),
                                                       tags$div(id="cite",
                                                                tags$p(tags$i("Sources: City of Seattle")))
                                                   )),
                                            column(width = 4,
                                                   fluidRow(
                                                           column(12,
                                                                  # div(style = "display: inline-block",h4("Data Preview")),
                                                                  div(style = "margin-top: 10px; display: inline-block; cursor: pointer;",
                                                                      h4(HTML(paste0('<button_style data-toggle="collapse" data-target="#dtPrev">',"Data Preview ",icon("angle-down"),'</button_style>')))))
                                                   ),
                                                   fluidRow(
                                                           column(width = 12,
                                                                  tags$div(id = 'dtPrev',  class="collapse",
                                                                           DT::dataTableOutput(outputId = "dt1"))
                                                                  
                                                           )),
                                                   fluidRow(
                                                           column(12,
                                                                  hr()),
                                                            column(width = 5,
                                                                   h4("Filters"),
                                                                   checkboxGroupInput(inputId = "uv",
                                                                                      label = "Urban Villages",
                                                                                      choices = unique(parcel_ycc_reduc@data$UV_BG),
                                                                                      selected = unique(parcel_ycc_reduc@data$UV_BG))
                                                                   
                                                                   
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
                                                                                          div(
                                                                                              title = "Refresh the map",
                                                                                              HTML("
                                                                                                   <button id='apply_m1' type='button' class='btn action-button'><i class='fa fa-refresh'></i></button>
                                                                                                   "))
                                                                                          # div(actionButton(inputId = "apply_m1",label = "Refresh",icon = icon(name = "refresh")),
                                                                                          #     title = "Refresh Map")
                                                                                          ),
                                                                                   column(width = 6,
                                                                                          downloadButton(outputId = "dl_m1",label = "CSV"))
                                                                           )
                                                                           
                                                                   )
                                                                   )
                                                   )
                                                   # fluidRow(column(width = 12,
                                                   #                 tags$hr(),
                                                   #                 h4("Data Preview"),
                                                   #                 DT::dataTableOutput(outputId = "dt1"))
                                                   # )
                                                   ))
                                    ),
                           tabPanel(title = "Map II",
                                    tags$head(
                                            tags$style(HTML("
                                                        
                                                        .shiny-output-error-validation {
                                                            color: red;
                                                            }
                                                            ")),
                                            tags$style(HTML("
                                                        
                                                            .button_style {
                                                            background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                                            }
                                                            ")),
                                            tags$style(type="text/css", ".span3 { max-width: 150px; display: inline-block }"),
                                            tags$style(type="text/css", ".row-fluid { display: inline-block }")
                                            
                                            
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
                                                        # fluidRow(column(12, verbatimTextOutput("dynamicPrint"))),
                                                        fluidRow(column(width = 12,
                                                                        
                                                                        fluidRow(
                                                                                column(4,
                                                                                       div(style = "margin-top: 10px; display: inline-block; cursor: pointer;",
                                                                                           h4(HTML(paste0('<button_style data-toggle="collapse" data-target="#dtPreview">',"Data Preview ",icon("angle-down"),'</button_style>'))))),
                                                                                column(width = 3,
                                                                                       uiOutput("clear")),
                                                                                column(width = 4,offset = 1,
                                                                                       uiOutput("zoom"))
                                                                              ),
                                                                        fluidRow(
                                                                                column(width = 12,
                                                                                       tags$div(id = 'dtPreview',  class="collapse",
                                                                                                DT::dataTableOutput(outputId = "dt"))
                                                                                       
                                                                        ))
                                                        )),
                                                        fluidRow(
                                                                column(12,tags$hr()),
                                                            column(width = 5,
                                                                   h4("Parcel Categories"),
                                                                   checkboxGroupInput(inputId = "type",
                                                                                      label = "",
                                                                                      choices = c("ALL","PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK"),
                                                                                      selected = c("ALL","PUBLIC","TAX_EXEMPT","REDEV","PARKING","CHURCH","HIST_LNDMRK")
                                                                                      )
                                                                   ),
                                                            # column(width = 1,
                                                            #        ""),
                                                            column(width = 6,
                                                                   h4("Controls"),
                                                                   div(style = "margin-top: 35px;",
                                                                       wellPanel(
                                                                           fluidRow(
                                                                                   column(width = 12,
                                                                                          selectInput(inputId = "input_type",label = "Toggle Filter Type",choices = c("numeric","slider")))
                                                                                           ),
                                                                           fluidRow(
                                                                                   div(column(width = 6,
                                                                                              actionButton(inputId = "apply",label = "Refresh",icon = icon(name = "refresh"))),
                                                                                       title = "Refresh Map"),
                                                                                   column(width = 6,
                                                                                          downloadButton(outputId = "dl",label = "CSV"))
                                                                           )),
                                                                           
                                                                           
                                                                           fluidRow(
                                                                                   column(width = 12,
                                                                                          uiOutput(outputId = "clustOpts")
                                                                                   )
                                                                           )
                                                                   )
                                                                   )),
                                                            fluidRow(column(12,tags$hr()),
                                                                     column(12,
                                                                            div(style = "margin-top: 10px; display: inline-block; cursor: pointer;",
                                                                                h4(HTML(paste0('<button_style data-toggle="collapse" data-target="#devcontrols">',"Development ",icon("angle-down"),'</button_style>')))))),
                                                    div(id = "devcontrols",class="collapse",
                                                            fluidRow(column(width = 5,
                                                                    uiOutput("capfla_low"),
                                                                    uiOutput("landval_low"),
                                                                    uiOutput("bldgval_low")),
                                                             column(width = 7,
                                                                    div(class="row-fluid",
                                                                        div(class="span3",uiOutput("capfla_hi")),
                                                                        div(class="span3",uiOutput("capfla_hi_max"))),
                                                                    div(class="row-fluid",
                                                                        div(class="span3",uiOutput("landval_hi")),
                                                                        div(class="span3",uiOutput("landval_hi_max"))),
                                                                    div(class="row-fluid",
                                                                        div(class="span3",uiOutput("bldgval_hi")),
                                                                        div(class="span3",uiOutput("bldgval_hi_max")))
                                                                    
                                                                    ))),
                                                fluidRow(column(12,tags$hr()),
                                                         column(12,
                                                                div(style = "margin-top: 10px; display: inline-block; cursor: pointer;",
                                                                    h4(HTML(paste0('<button_style data-toggle="collapse" data-target="#neighbcontrols">',"Neighborhoods ",icon("angle-down"),'</button_style>')))))),
                                                div(id = "neighbcontrols", class = "collapse",
                                                    column(12,
                                                           checkboxGroupInput(inputId = "uv_map2",
                                                                              label = "",
                                                                              choices = unique(parcel_ycc_reduc@data$UV_BG),
                                                                              selected = unique(parcel_ycc_reduc@data$UV_BG))))
                                                                    
                                                    
# +-- Data Table ----------------------------------
                                                    
                                                
                                    )
                           )),
                tabPanel(title = "Notes",
                         tags$head(
                                 tags$style(HTML("

                                                 dd {
                                                 margin-left: 33%;
                                                 }
                                                 ")),
                                 tags$style(HTML("

                                                 pre {
                                                 font-size: 10px;
                                                 }
                                                 ")),
                                 tags$style(HTML("

                                                 table {
                                                border-collapse:separate; 
                                                border-spacing:0 1em;
                                                }
                                                 ")),
                                 tags$style(HTML("

                                                 td {
                                                 
                                                 vertical-align: top;
                                                 }
                                                 ")),
                                 tags$style(HTML("
                                                        
                                                 .button_style {
                                                 background-color: Transparent; background-repeat:no-repeat; border: none; cursor: pointer; cursor: hand; hover: pointer; overflow: hidden; outline:none;
                                                 }
                                                 "))
                                 
                                 
                                 ),
                         
                         fluidPage(
                                 column(width = 1),
                                 column(width = 3,
                                        h4("Source"),
                                        tags$hr(),
                                        p(HTML(
                                                paste0(
                                                        "The data used by this tool was produced by the Seattle Office of Planning and Community Development during their ",
                                                        HTML(paste0("<a href=\"", "http://www.seattle.gov/dpd/cs/groups/pan/@pan/documents/web_informational/p2182731.pdf","\"", "\ target=\"_blank", "\">", paste0("Development Capacity Report ",icon("external-link")), "</a>")),
                                                        " (released September 2014)."
                                                ))),
                                        p("The original file can be found here:",HTML(paste0("<a href=\"", "https://data.seattle.gov/dataset/Capacity-For-All-Parcel-2015/n2mk-9di2","\"", "\ target=\"_blank", "\">", paste0("https://data.seattle.gov/dataset/Capacity-For-All-Parcel-2015/n2mk-9di2 ",icon("external-link")), "</a>"))),
                                        div(style = "margin-top: 10px; display: inline-block; cursor: pointer;",
                                            h4(HTML(paste0('<button_style data-toggle="collapse" data-target="#glimpse">',"Data Summary ",icon("angle-down"),'</button_style>')))),
                                        tags$hr(),
                                        # h6(br()),
                                        tags$div(id = 'glimpse',  class="collapse",
                                                 verbatimTextOutput(outputId = "orig_data_glimpse"))
                                        
                                        ),
                                 column(width = 4,
                                        h4("Glossary"),
                                        tags$hr(),
                                        h5("Filters (Map I)"),
                                        # tags$dl(
                                        #         tags$dt("Outside YCC"),
                                        #         tags$dd("Parcels that are not within the YCC Baseline Analysis neighborhood boundaries, but are within a 500ft buffer of the YCC area.")
                                        # ),
                                        tags$table(
                                                tags$col(tags$th("Item"),
                                                         width = "25%"),
                                                tags$th("Description"),
                                                tags$tr(
                                                        tags$td("Outside YCC"),
                                                        tags$td("Here's some text that is longer and may need to be wrapped.")
                                                ),
                                                style="vertical-align: top;"),
                                        h5("Filters (Map II)"),
                                        
                                        tags$table(
                                                   tags$col(tags$th("Item"),
                                                            width = "25%"),
                                                   tags$th("Description"),
                                                   tags$tr(
                                                           tags$td("ALL"),
                                                           tags$td("All parcels.")
                                                   ),
                                                   tags$tr(
                                                           tags$td("PUBLIC"),
                                                           tags$td("Parcels owned by public bodies (including the City of Seattle).")
                                                   ),
                                                   tags$tr(
                                                           tags$td("TAX_EXEMPT"),
                                                           tags$td("Privately-owned parcels that are not subject to taxation.")
                                                   ),
                                                   tags$tr(
                                                           tags$td("REDEV"),
                                                           tags$td("Parcels with excess capacity for redevelopment.",br(),
                                                                   "Capacity is calculated by a formula developed by the Seattle Office of Planning and Community Development",
                                                                   "For details, see", HTML(paste0("<a href=\"", "http://www.seattle.gov/dpd/cs/groups/pan/@pan/documents/web_informational/p2182731.pdf","\"", "\ target=\"_blank", "\">", "this document", "</a>")), ".")
                                                   ),
                                                   tags$tr(
                                                           tags$td("PARKING"),
                                                           tags$td("Parcels whose primary use is parking.")
                                                   ),
                                                   tags$tr(
                                                           tags$td("CHURCH"),
                                                           tags$td("Parcels whose primary use is as a center for faith-based activities.",
                                                                   br(),
                                                                   "Note: despite the title, this is not limited to any particular religion.")
                                                   ),
                                                   tags$tr(
                                                           tags$td("HIST_LNDMRK"),
                                                           tags$td("Parcels whose development is constrained by their locations within recognized historic zones or by their designation as landmark sites.")
                                                   ),
                                                   style="vertical-align: top;")
                                        ),
                                 column(width = 3,
                                        h4("Disclaimer"),
                                        tags$hr(),
                                        p("The City of Seattle would like all users of their data products to know the following:"),
                                        tags$blockquote("The data made available here has been modified for use from its original source, which is the City of Seattle. Neither the City of Seattle nor the Office of the Chief Technology Officer (OCTO) makes any claims as to the completeness, timeliness, accuracy or content of any data contained in this application; makes any representation of any kind, including, but not limited to, warranty of the accuracy or fitness for a particular use; nor are any such warranties to be implied or inferred with respect to the information or data furnished herein. The data is subject to change as modifications and updates are complete. It is understood that the information contained in the web feed is being used at one's own risk.",style = "font-size: 12px")),
                                 column(width = 1)
                         )
                         # fluidRow(column(width = 3),
                         #          column(width = 6,
                         #                 htmlOutput("notes")),
                         #          column(width = 3))
                         )))
                           
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
                              df = parcel_ycc_reduc@data,
                              uv = unique(parcel_ycc_reduc@data$UV_BG)
        )
      
        filter_shp1 <- function(){
                
                rv1$shp <- parcel_ycc_reduc %>% subset(subset = UV_BG %in% rv1$uv)
                
                return(rv1$shp)
                
        }
        
        observeEvent(input$apply_m1,{
                rv1$uv <- input$uv
        })
        
        observeEvent(input$apply_m1,{
                filter_shp1 <<- function(){
                        
                        rv1$shp <- parcel_ycc_reduc %>% subset(subset = UV_BG %in% rv1$uv)
                        
                        return(rv1$shp)
                        
                }
        })
        
        
        
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
        
        output$zoom <- renderUI({
                if (is.null(input$dt_rows_selected))
                        return()
                div(p(),
                    actionButton(inputId = "zoom", label="Zoom to Pin",icon = icon("search-plus")),
                    p())
                
                
        })
        output$clear <- renderUI({
                if (is.null(input$dt_rows_selected))
                        return()
                div(p(),
                    actionButton(inputId = "clear", label="Remove Pin",icon = icon("times")),
                    p())
                
                
        })
        
        output$clustOpts <- renderUI({
                if (nrow(as.data.frame(rv$df)) > 500)
                        return()
                checkboxInput(inputId = "clustOpts",label = HTML(paste("Clustered",icon("map-marker"))),value = TRUE)
        })
        
        
        output$capfla_low <- renderUI({
                if (is.null(input$input_type))
                        return()
                
                switch(input$input_type,
                       "slider" = sliderInput("capfla", "Maximum Floor Area",
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
                       "slider" = sliderInput("landval", "Appraised Land Value",
                                              value = c(0,max(parcel_ycc_reduc@data$LAND_AV)),min = 0,max = max(parcel_ycc_reduc@data$LAND_AV),step = 1000),
                       "numeric" =  numericInput("landval_low", "Appr. Land $: Min.",
                                                 value = 0,min = 0,max = max(parcel_ycc_reduc@data$LAND_AV)))
        })
        
        output$bldgval_low <- renderUI({
                if (is.null(input$input_type))
                        return()
                
                switch(input$input_type,
                       "slider" = sliderInput("bldgval", "Appraised Building Value",
                                              value = c(0,max(parcel_ycc_reduc@data$BLDG_AV)),min = 0,max = max(parcel_ycc_reduc@data$BLDG_AV),step = 1000),
                       "numeric" =  numericInput("bldgval_low", "Appr. Bldg. $: Min.",
                                                 value = 0,min = 0,max = max(parcel_ycc_reduc@data$BLDG_AV)))
        })
        
        
        output$landval_hi <- renderUI({
                if (input$input_type == "slider")
                        return()
                
                switch(input$input_type,
                       "numeric" =  numericInput("landval_hi", "Appr. Land $: Max.",
                                                 value = max(parcel_ycc_reduc@data$LAND_AV), min = 0, max = max(parcel_ycc_reduc@data$LAND_AV)))
        })
        
        output$bldgval_hi <- renderUI({
                if (input$input_type == "slider")
                        return()
                
                switch(input$input_type,
                       "numeric" =  numericInput("bldgval_hi", "Appr. Bldg $: Max.",
                                                 value = max(parcel_ycc_reduc@data$BLDG_AV), min = 0, max = max(parcel_ycc_reduc@data$BLDG_AV)))
        })

        output$capfla_hi_max <- renderUI({
                if (input$input_type == "slider")
                        return()
                div(HTML(paste("&le;","2.5M",paste0("ft",tags$sup(2)))),style = "color:LightGrey")
        })
        
        output$landval_hi_max <- renderUI({
                if (input$input_type == "slider")
                        return()
                div(HTML(paste("&le;","$250M")),style = "color:LightGrey")
        })
        
        output$bldgval_hi_max <- renderUI({
                if (input$input_type == "slider")
                        return()
                div(HTML(paste("&le;","$400M")),style = "color:LightGrey")
        })
        
# +-- Map2 ----
        # Reactive Values
        
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

                # if(input$input_type == "slider"){
                #         rv$capfla_low  <- input$capfla[[1]]
                #         rv$capfla_hi  <- input$capfla[[2]]
                #         rv$landval_low <- input$landval[[1]]
                #         rv$landval_hi <- input$landval[[2]]
                #         rv$bldgval_low <- input$bldgval[[1]]
                #         rv$bldgval_hi <- input$bldgval[[2]]
                # }
                # if(input$input_type == "numeric"){
                #         rv$capfla_low  <- input$capfla_low
                #         rv$capfla_hi  <- input$capfla_hi
                #         rv$landval_low <- input$landval_low
                #         rv$landval_hi <- input$landval_hi
                #         rv$bldgval_low <- input$bldgval_low
                #         rv$bldgval_hi <- input$bldgval_hi
                # }

        })
        
        # Adjust the 'types' reactive value
        
        observeEvent(input$apply,{
                rv$types <- input$type
                rv$uv <- input$uv_map2
        })
        
        # Filter Function
        
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
        
        # Create Pins
        
        icon.blue <- makeAwesomeIcon(icon = 'circle', markerColor = 'blue', library='fa',
                                     iconColor = 'white')
        icon.red <- makeAwesomeIcon(icon = 'circle', markerColor = 'red', library='fa',
                                    iconColor = 'white')
        
        
        # Base map
        
        output$map2 <- renderLeaflet({
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
                
                leafletProxy(mapId = "map2") %>%
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
                
                leafletProxy(mapId = "map2") %>%
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
                        return(leafletProxy(mapId = "map2") %>%
                                       clearMarkers() %>%
                                       clearMarkerClusters() %>% 
                                       addAwesomeMarkers(data = rv$cnts,popup = popup,icon = icon.blue,clusterOptions = markerClusterOptions()))
                else(leafletProxy(mapId = "map2") %>%
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
                leafletProxy(mapId = "map2") %>%
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
                
                # This didnt work
                # hideHighlight <- function(map){
                #         if(is.null(rv$rowSel))(map %>% hideGroup("highlight"))
                #         else(map %>% addControl(html = ""))
                # }
                
                
                leafletProxy(mapId = "map2") %>% 
                        clusters() %>%
                        highlight()
                
                
        })
        
        observeEvent(input$clear,{
                leafletProxy(mapId = "map2") %>% hideGroup("highlight")
        })
        
        
        
        
# +-- Data Table (Map1) -------
        
        output$dt1 <- DT::renderDataTable(server = FALSE, selection = 'single',{
                
                shp_test1 <- try(filter_shp1(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test1) != "try-error","The current combination of filters excludes all parcels - please revise filter choices.")
                )

                rv1$df %>%
                        as.data.frame() %>%
                        select('Property Name' = PROP_NAME,'Neighborhood' = UV_BG) %>% 
                        DT::datatable(options = list(pageLength = 5))
                
        })
        
# +--- Dynamic Print ----------

        # This was used for debugging the dynamic inputs
         
        # output$dynamicPrint <- renderPrint({
        #         # vals <- c(rv$capfla_low,rv$capfla_hi,rv$landval_low,rv$landval_hi)
        #         # vals <- c(input$input_type,
        #         #           ifelse(is.null(rv$capfla_low),
        #         #                  "NULL",
        #         #                  rv$capfla_low),
        #         #           rv$capfla_hi,
        #         #           rv$landval_low,
        #         #           rv$landval_hi,
        #         #           rv$uv)
        #         # vals <- c(input$capfla[[1]],
        #         #           input$capfla[[2]],
        #         #           input$landval[[1]],
        #         #           input$landval[[2]])
        #         print(rv$rowSel)
        # })
        
        
# +-- Data Table (Map2) -------
        
        
        output$dt <- DT::renderDataTable(server = FALSE, selection = 'single',{
                
                shp_test <- try(filter_shp(),
                                silent = TRUE)

                validate(
                        need(class(shp_test) != "try-error","The current combination of filters excludes all parcels - please revise filter choices.")
                )
                
                rv$df %>%
                        as.data.frame() %>%
                        select('Property Name' = PROP_NAME,'Zoning' = ZONING,'Max. Floor Area' = ADJRCAP_FL_AREA_MAX,'Appr. Land Value' = LAND_AV, 'Appr. Bldg. Value' = BLDG_AV) %>% 
                        DT::datatable(selection = 'single',rownames = FALSE,options = list(pageLength = 5))
                
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
 
        
                
# +-- Welcome ------
        getPageWelcome<-function() {
                return(includeHTML("welcome.html"))
        }
        output$welcome <- renderUI({getPageWelcome()})
        
# +-- Notes ------
        output$orig_data_glimpse <- renderPrint(width = 50,{
                orig_df %>% 
                        select(PROP_NAME:ADJRCAP_FL_AREA_MAX) %>% 
                        glimpse()
                
        })
        # getPageTest<-function() {
        #         return(includeHTML("test.html"))
        # }
        # output$test <- renderUI({getPageTest()})
        
}

# RUN ---------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)