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
        titleWidth = "550px"
)
# +--- Sidebar ---------
sidebar <- dashboardSidebar(width = "550px",
# ---- +--- Custom CSS ---------
                            tags$style(HTML("
                                            .dropdown-menu>.active>a, .dropdown-menu>.active>a:focus, .dropdown-menu>.active>a:hover {
                                            background-color: Transparent !important; border-color: Transparent !important;
                                            font-weight: bold;
                                            color: #FFFFFF; opacity: 1;
                                            }
                                            .dropdown-menu, .dropdown-menu>a:focus, .dropdown-menu>a:hover {
                                            background-color: Transparent !important; border-color: Transparent !important;
                                            }
                                            table, th, td {
                                            vertical-align: top !important;
                                            }
                                            .li {
                                            list-style-type: disc;
                                            list-style-position: inside;
                                            text-indent: -1em;
                                            padding-left: 1em;
                                            }
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
                                            #units_low div label:first-child {
                                            display: none;
                                            margin-bottom: 0px; height: 0px;
                                            }
                                            #units_hi div label:first-child {
                                            display: none;
                                            }
                                            .navbar-default,.navbar-default .navbar-nav>li>a {
                                            color: #FFFFFF !important; 
                                            opacity: .75 !important;
                                            background-color: Transparent !important;
                                            border-color: color: #FFFFFF !important;
                                            }
                                            .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover {
                                            color: #FFFFFF !important; opacity: 1 !important;
                                            background-color: Transparent !important;
                                            border-color: Transparent !important;
                                            }
                                            pre#orig_data_glimpse.shiny-text-output.shiny-bound-output {
                                            color: #FFFFFF !important; 
                                            background-color: Transparent !important;
                                            border: 0px;
                                            }
                                            .tab-content>.active {
                                            max-height: 60vh; overflow-y: auto; overflow-x: initial;
                                            }
                                            .dataTables_scrollBody {
                                            height: 450px !important; overflow-y: scroll; overflow-x: initial;
                                            }
                                            .checkbox{
                                            margin-top: 2.5px;
                                            }
                                            .form-control {
                                            color: #FFFFFF !important; opacity: 1 !important;
                                            background-color: #1e282c !important;
                                            border-color: Transparent !important;
                                            }
                                            .selectize-input, .selectize-control.single .selectize-input.input-active {
                                            background-color: Transparent !important;
                                            border-color: Transparent !important;
                                            }
                                            .selectize-control.multi .selectize-input > div{
                                            color: #FFFFFF !important; opacity: 1 !important;
                                            background-color: #1e282c !important;
                                            border-color: Transparent !important;
                                            }
                                            .selectize-input.focus {
                                            box-shadow: none;
                                            }
                                            #fw_logo  {
                                            color: #FFFFFF !important; opacity: 1 !important;
                                            font-size: 80%;
                                            vertical-align: top;
                                            position: fixed;
                                            bottom: 25px;
                                            right: 6px;
                                            display: inline-block;
                                            }
                                            #fw_logo_txt, #fw_logo_png {
                                            vertical-align: bottom;
                                            opacity: 1 !important;
                                            display: inline-block;
                                            padding: 0;
                                            }
                                            #fw_logo_txt{
                                            height: 13px !important;
                                            }
                                            #fw_logo_png{
                                            margin-right: 5px
                                            }
                                            #dt {
                                            max-width: 450px;
                                            }
                                            ")),
# ---- +--- Sidebar Panel Content ---------
                            # h6(br()),
                            fluidRow(
                                    column(11,
                                           sidebarMenu(id = "menu",
                                                       menuItem("About", tabName = "about", icon = icon("question-circle")),
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
                                                column(10, offset = 1,
                                                       strong("Zoning"),
                                                       selectizeInput(inputId = "zoning_sel", label = "", choices = sort(unique(parcel_ycc_reduc@data$ZONELUT)), multiple = TRUE)
                                                       # div(id = "zoning",class = "multicol",
                                                       #     checkboxGroupInput(inputId = "zoning_chk",
                                                       #                        label = "",
                                                       #                        choices = unique(parcel_ycc_reduc@data$ZONELUT),
                                                       #                        selected = unique(parcel_ycc_reduc@data$ZONELUT))
                                                       # )
                                                ),
                                                column(1)
                                        ),
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
                                                           div(style="display: inline-block;",uiOutput("bldgval_hi_max"))),
                                                       div(class="row-fluid",
                                                           div(uiOutput("units")),
                                                           div(style = "margin-bottom: 0px;", uiOutput("units_label")),
                                                           div(style = "max-width: 100px; display: inline-block",uiOutput("units_low")),
                                                           div(style = "max-width: 100px; display: inline-block",uiOutput("units_hi")),
                                                           div(style="display: inline-block;",uiOutput("units_hi_max")))
                                                       ),
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
                                        )
                                        
                                ),
conditionalPanel(condition = "input.menu == 'about'",
                 column(10, offset = 1,
                        navbarPage("",
                                   tabPanel(title = "Welcome",
                                            h2("Welcome"),
                                            p("Welcome to the Yesler Community Collaborative",HTML(paste0("(",paste0("<a href=\"", "http://yescollab.org","\"", "\ target=\"_blank", "\">", "YCC", "</a>"),")"),"ParcelSearch tool.")),
                                            p("The purpose of this tool is to empower YCC partners to identify sites where affordable housing could be developed."),
                                            p("While using this tool, please be aware of the following points:",
                                              br(),
                                              tags$li(HTML(paste("this tool is very slow -- allow 10-20 seconds to pass after clicking the ",
                                                            icon("refresh"),
                                                            "button before seeing refreshed map results"))),
                                              tags$li("this is an unfinished, draft product and therefore may contain bugs"),
                                              tags$li("the City of Seattle requests that all applications making use of their data provide a disclaimer (see the 'License' tab on the navigation bar above)")),
                                            br(),
                                            p("~ The Futurewise Team,",HTML(paste0("<a href=\"", "http://www.futurewise.org","\"", "\ target=\"_blank", "\">", "www.futurewise.org", "</a>"))),
                                            # h3(br()),
                                            p(
                                                    div(HTML(
                                                            paste("Please direct any questions to",tags$a(href="mailto:tiernan@futurewise.org","tiernan@futurewise.org"))
                                                    ),
                                                    style = "opacity: .5")
                                            ),
                                            br(),
                                            p(
                                                    div(HTML(
                                                            paste0("YCC ParcelSearch, Copyright (C) 2016 Futurewise",
                                                                   "<br>",
                                                                   "YCC ParcelSearch comes with ABSOLUTELY NO WARRANTY;",
                                                                   "<br>",
                                                                   "This is free software, and you are welcome to redistribute it under certain conditions;",
                                                                   "<br>",
                                                                   "click the following link for details: ",
                                                                   tags$a(href="http://tiernanmartin.github.io/FW-YCC-Baseline-Conditions/LICENSE","GNU GENERAL PUBLIC LICENSE"))
                                                    ),
                                                    style = "opacity: .5; text-align: center; font-size: 75%")
                                            )
                                            ),
                                   tabPanel(title = "Source",
                                            h2("Source"),
                                            p(HTML(
                                                    paste0(
                                                            "The data used by this tool was produced by the Seattle Office of Planning and Community Development during their ",
                                                            HTML(paste0("<a href=\"", "http://www.seattle.gov/dpd/cs/groups/pan/@pan/documents/web_informational/p2182731.pdf","\"", "\ target=\"_blank", "\">", paste0("Development Capacity Report ",icon("external-link")), "</a>")),
                                                            " (released September 2014)."
                                                    ))),
                                            p("The original file can be found here:",HTML(paste0("<a href=\"", "https://data.seattle.gov/dataset/Capacity-For-All-Parcel-2015/n2mk-9di2","\"", "\ target=\"_blank", "\">", paste0("https://data.seattle.gov/dataset/Capacity-For-All-Parcel-2015/n2mk-9di2 ",icon("external-link")), "</a>"))),
                                            div(style = "margin-top: 10px; display: inline-block; cursor: pointer;",
                                                h4(HTML(paste0('<button_style data-toggle="collapse" data-target="#glimpse">',"Data Summary ",icon("angle-down"),'</button_style>')))),
                                            tags$div(id = 'glimpse',  class = 'collapse',
                                                     verbatimTextOutput(outputId = "orig_data_glimpse"))),
                                   navbarMenu(title = "Glossary",
                                              tabPanel(title = "Parcel Categories",
                                                      tags$table(
                                                              tags$th( h4("Parcel Categories")),
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
                                                              tags$tr(
                                                                      h2(br())
                                                              )
                                              )),
                                              tabPanel(title = "Neighborhoods",
                                                      h4("Neighborhoods"),
                                                      tags$table(
                                                              tags$col(tags$th("Item"),
                                                                       width = "25%"),
                                                              tags$th("Description"),
                                                              tags$tr(
                                                                      tags$td("Outside YCC"),
                                                                      tags$td("Parcels outside the boundary of the YCC neighborhoods but within the 500ft of these neighborhoods. ")
                                                              ))
                                              )
                                   ),
                                   # tabPanel(title = "Glossary",
                                   #              h2("Glossary"),
                                   #          # h4("Parcel Categories"),
                                   #          tags$table(
                                   #                  tags$th( h4("Parcel Categories")),
                                   #                  tags$col(tags$th("Item"),
                                   #                           width = "25%"),
                                   #                  tags$th("Description"),
                                   #                  tags$tr(
                                   #                          tags$td("ALL"),
                                   #                          tags$td("All parcels.")
                                   #                  ),
                                   #                  tags$tr(
                                   #                          tags$td("PUBLIC"),
                                   #                          tags$td("Parcels owned by public bodies (including the City of Seattle).")
                                   #                  ),
                                   #                  tags$tr(
                                   #                          tags$td("TAX_EXEMPT"),
                                   #                          tags$td("Privately-owned parcels that are not subject to taxation.")
                                   #                  ),
                                   #                  tags$tr(
                                   #                          tags$td("REDEV"),
                                   #                          tags$td("Parcels with excess capacity for redevelopment.",br(),
                                   #                                  "Capacity is calculated by a formula developed by the Seattle Office of Planning and Community Development",
                                   #                                  "For details, see", HTML(paste0("<a href=\"", "http://www.seattle.gov/dpd/cs/groups/pan/@pan/documents/web_informational/p2182731.pdf","\"", "\ target=\"_blank", "\">", "this document", "</a>")), ".")
                                   #                  ),
                                   #                  tags$tr(
                                   #                          tags$td("PARKING"),
                                   #                          tags$td("Parcels whose primary use is parking.")
                                   #                  ),
                                   #                  tags$tr(
                                   #                          tags$td("CHURCH"),
                                   #                          tags$td("Parcels whose primary use is as a center for faith-based activities.",
                                   #                                  br(),
                                   #                                  "Note: despite the title, this is not limited to any particular religion.")
                                   #                  ),
                                   #                  tags$tr(
                                   #                          tags$td("HIST_LNDMRK"),
                                   #                          tags$td("Parcels whose development is constrained by their locations within recognized historic zones or by their designation as landmark sites.")
                                   #                  ),
                                   #                  tags$tr(
                                   #                          h2(br())
                                   #                  )),
                                   #              h4("Neighborhoods"),
                                   #              tags$table(
                                   #                      tags$col(tags$th("Item"),
                                   #                               width = "25%"),
                                   #                      tags$th("Description"),
                                   #                      tags$tr(
                                   #                              tags$td("Outside YCC"),
                                   #                              tags$td("Here's some text that is longer and may need to be wrapped.")
                                   #                      ))
                                   #          ),
                                   tabPanel(title = "License",
                                            h2("License"),
                                            tags$blockquote(
                                                    HTML(
                                                            paste0("YCC ParcelSearch",
                                                                   "<br>",
                                                                   "Copyright (C) 2016 Futurewise",
                                                                   "<br>",
                                                                   "This program is free software; you can redistribute it and/or modify
                                                                   it under the terms of the GNU General Public License as published by
                                                                   the Free Software Foundation; either version 2 of the License, or
                                                                   (at your option) any later version.
                                                                   
                                                                   This program is distributed in the hope that it will be useful,
                                                                   but WITHOUT ANY WARRANTY; without even the implied warranty of
                                                                   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
                                                                   GNU General Public License for more details.
                                                                   
                                                                   You should have received a copy of the GNU General Public License along
                                                                   with this program; if not, write to the Free Software Foundation, Inc.,
                                                                   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.")
                                                    ),style = "font-size: 12px"
                                                    
                                            ),
                                            h2("Disclaimer"),
                                            p("The City of Seattle would like all users of their data products to know the following information:"),
                                            tags$blockquote("The data made available here has been modified for use from its original source, which is the City of Seattle. Neither the City of Seattle nor the Office of the Chief Technology Officer (OCTO) makes any claims as to the completeness, timeliness, accuracy or content of any data contained in this application; makes any representation of any kind, including, but not limited to, warranty of the accuracy or fitness for a particular use; nor are any such warranties to be implied or inferred with respect to the information or data furnished herein. The data is subject to change as modifications and updates are complete. It is understood that the information contained in the web feed is being used at one's own risk.",style = "font-size: 12px")
                                            )
                                   )
                               
                        ))



                            
                            
                            
                           
                            
                            
                            
                            
                            )
# +--- Body ---------
body <- dashboardBody(
        bootstrapPage(
                tags$script(HTML('
                                $(function() {
                                 $("body").addClass("sidebar-collapse"); 
                                 })
                                 ')),
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
                # uiOutput("body_table"),
                # uiOutput("body_categories"),
                # uiOutput("body_dev"),
                # tags$div(class = 'outer', leafletOutput("map",height = "100%", width = '100%')),
                # uiOutput("body_about")
                tags$div(class = 'outer', leafletOutput("map",height = "100%", width = '100%')),
                tags$div(id = "fw_logo",  
                         div(id = "fw_logo_txt",
                             strong("Created by  ")),
                         div(id = "fw_logo_png",
                             tags$a(href = "http://www.futurewise.org/",
                                    target = "_blank",
                                    img(src = "Futurewise_Logo_Reversed_2.png"))
                             )
                         )
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
                             units_low = 0,
                             units_hi = max(parcel_ycc_reduc@data$POT_UNITS),
                             pin = NULL,
                             uv = unique(parcel_ycc_reduc@data$UV_BG),
                             rowSel = NULL,
                             zoning_sel = sort(unique(parcel_ycc_reduc@data$ZONELUT)))
        
        # Adjust the rowSel (row selection) value
        
        observe({
                rv$rowSel <- input$dt_rows_selected
        })
        
        # Adjustthe slider/select variables
        
        observeEvent(input$refresh,{
                
                if(input$input_type == "Slider"){
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
                        rv$units_low <- ifelse(is.null(input$units[[1]]),
                                                 rv$bldgval_low,
                                                 input$units[[1]])
                        rv$units_hi <- ifelse(is.null(input$units[[2]]),
                                                rv$units_hi,
                                                input$units[[2]])
                }
                if(input$input_type == "Numeric"){
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
                        rv$units_low <- ifelse(is.null(input$units_low),
                                                 rv$units_low,
                                                 input$units_low)
                        rv$units_hi <- ifelse(is.null(input$units_hi),
                                                rv$units_hi,
                                                input$units_hi)
                }

                
        })
        
        # Adjust the 'types' reactive value
        
        observeEvent(input$refresh,{
                rv$categories <- input$categories
                rv$uv <- input$uv
                rv$zoning_sel <- if(is.null(input$zoning_sel)) sort(unique(parcel_ycc_reduc@data$ZONELUT)) else input$zoning_sel
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
                                       dplyr::between(BLDG_AV,rv$bldgval_low,rv$bldgval_hi) &
                                       dplyr::between(POT_UNITS,rv$units_low,rv$units_hi) &
                                       UV_BG %in% rv$uv &
                                       ZONELUT %in% rv$zoning_sel)
                
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
                                               dplyr::between(POT_UNITS,rv$units_low,rv$units_hi) &
                                               UV_BG %in% rv$uv &
                                               ZONELUT %in% rv$zoning_sel)
                        # subset(subset = FILTER &
                        #                dplyr::between(ADJRCAP_FL_AREA_MAX,rv$capfla_low,rv$capfla_hi) &
                        #                UV_BG %in% rv$uv)
                        
                        return(rv$shp)
                        
                }
        })
#  +--- Render UI ---------
        
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
        output$units <- renderUI({
                if (input$input_type == "Numeric")
                        return()
                else 
                        div(strong("Potential Additional Units Range"),
                            sliderInput("units", "",
                                        value = c(0,max(parcel_ycc_reduc@data$POT_UNITS)),min = 0,max = max(parcel_ycc_reduc@data$POT_UNITS),step = 1))
                
                
                
        })
        
        output$units_label <- renderUI({
                if (input$input_type == "Slider")
                        return()
                else strong("Maximum Potential Units Range")
                
        })
        
        output$units_low <- renderUI({
                if (input$input_type == "Slider")
                        return()
                else numericInput("units_low", "",
                                  value = 0,min = 0,max = max(parcel_ycc_reduc@data$POT_UNITS))
                
        })
        
        output$units_hi <- renderUI({
                if (input$input_type == "Slider")
                        return()
                else div(
                        numericInput("units_hi", "",
                                     value = max(parcel_ycc_reduc@data$POT_UNITS), min = 0, max = max(parcel_ycc_reduc@data$POT_UNITS)))
                
        })
        
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
        
        # output$body_table <- renderUI({
        #         map_page <- function(){
        #                 tags$div(class = 'outer', leafletOutput("map",height = "100%", width = '100%'))
        #         }
        # 
        #         conditionalPanel(condition = "input.menu == 'table'", map_page())
        # 
        # })
        # output$body_categories <- renderUI({
        # 
        # 
        #         conditionalPanel(condition = "input.menu == 'categories'", map_page())
        # 
        # })
        # output$body_dev <- renderUI({
        # 
        #         conditionalPanel(condition = "input.menu == 'categories'", map_page())
        # 
        # })
        # output$body_about <- renderUI({
        #         
        #         about_page <- function(){
        #                 tags$div(class = 'outer', 
        #                          HTML("Hello, World!"))
        #         }
        #         
        #         conditionalPanel(condition = "input.menu == 'about'", about_page())
        #         
        # })
        
#  +--- Map Content -----
        # Create Pins
        
        icon.blue <- makeAwesomeIcon(icon = 'circle', markerColor = 'blue', library='fa',
                                     iconColor = 'white')
        icon.red <- makeAwesomeIcon(icon = 'circle', markerColor = 'red', library='fa',
                                    iconColor = 'white')
        
        
        # Base map
        
        output$map <- renderLeaflet({
                cnts <- parcel_ycc_reduc %>% mySptlPntsDF()
                
                
                popup <- paste0(strong("Property Name: "),cnts@data$PROP_NAME,br(),
                                strong("PIN: "), cnts@data$PIN,br(),
                                strong("Zoning: "),cnts@data$ZONING,br(),
                                strong("Parcel Area: "),paste0(cnts@data$LAND_SQFT," ft",tags$sup(2)),br(),
                                strong("Existing Units: "),cnts@data$EXIST_UNITS,br(),
                                strong("Developable Footprint: "),paste0(cnts@data$PARCEL_DEV_SQFT," ft",tags$sup(2)),br(),
                                strong("Additional Developable Floor Area: "),paste0(cnts@data$ADJRCAP_FL_AREA_MAX," ft",tags$sup(2)),br(),
                                strong("Additional Units: "),cnts@data$POT_UNITS,br(),
                                paste0("<a href=\"", cnts@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>")
                )
                myLfltShiny() %>%
                        setView(lng = bounds_ycc_cntr[1],lat = bounds_ycc_cntr[2],zoom = 11) %>% 
                        # fitBounds(lng1 = bounds_ycc["min","x"],lat1 = bounds_ycc["min","y"],
                        #           lng2 = bounds_ycc["max","x"],lat2 = bounds_ycc["max","y"]) %>% 
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
                
                if(length(rv$shp@polygons) == 0){
                        rv$cnts <- np
                        
                        
                }
                else {
                        rv$cnts <- rv$shp %>% 
                                mySptlPntsDF()
                }
                
        
                
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
                                strong("Parcel Area: "),paste0(rv$cnts@data$LAND_SQFT," ft",tags$sup(2)),br(),
                                strong("Existing Units: "),rv$cnts@data$EXIST_UNITS,br(),
                                strong("Developable Footprint: "),paste0(rv$cnts@data$PARCEL_DEV_SQFT," ft",tags$sup(2)),br(),
                                strong("Additional Developable Floor Area: "),paste0(rv$cnts@data$ADJRCAP_FL_AREA_MAX," ft",tags$sup(2)),br(),
                                strong("Additional Units: "),rv$cnts@data$POT_UNITS,br(),
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
                
                shp_test <- try(filter_shp(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","")
                )
                
                popup <- paste0(strong("Property Name: "),rv$cnts@data$PROP_NAME,br(),
                                strong("PIN: "), rv$cnts@data$PIN,br(),
                                strong("Zoning: "),rv$cnts@data$ZONING,br(),
                                strong("Parcel Area: "),paste0(rv$cnts@data$LAND_SQFT," ft",tags$sup(2)),br(),
                                strong("Existing Units: "),rv$cnts@data$EXIST_UNITS,br(),
                                strong("Developable Footprint: "),paste0(rv$cnts@data$PARCEL_DEV_SQFT," ft",tags$sup(2)),br(),
                                strong("Additional Developable Floor Area: "),paste0(rv$cnts@data$ADJRCAP_FL_AREA_MAX," ft",tags$sup(2)),br(),
                                strong("Additional Units: "),rv$cnts@data$POT_UNITS,br(),
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
                                strong("Parcel Area: "),paste0(rv$cnts@data$LAND_SQFT," ft",tags$sup(2)),br(),
                                strong("Existing Units: "),rv$cnts@data$EXIST_UNITS,br(),
                                strong("Developable Footprint: "),paste0(rv$cnts@data$PARCEL_DEV_SQFT," ft",tags$sup(2)),br(),
                                strong("Additional Developable Floor Area: "),paste0(rv$cnts@data$ADJRCAP_FL_AREA_MAX," ft",tags$sup(2)),br(),
                                strong("Additional Units: "),rv$cnts@data$POT_UNITS,br(),
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
                                strong("Parcel Area: "),paste0(rv$pin@data$LAND_SQFT," ft",tags$sup(2)),br(),
                                strong("Existing Units: "),rv$pin@data$EXIST_UNITS,br(),
                                strong("Developable Footprint: "),paste0(rv$pin@data$PARCEL_DEV_SQFT," ft",tags$sup(2)),br(),
                                strong("Additional Developable Floor Area: "),paste0(rv$pin@data$ADJRCAP_FL_AREA_MAX," ft",tags$sup(2)),br(),
                                strong("Additional Units: "),rv$pin@data$POT_UNITS,br(),
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
                
                popup <- paste0(strong("Property Name: "),rv$cnts@data$PROP_NAME,br(),
                                strong("PIN: "), rv$cnts@data$PIN,br(),
                                strong("Zoning: "),rv$cnts@data$ZONING,br(),
                                strong("Parcel Area: "),paste0(rv$cnts@data$LAND_SQFT," ft",tags$sup(2)),br(),
                                strong("Existing Units: "),rv$cnts@data$EXIST_UNITS,br(),
                                strong("Developable Footprint: "),paste0(rv$cnts@data$PARCEL_DEV_SQFT," ft",tags$sup(2)),br(),
                                strong("Additional Developable Floor Area: "),paste0(rv$cnts@data$ADJRCAP_FL_AREA_MAX," ft",tags$sup(2)),br(),
                                strong("Additional Units: "),rv$cnts@data$POT_UNITS,br(),
                                paste0("<a href=\"", rv$cnts@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>")
                )

                hipopup <- paste0(strong("Property Name: "),rv$pin@data$PROP_NAME,br(),
                                strong("PIN: "), rv$pin@data$PIN,br(),
                                strong("Zoning: "),rv$pin@data$ZONING,br(),
                                strong("Parcel Area: "),paste0(rv$pin@data$LAND_SQFT," ft",tags$sup(2)),br(),
                                strong("Existing Units: "),rv$pin@data$EXIST_UNITS,br(),
                                strong("Developable Footprint: "),paste0(rv$pin@data$PARCEL_DEV_SQFT," ft",tags$sup(2)),br(),
                                strong("Additional Developable Floor Area: "),paste0(rv$pin@data$ADJRCAP_FL_AREA_MAX," ft",tags$sup(2)),br(),
                                strong("Additional Units: "),rv$pin@data$POT_UNITS,br(),
                                paste0("<a href=\"", rv$pin@data$URL,"\"", "\ target=\"_blank", "\">", paste0("KC Asessor Property Report ",icon("external-link")), "</a>")
                )

                clusters <<- function(map){
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
                                                                 popup = hipopup,
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
                        select('Property\nName' = PROP_NAME,'Current\nZoning' = ZONING,'Potential\nAdditional\nUnits' = POT_UNITS,'Max. Floor Area' = ADJRCAP_FL_AREA_MAX,'Appr. Land Value' = LAND_AV) %>%
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
#  +--- Data Table Glimpse --------
        
        output$orig_data_glimpse <- renderPrint(width = 50,{
                glimpse(orig_data_glimpse)

        })
#  +--- Download Button ----
        observeEvent(input$refresh,{
                
                shp_test <- try(filter_shp(),
                                silent = TRUE)
                
                validate(
                        need(class(shp_test) != "try-error","The current combination of filters excludes all parcels - please revise filter choices.")
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