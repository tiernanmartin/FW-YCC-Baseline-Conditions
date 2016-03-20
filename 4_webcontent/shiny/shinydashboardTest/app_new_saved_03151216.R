library(shiny)
library(leaflet)
library(markdown)
library(rmarkdown)
library(shinythemes)
library(DT)
library(shinydashboard)
library(dplyr)

header <- dashboardHeader(
        title = "Example",
        titleWidth = "500px"
)

sidebar <- dashboardSidebar(width = "500px",
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

                                            .pagination>.disabled>a, .pagination>.disabled>a:focus, .pagination>.disabled>a:hover, .pagination>.disabled>span, .pagination>.disabled>span:focus, .pagination>.disabled>span:hover{
                                            color: #FFFFFF; opacity: .75;
                                            cursor: not-allowed;
                                            background-color: Transparent;
                                            border-color: Transparent;
                                            }
                                            ")),
                            # h6(br()),
                            fluidRow(
                                    column(11,
                                           sidebarMenu(
                                                   menuItem("Data Table", tabName = "table"),
                                                   menuItem("Parcel Categories", tabName = "categories"),
                                                   menuItem("Development Conditions", tabName = "dev"),
                                                   menuItem("Neighborhoods", tabName = "hoods")
                                                   
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
                                               )
                            ),
                            conditionalPanel("input.tabs == 'table'",
                                             hr(),
                                             fluidRow(
                                                     column(11,
                                                            div(style = "float: right;",
                                                                DT::dataTableOutput("table1",width = "100%")))
                                                     
                                             )
                            )
                            
                            
                            
                            
                            
                            
                            
                            
                            
                            )


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

ui <- dashboardPage(header, sidebar, body)        

server <- function(input, output) {
        
        output$table1 <- renderDataTable(server = FALSE, selection = 'single',{
                quakes %>% DT::datatable(extensions = 'Responsive',style = 'bootstrap',
                                         class = 'table table-condensed',
                                         options = list(pageLength = 15,
                                                        dom = 'ftip'))
        })
        
        
        
        
        # output$table <- DT::renderDataTable(server = FALSE, selection = 'single',{
        #         
        #         
        #         quakes %>%
        #                 as.data.frame() %>%
        #                 DT::datatable(selection = 'single',rownames = FALSE,options = list(pageLength = 5))
        #         
        # })
        
        
        output$map <- renderLeaflet({
                
                pal <- colorNumeric("Set2", quakes$mag)
                
                leaflet(quakes) %>% 
                        addTiles() %>%
                        fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
                        addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                                   fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
                        )
        })
        
}

shinyApp(ui,server)