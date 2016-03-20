library(shiny)
library(markdown)
library(rmarkdown)
library(shinythemes)
library(DT)
library(shinydashboard)

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

                                            ")),
                            # h6(br()),
                            fluidRow(
                                    column(1),
                                    column(5,
                                           h4("Map Filters")),
                                    column(5,
                                           div(style = "font-size: 150%;float: right; display: inline-block;",
                                               title = "Download the data as a .csv file",
                                               HTML("
                                                    <button id='download' type='button' class='action-button'><i class='fa fa-download'></i></button>
                                                    ")),
                                           div(style = "font-size: 150%;float: right; display: inline-block;",
                                               title = "Refresh the map",
                                               HTML("
                                                    <button id='refresh' type='button' class='action-button'><i class='fa fa-refresh'></i></button>
                                                    ")),
                                           div(style = "font-size: 150%;float: right; display: inline-block;",
                                               title = "Zoom in to the pin",
                                               HTML("
                                                    <button id='zoom' type='button' class='action-button'><i class='fa fa-search-plus'></i></button>
                                                    ")),
                                           div(style = "font-size: 150%; float: right; display: inline-block;",
                                               title = "Remove the pin",
                                               HTML("
                                                    <button id='remove' type='button' class='action-button'><i class='fa fa-times'></i></button>
                                                    "))
                                               ),
                                    column(1)
                            ),
                            
                            sidebarMenu(
                                    menuItem("Data Table", tabName = "dt", icon = icon("question"))
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