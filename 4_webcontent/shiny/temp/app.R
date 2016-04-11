library(shiny)
library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
        title = "Example"
)

sidebar <- dashboardSidebar(
        sidebarMenu(id = "menu",
                    menuItem("Map A", tabName = "mapA"),
                    menuItem("Map B", tabName = "mapB"),
                    menuItem("Text", tabName = "text"))
)


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
                uiOutput("mapA"),
                uiOutput("mapB"),
                uiOutput("text")
        )
       
)

ui <- dashboardPage(header, sidebar, body)        

server <- function(input, output) {
        
        output$mapObjA <- renderLeaflet({
                
                pal <- colorNumeric("Set2", quakes$mag)
                
                leaflet(quakes) %>%
                        addTiles() %>%
                        fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
                        addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                                   fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
                        )
        })
        output$mapObjB <- renderLeaflet({
                
                pal <- colorNumeric("Set2", quakes$mag)
                
                leaflet(quakes) %>%
                        addTiles() %>%
                        fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
                        addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                                   fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
                        )
        })
        output$mapA <- renderUI({
                # tags$div(class = 'outer',leafletOutput("mapObj",height = "100%", width = '100%'))
                conditionalPanel(condition = "input.menu == 'mapA'", tags$div(class = 'outer',leafletOutput("mapObjA",height = "100%", width = '100%')))
        })
        output$mapB <- renderUI({
                # tags$div(class = 'outer',leafletOutput("mapObj",height = "100%", width = '100%'))
                conditionalPanel(condition = "input.menu == 'mapB'", tags$div(class = 'outer',leafletOutput("mapObjB",height = "100%", width = '100%')))
        })
        
        output$text <- renderUI({
                conditionalPanel(condition = "input.menu == 'text'", tags$div(class = 'outer',HTML("Text Content")))
        })
        
}

shinyApp(ui,server)