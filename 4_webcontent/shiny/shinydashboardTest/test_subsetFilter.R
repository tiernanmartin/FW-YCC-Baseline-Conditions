# 08-reactiveValues

library(shiny)
library(dplyr)

set.seed(1)

my_mpg <- mpg %>% 
        mutate(var1 = sample(c(TRUE,FALSE),size = nrow(mpg),replace = TRUE)) %>% 
        mutate(var2 = sample(c(TRUE,FALSE),size = nrow(mpg),replace = TRUE)) %>% 
        mutate(var3 = sample(c(TRUE,FALSE),size = nrow(mpg),replace = TRUE)) %>% 
        select(cyl,year,drv,var1:var3)

ui <- fluidPage(
  checkboxGroupInput(inputId = "type", label = "Filters",choices = c("var1","var2","var3"),selected = "var1"),
  verbatimTextOutput(outputId = "print"),
  plotOutput("plot"),
  dataTableOutput(outputId = "dt")
  
 
)

server <- function(input, output) {

  rv <- reactiveValues(data = my_mpg)

  observe({
          
          df1 <- rv$data %>% .[input$type]
          
          rv$data$FILTER <- df1 %>% rowwise() %>% do(i = ifelse(any(. == TRUE),TRUE,FALSE)) %>% unlist()
          })
  
  
  output$print <- renderPrint({
          
          rv$data %>% glimpse()
          })
  
  

  # 
  # 
  #         rv$data <-  filter_my_mpg()
  # 
  #         })
  # 
  # output$plot <- renderPlot({
  # 
  #         filter_my_mpg <- function(){
  #                 
  #                 rv$data
  #                 # df1 <- rv$data %>% select(cyl:drv)
  #                 # 
  #                 # cols <- paste(input$type,collapse = "|")
  #                 # 
  #                 # df2 <- rv$data %>% as.data.frame() %>%
  #                 #         .[,grepl(cols,colnames(.))]
  #                 # 
  #                 # df2 <- rv$data %>%
  #                 #         as.data.frame() %>%
  #                 #         rowwise() %>%
  #                 #         mutate(FILTER = ifelse(any(var1:var3 == T),TRUE,FALSE)) %>%
  #                 #         select(FILTER)
  #                 # 
  #                 # df3 <- cbind(df1,df2)
  #                 # 
  #                 # df4 <- df3[df3$FILTER,]
  #                 # 
  #                 # return(df4)
  # 
  #         }
  # 
  #         testTry <- try(filter_my_mpg(),silent = TRUE)
  # 
  #         validate(
  #                 need(
  #                         class(testTry) != "try-error", "This is an error"
  #                 )
  #         )
  # 
  #         g <- ggplot(rv$data, aes(year, drv)) +
  #                 geom_point(aes(color = cyl)) +
  #                 geom_smooth(method ="lm") +
  #                 coord_cartesian() +
  #                 scale_color_gradient() +
  #                 theme_bw()
  #         g
  # })
  # 
  output$dt <- renderDataTable({

          # filter_my_mpg <- function(){
          # 
          #         df1 <- rv$data %>% select(cyl:drv)
          # 
          #         cols <- paste(input$type,collapse = "|")
          # 
          #         df2 <- rv$data %>% as.data.frame() %>%
          #                 .[,grepl(cols,colnames(.))]
          # 
          #         df2 <- rv$data %>%
          #                 as.data.frame() %>%
          #                 rowwise() %>%
          #                 mutate(FILTER = ifelse(any(var1:var3 == T),TRUE,FALSE)) %>%
          #                 select(FILTER)
          # 
          #         df3 <- cbind(df1,df2)
          # 
          #         df4 <- df3[df3$FILTER,]
          # 
          #         return(df4)
          # 
          # }

          testTry <- try(filter_my_mpg(),silent = TRUE)

          validate(
                  need(
                          class(testTry) != "try-error", "This is an error"
                  )
          )

          table1 <- rv$data %>% as.data.frame()

  })
}

shinyApp(ui = ui, server = server)
