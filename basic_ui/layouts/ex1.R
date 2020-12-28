library(shiny)

ui <- fluidPage(
  fluidRow(
    column(6, plotOutput("plot1")),
    column(6, plotOutput("plot2"))
  )
)

server <- function(input, output, session) {
  output$plot1 <- renderPlot(plot(1:5), res = 96)
  output$plot2 <- renderPlot(plot(1:10), res = 96)
}

shinyApp(ui, server)