library(shiny)

# text
ui <- fluidPage(
  textOutput("text"),
  verbatimTextOutput("code")
)

server <- function(input, output, session) {
  # note how we don't need {} for render* since they only need a one-liner piece of code
  output$text <- renderText("Hello friend!")
  output$code <- renderPrint(summary(1:10))
}

# render table
ui <- fluidPage(
  tableOutput("static"),
  dataTableOutput("dynamic")
)

server <- function(input, output, session) {
  output$static <- renderTable(head(mtcars))
  output$dynamic <- renderDataTable(mtcars, options = list(pageLength = 5))
}

# render R graphics (base, ggplot2 etc)
ui <- fluidPage(
  # note that arguments such as click allows us to include input elements in plot
  plotOutput("plot", width = "400px", click = "plot_click")
)
server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:5), res = 96)
}





shinyApp(ui, server)