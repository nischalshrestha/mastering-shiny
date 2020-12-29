library(shiny)

ui <- fluidPage(
  # the purpose of the step argument here is so that when you increment/decrement by 50
  # instead of by 1 for default step
  numericInput("number", "Select a value", value = 150, min = 0, max = 1000, step = 50)
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)