library(shiny)

ui <- fluidPage(
  textInput("name", NULL, placeholder = "Your name")
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)