library(shiny)

ui <- fluidPage(
  sliderInput("dates", "When should we deliver?",
              min = as.Date(strftime("2020-09-16")), 
              max = as.Date(strftime("2020-09-23")), 
              value = as.Date(strftime("2020-09-16"))
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)