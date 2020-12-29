library(shiny)

ui <- fluidPage(
  selectInput("headings", "Boxing offense and defense", 
              list("boxing punches" = c("jab", "cross", "hook"), 
                   "boxing defense" = c("slipping", "bobbing", "blocking")))
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)