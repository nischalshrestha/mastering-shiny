library(shiny)

ui <- fluidPage(
  textOutput("greeting")
)

# render*() sets up a special context that automatically tracks what inputs
# the output uses
# render*() also converts output of R code to HTML
server <- function(input, output, session) {
  output$greeting <- renderText("Hello human!")
}

# missing render*()
server <- function(input, output, session) {
  output$greeting <- "Hello human"
}
#> Error: Unexpected character output for greeting

# attempt to read from output
server <- function(input, output, session) {
  message("The greeting is ", output$greeting)
}
#> Error: Reading from shinyoutput object is not allowed.

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server <- function(input, output, session) {
  # the code doesn't tell Shiny to create the string and send it to the browser (imperative thinking)
  # it tells Shiny how it could create the string if needed (declarative thinking; you provide recipes, Shiny does the rest)
  output$greeting <- renderText({
    paste0("Hello ", input$name, "!")
  })
}

server <- function(input, output, session) {
  # example of laziness: silent error because there's a type for greeting
  # double check that your UI and server functions are using the same identifiers.
  output$greetnig <- renderText({
    paste0("Hello ", input$name, "!")
  })
}

server <- function(input, output, session) {
  # example of a reactive expression to reduce duplication and adding a node to reactive graph
  string <- reactive(paste0("Hello ", input$name, "!"))
  output$greeting <- renderText(string())
}

server <- function(input, output, session) {
  # the order of code here does not matter because Shiny is lazy and will only
  # run code when needed so the graph remains the same; but you should avoid
  # confusing order of code like this!
  # summary: order in which reactive code is run is determined by the reactive graph
  # not the layout in the server function
  output$greeting <- renderText(string())
  string <- reactive(paste0("Hello ", input$name, "!"))
}

### Exercises

# 1. the ex1_server*.png files are the reactive graphs for each server functions
# 2. I think it's because `var` is actually the variance function and will result in an error since it 
# is within a non-reactive context within `range` and is missing argument `x`; hence, it's a bad name
# because it is an existing base function.

shinyApp(ui, server)