library(shiny)

ui <- fluidPage(
  numericInput("a", "a", value = 10),
  numericInput("b", "b", value = 1),
  numericInput("c", "c", value = 1),
  plotOutput("x"),
  tableOutput("y"),
  textOutput("z")
)

server <- function(input, output, session) {
  rng <- reactive(input$a * 2)
  smp <- reactive(sample(rng(), input$b, replace = TRUE))
  bc <- reactive(input$b * input$c)
  
  output$x <- renderPlot(hist(smp()))
  output$y <- renderTable(max(smp()))
  output$z <- renderText(bc())
  
  # for answering 3 :P
  # x <- reactiveVal(1)
  # y <- reactive(x() + y())
  # output$z <- renderText(y())
}

shinyApp(ui, server)


### Exercises

# 1. There are no outputs, so there is no need for any reactive computations.
# 2. Might just be 1s for the y1. The y2 and y3 won't have to recompute anything since x2/x3 did not change.
# x2 -> 2s, while x3 -> 1s.
# 3. STACK OVERFLOW! Or infinite recursion.