library(shiny)
library(ggplot2)

histogram <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("x1", length(x1)), rep("x2", length(x2)))
  )

  ggplot(df, aes(x, fill = g)) +
    geom_histogram(binwidth = binwidth) +
    coord_cartesian(xlim = xlim)
}

t_test <- function(x1, x2) {
  test <- t.test(x1, x2)

  sprintf(
    "p value: %0.3f\n[%0.2f, %0.2f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}

ui <- fluidPage(
  fluidRow(
    column(
      4,
      "Distribution 1",
      numericInput("n1", label = "n", value = 1000, min = 1),
      numericInput("mean1", label = "µ", value = 0, step = 0.1),
      numericInput("sd1", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(
      4,
      "Distribution 2",
      numericInput("n2", label = "n", value = 1000, min = 1),
      numericInput("mean2", label = "µ", value = 0, step = 0.1),
      numericInput("sd2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(
      4,
      "Histogram",
      numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
      sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
    )
  ),
  fluidRow(
    column(9, plotOutput("hist")),
    column(3, verbatimTextOutput("ttest"))
  )
)

# inefficient version which creates a reactive graph that's complex because we have
# redundancy for `x1` and `x2` and recompute for histogram and ttest outputs
server <- function(input, output, session) {
  output$hist <- renderPlot({
    x1 <- rnorm(input$n1, input$mean1, input$sd1)
    x2 <- rnorm(input$n2, input$mean2, input$sd2)
    histogram(x1, x2, binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  
  output$ttest <- renderText({
    x1 <- rnorm(input$n1, input$mean1, input$sd1)
    x2 <- rnorm(input$n2, input$mean2, input$sd2)
    t_test(x1, x2)
  })
}

# more efficient version by making using reactive expressions for `x1` and `x2`
server <- function(input, output, session) {
  x1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
  x2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
  
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  
  output$ttest <- renderText({
    t_test(x1(), x2())
  })
}

# this is bad because inputs are not in a reactive context
server <- function(input, output, session) {
  x1 <- rnorm(input$n1, input$mean1, input$sd1)
  x2 <- rnorm(input$n2, input$mean2, input$sd2)
  
  output$hist <- renderPlot({
    histogram(x1, x2, binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  
  output$ttest <- renderText({
    t_test(x1, x2)
  })
}

# this fixes the above issue but creates another: you recompute x1, x2 for histogram / t_ttest
server <- function(input, output, session) { 
  x1 <- function() rnorm(input$n1, input$mean1, input$sd1)
  x2 <- function() rnorm(input$n2, input$mean2, input$sd2)
  
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  
  output$ttest <- renderText({
    t_test(x1(), x2())
  })
}

# lesson: Reactive expressions automatically cache their results, and only update when 
# their inputs change, so use them whenever you repeat yourself more than once for the
# same code

ui <- fluidPage(
  fluidRow(
    column(3, 
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 3),
           numericInput("n", label = "n", value = 1e4, min = 0)
    ),
    column(9, plotOutput("hist"))
  )
)
server <- function(input, output, session) {
  x1 <- reactive(rpois(input$n, input$lambda1))
  x2 <- reactive(rpois(input$n, input$lambda2))
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
}

# we can use a `reactiveTimer` to control how often a reactive expression updates (or, invalidates itself)
server <- function(input, output, session) {
  timer <- reactiveTimer(500)
  
  x1 <- reactive({
    timer()
    rpois(input$n, input$lambda1)
  })
  x2 <- reactive({
    timer()
    rpois(input$n, input$lambda2)
  })
  
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
}

# but the problem is if the computation (e.g. `rpois`) takes longer than the timer ms, Shiny
# now has a backlog of computations that blocks the UI for other user events while it's computing

# one good solution is to let user control when the simulation happens with button
ui <- fluidPage(
  fluidRow(
    column(3, 
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 3),
           numericInput("n", label = "n", value = 1e4, min = 0),
           actionButton("simulate", "Simulate!")
    ),
    column(9, plotOutput("hist"))
  )
)

server <- function(input, output, session) {
  # the problem here is that we'll still update the histogram when changing the other
  # inputs like `lambda1`, `lambda2`, and `n`
  x1 <- reactive({
    input$simulate
    rpois(input$n, input$lambda1)
  })
  
  x2 <- reactive({
    input$simulate
    rpois(input$n, input$lambda2)
  })
  
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
  
}

# `event_reactive*` to the rescue!
server <- function(input, output, session) {
  # now we only compute when the button is clicked
  # note: we continue to use the `n` and `lambda1`/`lambda2`, but the difference
  # now is `x1` and `x2` don't take a reactive dependency on them
  x1 <- eventReactive(input$simulate, {
    rpois(input$n, input$lambda1)
  })
  
  x2 <- eventReactive(input$simulate, {
    rpois(input$n, input$lambda2)
  })
  
  output$hist <- renderPlot({
    histogram(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
  
}

# sometimes, we want to do a side effect based on an event and not necessarily for
# affecting how the app looks. we can use `observer` for this case

# example:
# text
ui <- fluidPage(
  textInput("name", "What's your name?"),
  verbatimTextOutput("greeting")
)

server <- function(input, output, session) {
  string <- reactive(paste0("Hello ", input$name, "!"))
  
  output$greeting <- renderText(string())
  # check the console and note how every time the input changes, we log "Greeting performed"
  # differences from `reactiveEvent`:
  # - we don't use a return value
  # - we can't refer to it from other reactive consumers
  observeEvent(input$name, {
    message("Greeting performed")
  })
}


### No exercises in book yet.

shinyApp(ui, server)
