
count_top <- function(df, var, n = 5) {
  df %>%
    # only count the top 5 or `n` and lump other variables to "Other"
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

ui <- fluidPage(
  fluidRow(
    column(
      6,
      selectInput("code", "Product", setNames(products$prod_code, products$title), width = "100%")
    ),
    # to choose how many rows to display for each summary table
    column(
      2,
      numericInput("rows", "Summary rows", min = 1, max = 10, value = 5)
    ),
    # to choose btw two ways to plot the age/sex vs injuries plot (default to rate)
    column(
      2,
      selectInput("y", "Y axis", c("rate", "count"))
    )
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("prev_story", "Previous story")),
    column(2, actionButton("next_story", "Next story")),
    column(8, textOutput("narrative"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code) %>% head(10))
  
  # aesthetic change of width="100%" is to make all tables appear the same
  output$diag <- renderTable(selected() %>% count_top(diag, n = input$rows), width = "100%")
  output$body_part <- renderTable(selected() %>% count_top(body_part, n = input$rows), width = "100%")
  output$location <- renderTable(selected() %>% count_top(location, n = input$rows), width = "100%")
  
  # this reactive expr is not really necessary but used to separate computation code from plotting code
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  # conditionally show either the count plot or the rate plot
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  
  # set up a reactive value for stepping through narrative
  narrative_val <- reactiveVal(1)
  
  # on the previous story button click, decrement narrative_val so we retrieve previous narrative
  observeEvent(input$prev_story,
    {
      if (narrative_val() > 1) {
        narrative_val(narrative_val() - 1)
      }
    }
  )
  
  # on the next story button click, increment narrative_val so we retrieve next narrative or
  # back to first and so on.
  observeEvent(input$next_story, 
    {
      # to create circular list, we only move forward if below nrow of dataset
      if (narrative_val() < nrow(selected())) {
        narrative_val(narrative_val() + 1)
      } else {
        # once we're equal to last one, the next will be reset to 1
        narrative_val(1)
      }
    }
  )

  output$narrative <- renderText({
    # show the narrative according to the row number based on `narrative_val`
    story <- selected() %>% slice(narrative_val()) %>% pull(narrative)
  })
}

shinyApp(ui, server)