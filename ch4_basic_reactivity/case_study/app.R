library(shiny)

# uncomment this line if running app in a fresh R session
# source(here::here("ch4_basic_reactivity/case_study/analysis.R"))

ui <- fluidPage(
  fluidRow(
    column(
      6,
      selectInput("code", "Product", setNames(products$prod_code, products$title))
    )
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
)

# 1st app
server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  output$diag <- renderTable(
    selected() %>% count(diag, wt = weight, sort = TRUE)
  )
  output$body_part <- renderTable(
    selected() %>% count(body_part, wt = weight, sort = TRUE)
  )
  output$location <- renderTable(
    selected() %>% count(location, wt = weight, sort = TRUE)
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  }, res = 96)
}


# 2nd app
count_top <- function(df, var, n = 5) {
  df %>%
    # only count the top 5 or `n` and lump other variables to "Other"
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))

  # aesthetic change of width="100%" is to make all tables appear the same
  output$diag <- renderTable(selected() %>% count_top(diag), width = "100%")
  output$body_part <- renderTable(selected() %>% count_top(body_part), width = "100%")
  output$location <- renderTable(selected() %>% count_top(location), width = "100%")

  # this reactive expr is not really necessary but used to separate computation code from plotting code
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })

  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  }, res = 96)
}

# 3rd app



shinyApp(ui, server)
