library(shiny)

# uncomment this line if running app in a fresh R session
source(here::here("ch5_case_study/analysis.R"))

# app 1
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

# app 2: only show a few top counts for diag/body_part/location and lump all others to Other
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

# app 3: allow choice btw count vs rate plot
ui <- fluidPage(
  fluidRow(
    column(
      8,
      selectInput("code", "Product", setNames(products$prod_code, products$title), width = "100%")
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
  )
)

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
}

# app 4: add narrative triggered by action button
ui <- fluidPage(
  fluidRow(
    column(
      8,
      selectInput("code", "Product", setNames(products$prod_code, products$title), width = "100%")
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
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
)

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
    
  output$narrative <- renderText({
    # actionButton is an integer that + each time it's clicked and can be used just as 
    # a trigger for a render function
    input$story
    selected() %>% pull(narrative) %>% sample(1)
  })
}

shinyApp(ui, server)

#### Exercises

# 1) see pngs
# 2) if we first fct_lump before fct_infreq (`mutate({{ var }} := fct_infreq(fct_lump({{ var }}, n = n)))`),
# we will first lump and add an Other category, and then include it in the reordering via `fct_infreq`,
# which results in "Other" not necessarily ending up as last item which is what we want.
# 3) see ex3.R
# 4) see ex4.R

