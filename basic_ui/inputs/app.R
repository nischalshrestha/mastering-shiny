library(shiny)

# free text
ui <- fluidPage(
  textInput("name", "What's your name?"),
  passwordInput("password", "What's your password?"),
  textAreaInput("story", "Tell me about yourself", rows = 3)
)

# numeric inputs
ui <- fluidPage(
  numericInput("num", "Number one", value = 0, min = 0, max = 100),
  sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
  sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100)
)

# dates
ui <- fluidPage(
  dateInput("dob", "When were you born?"),
  dateRangeInput("holiday", "When do you want to go on vacation next?")
)

# limited choices
animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")

ui <- fluidPage(
  selectInput("state", "What's your favourite state?", state.name),
  radioButtons("animal", "What's your favourite animal?", animals)
)

# non text choice for radio buttons
ui <- fluidPage(
  radioButtons("rb", "Choose one:",
     choiceNames = list(
       icon("angry"),
       icon("smile"),
       icon("sad-tear")
     ),
     choiceValues = list("angry", "happy", "sad")
  )
)

# dropdowns
ui <- fluidPage(
  selectInput(
    "state", "What's your favourite state?", state.name,
    multiple = TRUE
  )
)

# select multiple items with "radio buttons"
ui <- fluidPage(
  checkboxGroupInput("animal", "What animals do you like?", animals)
)

# just one choice for yes/no checkbox
ui <- fluidPage(
  checkboxInput("cleanup", "Clean up?", value = TRUE),
  checkboxInput("shutdown", "Shutdown?")
)

# file upload
ui <- fluidPage(
  fileInput("upload", NULL)
)

# action buttons
ui <- fluidPage(
  actionButton("click", "Click me!"),
  actionButton("drink", "Drink me!", icon = icon("cocktail"))
)

# change up the size and class style
ui <- fluidPage(
  fluidRow(
    actionButton("click", "Click me!", class = "btn-danger"),
    actionButton("drink", "Drink me!", class = "btn-lg btn-success")
  ),
  fluidRow(
    actionButton("eat", "Eat me!", class = "btn-block")
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
