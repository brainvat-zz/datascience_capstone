library(shiny)

shinyUI(fluidPage(
  
  img(src = "proogle.png", height = 110, width = 315),
  #titlePanel("Autocomplete"),
  p(tags$b("Coming Soon!"), "Type a 1- or 2-word phrase and Proogle will autocomplete your search and show you the results."),
  
  sidebarLayout(
    sidebarPanel(
      textInput("query", "Type query here:", ""),
      
      selectInput("corpus", "Choose a corpus:", 
                  choices = c("blogs", "news", "tweets")),
      
      numericInput("num_results", "Most frequent Ngrams:", 10, min = 1, max = 100),
      
      numericInput("num_ngrams", "Minimum number of Ngrams", 1, min = 1, max = 3)
    ),
    
    mainPanel(
      h3(textOutput("query", container = span)),
      
      conditionalPanel(
        condition = "input.query != ''",
        verbatimTextOutput("summary")),
      
      tableOutput("view"),
      
      plotOutput(outputId = "main_plot")
    )
  )
))