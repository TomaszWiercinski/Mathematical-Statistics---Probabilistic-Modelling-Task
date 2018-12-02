library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Kelly Criterion"),
  sidebarLayout(
    sidebarPanel(
      h3("Coin:"),
      sliderInput("coinProb",
                  "Probability of win:",
                  min = 0,
                  max = 1,
                  value = .6,
                  step = .01),
      textOutput("coinDesc"),
      h3("Game options:"),
      numericInput("seed",
                   "Seed:",
                   min = 0,
                   max = 9999,
                   value = 1008,
                   step = 100),
      numericInput("tosses",
                   "Number of tosses:",
                   min = 1,
                   max = 2000,
                   value = 50,
                   step = 100),
      numericInput("bankroll", 
                   "Initial Bankroll:",
                   min = 1, 
                   max = 100000, 
                   value = 100,
                   step = 100)),
    mainPanel(
      plotOutput("medianPlot")
    )),
  sidebarLayout(
    sidebarPanel(
      h3("First strategy:"),
      sliderInput("fraction", 
                   "Fraction:",
                   min = 0, 
                   max = 1, 
                   value = .5,
                   step = .01),
      checkboxInput("currency",
                    "Use actual currency",
                    value = FALSE),
      checkboxInput("compare",
                   "Show secondary strategy",
                   value = FALSE),
      hidden(
        h3(id="secondaryTitle", "Second strategy:"),
        sliderInput("fractionSecondary", 
                     "Fraction:",
                     min = 0, 
                     max = 1, 
                     value = .5,
                     step = .01),
        checkboxInput("currencySecondary",
                      "Use actual currency",
                      value = FALSE)
      )
      
    ),
    
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("desc")
    )
  )
)