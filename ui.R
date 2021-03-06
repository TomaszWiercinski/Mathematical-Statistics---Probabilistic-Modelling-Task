# 3. What is the median return after, say, 1000 (you can choose a different number) tosses 
#    if you bet 1%, 2%, 3%, ..., 20%, 21%, ... of the bankroll. You can choose a p=0.6 coin 
#    or a different coin.

library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  withMathJax(),
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
      plotOutput("medianPlot"),
      br(),
      uiOutput("medianDesc")
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
      plotOutput("plot",
                 dblclick = "plot_dblclick",
                 brush = brushOpts(
                   id = "plot_brush",
                   resetOnNew = TRUE
                 )),
      verbatimTextOutput("desc")
    )
  )
)