library(shiny)
library(shinyjs)

stake<-0.2
coin<-data.frame(value = c(-1,1), prob = c(.4,.6))

ui <- fluidPage(
   useShinyjs(),
   titlePanel("Kelly Criterion"),
   
   sidebarLayout(
      sidebarPanel(
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
                      step = 100),
         numericInput("fraction", 
                      "Fraction:",
                      min = 0, 
                      max = 1, 
                      value = .5,
                      step = .01),
         checkboxInput("currency",
                       "Use actual currency",
                       value = FALSE),
         actionButton("compare",
                      "Add second line",
                      value = FALSE),
         hidden(
           numericInput("fractionSecondary", 
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

server <- function(input, output) {
   
  vals <- reactiveValues(finalBankroll = NULL, kellyCriterion = NULL, secondary = FALSE, finalBankrollSecondary = NULL)
  
  observeEvent(input$compare, {
    toggle("fractionSecondary", anim = TRUE, animType = "slide")
    toggle("currencySecondary", anim = TRUE, animType = "slide")
    vals$secondary <- !vals$secondary
  })
  
   output$plot <- renderPlot({
     
     set.seed(input$seed)
     
     bankroll<-rep(0,input$tosses+1)
     bankroll[1]<-input$bankroll
     
     sample1<- sample(c(1, -1), size=input$tosses, prob=c(.6, .4), replace=TRUE)
     for (j in 1:input$tosses) {
       bankroll[j+1]<-(1+sample1[j]*input$fraction)*bankroll[j]
       if (input$currency)
         bankroll[j+1] <- round(bankroll[j+1], digits = 2)
     }
     
     vals$finalBankroll = tail(bankroll, n = 1)
     plot(bankroll, type="l")
     
     if (vals$secondary)
     {
       for (j in 1:input$tosses) {
         bankroll[j+1]<-(1+sample1[j]*input$fractionSecondary)*bankroll[j]
         if (input$currencySecondary)
           bankroll[j+1] <- round(bankroll[j+1], digits = 2)
       }
       lines(bankroll, col='red')
       vals$finalBankrollSecondary = tail(bankroll, n = 1)
     }
   })
   
   output$desc <- renderPrint({
     out <- paste("Final bankroll:", vals$finalBankroll)
     if (vals$secondary)
       out <- paste(out, "\nMeanwhile the final bankroll for the secondary series of bets is equal:", vals$finalBankrollSecondary)
     cat(out)
   })
}

shinyApp(ui = ui, server = server)

