#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

stake<-0.2
coin<-data.frame(value = c(-1,1), prob = c(.4,.6))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Kelly Criterion"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("tosses",
                     "Number of tosses:",
                     min = 1,
                     max = 2000,
                     value = 50),
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
                      step = .01)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot"),
         verbatimTextOutput("desc")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  vals <- reactiveValues(finalBankroll = NULL, kellyCriterion = NULL)
  
   output$plot <- renderPlot({
     
     bankroll<-rep(0,input$tosses+1)
     bankroll[1]<-input$bankroll
     
     sample1<- sample(c(1, -1), size=input$tosses, prob=c(.6, .4), replace=TRUE)
     for (j in 1:input$tosses) {
       bankroll[j+1]<-(1+sample1[j]*input$fraction)*bankroll[j]
     }
     
     vals$finalBankroll = tail(bankroll, n = 1)
     plot(bankroll, type="l")
   })
   
   output$desc <- renderPrint({
     if (is.null(vals$finalBankroll))
     {
       cat("Bankroll unavailable!")
     }
     else
     {
       cat("Final bankroll:", vals$finalBankroll)
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

