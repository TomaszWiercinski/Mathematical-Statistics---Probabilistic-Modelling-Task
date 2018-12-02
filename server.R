library(shiny)
library(shinyjs)

kelly <- function(coin) {
  return((coin$value[2] * coin$prob[2] - coin$prob[1]) / coin$value[2])
}

createCoin <- function(prob) {
  return(data.frame(value = c(-1,1), prob = c(1-prob,prob)))
}

server <- function(input, output) {
  
  vals <- reactiveValues(finalBankroll = NULL,
                         simTosses = NULL,
                         kellyCriterion = NULL, 
                         finalBankrollSecondary = NULL,
                         coin = NULL)
  
  observeEvent(input$compare, {
    if (input$compare)
    {
      show("fractionSecondary", anim = TRUE, animType = "slide")
      show("currencySecondary", anim = TRUE, animType = "slide")
      show("secondaryTitle", anim = TRUE, animType = "slide")
    }
    else
    {
      hide("fractionSecondary", anim = TRUE, animType = "slide")
      hide("currencySecondary", anim = TRUE, animType = "slide")
      hide("secondaryTitle", anim = TRUE, animType = "slide")
    }
  })
  
  # create a coin and calculate the kelly criterion
  observeEvent(input$coinProb, {
    vals$coin <- createCoin(input$coinProb)
    vals$kellyCriterion <- kelly(vals$coin)
  })
  
  output$plot <- renderPlot({
    
    validate(
      need(input$tosses != "" && input$tosses > 0, label = "A valid number of tosses"),
      need(input$bankroll != "" && input$bankroll > 0, label = "A valid bankroll"),
      need(input$seed != "", label = "A valid seed")
    )
    
    # simulate tosses
    set.seed(input$seed)
    simTosses<- sample(vals$coin$value, size=input$tosses, prob=vals$coin$prob, replace=TRUE)
    
    bankroll <- rep(0, input$tosses + 1)
    bankroll[1] <- input$bankroll
    
    # calculate bankroll
    for (j in 1:input$tosses) {
      bankroll[j+1] <- 
        (1 + simTosses[j] * input$fraction) * bankroll[j]
      if (input$currency)
        bankroll[j+1] <- round(bankroll[j+1], digits = 2)
    }
    vals$finalBankroll <- tail(bankroll, n = 1)
      
    plot(bankroll, type="l")
    
    if (input$compare)
    {
      bankrollSecondary <- rep(0, input$tosses + 1)
      bankrollSecondary[1] <- input$bankroll
      
      # calculate secondary bankroll
      for (j in 1:input$tosses) {
        bankrollSecondary[j+1]<-(1+simTosses[j]*input$fractionSecondary)*bankrollSecondary[j]
        if (input$currencySecondary)
          bankrollSecondary[j+1] <- round(bankrollSecondary[j+1], digits = 2)
      }
      vals$finalBankrollSecondary <- tail(bankrollSecondary, n = 1)
      
      lines(bankrollSecondary, col='red')
    }
  })
  
  output$desc <- renderPrint({
    
    out <- paste("Final bankroll: ", 
                 vals$finalBankroll,
                 "\n",
                 sep = "")
    if (input$compare)
      out <- paste(out, 
                   "Final bankroll for the second strategy: ", 
                   vals$finalBankrollSecondary, 
                   "\nDifference: ",
                   abs(vals$finalBankrollSecondary - vals$finalBankroll),
                   sep = "")
    cat(out)
  })
  
  output$coinDesc <- renderText({
    paste("Kelly criterion:", vals$kellyCriterion)
  })
  
  output$medianPlot <- renderPlot({
    validate(
      need(input$tosses != "" && input$tosses > 0, label = "A valid number of tosses"),
      need(input$bankroll != "" && input$bankroll > 0, label = "A valid bankroll")
    )
    
    stake <- seq(from = 0, to = 1, by = .01)
    
    successes <- qbinom(.5, input$tosses, vals$coin$prob[2])
    median_bankroll <- 
      input$bankroll * (1 + stake) ^ successes * (1 - stake) ^ (input$tosses - successes)
    
    plot(stake, median_bankroll, type = "l")
    if (vals$kellyCriterion >= 0)
    {
      abline(v = vals$kellyCriterion, col = "red")
      offset <- ((vals$kellyCriterion <= .90) - .5) * .1 # ???
      text(x = vals$kellyCriterion + offset, y = input$bankroll * 0.05, labels = "Kelly criterion", col = "red")
    }
  })
}