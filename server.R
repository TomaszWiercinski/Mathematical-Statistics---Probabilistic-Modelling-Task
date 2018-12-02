library(shiny)
library(shinyjs)
library(ggplot2)

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
  
  zoomVals <- reactiveValues(x = NULL, y = NULL)
  
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
  
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      zoomVals$x <- c(brush$xmin, brush$xmax)
      zoomVals$y <- c(brush$ymin, brush$ymax)
      
    } else {
      zoomVals$x <- NULL
      zoomVals$y <- NULL
    }
  })
  
  output$plot <- renderPlot({
    
    validate(
      need(input$tosses != "" && input$tosses > 0, label = "A valid number of tosses"),
      need(input$bankroll != "" && input$bankroll > 0, label = "A valid bankroll"),
      need(input$seed != "", label = "A valid seed"),
      need(input$tosses <= 100000, label = "A lower number of tosses")
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
    
    plot <- ggplot(data.frame(y = bankroll, x = 1:(input$tosses + 1)), aes(y = y, x = x)) + 
      geom_path() +
      coord_cartesian(xlim = zoomVals$x, ylim = zoomVals$y, expand = FALSE)
    
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

      plot <- plot + geom_path(aes(y = bankrollSecondary), color = 'red')
    }
    plot
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
  
  # median plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  medianPlotVals <- reactiveValues(successes = NULL,
                                   stake = seq(from = 0, to = 1, by = .01),
                                   median_bankroll = NULL)
  observe({
    validate({
      need(input$tosses <= 1700, label = "A lower number of tosses")
    })
    
    medianPlotVals$successes <- qbinom(.5, input$tosses, vals$coin$prob[2])
    medianPlotVals$median_bankroll <- 
      input$bankroll * (1 + medianPlotVals$stake) ^ medianPlotVals$successes * 
      (1 - medianPlotVals$stake) ^ (input$tosses - medianPlotVals$successes)
  })
  
  output$medianPlot <- renderPlot({
    validate(
      need(input$tosses != "" && input$tosses > 0, label = "A valid number of tosses"),
      need(input$tosses <= 1700, label = "A lower number of tosses"),
      need(input$bankroll != "" && input$bankroll > 0, label = "A valid bankroll")
    )
    
    plot <- 
      ggplot(NULL,
             aes(y = medianPlotVals$median_bankroll, x = medianPlotVals$stake)) + 
      geom_path()
    if (vals$kellyCriterion >= 0)
    {
      offset <- ((vals$kellyCriterion <= .90) - .5) * .1 # ???
      plot <- plot + 
        geom_vline(aes(xintercept = vals$kellyCriterion), color = "red") +
        annotate("label", 
                 label = "Kelly criterion", 
                 x = vals$kellyCriterion + offset, 
                 y = input$bankroll * 0.05, 
                 size = 4, 
                 colour = "red")
    }
    plot
  })
}