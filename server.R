# 3. What is the median return after, say, 1000 (you can choose a different number) tosses 
#    if you bet 1%, 2%, 3%, ..., 20%, 21%, ... of the bankroll. You can choose a p=0.6 coin 
#    or a different coin.

library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)

kelly <- function(coin) {
  return((coin$value[2] * coin$prob[2] - coin$prob[1]) / coin$value[2])
}

createCoin <- function(prob) {
  return(data.frame(value = c(-1,1), prob = c(1-prob,prob)))
}

server <- function(input, output) {
  
  # game and second plot values
  vals <- reactiveValues(finalBankroll = NULL,
                         simTosses = NULL,
                         kellyCriterion = NULL, 
                         finalBankrollSecondary = NULL,
                         coin = NULL)
  
  # values for zoom functionality
  zoomVals <- reactiveValues(x = NULL, y = NULL)
  
  # values for first plot
  medianPlotVals <- reactiveValues(successes = NULL,
                                   stake = seq(from = 0, to = 1, by = .01),
                                   median_bankroll = NULL)
  
  # show and hide secondary bets options
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
  
  # set zoom values when double clicked
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    # if no area is selected - show entire plot
    if (!is.null(brush)) {
      zoomVals$x <- c(brush$xmin, brush$xmax)
      zoomVals$y <- c(brush$ymin, brush$ymax)
      
    } else {
      zoomVals$x <- NULL
      zoomVals$y <- NULL
    }
  })
  
  # second plot
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
    
    # calculate bankroll
    bankroll <- Reduce(
      function(v, x) {
        input$currency %>%
          if_else(
            ((1 + x * input$fraction) * v) %>% round(2),
            (1 + x * input$fraction) * v)},
      x=simTosses,  
      init=input$bankroll, 
      accumulate=TRUE)
    vals$finalBankroll <- tail(bankroll, n = 1)
    
    plot <- ggplot(data.frame(y = bankroll, x = 1:(input$tosses + 1)), aes(y = y, x = x)) + 
      geom_path() +
      coord_cartesian(xlim = zoomVals$x, ylim = zoomVals$y, expand = FALSE)
    
    if (input$compare)
    {
      # calculate secondary bankroll
      bankrollSecondary <- Reduce(
        function(v, x) {
          input$currencySecondary %>%
            if_else(
              ((1 + x * input$fractionSecondary) * v) %>% round(2),
              (1 + x * input$fractionSecondary) * v)},
        x=simTosses,  
        init=input$bankroll, 
        accumulate=TRUE)
      vals$finalBankrollSecondary <- tail(bankrollSecondary, n = 1)

      plot <- plot + geom_path(aes(y = bankrollSecondary), color = 'red')
    }
    plot
  })
  
  # second plot description
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
  
  # display kelly criterion in sidebar
  output$coinDesc <- renderText({
    paste("Kelly criterion:", vals$kellyCriterion)
  })
  
  # calculate median bankroll for first plot
  observe({
    validate({
      need(input$tosses <= 1500, label = "A lower number of tosses")
    })
    
    medianPlotVals$successes <- qbinom(.5, input$tosses, vals$coin$prob[2]) # basically just input$tosses * vals$coin$prob[2]
    medianPlotVals$median_bankroll <- 
      input$bankroll * (1 + medianPlotVals$stake) ^ medianPlotVals$successes * 
      (1 - medianPlotVals$stake) ^ (input$tosses - medianPlotVals$successes)
  })
  
  # first plot
  output$medianPlot <- renderPlot({
    validate(
      need(input$tosses != "" && input$tosses > 0, label = "A valid number of tosses"),
      need(input$tosses <= 1500, label = "A lower number of tosses"),
      need(input$bankroll != "" && input$bankroll > 0, label = "A valid bankroll")
    )
    
    plot <- 
      ggplot(NULL, aes(y = medianPlotVals$median_bankroll, x = medianPlotVals$stake)) + 
      geom_path()
    
    if (vals$kellyCriterion >= 0)
    {
      offset <- ((vals$kellyCriterion <= .90) - .5) * .1 # offset for label (left side or right side of vline)
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
  
  # first plot description
  output$medianDesc <- renderUI({
    withMathJax(paste("\nThe median bankroll for a certain stake can be found with ease using the following equation:",
                "$$\\text{medianBankroll} = \\text{bankroll}\\cdot(1+\\text{stake})^\\text{successes}\\cdot(1-\\text{stake})^\\text{tosses - successes}$$",
                "The variable \"successes\" here is the median number of successes for a series of tosses and is equal to the second quantile of a binomial distribution (or in other words - its expected value)",
                "$$Binom(n = \\text{number of tosses}, p = \\text{probability of winning a toss})$$",
                "which in the case of this specific game looks like this:",
                sprintf("$$Binom(n = %d, p = %.02f)$$", input$tosses, input$coinProb), sep = ""))
  })
}