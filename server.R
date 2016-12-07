require(shiny)
require(quantmod)
require(PerformanceAnalytics)
require(dygraphs)
source('functions.R')

if (!require(PerformanceAnalytics)) {
  stop("This app requires the PerformanceAnalytics package. To install it, run 'install.packages(\"PerformanceAnalytics\")'.\n")
}
if (!require(quantmod)) {
  stop("This app requires the quantmod package. To install it, run 'install.packages(\"quantmod\")'.\n")
}
if (!require(shiny)) {
  stop("This app requires the shiny package. To install it, run 'install.packages(\"shiny\")'.\n")
}
if (!require(dygraphs)) {
  stop("This app requires the dygraphs package. To install it, run 'install.packages(\"dygraphs\")'.\n")
}

#########################
options(digits=5)

BENCHMARK_SYMBOLS <- c('SPY','TLT')

shinyServer(function(input, output, session){
  
  #Dynamic date updater (reactive)
  observe({
    updateDateRangeInput(session,"dateRange", start = Sys.Date() - 1 - 365, end = Sys.Date() - 1, max = Sys.Date())
    #     updateNumericInput(session, "smoothing")
    #     updateNumericInput(session, "length")
    #     updateNumericInput(session, "threshold")
    
  })
  
  dataInput <- reactive({   
    
    validate(
      need(input$selectPrimary != "", "Please select a symbol")
    )
    
    data <- lapply(input$selectPrimary, function(x) {getSymbols(x, src = "google",      #Seperated the data into two seperate data sets and set auto.assign=FALSE
                                                                from = input$dateRange[1],
                                                                to = input$dateRange[2],
                                                                auto.assign = FALSE)})
    
    Closes <- lapply(data, function(x) Cl(x))
    Closes <- do.call(merge, Closes)
    return(Closes)
  })
  
  output$selectStrategy = renderUI({
    selectInput('selectStrategy','Select Strategy', choices = strategyMethods)
  })
  
  #$1 dollar index panel output
  output$index <- renderDygraph({
    input$submitButton
    isolate(
      if(input$radio.Benchmark == 'SPY')
      {
        ben <- getSymbols('SPY', src = "google",from = input$dateRange[1],to = input$dateRange[2],auto.assign = FALSE)
        ben <- EW.Portfolio(na.omit(ROC(Cl(ben))), T)
      }
      else
      {
        
        ben <- lapply(c('SPY','TLT'), function(x) {getSymbols(x, src = "google",  from = input$dateRange[1],to = input$dateRange[2],auto.assign = FALSE)})
        ben <- lapply(ben, function(x) Cl(x))
        ben <- do.call(merge, ben)
        ben <- EW.Portfolio(na.omit(ROC(Cl(ben))), T)
      }
    )
    
    names(ben) <- 'Benchmark'
    
    isolate(
      returns <- na.omit(ROC(dataInput()))
    )
    ew <- EW.Portfolio(returns, T)
    names(ew) <- 'EW'
    
    dygraph(cbind(ew,ben), main = "EW Portfolio $1 Index", group = "charts") %>%
      dySeries("EW",  strokeWidth = 2, color = "blue") %>%
      dySeries("Benchmark",  strokePattern = "dashed", color = 'green') %>%
      dyRangeSelector()     
    
  })
  
  #price panel output
  output$price <- renderDygraph({
    input$submitButton
    isolate(
      dygraph(dataInput(), main = "Raw Close(s)", group = "charts")
    )
  })
  
  #Performance stats
  output$perf.Stats <- renderTable({
    
    if(input$radio.Benchmark == 'SPY')
    {
      ben <- getSymbols('SPY', src = "google",from = input$dateRange[1],to = input$dateRange[2],auto.assign = FALSE)
      ben <- EW.Portfolio(na.omit(ROC(Cl(ben))), F)
    }
    else
    {
      ben <- lapply(c('SPY','TLT'), function(x) {getSymbols(x, src = "google",  from = input$dateRange[1],to = input$dateRange[2],auto.assign = FALSE)})
      ben <- lapply(ben, function(x) Cl(x))
      ben <- do.call(merge, ben)
      ben <- EW.Portfolio(na.omit(ROC(Cl(ben))), F)
    }
    
    names(ben) <- 'Benchmark'
    
    isolate(
      returns <- na.omit(ROC(dataInput()))
    )
    ew <- EW.Portfolio(returns, F)
    
    Output <- cbind(Performance.Stats(ben), Performance.Stats(ew))
    return(Output)
    
  }, rownames = TRUE)
  
  secondaryInput <- reactive({   
    
    if(input$selectSecondary != ""){
      data <- lapply(input$selectSecondary, function(x) {getSymbols(x, src = "google",      #Seperated the data into two seperate data sets and set auto.assign=FALSE
                                                                    from = input$dateRange[1],
                                                                    to = input$dateRange[2],
                                                                    auto.assign = FALSE)})
    }
    else
    {
      data <- dataInput()
    }
    
    Closes <- lapply(data, function(x) Cl(x))
    Closes <- do.call(merge, Closes)
    return(Closes)
  })
  
  #Strategy 
  output$strategy <- renderDygraph({
    
    if(input$applyStrategy != TRUE)
    {
      returns <- na.omit(ROC(dataInput()))
      ew <- EW.Portfolio(returns, T)
      names(ew) <- 'EW'
      dygraph(ew, main = "EW Portfolio $1 Index", group = "charts") %>%
        dySeries("EW",  strokeWidth = 2, color = "blue")
    }
    else
    {
      returns <- na.omit(ROC(dataInput()))
      ew <- EW.Portfolio(returns, T)
      
      returns.sec <- na.omit(ROC(secondaryInput()))
      ew.sec <- EW.Portfolio(returns.sec, T)
      
      input$changeButton
      isolate(
        if(input$selectStrategy == "Momentum")
        {
          perf.returns <- MomentumStrat(ew,ew.sec,input$smoothing, input$length)
        }
        else if(input$selectStrategy == "ZScore")
        {
          perf.returns <- ZscoreStrat(ew,ew.sec,input$smoothing, input$length, input$threshold)
        }
        else if(input$selectStrategy == "Counter-ZScore")
        {
          perf.returns <- CounterZscoreStrat(ew,ew.sec,input$smoothing, input$length, input$threshold)
        }
      )
      perf <- EW.Portfolio(perf.returns,T)
      names(perf) <- 'Equity'
      
      dygraph(cbind(ew,perf), main = "EW Portfolio $1 Index & Equity Curve", group = "charts") %>%
        dySeries("EW.Close",  strokeWidth = 0.5, color = "blue", axis = 'y') %>%
        dySeries(("Equity"), strokeWidth = 3, color = "green", axis = 'y2') %>%
        dyRangeSelector()
    }
    
  })
  
  output$secondary <- renderDygraph({
    
    dygraph(EW.Portfolio(na.omit(ROC(secondaryInput())),T), group = "charts")
    
  })
  
  output$indicator <- renderDygraph({
    
    input$changeButton
    ew <- EW.Portfolio(na.omit(ROC(secondaryInput())),T)
    
    if(input$selectStrategy == "Momentum")
    {
      isolate(
        ema <- EMA(ew, input$smoothing)
      )
      isolate(
        mom <- momentum(ema, input$length)
      )
      mom$trigger <- 0
      dygraph(mom, group = "charts")
    }
    else
    {
      isolate(
        ema <- EMA(ew, input$smoothing)
      )
      
      isolate(
        zscore <- ZScore(ema, input$length)
      )
      
      isolate(
        zscore$up <- input$threshold
      )
      
      isolate(
        zscore$dn <- -1 * input$threshold
      )
      dygraph(zscore,group = "charts")
    }
    
  })
  
})


