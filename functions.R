#universal <- c('TLT','IAU','DBC','VIS','VFH','VNQ','SH','VPU')
universal <- read.csv('Universal.csv', header = F,stringsAsFactors = F)[[1]]
strategyMethods <- c('Momentum','ZScore','Counter-ZScore')
CSI_HEADER <- c('Open','High','Low','Close', 'Volume','OpenInt','Expiration','Unadjusted')
ASCII_HEADER <- c('Close')

CsiBarLoader <- function(filepath, returnXTS = T) {
  df <- read.csv(filepath, header = F)
  
  if (returnXTS == T)
  {
    df[,1] <- as.Date(as.character(df[,1]), format = '%Y%m%d')
    df <- as.xts(df[,-1], OHLC = T, order.by = df[,1])
    colnames(df) <- CSI_HEADER
  }
  return (df)
}

AsciiBarLoader <- function(filepath, returnXTS = T) {
  df <- read.csv(filepath, header = F)
  
  if (returnXTS == T)
  {
    df[,1] <- as.Date(df[,1], format = '%m/%d/%Y')
    df <- as.xts(df[,-1], order.by = df[,1])
    colnames(df) <- ASCII_HEADER
  }
  return (df)
}

Performance.Stats <- function(returns)
{
  cror <- Return.annualized(returns)
  vol <- sd.annualized(returns)
  sharpe <- SharpeRatio.annualized(returns)
  
  Output <- rbind(cror, vol, sharpe)
  rownames(Output) <- c('CROR','Volatiltiy','Sharpe.Ratio')
  return(Output)
}

EW.Portfolio <- function(merged.ret, index = F)
{
  #merge ROC series
  if(ncol(merged.ret)<2)     # One return series then no need for rebalancing
  {
    ew <- Return.portfolio(merged.ret, rebalance_on = NA)
  }
  else                       # more than one return series then need for rebalancing
  {
    ew <- Return.portfolio(merged.ret, rebalance_on = 'days')
  }
  if (index == T) {
    ew <- cumprod(1 + ew)
    colnames(ew) <- "EW.Close"
  }
  else
  {
    colnames(ew) <- "EW.Return"
  }
  invisible(ew)
}

ZScore <- function(myxts, n = 100)
{
  colnames(myxts) <- "Z.Score"
  last_scale <- function(df)
  {
    return(last(scale(df)))
  }
  return(rollapply(myxts, n, last_scale))
}

##############################

MomentumStrat <-
  function(primary, secondary, ema_smoothing, mom_period, showChart = F) {
    #This is where we define the trading strategy
    
    #Lets print the name of whats running
    runName <-
      paste0("Momentum Strategy with ",ema_smoothing, "/", mom_period)
    cat("Running Strategy: ",runName)
    
    primary$Close <- Cl(primary)
    if (!missing(secondary))
    {
      #Find the column that contains the close price
      secondary$Close <- Cl(secondary)
      
      #indicator series
      secondary$Ema <- EMA(secondary$Close, ema_smoothing)
      secondary$Mom <- momentum(secondary$Ema, mom_period)
      
      #Create signal
      secondary$Sig <- rep(0, length(secondary$Close))
      secondary$Sig <- apply(secondary$Mom,1,function (x)
      {
        if (is.na(x)) {
          return (0)
        }
        else {
          if (x > 0) {
            return (1)
          }
          else if (x < 0) {
            return (-1)
          }
          else{
            return(0)
          }
        }
      })
      
      #Lag signal by 1 period
      secondary$Sig <- lag(secondary$Sig)
      
      #Calculate periodic return ROC is log return by default
      if(ncol(primary) == 8)   #CSI data
      {
        ret <- ROC(primary$Unadjusted, type = 'continuous') * secondary$Sig
      }
      else
      {
        ret <- ROC(primary$Close, type = 'continuous') * secondary$Sig
      }
    }
    else
    {
      #indicator series
      primary$Ema <- EMA(primary$Close, ema_smoothing)
      primary$Mom <- momentum(primary$Ema, mom_period)
      
      #Create signal
      primary$Sig <- rep(0, length(primary$Close))
      primary$Sig <- apply(primary$Mom,1,function (x)
      {
        if (is.na(x)) {
          return (0)
        }
        else {
          if (x > 0) {
            return (1)
          }
          else if (x < 0) {
            return (-1)
          }
          else{
            return(0)
          }
        }
      })
      
      #Lag signal by 1 period
      primary$Sig <- lag(primary$Sig)
      #Calculate periodic return ROC is log return by default
      if(ncol(primary) == 8)   #CSI data
      {
        ret <- ROC(primary$Unadjusted, type = 'continuous') * primary$Sig
      }
      else
      {
        ret <- ROC(primary$Close, type = 'continuous') * primary$Sig
      }
    }
    ret <- na.omit(ret)
    colnames(ret) <- runName
    
    cat("\nSharpe Ratio: ", SharpeRatio.annualized(ret))
    
    if(showChart)
    {
      charts.PerformanceSummary(ret, geometric = T, wealth.index = T)
    }
    invisible (na.omit(ret))
  }

ZscoreStrat <-
  function(primary, secondary, ema_smoothing, z_period, threshold = 0, showChart = F) {
    #This is where we define the trading strategy
    #Lets print the name of whats running
    runName <-
      paste0("Z-Score Strategy with ",ema_smoothing, "/", z_period,"/",threshold)
    cat("Running Strategy: ",runName)
    
    #Find the column that contains the close price
    primary$Close <- Cl(primary)
    if (!missing(secondary))
    {
      #Find the column that contains the close price
      secondary$Close <- Cl(secondary)
      
      #indicator series
      secondary$Ema <- EMA(secondary$Close, ema_smoothing)
      secondary$Zscore <- ZScore(secondary$Ema, z_period)
      
      secondary$Sig <- rep(0, length(secondary$Close))
      secondary$Sig <- apply(secondary$Zscore,1,function (x)
      {
        if (is.na(x)) {
          return (0)
        }
        else {
          if (x > threshold) {
            return (1)
          }
          else if (x < -1 * threshold) {
            return (-1)
          }
          else{
            return(0)
          }
        }
      })
      
      secondary$Sig <- lag(secondary$Sig)
      
      #Calculate periodic return ROC is log return by default
      if(ncol(primary) == 8)   #CSI data
      {
        ret <- ROC(primary$Unadjusted, type = 'continuous') * secondary$Sig
      }
      else
      {
        ret <- ROC(primary$Close, type = 'continuous') * secondary$Sig
      }
    }
    else
    {
      #indicator series
      primary$Ema <- EMA(primary$Close, ema_smoothing)
      primary$Zscore <- ZScore(primary$Ema, z_period)
      
      primary$Sig <- rep(0, length(primary$Close))
      primary$Sig <- apply(primary$Zscore,1,function (x)
      {
        if (is.na(x)) {
          return (0)
        }
        else {
          if (x > threshold) {
            return (1)
          }
          else if (x < -1 * threshold) {
            return (-1)
          }
          else{
            return(0)
          }
        }
      })
      
      primary$Sig <- lag(primary$Sig)
      
      #Calculate periodic return ROC is log return by default
      if(ncol(primary) == 8)   #CSI data
      {
        ret <- ROC(primary$Unadjusted, type = 'continuous') * primary$Sig
      }
      else
      {
        ret <- ROC(primary$Close, type = 'continuous') * primary$Sig
      }
    }
    colnames(ret) <- runName
    
    cat("\nSharpe Ratio: ", SharpeRatio.annualized(ret))
    if(showChart)
    {
      charts.PerformanceSummary(ret, geometric = T, wealth.index = T)
    }
    invisible (na.omit(ret))
  }

CounterZscoreStrat <-
  function(primary, secondary, ema_smoothing, z_period, threshold = 0, canShort = T, showChart = F) {
    #This is where we define the trading strategy
    #Lets print the name of whats running
    runName <-
      paste0("Counter Z-Score Strategy with ",ema_smoothing, "/", z_period,"/",threshold)
    cat("Running Strategy: ",runName)
    
    #Find the column that contains the close price
    primary$Close <- Cl(primary)
    if (!missing(secondary))
    {
      #Find the column that contains the close price
      secondary$Close <- Cl(secondary)
      
      #indicator series
      secondary$Ema <- EMA(secondary$Close, ema_smoothing)
      secondary$Zscore <- ZScore(secondary$Ema, z_period)
      
      secondary$Sig <- rep(0, length(secondary$Close))
      secondary$Sig <- apply(secondary$Zscore,1,function (x)
      {
        if (is.na(x)) {
          return (0)
        }
        else {
          if (x > threshold) {
            return (-1 * as.integer(canShort))
          }
          else if (x < -1 * threshold) {
            return (1)
          }
          else{
            return(0)
          }
        }
      })
      
      secondary$Sig <- lag(secondary$Sig)
      
      #Calculate periodic return ROC is log return by default
      if(ncol(primary) == 8)   #CSI data
      {
        ret <- ROC(primary$Unadjusted, type = 'continuous') * secondary$Sig
      }
      else
      {
        ret <- ROC(primary$Close, type = 'continuous') * secondary$Sig
      }
    }
    else
    {
      #indicator series
      primary$Ema <- EMA(primary$Close, ema_smoothing)
      primary$Zscore <- ZScore(primary$Ema, z_period)
      
      primary$Sig <- rep(0, length(primary$Close))
      primary$Sig <- apply(primary$Zscore,1,function (x)
      {
        if (is.na(x)) {
          return (0)
        }
        else {
          if (x > threshold) {
            return (-1 * as.integer(canShort))
          }
          else if (x < -1 * threshold) {
            return (1)
          }
          else{
            return(0)
            #na.locf(x)
          }
        }
      })
      
      primary$Sig <- lag(primary$Sig)
      
      #Calculate periodic return ROC is log return by default
      if(ncol(primary) == 8)   #CSI data
      {
        ret <- ROC(primary$Unadjusted, type = 'continuous') * primary$Sig
      }
      else
      {
        ret <- ROC(primary$Close, type = 'continuous') * primary$Sig
      }
    }
    colnames(ret) <- runName
    
    cat("\nSharpe Ratio: ", SharpeRatio.annualized(ret))
    if(showChart)
    {
      charts.PerformanceSummary(ret, geometric = T, wealth.index = T)
    }
    invisible (na.omit(ret))
  }

