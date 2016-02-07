  # Purpose: loads all forecasts and creates 4 tables of weekly forecast performance
  #
  # Note: Quite messy code.

  ######################
  ##  Initialization  ##
  ######################
  
  source("init.R")
  
  library(xtable)
  
  d.test <- window(d.xy, start = test.start, end = test.end)
  
  ##################
  ##  EPEX Price  ##
  ##################
  
  # Collect realized price in data frame
  rawdata <- window(d.xy[,c("PRI_DE")], start = test.start, end = test.end)
  colnames(rawdata) <- c('EPEX')
  hForecasts <- cbind(rawdata)
  rm(rawdata)
  
  ######################
  ##  EXAA Benchmark  ##
  ######################
  
  # Collect forecast in data frame
  rawdata <- window(d.exaa, start = test.start, end = test.end)
  colnames(rawdata) <- c('EXAA')
  hForecasts <- merge(hForecasts, rawdata)
  rm(rawdata)
  
  #######################
  ##  Model 1 - Linear ##
  #######################
  # Linear R
  # Linear with Rolling estimation

  rawdata <- read.csv('../forecasts/test/model1_linear.csv', sep=',', dec='.', stringsAsFactors=FALSE)
  Timestamp <- as.POSIXct(rawdata[,1], format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  forecast <- zoo(rawdata[,2:ncol(rawdata)], order.by = Timestamp)
  forecast <- window(forecast, start = test.start, end = test.end)
  
  # Save forecasts to data frame
  hForecasts <- merge(hForecasts, 'Linear' = forecast)
  rm(rawdata, Timestamp, forecast)

  ########################
  ##  Model 2 - ARIMAX  ##
  ########################
  # Linear model with arima error term
  
  # Get forecasts from file
  rawdata <- read.csv('../forecasts/test/ARIMAX.csv', sep=',', dec='.', stringsAsFactors=FALSE)
  Timestamp <- as.POSIXct(rawdata[,1], format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  forecast <- zoo(rawdata[,2:ncol(rawdata)], order.by = Timestamp)
  forecast <- window(forecast, start = test.start, end = test.end)
  
  # Save forecasts to data frame
  hForecasts <- merge(hForecasts, 'ARIMAX' = forecast)
  rm(rawdata, Timestamp, forecast)
  
  #######################################
  ##  Model 3 - Jonsson et al - Step 1 ##
  #######################################
  # JONSSON
  
  # Get forecasts from file
  rawdata <- read.csv('../forecasts/test/jonsson-step1.csv', sep=',', dec='.', stringsAsFactors=FALSE)
  Timestamp <- as.POSIXct(rawdata[,1], format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  forecast <- zoo(rawdata[,2], order.by = Timestamp)
  forecast <- window(forecast, start = test.start, end = test.end)
  
  # Save forecasts to data frame
  hForecasts <- merge(hForecasts, 'JonssonStep1' = forecast)
  rm(rawdata, Timestamp, forecast)
  
  #######################################
  ##  Model 4 - Jonsson et al - Step 2 ##
  #######################################
  # JONSSON
  
  # Get forecasts from file
  rawdata <- read.csv('../forecasts/test/jonsson-step2.csv', sep=',', dec='.', stringsAsFactors=FALSE)
  Timestamp <- as.POSIXct(rawdata[,1], format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  forecast <- zoo(rawdata[,2], order.by = Timestamp)
  forecast <- window(forecast, start = test.start, end = test.end)
  
  # Save forecasts to data frame
  hForecasts <- merge(hForecasts, 'JonssonStep2' = forecast)
  rm(rawdata, Timestamp, forecast)
  
  ###########################
  ##  Model 5 - SVM Linear ##
  ###########################
  # SVM with linear (vanilladot) kernel 
  
  # Get forecasts from file
  rawdata <- read.csv('../forecasts/test/svm-linear.csv', sep=',', dec='.', stringsAsFactors=FALSE)
  Timestamp <- as.POSIXct(rawdata[,1], format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  forecast <- zoo(rawdata[,2:ncol(rawdata)], order.by = Timestamp)
  forecast <- window(forecast, start = test.start, end = test.end)
  
  # Save forecasts to data frame
  hForecasts <- merge(hForecasts, 'SvmLinear' = forecast)
  rm(rawdata, Timestamp, forecast)
  
  ############################
  ##  Model 6 - SVM Radial ##
  ############################
  # SVM with radial basis function kernel 
  
  # Get forecasts from file
  rawdata <- read.csv('../forecasts/test/svm-radial.csv', sep=',', dec='.', stringsAsFactors=FALSE)
  Timestamp <- as.POSIXct(rawdata[,1], format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  forecast <- zoo(rawdata[,2:ncol(rawdata)], order.by = Timestamp)
  forecast <- window(forecast, start = test.start, end = test.end)
  
  # Save forecasts to data frame
  hForecasts <- merge(hForecasts, 'SvmRadial' = forecast)
  rm(rawdata, Timestamp, forecast)
  
  #################################
  ##  Model 7 - avNNet  ##
  #################################
  # 3 layer neural network model (averaged across 5 networks with different starting values)
  
  # Get forecasts from file
  rawdata <- read.csv('../forecasts/test/avnnet.csv', sep=',', dec='.', stringsAsFactors=FALSE)
  Timestamp <- as.POSIXct(rawdata[,1], format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  forecast <- zoo(rawdata[,2:ncol(rawdata)], order.by = Timestamp)
  forecast <- window(forecast, start = test.start, end = test.end)
  
  # Save forecasts to data frame
  hForecasts <- merge(hForecasts, 'avNNET' = forecast)
  rm(rawdata, Timestamp, forecast)

  ############################
  ##   Combined forecasts   ##
  ############################

  # CF1 forecast
  forecast <- (1/4)*(hForecasts[,c("ARIMAX")] + hForecasts[,c("SvmLinear")] + hForecasts[,c("SvmRadial")] + hForecasts[,c("avNNET")])
  colnames(forecast) <- 'CF1'
  hForecasts <- merge(hForecasts, 'CF1' = forecast)
  rm(forecast)
  
  # CF2 forecast - average of CF1 forecast and Jonsson step 2
  forecast <- (1/2)*(hForecasts[,c("JonssonStep2")] + hForecasts[,c("CF1")])
  colnames(forecast) <- 'CF2'
  hForecasts <- merge(hForecasts, 'CF2' = forecast)
  rm(forecast)
  
  # CF3 forecast - average of jonsson step 2 and exaa
  forecast <- (1/2)*(hForecasts[,c("JonssonStep2")] + hForecasts[,c("CF2")])
  colnames(forecast) <- 'CF3'
  hForecasts <- merge(hForecasts, 'CF3' = forecast)
  rm(forecast)

  # CF4 forecast - average of CF2 forecast and exaa
  forecast <- (1/2)*(hForecasts[,c("EXAA")] + hForecasts[,c("CF2")])
  colnames(forecast) <- 'CF4'
  hForecasts <- merge(hForecasts, 'CF4' = forecast)
  rm(forecast)
  
  #####################
  ##  Output Tables  ##
  #####################
  benchMarkModel <- 'EXAA'
  
  ## Create hourly MAE/MSE tables ##
  hMAE <- zoo(abs(hForecasts[,-1] - hForecasts[,1,drop=TRUE]), order.by = index(d.test))
  hMAEavg <- colMeans(hMAE, na.rm = TRUE)
  hMSE <- zoo((hForecasts[,-1] - hForecasts[,1,drop=TRUE])^2, order.by = index(d.test))
  hMSEavg <- sqrt(colMeans(hMSE, na.rm = TRUE))
  
  # Calculate average/median for each day or week
  hAverages <- ave(hForecasts[,c("EPEX")], format(index(hForecasts),"%Y-%W"),FUN=median)
  hAverages <- cbind(hAverages, "weeklyMean"=ave(hForecasts[,c("EPEX")], format(index(hForecasts),"%Y-%W"),FUN=mean))
  hAverages <- cbind(hAverages, "dailyMedian"=ave(hForecasts[,c("EPEX")], as.Date(index(hForecasts)), FUN=median))
  hAverages <- cbind(hAverages, "dailyMean"=ave(hForecasts[,c("EPEX")], as.Date(index(hForecasts)), FUN=mean))
  colnames(hAverages)<-c("weeklyMedian", "weeklyMean", "dailyMedian", "dailyMean")
  
  ## Set table Headers
  tblHeaderModels <- names(hMSE)
  names(hMAE) <- names(hMSE) <- names(hMAEavg) <- names(hMSEavg) <- tblHeaderModels
  
  strHighlight <- '\\textbf{'
  strHighlight <- '\\color{blue}{'
  
  #################
  ##  Weekly MAE ##
  #################
  
  ### Create MAE table ###
  { # Set header with model names for tables
    aggrData <- apply.weekly(hMAE, mean, na.rm= TRUE); # Create aggregate weekly MAE
    aggrDatadf <- as.data.frame(coredata(aggrData)) # Convert to data frame
    aggrDatadf <- rbind(aggrDatadf, hMAEavg) # Add Average
    
    tblHeader <- c('Week', tblHeaderModels)
    tblRowNames <- c(as.integer(format(index(aggrData),"%W")) + 1)
    
    # Start Code to add color for lowest value
    minIndex <- apply(aggrDatadf, 1, which.min) # Get vector of index of min
    # Round and then convert to strings
    aggrDatadf<- round(aggrDatadf, 2) # Round df
    aggrDatadf<- sapply(aggrDatadf, function(x) formatC(x, digits = 2,format = "f")) # Format as string
    
    # Loop and mark each min
    for(i in 1:nrow(aggrDatadf)){
      aggrDatadf[i, minIndex[i]] <- paste(strHighlight, aggrDatadf[i, minIndex[i]] ,'}', sep = "") # Color Code
    } 
    # End color coding
    
    # Create weeknumber as column, pad with NA
    aggrDatadf <- as.data.frame(cbind(c(tblRowNames, NA), aggrDatadf))
    # Save to file
    names(aggrDatadf) <- tblHeader # Set Column names
    xtab <- xtable(aggrDatadf, caption="Weekly Mean Absolute Error", align= rep('r', ncol(aggrDatadf)+1))
    print(xtab, file = "../tables/tbl_wMAE.tex", hline.after = c(-1, 0, nrow(xtab)-1, nrow(xtab)), floating=FALSE, include.rownames=FALSE, comment=FALSE, sanitize.text.function = function(x){x})
  }
  
  ### Create RMAE table ###
  { # Set header with model names for tables
    aggrData <- apply.weekly(hMAE, mean, na.rm= TRUE); # Create aggregate weekly MAE
    
    # Relative to Benchmark Model
    aggrData <- aggrData / aggrData[,colnames(aggrData) %in% c(benchMarkModel)]
    hMAREAvg <- hMAEavg / hMAEavg[names(hMAEavg) %in% c(benchMarkModel)] #For average
    
    aggrDatadf <- as.data.frame(coredata(aggrData)) # Convert to data frame
    aggrDatadf <- rbind(aggrDatadf, hMAREAvg) # Add Average
    
    tblHeader <- c('Week', tblHeaderModels)
    tblRowNames <- c(as.integer(format(index(aggrData),"%W")) + 1)
    
    # Start Code to add color for lowest value
    minIndex <- apply(aggrDatadf, 1, which.min) # Get vector of index of min
    # Round and then convert to strings
    aggrDatadf<- round(aggrDatadf, 2) # Round df
    aggrDatadf<- sapply(aggrDatadf, function(x) formatC(x, digits = 2,format = "f")) # Format as string
    
    # Loop and mark each min
    for(i in 1:nrow(aggrDatadf)){
      aggrDatadf[i, minIndex[i]] <- paste(strHighlight, aggrDatadf[i, minIndex[i]] ,'}', sep = "") # Color Code
    } 
    # End color coding
    
    # Create weeknumber as column, pad with NA
    aggrDatadf <- as.data.frame(cbind(c(tblRowNames, NA), aggrDatadf))
    # Save to file
    names(aggrDatadf) <- tblHeader # Set Column names
    xtab <- xtable(aggrDatadf, caption="Weekly Relative Mean Absolute Error", align= rep('r', ncol(aggrDatadf)+1))
    print(xtab, file = "../tables/tbl_wRMAE.tex", hline.after = c(-1, 0, nrow(xtab)-1, nrow(xtab)), floating=FALSE, include.rownames=FALSE, comment=FALSE, sanitize.text.function = function(x){x})
  }
  
  rm(aggrDatadf, aggrData, minIndex)
  
  ##################
  ##  Weekly RMSE ##
  ##################
  
  ### Create RMSE table ###
  {
    # Set header with model names for tables
    tblHeader <- c('Week', tblHeaderModels)
    
    aggrData <- apply.weekly(hMSE, mean, na.rm= TRUE) # Create aggregate weekly MSE
    aggrData <- sqrt(aggrData)
    
    # Set week as index
    tblRowNames <- c(as.integer(format(index(aggrData),"%W")) + 1)
    index(aggrData) <- tblRowNames
    
    aggrDatadf <- as.data.frame(coredata(aggrData)) # Transform to RMSE
    
    aggrDatadf <- rbind(aggrDatadf, hMSEavg) # Add Average
    
    # Start Code to add color for lowest value
    minIndex <- apply(aggrDatadf, 1, which.min) # Get vector of index of min
    # Round and then convert to strings
    aggrDatadf<- round(aggrDatadf, 2) # Round df
    aggrDatadf<- sapply(aggrDatadf, function(x) formatC(x, digits = 2,format = "f")) # Format as string
    
    # Loop and mark each minimum
    for(i in 1:nrow(aggrDatadf)){
      aggrDatadf[i, minIndex[i]] <- paste(strHighlight, aggrDatadf[i, minIndex[i]] ,'}', sep = "") # Color Code
    } 
    
    # Create weeknumber as column, pad with NA
    aggrDatadf <- as.data.frame(cbind(c(tblRowNames, NA), aggrDatadf))
    
    # Save to file
    names(aggrDatadf) <- tblHeader # Set Column names
    
    xtab <- xtable(aggrDatadf, caption="Weekly Root Mean Square Error", align= rep('r', ncol(aggrDatadf)+1))
    print(xtab, file = "../tables/tbl_wRMSE.tex", hline.after = c(-1, 0, nrow(xtab)-1, nrow(xtab)), floating=FALSE, include.rownames=FALSE, comment=FALSE, sanitize.text.function = function(x){x})
  }
  
  ### Create RRMSE table ###
  { # Set header with model names for tables
    aggrData <- apply.weekly(hMSE, mean, na.rm= TRUE); # Create aggregate weekly MSE
    aggrData <- sqrt(aggrData)
    # Relative to Benchmark Model
    aggrData <- aggrData / aggrData[,colnames(aggrData) %in% c(benchMarkModel)]
    hMSREAvg <- hMSEavg / hMSEavg[names(hMSEavg) %in% c(benchMarkModel)] #For average
    
    aggrDatadf <- as.data.frame(coredata(aggrData)) # Convert to data frame
    aggrDatadf <- rbind(aggrDatadf, hMSREAvg) # Add Average
    
    tblHeader <- c('Week', tblHeaderModels)
    tblRowNames <- c(as.integer(format(index(aggrData),"%W")) + 1)
    
    # Start Code to add color for lowest value
    minIndex <- apply(aggrDatadf, 1, which.min) # Get vector of index of min
    # Round and then convert to strings
    aggrDatadf<- round(aggrDatadf, 2) # Round df
    aggrDatadf<- sapply(aggrDatadf, function(x) formatC(x, digits = 2,format = "f")) # Format as string
    
    # Loop and mark each min
    for(i in 1:nrow(aggrDatadf)){
      aggrDatadf[i, minIndex[i]] <- paste(strHighlight, aggrDatadf[i, minIndex[i]] ,'}', sep = "") # Color Code
    } 
    # End color coding
    
    # Create weeknumber as column, pad with NA
    aggrDatadf <- as.data.frame(cbind(c(tblRowNames, NA), aggrDatadf))
    # Save to file
    names(aggrDatadf) <- tblHeader # Set Column names
    xtab <- xtable(aggrDatadf, caption="Weekly Relative Root Mean Squared Error", align= rep('r', ncol(aggrDatadf)+1))
    print(xtab, file = "../tables/tbl_wRRMSE.tex", hline.after = c(-1, 0, nrow(xtab)-1, nrow(xtab)), floating=FALSE, include.rownames=FALSE, comment=FALSE, sanitize.text.function = function(x){x})
  }
  
  rm(minIndex)
  rm(tblHeader, tblRowNames, aggrData, aggrDatadf)
  
  ##############
  ##  Done!!  ##  
  ##############