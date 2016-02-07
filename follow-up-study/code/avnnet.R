source("init.R")

#######################
##  Model 4 - avNNet ##
#######################
library(caret) #General ML models
set.seed(825)
ptmA <- Sys.time()

usr.roll <- 7 # Reestimate model every usr.roll days
usr.estimationDays <- 365 # Use last usr.estimationDays in estimation (or as far as the train.start parameter allows)

i.iterations <- ceiling(as.double(ceiling(difftime(test.end, test.start))) / usr.roll)
reestimationPoints <- as.Date(test.start) + seq(from=0, by=usr.roll, length.out= i.iterations) #;View(reestimationPoints)

cat("iterations:", i.iterations)

# Initialize container
container.forecast <- NULL

for (i in 1:i.iterations){  
  # subset data
  splitpoint  <- reestimationPoints[i]
  cat('\n', i,'/',i.iterations,'\n')
  # Select estimation data. Starts from max(train.start, estimation data - usr.estimationDays) and ends at test period
  d.est       <- window(d.xy, start = max(train.start, as.POSIXct(as.Date(splitpoint)-usr.estimationDays)), end = as.POSIXct(splitpoint) - as.difftime(1, unit="hours"))
  # Select train data. Starts from last estimation data and ends at start + usr.roll days.
  d.fcast     <- window(d.xy, start = as.POSIXct(splitpoint), end = min(as.POSIXct(as.Date(splitpoint) + usr.roll) - as.difftime(1, unit="hours"), test.end))
  
  # estimate model
  model.fit <- train(PRI_DE ~ CON_DE + CON_FR + PRO_DE_WND + PRO_DE_SPV + factor(Month) + factor(Hour) + factor(Weekday), 
                     data = d.est,
                     method = 'avNNet',
                     trControl = trainControl(method = 'none', verboseIter= TRUE),
                     tuneGrid = data.frame(size=37, decay=0.1, bag=TRUE),
                     MaxNWts = 4000,
                     repeats=5,
                     linout=TRUE,
                     preProc = c('center','scale'))
  # forecast
  model.fcast <- predict(model.fit, newdata=d.fcast) # Forecast    
  # save forecast
  container.forecast <- rbind(container.forecast, as.data.frame(model.fcast))
}
# Set forecast name
colnames(container.forecast)[1] <- 'ann'
rownames(container.forecast)<- seq(test.start, test.end, by=3600)
# Collect forecast in data frame
write.csv(as.data.frame(container.forecast), file='../forecasts/test/avnnet.csv')
cat("\n", "Time spent: ", Sys.time() - ptmA, sep="")

################
## End ARIMAX ##
################