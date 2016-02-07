source("init.R")

#################################
#  Model 1: Linear, rolling     #
#################################
#### Train (no rolling estimation) ####

d.train = window(d.xy, start = train.start, end = train.end)  

  #BEST MODEL (AIC)
  deg<-3
  fit <- lm(PRI_DE ~ poly(I(CON_DE-PRO_DE_WND-PRO_DE_SPV),deg)*Weekend*Peak*Summer -Weekend -Peak -Summer + poly(CON_FR, deg) + factor(Month) + factor(Hour) + factor(Weekday), data=d.train) # Estimate
  
  # Train
  fcast <- predict(fit) # Forecast
  
  # Collect forecast in data frame
  write.csv(as.data.frame(fcast), file='../forecasts/train/linear.csv')

#### Test ####

usr.roll <- 1 # Reestimate model every usr.roll days
usr.estimationDays <- 365 # Use last usr.estimationDays in estimation (or as far as the train.start parameter allows)
deg<-3

i.iterations <- ceiling(as.double(ceiling(difftime(test.end, test.start))) / usr.roll)
reestimationPoints <- as.Date(test.start) + seq(from=0, by=usr.roll, length.out= i.iterations) #;View(reestimationPoints)

cat("iterations:", i.iterations)

# Initialize container
container.forecast <- NULL

for (i in 1:i.iterations){  
  # subset data
  splitpoint  <- reestimationPoints[i]
  # Select estimation data. Starts from max(train.start, estimation data - usr.estimationDays) and ends at test period
  d.est       <- window(d.xy, start = max(train.start, as.POSIXct(as.Date(splitpoint)-usr.estimationDays)), end = as.POSIXct(splitpoint) - as.difftime(1, unit="hours"))
  # Select train data. Starts from last estimation data and ends at start + usr.roll days.
  d.fcast     <- window(d.xy, start = as.POSIXct(splitpoint), end = min(as.POSIXct(as.Date(splitpoint) + usr.roll) - as.difftime(1, unit="hours"), test.end))
  
  # estimate model
  model.fit   <- lm(PRI_DE ~ poly(I(CON_DE-PRO_DE_WND-PRO_DE_SPV),deg)*Weekend*Peak*Summer -Weekend -Peak -Summer + poly(CON_FR, deg) + factor(Month) + factor(Hour) + factor(Weekday), data=d.est) # Estimate  
  # forecast
  model.fcast <- predict(model.fit, newdata=d.fcast) # Forecast    
  # save forecast
  container.forecast <- rbind(container.forecast, as.data.frame(model.fcast))
}
# Set forecast name
colnames(container.forecast)[1] <- 'Linear'
# Collect forecast in data frame
write.csv(container.forecast, file='../forecasts/test/linear.csv')

rm(model.fit, model.fcast, splitpoint, i.iterations, i, reestimationPoints, container.forecast, d.est, d.fcast)

################
## End linear ##
################