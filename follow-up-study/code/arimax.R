source("init.R")

########################
##  Model 2 - ARIMAX  ##
########################
library(forecast) #ARIMA

# Load linear forecasts from train
rawdata1 <- read.csv(file='../forecasts/train/model1_linear.csv')
timestamp1 <- strptime(rawdata1[,1], format="%Y-%m-%d %H:%M:%S")
dat1 <- xts(rawdata1[,-1], timestamp1, frequency=24)

# Load linear forecasts from test
rawdata2 <- read.csv(file='../forecasts/test/model1_linear.csv')
timestamp2 <- strptime(rawdata2[,1], format="%Y-%m-%d %H:%M:%S")
dat2 <- xts(rawdata2[,-1], timestamp2, frequency=24)

#Combine and add actual + error + sqerror
dat <- rbind(dat1,dat2)
dat<-merge(dat,d.y)
dat<-cbind(dat, dat[,1]-dat[,2], (dat[,1]-dat[,2])^2)
colnames(dat)<-c('linear','actual','error','sqerror')
linear<-dat

#take errors and model
dat <- linear[,3]
usr.roll <- 1 # Reestimate parameters every usr.roll days
usr.estimationDays <- 365 # Use last usr.estimationDays in estimation (or as far as the train.start parameter allows)
model.fit<-list()

i.iterations <- ceiling(as.double(ceiling(difftime(test.end, test.start))) / usr.roll)
reestimationDates <- as.Date(test.start) + seq(from=0, by=usr.roll, length.out= i.iterations)

# Initialize container
container.forecast <- matrix(NA,nrow = test.length)

# Loop from "2013-12-31" to "2014-12-27". First period has data until 2013-12-31 23:00
for (i in 0:(test.length/24-1)){
  loopDate <- as.Date(test.start) + i
  # Select estimation data. Starts from max(train.start, estimation data - usr.estimationDays) and ends at test period
  d.est   <- window(dat, start = max(train.start, as.POSIXct(as.Date(loopDate)-usr.estimationDays)),
                    end = as.POSIXct(loopDate) - as.difftime(1, unit="hours"))
  # Split on hour
  d.est.split <- split(d.est, as.numeric(format(time(d.est), "%H")))
  #d.fcast.split  <- split(d.fcast, d.fcast$Hour)
  
  for (j in 1:24){
    if(j%%24==1){ cat(format(loopDate,'%Y-%m-%d'), ' H' ,j,'\n')}
    #Execute if reestimationDate
    if (loopDate %in% reestimationDates){
      #Reestimate model
      model.fit[[j]] <- Arima(as.matrix(d.est.split[[j]]), order=c(1,0,1), method='ML') #Try this
    } else {
      #Update with new data
      print('pass')
      model.fit[[j]] <- Arima(as.matrix(d.est.split[[j]]), model=model.fit[[j]])
    }
    #Forecast
    model.fcast <- forecast.Arima(model.fit[[j]], h=1)$mean # Forecast    
    # save forecast
    container.forecast[(24*i+j),] <- model.fcast
  }
}
# Set forecast name
colnames(container.forecast)[1] <- 'ARIMAXStep2'
# Collect forecast in data frame
container.xts <- xts(container.forecast,order.by = index(window(dat, start = test.start, end = test.end)))
write.csv(as.data.frame(container.xts), file='../forecasts/test/ARIMAX-correction.csv')

# rm(model.fit, model.fcast, i.iterations, i, reestimationDates, container.forecast, d.est)

############################
# Create Combined Forecast #
############################

rawdata <- read.csv(file='../forecasts/test/model1_linear.csv')
timestamp <- strptime(rawdata[,1], format="%Y-%m-%d %H:%M")

rawdata2 <- read.csv(file='../forecasts/test/ARIMAX-correction.csv')
timestamp2 <- strptime(rawdata2[,1], format="%Y-%m-%d %H:%M")

# Create full data set
d.step1 <- xts(rawdata[,-1], timestamp)
d.step2corr <- xts(rawdata2[,-1], timestamp2)

d.step2 <- merge(d.step1, d.step2corr)
d.step2 <- merge(d.step2, d.y)
d.step2 <- window(d.step2, start = test.start, end = test.end)
d.step2 <- cbind(d.step2, d.step2[,1]-d.step2[,2])
d.step2 <- cbind(d.step2, d.step2[,4]-d.step2[,3], (d.step2[,4]-d.step2[,3])^2)
colnames(d.step2) <- c('step1','step2corr','actual','step2','step2error','step2sqerror')

write.csv(as.data.frame(d.step2[,4]), file='../forecasts/test/ARIMAX.csv')

test<-cbind(d.step2, (d.step2[,1]-d.step2[,3])^2)
colnames(test) <- c(colnames(d.step2),'MSE.Step1')
testresult<-apply(test[,6:7],2,function(x){sqrt(mean(x))}) #Calculate RMSE
print(testresult)

################
## End ARIMAX ##
################