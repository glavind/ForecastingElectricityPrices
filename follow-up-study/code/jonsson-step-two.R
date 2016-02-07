#####################
#  Jonsson Step-2   #
#####################
library(forecast) #ARIMA

#Load step 1 forecasts
rawdata <- read.csv(file='../forecasts/jonsson-standard-forecasts-step1.csv')
timestamp <- strptime(rawdata[,1], format="%Y-%m-%d %H:%M")
# Create full data set
d.jonsson <- xts(rawdata[,-1], timestamp, frequency=24)
d.jonsson <- cbind(d.jonsson, d.jonsson[,1]-d.jonsson[,2])
colnames(d.jonsson)[5]<-'error'
#Select errors
d.jonsson.err <- d.jonsson[,5]
rm(timestamp, rawdata) # Remove unneded objects

##########

dat <- d.jonsson.err
usr.roll <- 1 # Reestimate parameters every usr.roll days
usr.estimationDays <- 365 # Use last usr.estimationDays in estimation (or as far as the train.start parameter allows)
model.fit<-list()

i.iterations <- ceiling(as.double(ceiling(difftime(test.end, test.start))) / usr.roll)
reestimationDates <- as.Date(test.start) + seq(from=0, by=usr.roll, length.out= i.iterations)

# Initialize container
container.forecast <- matrix(NA, nrow = test.length)

# Loop from "2013-12-31" to "2014-12-27". First period has data until 2013-12-31 23:00
for (i in 0:(test.length/24 - 1)){
  loopDate <- as.Date(test.start) + i
  # Select estimation data. Starts from max(train.start, estimation data - usr.estimationDays) and ends at the previous day
  d.est   <- window(dat, start = max(train.start, as.POSIXct(as.Date(loopDate) - usr.estimationDays)),
                    end = as.POSIXct(loopDate) - as.difftime(1, unit="hours"))
  # Split on hour
  d.est.split <- split(d.est, as.numeric(format(time(d.est), "%H")))

  for (j in 1:24){
    if(j%%24==1){ cat(format(loopDate,'%Y-%m-%d'), ' H' ,j,'\n')} # print progress
    #Execute if reestimationDate
    if (loopDate %in% reestimationDates){
      #Reestimate model
      model.fit[[j]] <- Arima(as.matrix(d.est.split[[j]]), order=c(1,0,1)) #Try this
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
colnames(container.forecast)[1] <- 'JonssonStep2'
# Collect forecast in data frame
container.xts <- xts(container.forecast,order.by = index(window(dat, start = test.start, end = test.end)))
write.csv(as.data.frame(container.xts), file='../forecasts/test/jonsson-step2-corr.csv')

rm(model.fit, model.fcast, i.iterations, i, reestimationDates, container.forecast, d.est)

############################
# Create Combined Forecast #
############################

rawdata <- read.csv(file='../forecasts/jonsson-standard-forecasts-step1.csv')
timestamp <- strptime(rawdata[,1], format="%Y-%m-%d %H:%M")

rawdata2 <- read.csv(file='../forecasts/test/jonsson-step2-corr.csv')[,2]

# Create full data set
d.jonsson2step <- xts(rawdata[,-1], timestamp)
d.jonsson2step <- window(d.jonsson2step, start = test.start, end = test.end)[,1:2]
d.jonsson2step <- cbind(d.jonsson2step, rawdata2)
d.jonsson2step <- cbind(d.jonsson2step, d.jonsson2step[,1]-d.jonsson2step[,3])
d.jonsson2step <- cbind(d.jonsson2step, d.jonsson2step[,4]-d.jonsson2step[,2])
colnames(d.jonsson2step) <- c('Step1','actual','step2corr','Step2','step2error')

# Save forecasts
write.csv(as.data.frame(d.jonsson2step[,4]), file='../forecasts/test/jonsson-step2.csv')

# calculate and print RMSE
test<-cbind(d.jonsson2step,d.jonsson2step[,5]^2,(d.jonsson2step[,1]-d.jonsson2step[,2])^2)
colnames(test) <- c(colnames(d.jonsson2step),'MSE.Step2','MSE.Step1')
testresult<-apply(test[,6:7],2,function(x){sqrt(mean(x))}) #Calculate RMSE
print(testresult)

rm()
###############################
######## End Jonsson ##########
###############################