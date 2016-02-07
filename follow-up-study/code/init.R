# Purpose
#   Load dataset and dates. Set settings.
#
# Outputs 
#   [xts] d.y -- Contains epex price variable in train and test period.
#   [xts] d.x -- Contains explanatory variables in train and test period.
#   [xts] d.xy -- Contains all variables in train and test period (epex price in first column).
#   [xts] d.exaa -- Contains exaa price in train and test period.
#   [integer] train.length -- Number of observations in training partition.
#   [integer] test.length -- Number of observations in testing partition.
#   [posixct] train.start, train.end, test.start, and test.end.
#
# Sets
#   working directory
#   default tz to UTC


######################
##  Initialization  ##
######################

library(xts)

setwd("C:/Users/Glavind/git/ForecastingElectricityPrices/code")
Sys.setenv(TZ='UTC')

# Set splitpoints
train.start <-  as.POSIXct("2012-11-01 00:00", tz="UTC")
train.end <-    as.POSIXct("2014-12-31 23:00", tz="UTC")
test.start <-   as.POSIXct("2015-01-01 00:00", tz="UTC")
test.end <-     as.POSIXct("2015-12-31 23:00", tz="UTC")

################
##  Load Data ##
################

# Read data and create timedate index
rawdata <- read.csv('../data/tso-data-updated.csv')
timestamp <- as.POSIXct(strptime(rawdata[,1], format="%d/%m/%Y %H:%M"))

# Create xts object
d.all <- xts(rawdata[,-1], timestamp)
rm(timestamp, rawdata) # Remove unneeded objects

# Rescale to GW
d.all[,3:6] <- d.all[,3:6]/1000

# Create Hour, Weekday, Week and Month variables
d.all$Month <-    as.numeric(format(time(d.all), "%m"))
d.all$Hour <-     as.numeric(format(time(d.all), "%H"))
d.all$Weekday <-  as.numeric(format(time(d.all), "%w")) # 0-6, Sunday = 0
d.all$Week <-     as.numeric(format(time(d.all), "%W")) # First monday is day one of week one

# Create dummies
d.all$Peak <- ifelse(d.all$Hour>=8&d.all$Hour<=19,1,0) # Peak=1, Offpeak=0
d.all$Weekend <- ifelse(d.all$Weekday==6|d.all$Weekday==0, 1, 0) # Weekend=1, Weekday=0
d.all$Summer <- ifelse(d.all$Month>=5&d.all$Month<=8, 1, 0) # Summer=1 (1.may - 1. sept), Not summer=0 

####################
##  Split Series  ##
####################

train.length <- length(seq(train.start, train.end, "hours"))
test.length <- length(seq(test.start, test.end, "hours"))

# Create dataset with factors (not coded as factors --> do it in equations) (10 + 1)
d.y <-  d.all[, 1, drop = FALSE]

#Filter data
d.x <-  d.all[, colnames(d.all) %in% c('CON_DE', 'CON_FR', 'PRO_DE_WND', 'PRO_DE_SPV', 'Month', 'Hour', 'Weekday', 'Summer', 'Weekend', 'Peak'), drop = FALSE]
#Create one day lagged price
d.x <- cbind(d.x, lag(d.y[,1],24))
colnames(d.x)[ncol(d.x)] <- 'Lag_D_PRI_DE'
#Create one week lagged price
d.x <- cbind(d.x, lag(d.y[,1],168))
colnames(d.x)[ncol(d.x)] <- 'Lag_W_PRI_DE'
#Final dataset
d.xy <- cbind(d.y, d.x)

d.exaa <-  d.all[, colnames(d.all) %in% c('PRI_AT'), drop = FALSE]

rm(d.all)
# # Create Splits
# # Train split
# d.train = window(d.xy, start = train.start, end = train.end)
# y.train = window(d.y, start = train.start, end = train.end)
# x.train = window(d.x, start = train.start, end = train.end)
# 
# # Test split
# d.test = window(d.xy, start = test.start, end = test.end)
# y.test = window(d.y, start = test.start, end = test.end)
# x.test = window(d.x, start = test.start, end = test.end)