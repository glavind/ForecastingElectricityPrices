
# Model: Two-Step Local linear approximation with Holt-Winters for errors.
# Features:
# Filter extreme values in estimation (<0 DKK, >800 DKK)
# Daily updating parameters
# Forgetness factor
# Influence limiter

# TODO: Optimize updatings procedure.

##  Initialization  ##
library(xts)
library(akima)
library(compiler)
enableJIT(2)

ptmS <- Sys.time()

setwd("C:/Users/Glavind/Dropbox/_Thesis/a_code/")
#setwd("/home/rstudio/")
Sys.setenv(TZ='UTC')

d <- read.csv('data/tso-data.csv')
timestamp <- strptime(d[,1], format="%Y-%m-%d %H:%M")

dat <- xts(d[,2:ncol(d)], timestamp)
rm(d, timestamp)

burnin.start <-  as.POSIXct("2012-11-01 00:00")
burnin.end <-    as.POSIXct("2012-12-12 23:00")

train.start <-  as.POSIXct("2012-12-13 00:00")
train.end <-    as.POSIXct("2013-12-31 23:00")

test.start <-   as.POSIXct("2014-01-01 00:00")
test.end <-     as.POSIXct("2014-12-28 23:00")

savedates <- as.POSIXct("2014-07-15")
savedcurves <- list()

# Fitting points:
u1.fitpoints = 24
u2.fitpoints = 24

# Dummy for using train only else use test data
isTrain <- FALSE 

###  PREPROCESS  ###

# Make a copy of data
# PRI_DE, CON_DE, PRO_DE_WND + PRO_DE_SPV, PRI_DE
#a <- cbind(dat[,1], dat[,4]/1000, (dat[,6] + dat[,7])/1000, dat[,1])
a <- cbind(dat[,1], dat[,4]/1000, (dat[,6] + dat[,7])/1000, dat[,1])
colnames(a) <- c("PRI_DE","CON_DE","PRO_DE_RES", "Realized Prc")

## Perform censoring of PRI_DE ##
dMinPrc <- 0    # replaces 162 obs
dMaxPrc <- 110  # replaces 8 obs
# Censors prices: 106 total in train, 64 in test
a[a[,1] < dMinPrc, 1] <- dMinPrc       # Replace values lower than limit
a[a[,1] > dMaxPrc, 1] <- dMaxPrc       # Replace values higher than limit

## Perform scaling of explanatory variables ##
aT <- a[seq(burnin.start, train.end, by=60)] # Pull burn+train from dataset
aMin <- sapply(aT,FUN = min)     # Save train sample Min
aMax <- sapply(aT,FUN = max)     # Save train sample Max
# Scale explanatory variables by min/max of burn+test sample
a[, 2] <- -1 + 2*((a[, 2] - aMin[2])/(aMax[2] - aMin[2])) # Scale variables [-1:1]
a[, 3] <- -1 + 2*((a[, 3] - aMin[3])/(aMax[3] - aMin[3])) # Scale variables [-1:1]
rm(aT, dMinPrc, dMaxPrc, dat)

### END PREPROCESS ###


### INITIALIZE MODEL ###

## Define Functions ##
triCube <- function(x){   as.double(ifelse( x>=0 & x<1, (1-x^3)^3, 0)) }  # Tricube function
huber <- function(x, t){  as.double(sign(x)*min(abs(x),t)) }           # Huber function
delta <- function(x, t){  as.double(ifelse(abs(x)<t, 1, 0)) }          # Delta of huber function
euc.dist <- function(x1, x2){ as.double(sqrt(sum((x1 - x2)^2))) }    # Euclidian distance function
toPoly <- function(u){ c(1, u[1], u[2], u[1]^2, u[2]^2, u[1]*u[2])} # Convert 2d point to 6d polynomial

# Calculate Bandwidth in a fitting point
calc.bw <- function(u, gamma){
  distances <- apply(xt, 1, FUN=function(x){euc.dist(u,x)}) #Requires xt defined below
  as.double(quantile(distances, probs = c(gamma)))  
}

## Set fitpoints ##
u1 = seq(from = -1, to = 1, length.out = u1.fitpoints)
u2 = seq(from = -1, to = 1, length.out = u2.fitpoints)

## Expand fitpoints for use in Akima interpolation ##
xy <- expand.grid(u1,u2)

## Intialize matrixes of lists holding variables ##
p = matrix(list(), nrow = u1.fitpoints, ncol=u2.fitpoints) # Parameter vector (6*1)
R = matrix(list(), nrow = u1.fitpoints, ncol=u2.fitpoints) # R matrix (6*6)
u = matrix(list(), nrow = u1.fitpoints, ncol=u2.fitpoints) # u fitpoint values (2*1)
bw = matrix(list(), nrow = u1.fitpoints, ncol=u2.fitpoints) # bandwidth in each fitpoint (1*1)
FC = matrix(list(), nrow = u1.fitpoints, ncol=u2.fitpoints) # Forecast at each U point (1*1)

## Initial values for parameters ##
pInit = c(34, 0, 0, 0, 0, 0)
RInit = diag(6)*10^(-6)

## Load data as coredata ##
if(isTrain){
  # Train period
  yt <- coredata(a[seq(burnin.start, train.end, by=60),1]) #Pruned y
  xt <- coredata(a[seq(burnin.start, train.end, by=60),2:3]) #Explanatory vars
  rt <- coredata(a[seq(burnin.start, train.end, by=60),4]) #Realized y
} else {
  #Test period
  yt <- coredata(a[seq(burnin.start, test.end, by=60),1]) #Pruned y
  xt <- coredata(a[seq(burnin.start, test.end, by=60),2:3]) #Explanatory vars
  rt <- coredata(a[seq(burnin.start, test.end, by=60),4]) #Realized y
}

### END INITIALIZE ###

### Jonsson et al. MODEL ###
jonsson.model <- function(param, printInfo){
  # Set tune parameters
  gamma = param[1]
  lambda = param[2]
  tau   = param[3]
#   gamma = 0.8
#   lambda = 0.99
#   tau = 25
#   debug=TRUE
  
  if(printInfo){
    cat("\nGamma: ", gamma, sep="")
    cat("\nLambda :", lambda, sep="")
    cat("\nTau: ", tau, sep="")
    cat('\nStartAt: ', format.POSIXct(Sys.time(), format = "%y-%m-%d %H:%M:%S", tz="Europe/Berlin"), sep="")
  }
  
  ## INITIALIZE VARS ##
  
  # dataframes to hold forecast/actual
  forc <- data.frame(forecast=matrix(NA, nrow = nrow(xt), ncol = 1))
  actual <- data.frame(actual=matrix(NA, nrow = nrow(xt), ncol = 1))
  
  # Fill initial values to all fitting points
  for(cVar2 in 1:u2.fitpoints){
    for(cVar1 in 1:u1.fitpoints){
      p[[cVar1,cVar2]] <- pInit
      R[[cVar1,cVar2]] <- RInit
      u[[cVar1,cVar2]] <- c(u1[cVar1], u2[cVar2])
      bw[[cVar1,cVar2]] <- calc.bw(u[[cVar1,cVar2]], gamma)
    }
  }
  ## END INITIALIZE MODEL ##
  
  ## START MODEL ##
  
  for(i in 0:(nrow(yt)/24 - 1)){ # Loop over each day
    
    # Report status
    if(printInfo){
      if(i%%100==0){
        cat("\n", formatC(i+1,width=3,format="d", flag="0"), "/", nrow(yt)/24,"..", sep="")
        ptmD <- Sys.time()
      } else if (i%%10==0){
        cat("|", sep="")
      } else {
        cat("-", sep="")
      }
    }
     
    # Create grid of forecasts
    for(cVar2 in 1:u2.fitpoints){
      for(cVar1 in 1:u1.fitpoints){
        FC[[cVar1,cVar2]] <- toPoly(u[[cVar1,cVar2]]) %*% p[[cVar1,cVar2]]
      }
    }
    
    # create vector form of forecast grid for use in Akima.
    zz <- unlist(FC)
    
    # Create 24 hourly forecasts by interpolation from forecast grid
    for(j in 1:24){
      #Extract explanatory variables
      point <- xt[i*24 + j, ]
      
      #Do interpolation using akima package
      singleForecast <- interp.old(xy[,1], xy[,2], zz, xo = point[1], yo= point[2], ncp=2, extrap=TRUE)
      
      # Save forecast
      forc[24*i + j,] <- c(singleForecast$z[1])
      # Save actual
      actual[24*i + j,] <- rt[i*24 + j, ]
    }
    
    # On request: save information for later plotting of 3d forecast surface
    if((burnin.start+i*3600*24) %in% savedates){ 
      print('Saved Curve')
      realU1axis <- (u1 + 1)*(aMax[2] - aMin[2])/2 + aMin[2]
      realU2axis <- (u2 + 1)*(aMax[3] - aMin[3])/2 + aMin[3]
      realU1 <- (xt[(24*i+1):(24*i+24), 1] + 1)*(aMax[2] - aMin[2])/2 + aMin[2]
      realU2 <- (xt[(24*i+1):(24*i+24), 2] + 1)*(aMax[3] - aMin[3])/2 + aMin[3]
      # Save date, the vectors of u1 and u2 used in fitting points, the fitted surface values and the u1, u2, y and y_hat of the 24 hours.
      savedcurves <<- cbind(savedcurves, list(as.POSIXct(burnin.start+i*3600*24), 
                                             realU1axis, 
                                             realU2axis, 
                                             FC, 
                                             cbind(u1 = realU1,
                                                   u2 = realU2,
                                                   actual=rt[(24*i+1):(24*i+24),], 
                                                   forecast=forc[(24*i+1):(24*i+24),])
                                            )
                          )
      save(savedcurves, file = 'savedcurve.rda')
    }
    ## End forecast ##
    
    ## Start Update of parameters ##
    for(j in 1:24){
      #Extract explanatory variables
      point <- as.vector(xt[i*24 + j, ])
      
      # Is observation in burn sample -> TRUE/FALSE
      isBurn <- ifelse((24*i + j)<=as.double((train.start-burnin.start)*24), TRUE, FALSE)
      
      #Visit each fitting point and update parameters
      for(cVar2 in 1:u2.fitpoints){
        for(cVar1 in 1:u1.fitpoints){
          # Use censored y to calculate error
          eps <- yt[24*i + j] - toPoly(point) %*% p[[cVar1,cVar2]]
          
          # Calculate weight assigned to observation in fitting point u
          weight <- triCube(euc.dist(u[[cVar1,cVar2]], point) / bw[[cVar1,cVar2]])
          
          # Calculate lambdaEff [Eq. 12] - but allow larger updates in burn
          lambdaEff <- (1 - (1-lambda) * weight * ifelse(isBurn, 1, delta(eps, tau)))
          
          # Calculate R matrix - allow larger updates in burn
          R[[cVar1,cVar2]] <- lambdaEff * R[[cVar1,cVar2]] + weight * ifelse(isBurn, 1, delta(eps, tau)) * toPoly(point) %*% t(toPoly(point))
          
          #Dont update p for the first 100 observations - can give problem with oscillating p values.
          if((24*i + j)>100){       
            p[[cVar1,cVar2]] <- p[[cVar1,cVar2]] + weight * huber(eps, tau) * as.vector(solve(R[[cVar1,cVar2]]) %*% toPoly(point))
          }
        }
      }#next fitting point
    }#next hour
    ## End Update parameters ##  
  }#next day
  
  ## Handle results ##
  # Write forecast and actual series 
  if(isTrain){
    timestamp <- as.POSIXct(index(a[seq(burnin.start, train.end, by=60)]), format="%y-%m-%d %H:%M") #if train
    xtsForecast <- xts(cbind(forc,actual), timestamp)[seq(train.start, train.end, by=60)]
  } else {
    #Save train performance
    timestamp <- as.POSIXct(index(a[seq(burnin.start, test.end, by=60)]), format="%y-%m-%d %H:%M") #if train
    xtsForecast <- xts(cbind(forc,actual), timestamp)[seq(burnin.start, train.end, by=60)]
    write.csv(as.data.frame(xtsForecast), file='data/forecasts/train/jonsson-step1.csv')
    
    #Save test performance
    timestamp <- as.POSIXct(index(a[seq(burnin.start, test.end, by=60)]), format="%y-%m-%d %H:%M") #if train
    xtsForecast <- xts(cbind(forc,actual), timestamp)[seq(test.start, test.end, by=60)]
    write.csv(as.data.frame(xtsForecast), file='data/forecasts/test/jonsson-step1.csv')
    
    #Continue
    timestamp <- as.POSIXct(index(a[seq(burnin.start, test.end, by=60)]), format="%y-%m-%d %H:%M") #if train
    xtsForecast <- xts(cbind(forc,actual), timestamp)[seq(train.start, test.end, by=60)]
  }

  # Write above results including error measures
  summaryForecast <- cbind(xtsForecast, abs(xtsForecast[,2] - xtsForecast[,1]), (xtsForecast[,2] - xtsForecast[,1])^2)
  names(summaryForecast) <- c('forecast','actual','abs.error','sq.error')
  summaryForecast['2012-12-24/25',3:4] <- NA
  write.csv(as.data.frame(summaryForecast), file='data/forecasts/jonsson-standard-forecasts-step1.csv')

  # Format output to function
  returnval <- sapply(summaryForecast, FUN = mean, na.rm=TRUE)[3:4]
  returnval[2] <- sqrt(returnval[2])
  names(returnval) <- c('MAE','RMSE')

  return(returnval)
  #return(returnval[2])
  ## END MODEL ##
}

# Original values
# gamma   = 0.8529
# lambda  = 0.9877
# tau     = 55.67/7.45 = 7.47

gauss <- function(x){(1 + exp(-x))^(-1)}
gauss.inv <- function(x){-log((1/x)-1)}
bfgs.transform <- function(x){1/2*(1 + gauss(x))}
bfgs.transform.inv <- function(x){gauss.inv(x*2-1)}

jonsson.model.bfgs <- function(par){
  trpar <- matrix(nrow = 3, ncol = 1)
  trpar[1] <- bfgs.transform(par[1])
  trpar[2] <- bfgs.transform(par[2])
  trpar[3] <- exp(par[3])
  jonsson.model(trpar, TRUE)
}


# The BFGS method does converge when using a special parameterization 
# such that par[1:2] can fall in region [0.5,1] and par[3] can fall in region [0, +inf]
# bfgs.optim <- optim(par=c(0.8750721,3.680101, 2.0109), 
#               fn=jonsson.model.bfgs, 
#               method="BFGS", 
#               control=list(trace=TRUE))

# Convergence in 13 function calls, 6 gradient calls...
#           Gamma     Lambda    Tau          
# Start Val 0.8750721 3.680101  2.0109  
# equals... 0.8529    0.9877    7.47

# Output:   1.025308  3.247043 2.545327
# equals... 0.8680026 0.9812834 12.7474

# Final RMSE: 8.543875
  
  gamma = c(0.8680026)
  lambda = c(0.9812834)
  tau = c(12.7474)
  
  # Expand tune parameters to a grid
  glt <- expand.grid(gamma, lambda, tau)
  names(glt) <- c("gamma", "lambda", "tau")
  # Create container holding results
  container <- cbind(glt, matrix(nrow = nrow(glt), ncol=3))
  names(container) <- c("gamma", "lambda", "tau", "MAE", "RMSE","Time Spent")
  
  # Loop across all tune combinations
  for(i in 1:nrow(glt)){
    # Write model no.
    cat("\n","Model ", i, "/", nrow(glt), " (grid: ", u1.fitpoints, "x", u1.fitpoints, ", gamma: ", glt[i,1], ", lambda: ", glt[i,2], ", tau: ", glt[i,3], ")", sep="")
    
    # Save time
    ptmA <- Sys.time()
    
    # Do model
    container[i,4:5] <- jonsson.model(c(glt[i,1], glt[i,2], glt[i,3]), printInfo=TRUE)
    
    # Write runtime
    container[i,6] <- as.numeric(Sys.time() - ptmA, units='secs')
    cat("\n", "Run Time: ", container[i,6], '\n',sep="")
    write.csv2(as.data.frame(container), file='output.csv')
    #print(container)
  }
  
  container
  
  cat("\n", "Run Time: ", as.numeric(Sys.time() - ptmS, units='mins'), ' minutes',sep="")
