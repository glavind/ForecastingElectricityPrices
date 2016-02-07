# Introduction
This repo holds the follow-up study to my master thesis, as well as updated tso data, forecasts, and code snippets.

### code/init.R
Loads data and sets settings

### code/[arimax.R, avnnet.R, jonsson-step-one.R, jonsson-step-two.R, linear.R, svmLinear.R, svmRadial.R]
Runs relevant model.

### code/create-tables.R
Creates weekly aggregated RMSE, MAE, Relative RMSE, and relative MAE. 

### data/tso-data-updated.csv
In Jan 2016, I updated the data with an additional year so I could test whether the results from my thesis are still valid. 

Originally I had planned to use entsoe's transparency website but, surprisingly, it had a lot of missing values (and a horrible description of data). Therefore I defaulted to the tso data sources described in the thesis. The consumption forecast was again missing in around 5% of observations, while both consumption forecasts and actuals where missing in 0.5% of observations. For these missing values a combination of LOCF, previous day and linear was used for imputing missing values.

If you really want to battle with entsoe's transparency site here are a few tips: 

1. When logged in you can download a whole year of data as either xml, csv or xlsx (in an annoying format). 
2. When looking at a day, the D forecast is for the current day, D+1 is the forecast for next day, and D+2 is for the day after tomorrow. It is not different forecasts for the current day. 
3. All forecasts are the same. So todays D forecast is the same value as yesterdays D+1 forecast (I guess all forecasts are the ones made day-ahead, but they could also be updated during the delivery day -- the docs are not clear on this point). 
4. Entso's support staff is a redeeming feature. They are very competent and quickly answer data questions.

I thought it would be a breeze to update data, but it turned out that this is still quite an involved task.. Hopefully entsoe will improve their act and eventually make quality data easily accessible..