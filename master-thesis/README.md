# Introduction
This repo holds data and forecasts from my master thesis on forecasting epex spot electricity prices ("Evaluating Forecasts of Volatile Electricity Prices made by Competing Models").

### thesis.pdf
The main goals of the thesis are 

1. provide an introduction to the german electricity market with a focus on how fundamentals affect prices through supply/demand effects. 
2. give an overview of available data and data sources.
3. compare different machine learning and statistical methods for forecasting electricity prices.

### data/tso-data-used-in-thesis.csv
The data included is publicly available data (TSO forecasts of fundamentals, EPEX and EXAA prices).

The tso data covers forecast wind production, forecast solar production, and forecast consumption taken from the TSO's websites. Prices for the german EPEX day-ahead auction were taken from the Danish TSO Energinet's website. Prices for the Austrian exchange EXAA were taken from the EXAA website. All data is for the period 01-11-2012 to 31-12-2014. For a list of sources and a description of the preprocessing applied see the relevant section in the thesis. For newer data check ENTSOE's transparency website.
If you have proprietary forecast data, good for you. You will likely get marginally worse forecast performance using this data than the TSO forecasts, as the TSO's have pretty good forecast aggregation schemes (to the best of my knowledge). And remember, when forecasting DA prices you are not interested in how precise your forecast is compared to the realized fundamentals. You want to forecast the market participants expectations of fundamentals (which are difficult to know).

### data/forecasts-from-thesis.csv
Hold the forecasts during the test period (2014-01-01 to 2014-12-28).